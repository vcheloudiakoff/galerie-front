module Main exposing (..)

-- import Galerie.Object.Artwork as Artwork
-- import Html.Attributes exposing (alt, checked, href, src, type_)
-- import Html.Events exposing (onClick)

import BodyBuilder as B exposing (..)
import BodyBuilder.Attributes as A exposing (checked, href, label)
import BodyBuilder.Elements.Form exposing (CommonParams, buildInputText)
import BodyBuilder.Events exposing (onCheck, onClick, onMouseLeave, onMouseOver)
import BodyBuilder.Router as Router
    exposing
        ( History
        , Page
        , StandardHistoryMsg(..)
        , Transition
        , handleStandardHistory
        , historyView
        , initHistoryAndData
        , maybeTransitionSubscription
        , pageWithDefaultTransition
        , pageWithoutTransition
        , push
        )
import BodyBuilder.Style as Style
import Browser
import Browser.Events exposing (onKeyDown)
import Color
import Debug exposing (log)
import Elegant exposing (percent, px, vh, vw)
import Elegant.Block as Block
import Elegant.Box as Box
import Elegant.Cursor as Cursor
import Elegant.Dimensions as Dimensions
import Elegant.Display as Display
import Elegant.Extra
    exposing
        ( alignCenter
        , block
        , blockProperties
        , blockWithWidth
        , bold
        , border
        , box
        , cursorPointer
        , displayBlock
        , fontSize
        , grow
        , padding
        , paddingAll
        , paddingBottom
        , paddingHorizontal
        , paddingTop
        , paddingVertical
        , typoSize
        , typography
        )
import Elegant.Grid as Grid exposing (Repeatable(..), autofill)
import Elegant.Margin as Margin
import Elegant.Overflow as Overflow
import Elegant.Padding
import Elegant.Position as Position
import Elegant.Transform as Transform
import Elegant.Typography as Typography
import Galerie.InputObject
import Galerie.Mutation as Mutation
import Galerie.Object
import Galerie.Object.Artist as Artist
import Galerie.Object.Artwork as Artwork
import Galerie.Query as Query
import Graphql.Document as Document
import Graphql.Http exposing (..)
import Graphql.Http.GraphqlError
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helpers.ViewHelpers exposing (..)
import Html exposing (pre, text)
import Json.Decode as Decode
import List.Extra exposing (find)
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Modifiers exposing (..)
import PrintAny
import Regex
import RemoteData exposing (RemoteData)



---- PROGRAM ----


main : Program Flags Model Msg
main =
    B.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = chooseView (Document.serializeQuery rootQuery)
        }


init : Flags -> ( { data : Data, history : MyHistory }, Cmd Msg )
init _ =
    ( initHistoryAndData ArtistsIndex initData StandardHistoryWrapper, makeRequest )


initData : Data
initData =
    { hideAliases = False
    , response = RemoteData.Loading
    , toggleDebugView = False
    , artists = []
    , artworks = []
    , maybeHoveredArtistId = Nothing
    , artistInputType = draftArtistInputType
    , artworkInputType = draftArtworkInputType
    , maybeZoomedArtworkId = Nothing
    , error = ""
    }


draftArtworkInputType =
    { title = ""
    , image_url = ""
    , artist_id = Absent
    , width = Absent
    , height = Absent
    , description = Absent
    , exhibition_ids = Absent
    , support = Absent
    , techniques = Absent
    }


draftArtistInputType =
    { exhibition_ids = []
    , artwork_ids = []
    , first_name = ""
    , last_name = ""
    , nickname = ""
    , description = ""
    , preview_artwork_id = Absent
    }


type alias Flags =
    ()


type alias ArtistId =
    String


type alias ArtworkId =
    String


type alias ExhibitionId =
    String



---- ROUTER ----


type Route
    = ArtistsIndex
    | ArtistsShow ArtistId
    | ExhibitionsIndex
    | ExhibitionsShow ExhibitionId
    | GalerieIndex
    | ContactIndex
    | NewArtist
    | NewArtwork


type HistoryMsg
    = GoToArtistIndex
    | GoToArtistShow ArtistId
    | GoToExhibitionIndex
    | GoToExhibitionShow ExhibitionId
    | GoToGalerieMsg
    | GoToContactMsg
    | GoToNewArtist
    | GoToNewArtwork


handleHistory : HistoryMsg -> MyHistory -> MyHistory
handleHistory historyMsg history =
    case historyMsg of
        GoToArtistIndex ->
            history |> push (Router.pageWithoutTransition ArtistsIndex)

        GoToArtistShow id ->
            history |> push (Router.pageWithoutTransition (ArtistsShow id))

        GoToExhibitionIndex ->
            history |> push (Router.pageWithoutTransition ExhibitionsIndex)

        GoToExhibitionShow id ->
            history |> push (Router.pageWithoutTransition (ExhibitionsShow id))

        GoToGalerieMsg ->
            history |> push (Router.pageWithoutTransition GalerieIndex)

        GoToContactMsg ->
            history |> push (Router.pageWithoutTransition ContactIndex)

        GoToNewArtist ->
            history |> push (Router.pageWithoutTransition NewArtist)

        GoToNewArtwork ->
            history |> push (Router.pageWithoutTransition NewArtwork)


view : String -> Model -> NodeWithStyle Msg
view query ({ history, data } as model) =
    case data.error of
        "" ->
            B.div []
                [ ( Html.node "style" [] [ Html.text "body {display: grid} html {display: grid}" ], [] )
                , B.div
                    [ A.style
                        [ Style.box
                            [ Box.fontFamilySansSerif
                            ]
                        ]
                    ]
                    [ historyView (pageView query data) history ]
                ]

        error ->
            ( pre []
                [ error |> Html.text
                ]
            , []
            )


pageView : String -> Data -> Page Route Msg -> Maybe (Transition Route Msg) -> NodeWithStyle Msg
pageView query data { route } transition =
    case route of
        ArtistsIndex ->
            artistsIndex query data route

        ArtistsShow id ->
            artistsShow id data route

        ExhibitionsIndex ->
            exhibitionsIndex query data route

        ExhibitionsShow id ->
            exhibitionsShow id data route

        GalerieIndex ->
            galerieIndex query data route

        ContactIndex ->
            contactIndex query data route

        NewArtist ->
            newArtistView data route

        NewArtwork ->
            newArtworkView data route



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ maybeTransitionSubscription model.history
        , onKeyDown (Decode.map KeyDowns keyDecoder)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


errorDecoder : Decode.Decoder String
errorDecoder =
    Decode.field "error" Decode.string



---- MODEL ----


type Msg
    = HistoryMsgWrapper HistoryMsg
    | StandardHistoryWrapper StandardHistoryMsg
    | ToggleDebugView Bool
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | MouseArtistHover ArtistId
    | RemoveHover
    | ToggleAliases Bool
    | SendBufferArtist
    | SendBufferArtwork
    | GotArtist (RemoteData (Graphql.Http.Error (Maybe ArtistLookup)) (Maybe ArtistLookup))
    | GotArtwork (RemoteData (Graphql.Http.Error (Maybe ArtworkLookup)) (Maybe ArtworkLookup))
    | ZoomArtwork ArtworkId
    | KeyDowns String
    | ArtistBufferWrapper (ArtistBuffer String)



-- | ChangeBuffer (BufferWrapper String)
-- type BufferWrapper str
--     = ArtistBuffer str


type ArtistBuffer str
    = ArtistNickName str
    | ArtistFirstName str
    | ArtistLastName str
    | ArtistDescription str


type alias Data =
    { response : RemoteData (Graphql.Http.Error Response) Response
    , hideAliases : Bool
    , toggleDebugView : Bool
    , artists : List ArtistLookup
    , artworks : List ArtworkLookup
    , maybeHoveredArtistId : Maybe ArtistId
    , artistInputType : Galerie.InputObject.ArtistInputType
    , artworkInputType : Galerie.InputObject.ArtworkInputType
    , maybeZoomedArtworkId : Maybe ArtworkId
    , error : String
    }


type alias MyHistory =
    History Route Msg


type alias Model =
    { history : MyHistory
    , data : Data
    }


type alias Response =
    { artists : List ArtistLookup
    , artworks : List ArtworkLookup
    }


type alias ArtworkLookup =
    { id : ArtworkId
    , image_url : String
    , title : String
    }


type alias ArtistLookup =
    { id : String
    , nickname : String
    , firstname : String
    , lastname : String
    , previewArtwork : Maybe ArtworkLookup
    , artworks : List ArtworkLookup
    , description : String
    }


type alias ArtistWithArtworks =
    { id : String
    , nickname : String
    , firstname : String
    , lastname : String
    , artworks : ( Nonempty ArtworkLookup, ArtworkLookup )
    , description : String
    }


artistSelector : SelectionSet ArtistLookup Galerie.Object.Artist
artistSelector =
    SelectionSet.map7 ArtistLookup
        Artist.id
        Artist.nickname
        Artist.first_name
        Artist.last_name
        (Artist.preview_artwork artworkSelector)
        (Artist.artworks artworkSelector)
        Artist.description


artworkSelector : SelectionSet ArtworkLookup Galerie.Object.Artwork
artworkSelector =
    SelectionSet.map3 ArtworkLookup
        Artwork.id
        Artwork.image_url
        Artwork.title



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data

        artistInputType =
            data.artistInputType
    in
    case msg of
        HistoryMsgWrapper historyMsg ->
            ( { model | history = handleHistory historyMsg model.history, data = { data | maybeHoveredArtistId = Nothing } }, Cmd.none )

        StandardHistoryWrapper historyMsg ->
            model |> handleStandardHistory historyMsg

        GotArtist artistResponse ->
            ( model, Cmd.none )

        GotArtwork artworkResponse ->
            ( model, Cmd.none )

        -- case artistResponse of
        --     RemoteData.Success receiveData ->
        --         ( { model | data = { data | response = response, artists = receiveData.artists } }, Cmd.none )
        --     _ ->
        --         ( { model | data = { data | response = response } }, Cmd.none )
        GotResponse response ->
            case response of
                RemoteData.Success receiveData ->
                    ( { model
                        | data =
                            { data
                                | response = response
                                , artists = receiveData.artists
                                , artworks = receiveData.artworks
                            }
                      }
                    , Cmd.none
                    )

                RemoteData.Failure error ->
                    case error of
                        Graphql.Http.HttpError httpError ->
                            case httpError of
                                BadUrl str ->
                                    ( { model | data = { data | error = "BadUrl : " ++ str } }, Cmd.none )

                                Timeout ->
                                    ( { model | data = { data | error = "Timeout, la requete à mon trop de temps, veuillez recharger le navigateur" } }, Cmd.none )

                                NetworkError ->
                                    ( { model | data = { data | error = "Vous n'êtes plus connecté à internet" } }, Cmd.none )

                                BadStatus metadata str ->
                                    ( { model | data = { data | error = "BadStatus : " ++ str } }, Cmd.none )

                                BadPayload err ->
                                    ( { model | data = { data | error = "BadPayload : " ++ Decode.errorToString err } }, Cmd.none )

                        Graphql.Http.GraphqlError (Graphql.Http.GraphqlError.ParsedData parsedData) graphqlErrors ->
                            ( { model | data = { data | error = "parsedData err, still need to implement this error" } }, Cmd.none )

                        Graphql.Http.GraphqlError (Graphql.Http.GraphqlError.UnparsedData _) _ ->
                            ( { model | data = { data | error = "unparsedData, still need to implement this error" } }, Cmd.none )

                _ ->
                    ( { model | data = { data | response = response } }, Cmd.none )

        ToggleAliases bool ->
            ( { model | data = { data | hideAliases = bool } }, Cmd.none )

        ToggleDebugView bool ->
            ( { model | data = { data | toggleDebugView = bool } }, Cmd.none )

        MouseArtistHover artistId ->
            ( { model | data = { data | maybeHoveredArtistId = Just artistId } }, Cmd.none )

        RemoveHover ->
            ( { model | data = { data | maybeHoveredArtistId = Nothing } }, Cmd.none )

        ZoomArtwork artworkId ->
            ( { model | data = { data | maybeZoomedArtworkId = Just artworkId } }, Cmd.none )

        -- ChangeBuffer bufferWrapper ->
        --     ( { model
        --         | data = changeBuffer model.data bufferWrapper
        --       }
        --     , Cmd.none
        --     )
        ArtistBufferWrapper artistBuffer ->
            ( { model
                | data =
                    { data
                        | artistInputType =
                            case artistBuffer of
                                ArtistNickName nickName ->
                                    { artistInputType | nickname = nickName }

                                ArtistFirstName firstName ->
                                    { artistInputType | first_name = firstName }

                                ArtistLastName lastName ->
                                    { artistInputType | last_name = lastName }

                                ArtistDescription description ->
                                    { artistInputType | description = description }
                    }
              }
            , Cmd.none
            )

        SendBufferArtist ->
            ( model, createArtist data )

        SendBufferArtwork ->
            ( model, createArtwork data )

        KeyDowns code ->
            ( if code == "Escape" then
                { model | data = { data | maybeZoomedArtworkId = Nothing } }

              else
                model
            , Cmd.none
            )


makeRequest : Cmd Msg
makeRequest =
    rootQuery
        |> Graphql.Http.queryRequest endPoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


rootQuery : SelectionSet Response RootQuery
rootQuery =
    SelectionSet.map2 Response
        (Query.artists (\n -> { n | order_by = Present "nickname asc" }) artistSelector)
        (Query.artworks (\n -> n) artworkSelector)


createArtist : Data -> Cmd Msg
createArtist data =
    sendBufferArtist data
        |> Graphql.Http.mutationRequest endPoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotArtist)


createArtwork : Data -> Cmd Msg
createArtwork data =
    sendBufferArtwork data
        |> Graphql.Http.mutationRequest endPoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotArtwork)


endPoint : String
endPoint =
    "https://galerie-api.herokuapp.com/v1/graphql"



---- VIEWS ----
--Choose View/debug


debugView : String -> Data -> NodeWithStyle Msg
debugView query data =
    B.div []
        [ B.div []
            [ B.p [] [ toggleDebugViewCheckbox data ]
            , B.h1 [] [ B.text "Generated Query" ]
            , B.p [] [ toggleAliasesCheckbox data ]
            , ( pre []
                    [ (if data.hideAliases then
                        query
                            |> stripAliases

                       else
                        query
                      )
                        |> Html.text
                    ]
              , []
              )
            ]
        , div []
            [ B.h1 [] [ B.text "Response" ]
            , ( log "SUBMODEL" data |> PrintAny.view, [] )
            ]
        ]


toggleAliasesCheckbox : Data -> NodeWithStyle Msg
toggleAliasesCheckbox data =
    B.div []
        [ B.inputCheckbox [ onCheck ToggleAliases, checked data.hideAliases ]
        , B.text " Show Aliases "
        , B.a [ href "https://github.com/dillonkearns/elm-graphql/blob/master/FAQ.md#how-do-field-aliases-work-in-dillonkearnselm-graphql" ]
            [ B.text "(?)"
            ]
        ]


toggleDebugViewCheckbox : Data -> NodeWithStyle Msg
toggleDebugViewCheckbox data =
    B.div []
        [ B.inputCheckbox [ onCheck ToggleDebugView, checked data.toggleDebugView ]
        , B.text " Change view "
        ]


stripAliases : String -> String
stripAliases query =
    query
        |> Regex.replace
            (Regex.fromStringWith { multiline = True, caseInsensitive = True } "^(\\s*)\\w+: "
                |> Maybe.withDefault Regex.never
            )
            (\match -> match.submatches |> List.head |> Maybe.withDefault Nothing |> Maybe.withDefault "")


chooseView : String -> Model -> NodeWithStyle Msg
chooseView query model =
    if model.data.toggleDebugView then
        debugView query model.data

    else
        view query model



---- ARTISTS ----
-- HEADER --


headerViewRow currentRoute =
    pxRow 168
        []
        [ horizontalLayout []
            [ pxColumn 128 [] [ B.img "galerie cheloudiakoff" "public/logo.svg" [ cursorPointer, positionFixed, onClick <| HistoryMsgWrapper GoToArtistIndex ] ]
            , fillColumn [] []
            , autoColumn []
                [ horizontalCenteredLayout []
                    [ buttonNav currentRoute GoToArtistIndex [ B.text "ARTISTES" ]
                    , buttonNav currentRoute GoToExhibitionIndex [ B.text "EXPOSITIONS" ]
                    , buttonNav currentRoute GoToGalerieMsg [ B.text "GALERIE" ]
                    , buttonNav currentRoute GoToContactMsg [ B.text "CONTACT" ]
                    ]
                ]
            ]
        ]


buttonNav : Route -> HistoryMsg -> List (NodeWithStyle Msg) -> CustomGridItem Msg
buttonNav currentRoute historyMsg content =
    pxColumn 156
        [ A.style [ Style.box ([ Box.cursorPointer ] ++ addBoldIfRouteMatches currentRoute historyMsg) ]
        , onClick <| HistoryMsgWrapper historyMsg
        ]
        [ span [ positionFixed ] content ]


positionFixed : Modifier (A.BoxContainer a)
positionFixed =
    A.style [ Style.box [ Box.position (Position.fixed []) ] ]


backButton : NodeWithStyle Msg
backButton =
    B.div
        [ cursorPointer
        , positionFixed
        , onClick <| StandardHistoryWrapper Back
        ]
        [ B.text "< Retour" ]


addArtistButton : NodeWithStyle Msg
addArtistButton =
    B.div [ onClick <| HistoryMsgWrapper GoToNewArtist ] [ B.text "+ Ajouter un artiste" ]


addArtworkButton : NodeWithStyle Msg
addArtworkButton =
    B.div [ onClick <| HistoryMsgWrapper GoToNewArtwork ] [ B.text "+ Ajouter une oeuvre" ]


addBoldIfRouteMatches currentRoute historyMsg =
    case historyMsg of
        GoToArtistIndex ->
            case currentRoute of
                ArtistsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ArtistsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        GoToArtistShow _ ->
            case currentRoute of
                ArtistsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ArtistsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        GoToExhibitionIndex ->
            case currentRoute of
                ExhibitionsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ExhibitionsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        GoToExhibitionShow _ ->
            case currentRoute of
                ExhibitionsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ExhibitionsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        GoToGalerieMsg ->
            case currentRoute of
                GalerieIndex ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        GoToContactMsg ->
            case currentRoute of
                ContactIndex ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        _ ->
            []



-- INDEX --


artistsIndex : String -> Data -> Route -> NodeWithStyle Msg
artistsIndex query data route =
    B.div []
        [ B.p [] [ toggleDebugViewCheckbox data ]
        , verticalLayout []
            [ headerViewRow route
            , autoRow [] [ horizontallyCentered [ addArtistButton ] ]
            , fillRow []
                [ B.div [ A.style [ Style.box [ Box.paddingTop (px 96), Box.paddingHorizontal (px 100) ] ] ]
                    [ B.grid
                        [ displayBlock
                        , fillHeight
                        , gridContainerProperties
                            [ Grid.columns
                                [ Grid.template
                                    [ Repeat Grid.autofill [ px 288 ] ]
                                , Grid.alignItems (Grid.alignWrapper Grid.center)
                                , Grid.gap (px 76)
                                ]
                            , Grid.rows
                                [ Grid.template
                                    [ Repeat Grid.autofill [ px 288 ] ]
                                , Grid.align Grid.center
                                , Grid.gap (px 76)
                                ]
                            ]
                        ]
                        (List.map (showPreviewArtwork data.maybeHoveredArtistId) (getArtistsWithArtworks data.artists))
                    ]
                ]
            ]
        ]


getArtistsWithArtworks : List ArtistLookup -> List ArtistWithArtworks
getArtistsWithArtworks artists =
    List.concatMap
        (\artist ->
            case ( List.Nonempty.fromList artist.artworks, artist.previewArtwork ) of
                ( Nothing, _ ) ->
                    []

                ( Just artworks, maybeArtwork ) ->
                    [ ArtistWithArtworks
                        artist.id
                        artist.nickname
                        artist.firstname
                        artist.lastname
                        ( artworks
                        , Maybe.withDefault (List.Nonempty.head artworks) maybeArtwork
                        )
                        artist.description
                    ]
        )
        artists


artistsShow : ArtistId -> Data -> Route -> NodeWithStyle Msg
artistsShow artistId data route =
    let
        maybeArtist =
            find (\artist -> artist.id == artistId) data.artists

        ( artworkZoomed, styleOfLayout ) =
            case data.maybeZoomedArtworkId of
                Nothing ->
                    ( B.div [] [], [] )

                Just zoomedArtworkId ->
                    case find (\artwork -> artwork.id == zoomedArtworkId) data.artworks of
                        Nothing ->
                            ( B.div [] [], [] )

                        Just zoomedArtwork ->
                            -- set fixed image in middle of screen + style OPACITY
                            -- ( B.div
                            --     [ A.style
                            --         [ Style.blockProperties
                            --             [ Display.dimensions [ Dimensions.width (percent 100), Dimensions.height (percent 100) ]
                            --             ]
                            --         , Style.box
                            --             [ Box.zIndex 1
                            --             , Box.position (Position.absolute [])
                            --             ]
                            --         ]
                            --     ]
                            --     [ B.img ""
                            --         zoomedArtwork.image_url
                            --         [ A.style [ Style.box [ Box.margin [ Margin.bottom <| Margin.width (px 20) ] ] ] ]
                            --     ]
                            ( verticalLayout
                                [ A.style
                                    [ Style.block
                                        [ Block.width (vw 100)
                                        , Block.height (vh 100)
                                        ]
                                    ]
                                ]
                                [ fillRow [] []
                                , autoRow []
                                    [ horizontalLayout []
                                        [ fillColumn [] []
                                        , autoColumn []
                                            [ B.div
                                                [ A.style
                                                    [ Style.box
                                                        [ Box.zIndex 1
                                                        , Box.position (Position.absolute [])
                                                        ]
                                                    ]
                                                ]
                                                [ B.img "" zoomedArtwork.image_url []
                                                ]
                                            ]
                                        , fillColumn [] []
                                        ]
                                    ]
                                , fillRow [] []
                                ]
                            , [ A.style [ Style.box [ Box.opacity 0.55 ] ] ]
                            )
    in
    B.div []
        [ artworkZoomed
        , verticalLayout styleOfLayout
            [ headerViewRow route
            , autoRow [] [ horizontallyCentered [ addArtworkButton ] ]
            , fillRow []
                [ B.div [ A.style [ Style.box [ Box.paddingTop (px 96), Box.paddingHorizontal (px 100) ] ] ]
                    [ backButton
                    , case maybeArtist of
                        Just artist ->
                            horizontalLayout
                                []
                                [ autoColumn []
                                    [ verticalLayout []
                                        [ autoRow []
                                            [ showArtistArtworks (artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks) ]
                                        ]
                                    ]
                                , column (Grid.sizeUnitVal (percent 42))
                                    []
                                    [ verticalLayout []
                                        [ autoRow []
                                            [ artistDescription artist ]
                                        ]
                                    ]
                                ]

                        Nothing ->
                            B.div [] [ B.text "L'artiste n'existe pas, veuillez recharger la page." ]
                    ]
                ]
            ]
        ]


showArtistArtworks : List ArtworkLookup -> NodeWithStyle Msg
showArtistArtworks artworks =
    B.div
        []
        [ B.grid
            [ fillHeight
            , paddingVertical (px 40)
            , gridContainerProperties
                [ Grid.columns
                    [ Grid.template
                        [ Repeat Grid.autofill [ px 348 ] ]
                    , Grid.alignItems (Grid.alignWrapper Grid.center)
                    , Grid.gap (px 76)
                    ]
                , Grid.rows
                    [ Grid.template
                        [ Repeat Grid.autofill [ px 230 ] ]
                    , Grid.align Grid.center
                    , Grid.gap (px 76)
                    ]
                ]
            ]
            (List.map showArtwork artworks)
        ]


artistDescription : ArtistLookup -> NodeWithStyle Msg
artistDescription artist =
    B.div [ positionFixed, A.style [ Style.box [ Box.margin [ Margin.left <| Margin.width (px 60), Margin.right <| Margin.width (px 126) ] ] ] ] [ B.text artist.description ]


showPreviewArtwork : Maybe ArtistId -> ArtistWithArtworks -> GridItem Msg
showPreviewArtwork maybeHoveredArtistId artistWithArtworks =
    let
        ( artworks, previewArtwork ) =
            artistWithArtworks.artworks

        hover =
            case maybeHoveredArtistId of
                Just hoveredArtistId ->
                    hoveredArtistId == artistWithArtworks.id

                Nothing ->
                    False
    in
    B.gridItem []
        [ B.div
            ((if hover then
                onMouseLeave RemoveHover

              else
                onMouseOver (MouseArtistHover artistWithArtworks.id)
             )
                :: [ A.style
                        [ Style.box [ Box.position (Position.relative [ Position.all (px 0) ]), Box.cursorPointer ] ]
                   , onClick (HistoryMsgWrapper <| GoToArtistShow artistWithArtworks.id)
                   ]
            )
            ((if hover then
                B.div
                    [ A.style
                        [ Style.box
                            [ Box.position
                                (Position.absolute
                                    [ Position.top (percent 50)
                                    , Position.left (percent 50)
                                    ]
                                )
                            , Box.transform
                                [ Transform.translateX (percent -50)
                                , Transform.translateY (percent -50)
                                ]
                            , Box.textColor <|
                                Color.rgb 0 0 0
                            ]
                        ]
                    ]
                    [ B.text artistWithArtworks.nickname ]

              else
                B.div [] []
             )
                :: [ B.img ""
                        previewArtwork.image_url
                        [ A.style
                            ([ Style.block [ Block.width (percent 100) ]
                             ]
                                ++ pseudoClassArtistHoverBoxStyle hover
                            )
                        ]
                   ]
            )
        ]


showArtwork : ArtworkLookup -> GridItem Msg
showArtwork artwork =
    B.gridItem []
        [ B.div
            [ A.style
                [ Style.box [ Box.position (Position.relative [ Position.all (px 0) ]), Box.cursorPointer ]
                ]
            , onClick <| ZoomArtwork artwork.id
            ]
            [ B.img ""
                artwork.image_url
                [ A.style
                    [ Style.block [ Block.width (percent 100) ]
                    ]
                ]
            ]
        ]


pseudoClassArtistHoverBoxStyle hover =
    if hover then
        [ Style.box [ Box.opacity 0.55 ] ]

    else
        []



-- SHOW


exhibitionsIndex query data route =
    verticalLayout []
        [ headerViewRow route ]


exhibitionsShow exhibitionId data route =
    verticalLayout []
        [ headerViewRow route ]


galerieIndex query data route =
    verticalLayout []
        [ headerViewRow route ]


contactIndex query data route =
    verticalLayout []
        [ headerViewRow route ]


newArtistView : Data -> Route -> NodeWithStyle Msg
newArtistView data route =
    verticalLayout []
        [ headerViewRow route
        , artistForm
        ]


newArtworkView : Data -> Route -> NodeWithStyle Msg
newArtworkView data route =
    verticalLayout []
        [ headerViewRow route
        , artworkForm
        ]



-- changeBuffer : Data -> BufferWrapper String -> Data
-- changeBuffer data buffer =
--     let
--         artistInputType =
--             data.artistInputType
-- in
-- case buffer of
--     ArtistNickName nickname ->
--         { data
--             | artistInputType = { artistInputType | nickname = nickname }
--         }
--     ArtistFirstName firstname ->
--         { data
--             | artistInputType = { artistInputType | firstname = firstname }
--         }
--     ArtistLastName lastname ->
--         { data
--             | artistInputType = { artistInputType | lastname = lastname }
--         }
--     ArtistDescription description ->
--         { data
--             | artistInputType = { artistInputType | description = description }
--         }


artworkForm =
    autoRow []
        [ B.div
            [ A.style [ Style.box [ Box.cursorPointer ] ]
            , onClick <| SendBufferArtwork
            ]
            [ B.text "Sauvegarder l'oeuvre" ]
        ]


artistForm =
    autoRow []
        [ buildInputText
            { label = "Surnom d'artiste"
            , placeholder = Just "Saype"
            , error = Nothing
            }
            ""
            (ArtistBufferWrapper << ArtistNickName)
        , buildInputText
            { label = "Prénom"
            , placeholder = Just "Jean"
            , error = Nothing
            }
            ""
            (ArtistBufferWrapper << ArtistFirstName)
        , buildInputText
            { label = "Nom"
            , placeholder = Just "Peinture"
            , error = Nothing
            }
            ""
            (ArtistBufferWrapper << ArtistLastName)
        , buildInputText
            { label = "Description"
            , placeholder = Just "Description"
            , error = Nothing
            }
            ""
            (ArtistBufferWrapper << ArtistDescription)

        -- (ChangeBuffer ArtistNickName)
        -- , buildInputText
        --     { label = "Prenom"
        --     , placeholder = Just "Julian Beever"
        --     , error = Nothing
        --     }
        --     "firstname"
        --     (ChangeBuffer ArtistFirstName)
        -- , buildInputText
        --     { label = "Nom"
        --     , placeholder = Just "Julian Beever"
        --     , error = Nothing
        --     }
        --     "lastname"
        --     (ChangeBuffer ArtistLastName)
        -- , buildInputText
        --     { label = "Description"
        --     , placeholder = Just "Julian Beever"
        --     , error = Nothing
        --     }
        --     "description"
        --     (ChangeBuffer ArtistDescription)
        , B.div
            [ A.style [ Style.box [ Box.cursorPointer ] ]
            , onClick <| SendBufferArtist
            ]
            [ B.text "Sauvegarder l'artiste" ]
        ]


sendBufferArtist data =
    Mutation.create_artist { artist = data.artistInputType } artistSelector


sendBufferArtwork data =
    Mutation.create_artwork { artwork = data.artworkInputType } artworkSelector

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
import Color
import Debug exposing (log)
import Elegant exposing (percent, px, vw)
import Elegant.Block as Block
import Elegant.Box as Box
import Elegant.Cursor as Cursor
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
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helpers.ViewHelpers exposing (..)
import Html exposing (pre, text)
import List.Extra exposing (find)
import Maybe.Extra
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
    , maybeHoveredArtistId = Nothing
    , artistInputType = draftArtistInputType
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


type HistoryMsg
    = GoToArtistIndex
    | GoToArtistShow ArtistId
    | GoToExhibitionIndex
    | GoToExhibitionShow ExhibitionId
    | GoToGalerieMsg
    | GoToContactMsg
    | GoToNewArtist


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


view : String -> Model -> NodeWithStyle Msg
view query ({ history, data } as model) =
    B.div
        [ A.style
            [ Style.box
                [ Box.fontFamilySansSerif
                ]
            ]
        ]
        [ historyView (pageView query data) history ]


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



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    maybeTransitionSubscription model.history



---- MODEL ----


type Msg
    = HistoryMsgWrapper HistoryMsg
    | StandardHistoryWrapper StandardHistoryMsg
    | ToggleDebugView Bool
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | MouseArtistHover ArtistId
    | MouseArtistLeave
    | ToggleAliases Bool
    | ChangeBufferNickName String
    | SendBufferArtist
    | GotArtist (RemoteData (Graphql.Http.Error (Maybe ArtistLookup)) (Maybe ArtistLookup))


type alias Data =
    { response : RemoteData (Graphql.Http.Error Response) Response
    , hideAliases : Bool
    , toggleDebugView : Bool
    , artists : List ArtistLookup
    , maybeHoveredArtistId : Maybe ArtistId
    , artistInputType : Galerie.InputObject.ArtistInputType
    }


type alias MyHistory =
    History Route Msg


type alias Model =
    { history : MyHistory
    , data : Data
    }


type alias Response =
    { artists : List ArtistLookup
    }


type alias ArtworkLookup =
    { image_url : String
    }


type alias ArtistLookup =
    { nickname : String
    , id : String
    , previewArtwork : Maybe ArtworkLookup
    , artworks : List ArtworkLookup
    , description : String
    }


artistSelector : SelectionSet ArtistLookup Galerie.Object.Artist
artistSelector =
    SelectionSet.map5 ArtistLookup
        Artist.nickname
        Artist.id
        (Artist.preview_artwork artworkSelector)
        (Artist.artworks artworkSelector)
        Artist.description


artworkSelector : SelectionSet ArtworkLookup Galerie.Object.Artwork
artworkSelector =
    SelectionSet.map ArtworkLookup
        Artwork.image_url



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data
    in
    case msg of
        HistoryMsgWrapper historyMsg ->
            ( { model | history = handleHistory historyMsg model.history }, Cmd.none )

        StandardHistoryWrapper historyMsg ->
            model |> handleStandardHistory historyMsg

        GotArtist artistResponse ->
            ( model, Cmd.none )

        -- case artistResponse of
        --     RemoteData.Success receiveData ->
        --         ( { model | data = { data | response = response, artists = receiveData.artists } }, Cmd.none )
        --     _ ->
        --         ( { model | data = { data | response = response } }, Cmd.none )
        GotResponse response ->
            case response of
                RemoteData.Success receiveData ->
                    ( { model | data = { data | response = response, artists = receiveData.artists } }, Cmd.none )

                _ ->
                    ( { model | data = { data | response = response } }, Cmd.none )

        ToggleAliases bool ->
            ( { model | data = { data | hideAliases = bool } }, Cmd.none )

        ToggleDebugView bool ->
            ( { model | data = { data | toggleDebugView = bool } }, Cmd.none )

        MouseArtistHover artistId ->
            ( { model | data = { data | maybeHoveredArtistId = Just artistId } }, Cmd.none )

        MouseArtistLeave ->
            ( { model | data = { data | maybeHoveredArtistId = Nothing } }, Cmd.none )

        ChangeBufferNickName nickname ->
            let
                artistInputType =
                    data.artistInputType
            in
            ( { model
                | data =
                    { data
                        | artistInputType = { artistInputType | nickname = nickname }
                    }
              }
            , Cmd.none
            )

        SendBufferArtist ->
            ( model, createArtist data )


makeRequest : Cmd Msg
makeRequest =
    rootQuery
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


rootQuery : SelectionSet Response RootQuery
rootQuery =
    SelectionSet.map Response
        (Query.artists (\n -> { n | order_by = Present "nickname asc" }) artistSelector)


createArtist : Data -> Cmd Msg
createArtist data =
    sendBufferArtist data
        |> Graphql.Http.mutationRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotArtist)



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
            [ pxColumn 128 [] [ B.img "galerie cheloudiakoff" "public/logo.svg" [] ]
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
buttonNav currentRoute historyMsg =
    pxColumn 156
        [ A.style [ Style.box ([ Box.cursorPointer ] ++ addBoldIfRouteMatches currentRoute historyMsg) ]
        , onClick <| HistoryMsgWrapper historyMsg
        ]


backButton : NodeWithStyle Msg
backButton =
    B.div
        [ A.style [ Style.box [ Box.cursorPointer ] ]
        , onClick <| StandardHistoryWrapper Back
        ]
        [ B.text "< Retour" ]


addArtistButton : NodeWithStyle Msg
addArtistButton =
    B.div [ onClick <| HistoryMsgWrapper GoToNewArtist ] [ B.text "+ Ajouter un artiste" ]


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
    let
        artists =
            List.filter (\artist -> Maybe.Extra.isJust artist.previewArtwork) data.artists
    in
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
                        (List.map (showPreviewArtwork data.maybeHoveredArtistId) artists)
                    ]
                ]
            ]
        ]


artistsShow : ArtistId -> Data -> Route -> NodeWithStyle Msg
artistsShow artistId data route =
    let
        maybeArtist =
            find (\a -> a.id == artistId) data.artists
    in
    verticalLayout []
        [ headerViewRow route
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
                                        [ showArtistArtworks (artist.artworks ++ artist.artworks ++ artist.artworks ++ artist.artworks) ]
                                    ]
                                ]
                            , pxColumn 516
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


showArtistArtworks artworks =
    B.div [ A.style [ Style.box [ Box.paddingHorizontal (px 100) ] ] ]
        [ B.grid
            [ displayBlock
            , fillHeight
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
    B.div [] [ B.text artist.description ]


showPreviewArtwork : Maybe ArtistId -> ArtistLookup -> GridItem Msg
showPreviewArtwork maybeHoveredArtistId artist =
    let
        hover =
            case maybeHoveredArtistId of
                Just hoveredArtistId ->
                    hoveredArtistId == artist.id

                Nothing ->
                    False
    in
    B.gridItem []
        [ case artist.previewArtwork of
            Just previewArtwork ->
                B.div
                    ((if hover then
                        onMouseLeave MouseArtistLeave

                      else
                        onMouseOver (MouseArtistHover artist.id)
                     )
                        :: [ A.style
                                [ Style.box [ Box.position (Position.relative [ Position.all (px 0) ]), Box.cursorPointer ] ]
                           , onClick (HistoryMsgWrapper <| GoToArtistShow artist.id)
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
                            [ B.text artist.nickname ]

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

            Nothing ->
                B.div [] []
        ]


showArtwork : ArtworkLookup -> GridItem Msg
showArtwork artwork =
    B.gridItem []
        [ B.div
            [ A.style [ Style.box [ Box.position (Position.relative [ Position.all (px 0) ]), Box.cursorPointer ] ]

            -- , onClick (HistoryMsgWrapper <| GoToArtistShow artist.id)
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


artistForm =
    autoRow []
        [ buildInputText
            { label = "Surnom d'artiste"
            , placeholder = Just "Julian Beever"
            , error = Nothing
            }
            "nickname"
            ChangeBufferNickName
        , B.div
            [ A.style [ Style.box [ Box.cursorPointer ] ]
            , onClick <| SendBufferArtist
            ]
            [ B.text "Sauvegarder l'artiste" ]
        ]


sendBufferArtist data =
    Mutation.create_artist { artist = data.artistInputType } artistSelector

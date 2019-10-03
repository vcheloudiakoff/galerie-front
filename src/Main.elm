module Main exposing (..)

-- import Galerie.Object.Artwork as Artwork
-- import Html.Attributes exposing (alt, checked, href, src, type_)
-- import Html.Events exposing (onClick)

import BodyBuilder as B exposing (..)
import BodyBuilder.Attributes as A exposing (checked, href, label)
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
    }


type alias Flags =
    ()


type alias ArtistId =
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


type HistoryMsg
    = ArtistIndex
    | ArtistShow ArtistId
    | ExhibitionIndex
    | ExhibitionShow ExhibitionId
    | GalerieMsg
    | ContactMsg


handleHistory : HistoryMsg -> MyHistory -> MyHistory
handleHistory route history =
    case route of
        ArtistIndex ->
            history |> push (Router.pageWithoutTransition ArtistsIndex)

        ArtistShow id ->
            history |> push (Router.pageWithoutTransition (ArtistsShow id))

        ExhibitionIndex ->
            history |> push (Router.pageWithoutTransition ExhibitionsIndex)

        ExhibitionShow id ->
            history |> push (Router.pageWithoutTransition (ExhibitionsShow id))

        GalerieMsg ->
            history |> push (Router.pageWithoutTransition GalerieIndex)

        ContactMsg ->
            history |> push (Router.pageWithoutTransition ContactIndex)


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


type alias Data =
    { response : RemoteData (Graphql.Http.Error Response) Response
    , hideAliases : Bool
    , toggleDebugView : Bool
    , artists : List ArtistLookup
    , maybeHoveredArtistId : Maybe ArtistId
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
    , preview_artwork : Maybe ArtworkLookup
    }


artistSelector : SelectionSet ArtistLookup Galerie.Object.Artist
artistSelector =
    SelectionSet.map3 ArtistLookup
        Artist.nickname
        Artist.id
        (Artist.preview_artwork artworkSelector)


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

        HistoryMsgWrapper historyMsg ->
            ( { model | history = handleHistory historyMsg model.history }, Cmd.none )

        StandardHistoryWrapper historyMsg ->
            model |> handleStandardHistory historyMsg


makeRequest : Cmd Msg
makeRequest =
    rootQuery
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


rootQuery : SelectionSet Response RootQuery
rootQuery =
    SelectionSet.map Response
        (Query.artists (\n -> { n | order_by = Present "nickname asc" }) artistSelector)



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
                    [ buttonNav currentRoute ArtistIndex [ B.text "ARTISTES" ]
                    , buttonNav currentRoute ExhibitionIndex [ B.text "EXPOSITIONS" ]
                    , buttonNav currentRoute GalerieMsg [ B.text "GALERIE" ]
                    , buttonNav currentRoute ContactMsg [ B.text "CONTACT" ]
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


addBoldIfRouteMatches currentRoute historyMsg =
    case historyMsg of
        ArtistIndex ->
            case currentRoute of
                ArtistsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ArtistsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        ArtistShow _ ->
            case currentRoute of
                ArtistsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ArtistsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        ExhibitionIndex ->
            case currentRoute of
                ExhibitionsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ExhibitionsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        ExhibitionShow _ ->
            case currentRoute of
                ExhibitionsIndex ->
                    [ Box.typography [ Typography.bold ] ]

                ExhibitionsShow _ ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        GalerieMsg ->
            case currentRoute of
                GalerieIndex ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []

        ContactMsg ->
            case currentRoute of
                ContactIndex ->
                    [ Box.typography [ Typography.bold ] ]

                _ ->
                    []



-- INDEX --


artistsIndex : String -> Data -> Route -> NodeWithStyle Msg
artistsIndex query data route =
    let
        artists =
            data.artists
    in
    B.div []
        [ B.p [] [ toggleDebugViewCheckbox data ]
        , verticalLayout []
            [ headerViewRow route
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
                        (List.map (showPreviewArtwork data.maybeHoveredArtistId) (artists ++ artists ++ artists))
                    ]
                ]
            ]
        ]


artistsShow : ArtistId -> Data -> Route -> NodeWithStyle Msg
artistsShow artistId data route =
    let
        maybeArtist =
            find (\artist -> artist.id == artistId) data.artists
    in
    verticalLayout []
        [ headerViewRow route
        , fillRow []
            [ B.div [ A.style [ Style.box [ Box.paddingTop (px 96), Box.paddingHorizontal (px 100) ] ] ]
                [ backButton
                , horizontalLayout
                    []
                    [ autoColumn []
                        [ verticalLayout []
                            [ autoRow []
                                [ showArtistArtwork ]
                            ]
                        ]
                    , autoColumn []
                        [ verticalLayout []
                            [ autoRow []
                                [ artistDescription ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


showArtistArtwork =
    B.div [] []


artistDescription =
    B.div [] []


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
        [ case artist.preview_artwork of
            Just preview_artwork ->
                B.div
                    ((if hover then
                        onMouseLeave MouseArtistLeave

                      else
                        onMouseOver (MouseArtistHover artist.id)
                     )
                        :: [ A.style
                                [ Style.box [ Box.position (Position.relative [ Position.all (px 0) ]), Box.cursorPointer ] ]
                           , onClick (HistoryMsgWrapper <| ArtistShow artist.id)
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
                                preview_artwork.image_url
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

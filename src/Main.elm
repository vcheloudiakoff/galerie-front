module Main exposing (..)

-- import Galerie.Object.Artwork as Artwork
-- import Html.Attributes exposing (alt, checked, href, src, type_)
-- import Html.Events exposing (onClick)

import BodyBuilder as B exposing (..)
import BodyBuilder.Attributes as A exposing (checked, href, label)
import BodyBuilder.Events exposing (onCheck, onClick, onMouseLeave, onMouseOver)
import BodyBuilder.Style as Style
import Browser
import Color
import Debug exposing (log)
import Elegant exposing (percent, px)
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
import Elegant.Position as Position
import Elegant.Transform as Transform
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
import PrintAny
import Regex
import RemoteData exposing (RemoteData)



---- MODEL ----


type Msg
    = ToggleAliases Bool
    | ToggleDebugView Bool
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | MouseArtistHover ArtistId
    | MouseArtistLeave


type alias Model =
    { hideAliases : Bool
    , response : RemoteData (Graphql.Http.Error Response) Response
    , toggleDebugView : Bool
    , artists : List ArtistLookup
    , maybeHoveredArtistId : Maybe ArtistId
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
        (Artist.preview_artwork preview_artwork_selector)


preview_artwork_selector : SelectionSet ArtworkLookup Galerie.Object.Artwork
preview_artwork_selector =
    SelectionSet.map ArtworkLookup
        Artwork.image_url



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            case response of
                RemoteData.Success data ->
                    ( { model | response = response, artists = data.artists }, Cmd.none )

                _ ->
                    ( { model | response = response }, Cmd.none )

        ToggleAliases bool ->
            ( { model | hideAliases = bool }, Cmd.none )

        ToggleDebugView bool ->
            ( { model | toggleDebugView = bool }, Cmd.none )

        MouseArtistHover artistId ->
            ( { model | maybeHoveredArtistId = Just artistId }, Cmd.none )

        MouseArtistLeave ->
            ( { model | maybeHoveredArtistId = Nothing }, Cmd.none )


makeRequest : Cmd Msg
makeRequest =
    rootQuery
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


rootQuery : SelectionSet Response RootQuery
rootQuery =
    SelectionSet.map Response
        (Query.artists (\n -> { n | order_by = Present "nickname asc" }) artistSelector)



---- VIEW ----
---- PROGRAM ----


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initModel, makeRequest )


initModel : Model
initModel =
    { hideAliases = False
    , response = RemoteData.Loading
    , toggleDebugView = False
    , artists = []
    , maybeHoveredArtistId = Nothing
    }


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    B.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = chooseView (Document.serializeQuery rootQuery)
        }


chooseView : String -> Model -> NodeWithStyle Msg
chooseView query model =
    if model.toggleDebugView then
        debugView query model

    else
        view query model


view : String -> Model -> NodeWithStyle Msg
view query model =
    let
        artists =
            model.artists
    in
    B.div []
        [ B.p [] [ toggleDebugViewCheckbox model ]
        , verticalLayout []
            [ pxRow 168
                []
                [ horizontalLayout []
                    [ pxColumn 128 [] []
                    , fillColumn [] []
                    , autoColumn []
                        [ horizontalCenteredLayout []
                            [ pxColumn 156 [] [ B.text "ARTISTES" ]
                            , pxColumn 156 [] [ B.text "EXPOSITIONS" ]
                            , pxColumn 156 [] [ B.text "GALERIE" ]
                            , pxColumn 156 [] [ B.text "CONTACT" ]
                            ]
                        ]
                    ]
                ]
            , fillRow []
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
                            [ Grid.align Grid.center
                            ]
                        ]
                    ]
                    (List.map (showPreviewArtwork model.maybeHoveredArtistId) artists)
                ]
            ]
        ]


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
                    ([ A.style
                        [ Style.box [ Box.position (Position.relative [ Position.all (px 0) ]), Box.cursorPointer ] ]
                     ]
                        ++ (if hover then
                                [ onMouseLeave MouseArtistLeave ]

                            else
                                [ onMouseOver (MouseArtistHover artist.id) ]
                           )
                    )
                    ([ B.img ""
                        preview_artwork.image_url
                        [ A.style
                            ([ Style.block [ Block.width (percent 100) ]
                             ]
                                ++ pseudoClassArtistHoverBoxStyle hover
                            )
                        ]
                     ]
                        ++ (if hover then
                                [ B.div
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
                                ]

                            else
                                []
                           )
                    )

            Nothing ->
                B.div [] []
        ]


pseudoClassArtistHoverBoxStyle hover =
    if hover then
        [ Style.box [ Box.opacity 0.55 ] ]

    else
        []


debugView : String -> Model -> NodeWithStyle Msg
debugView query model =
    B.div []
        [ B.div []
            [ B.p [] [ toggleDebugViewCheckbox model ]
            , B.h1 [] [ B.text "Generated Query" ]
            , B.p [] [ toggleAliasesCheckbox model ]
            , ( pre []
                    [ (if model.hideAliases then
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
            , ( log "SUBMODEL" model |> PrintAny.view, [] )
            ]
        ]


toggleAliasesCheckbox : Model -> NodeWithStyle Msg
toggleAliasesCheckbox model =
    B.div []
        [ B.inputCheckbox [ onCheck ToggleAliases, checked model.hideAliases ]
        , B.text " Show Aliases "
        , B.a [ href "https://github.com/dillonkearns/elm-graphql/blob/master/FAQ.md#how-do-field-aliases-work-in-dillonkearnselm-graphql" ]
            [ B.text "(?)"
            ]
        ]


toggleDebugViewCheckbox : Model -> NodeWithStyle Msg
toggleDebugViewCheckbox model =
    B.div []
        [ B.inputCheckbox [ onCheck ToggleDebugView, checked model.toggleDebugView ]
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


type alias ArtistId =
    String

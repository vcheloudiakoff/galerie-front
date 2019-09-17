module Main exposing (..)

-- import Galerie.Object.Artwork as Artwork

import Browser
import Debug exposing (log)
import Galerie.Object
import Galerie.Object.Artist as Artist
import Galerie.Object.Artwork as Artwork
import Galerie.Query as Query
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helpers.Main
import Html exposing (Html, a, div, h1, img, input, label, p, pre, text)
import Html.Attributes exposing (href, src, type_)
import Html.Events exposing (onClick)
import PrintAny
import Regex
import RemoteData exposing (RemoteData)



---- MODEL ----


type Msg
    = ToggleAliases
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)


type alias Model =
    { hideAliases : Bool
    , response : RemoteData (Graphql.Http.Error Response) Response
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


artist : SelectionSet ArtistLookup Galerie.Object.Artist
artist =
    SelectionSet.map3 ArtistLookup
        Artist.nickname
        Artist.id
        (Artist.preview_artwork preview_artwork)


preview_artwork : SelectionSet ArtworkLookup Galerie.Object.Artwork
preview_artwork =
    SelectionSet.map ArtworkLookup
        Artwork.image_url



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( { model | response = response }, Cmd.none )

        ToggleAliases ->
            ( { model | hideAliases = not model.hideAliases }, Cmd.none )


makeRequest : Cmd Msg
makeRequest =
    rootQuery
        |> Graphql.Http.queryRequest "http://localhost:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


rootQuery : SelectionSet Response RootQuery
rootQuery =
    SelectionSet.map Response
        (Query.artists (\n -> { n | order_by = Present "nickname asc" }) artist)



---- VIEW ----
-- view : Model -> Html Msg
-- view model =
--     div []
--         [ img [ src "/logo.svg" ] []
--         , h1 [] [ text "Artistes" ]
--         , div
--             []
--             [ img [ src "/logo.svg" ] []
--             , h1 [] [ text model.artist.nickname ]
--             ]
--         ]
---- PROGRAM ----


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { hideAliases = False, response = RemoteData.Loading }, makeRequest )


type alias Flags =
    ()


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view (Document.serializeQuery rootQuery)
        }


view : String -> Model -> Browser.Document Msg
view query model =
    { title = "Query Explorer"
    , body =
        [ div []
            [ div []
                [ h1 [] [ Html.text "Generated Query" ]
                , p [] [ toggleAliasesCheckbox ]
                , pre []
                    [ (if model.hideAliases then
                        query
                            |> stripAliases

                       else
                        query
                      )
                        |> Html.text
                    ]
                ]
            , div []
                [ h1 [] [ Html.text "Response" ]
                , log "SUBMODEL" model |> PrintAny.view
                ]
            ]
        ]
    }


toggleAliasesCheckbox : Html Msg
toggleAliasesCheckbox =
    label []
        [ input [ type_ "checkbox", onClick ToggleAliases ] []
        , Html.text " Show Aliases "
        , a [ href "https://github.com/dillonkearns/elm-graphql/blob/master/FAQ.md#how-do-field-aliases-work-in-dillonkearnselm-graphql" ]
            [ Html.text "(?)"
            ]
        ]


stripAliases : String -> String
stripAliases query =
    query
        |> Regex.replace
            (Regex.fromStringWith { multiline = True, caseInsensitive = True } "^(\\s*)\\w+: "
                |> Maybe.withDefault Regex.never
            )
            (\match -> match.submatches |> List.head |> Maybe.withDefault Nothing |> Maybe.withDefault "")

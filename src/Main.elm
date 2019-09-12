module Main exposing (..)

-- import Galerie.Object.Artwork as Artwork

import Browser
import Galerie.Object
import Galerie.Object.Artist as Artist
import Galerie.Query as Query
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helpers.Main
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import RemoteData exposing (RemoteData)



---- MODEL ----


type alias Response =
    { vincent : ArtistLookup
    }



-- type alias Artwork =
--     { urlImg : String
--     }


type alias ArtistLookup =
    { nickname : String
    , id : String
    }


artist : SelectionSet ArtistLookup Galerie.Object.Artist
artist =
    SelectionSet.map2 ArtistLookup
        Artist.nickname
        Artist.id



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest "http://localhost:3000"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


query : SelectionSet Response RootQuery
query =
    SelectionSet.map Response
        (Query.artist { id = "8aefadc3-7fe6-4ccd-a1ea-73150b208101" } artist)



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


type Msg
    = GotResponse Model


type alias Model =
    RemoteData (Graphql.Http.Error Response) Response


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading, makeRequest )


type alias Flags =
    ()


main : Helpers.Main.Program Flags Model Msg
main =
    Helpers.Main.document
        { init = init
        , update = update
        , queryString = Document.serializeQuery query
        }

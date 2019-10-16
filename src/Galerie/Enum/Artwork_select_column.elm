-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Galerie.Enum.Artwork_select_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "artwork"

  - Artist\_id - column name
  - Created\_at - column name
  - Description - column name
  - Height - column name
  - Id - column name
  - Image\_url - column name
  - Support - column name
  - Techniques - column name
  - Title - column name
  - Updated\_at - column name
  - Width - column name

-}
type Artwork_select_column
    = Artist_id
    | Created_at
    | Description
    | Height
    | Id
    | Image_url
    | Support
    | Techniques
    | Title
    | Updated_at
    | Width


list : List Artwork_select_column
list =
    [ Artist_id, Created_at, Description, Height, Id, Image_url, Support, Techniques, Title, Updated_at, Width ]


decoder : Decoder Artwork_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "artist_id" ->
                        Decode.succeed Artist_id

                    "created_at" ->
                        Decode.succeed Created_at

                    "description" ->
                        Decode.succeed Description

                    "height" ->
                        Decode.succeed Height

                    "id" ->
                        Decode.succeed Id

                    "image_url" ->
                        Decode.succeed Image_url

                    "support" ->
                        Decode.succeed Support

                    "techniques" ->
                        Decode.succeed Techniques

                    "title" ->
                        Decode.succeed Title

                    "updated_at" ->
                        Decode.succeed Updated_at

                    "width" ->
                        Decode.succeed Width

                    _ ->
                        Decode.fail ("Invalid Artwork_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Artwork_select_column -> String
toString enum =
    case enum of
        Artist_id ->
            "artist_id"

        Created_at ->
            "created_at"

        Description ->
            "description"

        Height ->
            "height"

        Id ->
            "id"

        Image_url ->
            "image_url"

        Support ->
            "support"

        Techniques ->
            "techniques"

        Title ->
            "title"

        Updated_at ->
            "updated_at"

        Width ->
            "width"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Artwork_select_column
fromString enumString =
    case enumString of
        "artist_id" ->
            Just Artist_id

        "created_at" ->
            Just Created_at

        "description" ->
            Just Description

        "height" ->
            Just Height

        "id" ->
            Just Id

        "image_url" ->
            Just Image_url

        "support" ->
            Just Support

        "techniques" ->
            Just Techniques

        "title" ->
            Just Title

        "updated_at" ->
            Just Updated_at

        "width" ->
            Just Width

        _ ->
            Nothing

-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Galerie.Object.Artist exposing (..)

import Galerie.InputObject
import Galerie.Interface
import Galerie.Object
import Galerie.Scalar
import Galerie.ScalarCodecs
import Galerie.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


created_at : SelectionSet (Maybe String) Galerie.Object.Artist
created_at =
    Object.selectionForField "(Maybe String)" "created_at" [] (Decode.string |> Decode.nullable)


description : SelectionSet (Maybe String) Galerie.Object.Artist
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


first_name : SelectionSet (Maybe String) Galerie.Object.Artist
first_name =
    Object.selectionForField "(Maybe String)" "first_name" [] (Decode.string |> Decode.nullable)


id : SelectionSet String Galerie.Object.Artist
id =
    Object.selectionForField "String" "id" [] Decode.string


last_name : SelectionSet (Maybe String) Galerie.Object.Artist
last_name =
    Object.selectionForField "(Maybe String)" "last_name" [] (Decode.string |> Decode.nullable)


nickname : SelectionSet String Galerie.Object.Artist
nickname =
    Object.selectionForField "String" "nickname" [] Decode.string


preview_artwork : SelectionSet decodesTo Galerie.Object.Artwork -> SelectionSet (Maybe decodesTo) Galerie.Object.Artist
preview_artwork object_ =
    Object.selectionForCompositeField "preview_artwork" [] object_ (identity >> Decode.nullable)


preview_artwork_id : SelectionSet (Maybe String) Galerie.Object.Artist
preview_artwork_id =
    Object.selectionForField "(Maybe String)" "preview_artwork_id" [] (Decode.string |> Decode.nullable)


updated_at : SelectionSet (Maybe String) Galerie.Object.Artist
updated_at =
    Object.selectionForField "(Maybe String)" "updated_at" [] (Decode.string |> Decode.nullable)

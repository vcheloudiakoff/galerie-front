-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Galerie.Object.Exhibition exposing (..)

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


artist : SelectionSet decodesTo Galerie.Object.Artist -> SelectionSet (Maybe decodesTo) Galerie.Object.Exhibition
artist object_ =
    Object.selectionForCompositeField "artist" [] object_ (identity >> Decode.nullable)


artist_id : SelectionSet (Maybe String) Galerie.Object.Exhibition
artist_id =
    Object.selectionForField "(Maybe String)" "artist_id" [] (Decode.string |> Decode.nullable)


artwork_ids : SelectionSet (Maybe (List (Maybe String))) Galerie.Object.Exhibition
artwork_ids =
    Object.selectionForField "(Maybe (List (Maybe String)))" "artwork_ids" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


artworks : SelectionSet decodesTo Galerie.Object.Artwork -> SelectionSet (Maybe (List (Maybe decodesTo))) Galerie.Object.Exhibition
artworks object_ =
    Object.selectionForCompositeField "artworks" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


beginning_date : SelectionSet (Maybe String) Galerie.Object.Exhibition
beginning_date =
    Object.selectionForField "(Maybe String)" "beginning_date" [] (Decode.string |> Decode.nullable)


created_at : SelectionSet (Maybe String) Galerie.Object.Exhibition
created_at =
    Object.selectionForField "(Maybe String)" "created_at" [] (Decode.string |> Decode.nullable)


description : SelectionSet (Maybe String) Galerie.Object.Exhibition
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


ending_date : SelectionSet (Maybe String) Galerie.Object.Exhibition
ending_date =
    Object.selectionForField "(Maybe String)" "ending_date" [] (Decode.string |> Decode.nullable)


id : SelectionSet String Galerie.Object.Exhibition
id =
    Object.selectionForField "String" "id" [] Decode.string


title : SelectionSet (Maybe String) Galerie.Object.Exhibition
title =
    Object.selectionForField "(Maybe String)" "title" [] (Decode.string |> Decode.nullable)


updated_at : SelectionSet (Maybe String) Galerie.Object.Exhibition
updated_at =
    Object.selectionForField "(Maybe String)" "updated_at" [] (Decode.string |> Decode.nullable)

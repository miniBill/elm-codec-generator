module Utils exposing (firstLower, firstUpper, typeToDefault, typeToSimpleDefault)

import Elm
import Gen.Array
import Gen.Dict
import Gen.Maybe
import Gen.Result
import Gen.Set
import Maybe.Extra
import Types exposing (Type(..))


firstLower : String -> String
firstLower n =
    case String.uncons n of
        Nothing ->
            ""

        Just ( h, tail ) ->
            String.cons (Char.toLower h) tail


firstUpper : String -> String
firstUpper n =
    case String.uncons n of
        Nothing ->
            ""

        Just ( h, tail ) ->
            String.cons (Char.toUpper h) tail


typeToDefault : Type -> Elm.Expression
typeToDefault tipe =
    case tipe of
        Unit ->
            Elm.unit

        Maybe _ ->
            Gen.Maybe.make_.nothing

        List _ ->
            Elm.list []

        Array _ ->
            Gen.Array.empty

        Set _ ->
            Gen.Set.empty

        Dict _ _ ->
            Gen.Dict.empty

        Tuple a b ->
            Elm.tuple (typeToDefault a) (typeToDefault b)

        Result _ o ->
            Gen.Result.make_.ok <| typeToDefault o

        Triple a b c ->
            Elm.triple (typeToDefault a) (typeToDefault b) (typeToDefault c)

        Object fields ->
            fields
                |> List.map
                    (\( fieldName, fieldType ) ->
                        ( fieldName, typeToDefault fieldType )
                    )
                |> Elm.record

        Named n ->
            case n of
                "String" ->
                    Elm.string ""

                "Char" ->
                    Elm.char ' '

                "Bool" ->
                    Elm.bool True

                "Int" ->
                    Elm.int 0

                "Float" ->
                    Elm.float 0

                _ ->
                    Elm.value
                        { importFrom = []
                        , name = firstLower n ++ "Default"
                        , annotation = Nothing
                        }


typeToSimpleDefault : Type -> Maybe Elm.Expression
typeToSimpleDefault tipe =
    case tipe of
        Unit ->
            Just Elm.unit

        Maybe _ ->
            Just Gen.Maybe.make_.nothing

        List _ ->
            Just <| Elm.list []

        Array _ ->
            Just Gen.Array.empty

        Set _ ->
            Just Gen.Set.empty

        Dict _ _ ->
            Just Gen.Dict.empty

        Tuple a b ->
            Maybe.map2 Elm.tuple (typeToSimpleDefault a) (typeToSimpleDefault b)

        Result _ o ->
            Maybe.map Gen.Result.make_.ok (typeToSimpleDefault o)

        Triple a b c ->
            Maybe.map3 Elm.triple (typeToSimpleDefault a) (typeToSimpleDefault b) (typeToSimpleDefault c)

        Object fields ->
            fields
                |> Maybe.Extra.traverse
                    (\( fieldName, fieldType ) ->
                        Maybe.map (Tuple.pair fieldName) (typeToSimpleDefault fieldType)
                    )
                |> Maybe.map Elm.record

        Named n ->
            case n of
                "String" ->
                    Just <| Elm.string ""

                "Char" ->
                    Just <| Elm.char ' '

                "Bool" ->
                    Just <| Elm.bool True

                "Int" ->
                    Just <| Elm.int 0

                "Float" ->
                    Just <| Elm.float 0

                _ ->
                    Nothing

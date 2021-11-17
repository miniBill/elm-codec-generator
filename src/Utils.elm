module Utils exposing (firstLower, firstUpper, typeToDefault, typeToSimpleDefault)

import Elm
import Elm.Gen.Array
import Elm.Gen.Dict
import Elm.Gen.Maybe
import Elm.Gen.Result
import Elm.Gen.Set
import Maybe.Extra
import Model exposing (Type(..))


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
            Elm.Gen.Maybe.make_.maybe.nothing

        List _ ->
            Elm.list []

        Array _ ->
            Elm.Gen.Array.empty

        Set _ ->
            Elm.Gen.Set.empty

        Dict _ _ ->
            Elm.Gen.Dict.empty

        Tuple a b ->
            Elm.tuple (typeToDefault a) (typeToDefault b)

        Result _ o ->
            Elm.Gen.Result.make_.result.ok <| typeToDefault o

        Triple a b c ->
            Elm.triple (typeToDefault a) (typeToDefault b) (typeToDefault c)

        Object fields ->
            fields
                |> List.map
                    (\( fieldName, fieldType ) ->
                        fieldType
                            |> typeToDefault
                            |> Elm.field fieldName
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
                    Elm.value <| firstLower n ++ "Default"


typeToSimpleDefault : Type -> Maybe Elm.Expression
typeToSimpleDefault tipe =
    case tipe of
        Unit ->
            Just Elm.unit

        Maybe _ ->
            Just Elm.Gen.Maybe.make_.maybe.nothing

        List _ ->
            Just <| Elm.list []

        Array _ ->
            Just Elm.Gen.Array.empty

        Set _ ->
            Just Elm.Gen.Set.empty

        Dict _ _ ->
            Just Elm.Gen.Dict.empty

        Tuple a b ->
            Maybe.map2 Elm.tuple (typeToSimpleDefault a) (typeToSimpleDefault b)

        Result _ o ->
            Maybe.map Elm.Gen.Result.make_.result.ok (typeToSimpleDefault o)

        Triple a b c ->
            Maybe.map3 Elm.triple (typeToSimpleDefault a) (typeToSimpleDefault b) (typeToSimpleDefault c)

        Object fields ->
            fields
                |> Maybe.Extra.traverse
                    (\( fieldName, fieldType ) ->
                        Maybe.map (Elm.field fieldName) (typeToSimpleDefault fieldType)
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

module Utils exposing (firstLower, firstUpper, typeToDefault)

import Elm
import Elm.Gen.Array
import Elm.Gen.Dict
import Elm.Gen.Maybe
import Elm.Gen.Result
import Elm.Gen.Set
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

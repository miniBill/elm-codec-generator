module FileParser exposing (parse, typeToString)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Result.Extra
import Types exposing (Type(..), TypeDecl(..))


parse : String -> { moduleName : ModuleName, typeDecls : List (Result String TypeDecl) }
parse input =
    case
        Elm.Parser.parse input |> Result.map (Elm.Processing.process Elm.Processing.init)
    of
        Err _ ->
            { moduleName = [ "Model" ]
            , typeDecls = [ Err "Error parsing file. If you want to have a more detailed error, feel free to open a PR ;)" ]
            }

        Ok file ->
            { moduleName = toModuleName file
            , typeDecls = fileToTypeDecls file
            }


toModuleName : File.File -> ModuleName
toModuleName { moduleDefinition } =
    case Node.value moduleDefinition of
        Module.EffectModule { moduleName } ->
            Node.value moduleName

        Module.NormalModule { moduleName } ->
            Node.value moduleName

        Module.PortModule { moduleName } ->
            Node.value moduleName


fileToTypeDecls : File.File -> List (Result String TypeDecl)
fileToTypeDecls { declarations } =
    let
        declarationToTypeDecl : Node Declaration.Declaration -> Maybe (Result String TypeDecl)
        declarationToTypeDecl (Node _ decl) =
            case decl of
                Declaration.AliasDeclaration { name, generics, typeAnnotation } ->
                    typeAnnotationToType typeAnnotation
                        |> Result.map
                            (Alias
                                (String.join " " <| List.map Node.value <| name :: generics)
                            )
                        |> addNameToError name
                        |> Just

                Declaration.CustomTypeDeclaration t ->
                    Just <| customTypeToTypeDecl t

                _ ->
                    Nothing
    in
    List.filterMap declarationToTypeDecl declarations


typeAnnotationToType : Node TypeAnnotation.TypeAnnotation -> Result String Type
typeAnnotationToType tyan =
    case Node.value tyan of
        TypeAnnotation.Typed ctor args ->
            let
                ( mod, name ) =
                    Node.value ctor
            in
            if List.isEmpty mod then
                case ( name, Result.Extra.combineMap typeAnnotationToType args ) of
                    ( _, Err e ) ->
                        Err e

                    ( _, Ok [] ) ->
                        Ok <| Named name

                    ( "Dict", Ok [ k, v ] ) ->
                        Ok <| Dict k v

                    ( "Result", Ok [ e, o ] ) ->
                        Ok <| Result e o

                    ( "List", Ok [ i ] ) ->
                        Ok <| List i

                    ( "Array", Ok [ i ] ) ->
                        Ok <| Array i

                    ( "Set", Ok [ i ] ) ->
                        Ok <| Set i

                    ( "Maybe", Ok [ i ] ) ->
                        Ok <| Maybe i

                    ( _, Ok ts ) ->
                        Err <|
                            "Codec generation not supported for "
                                ++ String.join " " (name :: List.map (typeToString True) ts)

            else
                Err <| "Qualified names, like " ++ String.join "." (mod ++ [ name ]) ++ ", are not supported"

        TypeAnnotation.Record fields ->
            fields
                |> Result.Extra.combineMap
                    (\field ->
                        let
                            ( name, type_ ) =
                                Node.value field
                        in
                        Result.map
                            (Tuple.pair <| Node.value name)
                            (typeAnnotationToType type_)
                    )
                |> Result.map Object

        TypeAnnotation.Unit ->
            Ok Unit

        TypeAnnotation.Tupled args ->
            case Result.Extra.combineMap typeAnnotationToType args of
                Err e ->
                    Err e

                Ok [] ->
                    Ok Unit

                Ok [ l, r ] ->
                    Ok <| Tuple l r

                Ok [ l, m, r ] ->
                    Ok <| Triple l m r

                Ok _ ->
                    unsupported "tuple of more than three arguments"

        TypeAnnotation.GenericType _ ->
            unsupported "generic types"

        TypeAnnotation.GenericRecord _ _ ->
            unsupported "generic objects"

        TypeAnnotation.FunctionTypeAnnotation _ _ ->
            unsupported "function types"


typeToString : Bool -> Type -> String
typeToString needParens t =
    let
        fieldToElm : ( String, Type ) -> String
        fieldToElm ( name, ft ) =
            name ++ " : " ++ typeToString False ft

        parens : String -> String
        parens r =
            if needParens then
                "(" ++ r ++ ")"

            else
                r
    in
    case t of
        Object fs ->
            "{ " ++ String.join "\n    , " (List.map fieldToElm fs) ++ "\n    }"

        Array c ->
            parens <|
                "Array "
                    ++ typeToString True c

        List c ->
            parens <|
                "List "
                    ++ typeToString True c

        Maybe c ->
            parens <|
                "Maybe "
                    ++ typeToString True c

        Set c ->
            parens <|
                "Set "
                    ++ typeToString True c

        Result k v ->
            parens <|
                "Result "
                    ++ typeToString True k
                    ++ " "
                    ++ typeToString True v

        Dict k v ->
            parens <|
                "Dict "
                    ++ typeToString True k
                    ++ " "
                    ++ typeToString True v

        Named n ->
            if String.contains " " n then
                "( " ++ n ++ " )"

            else
                n

        Unit ->
            "()"

        Tuple a b ->
            "(" ++ typeToString False a ++ ", " ++ typeToString False b ++ ")"

        Triple a b c ->
            "(" ++ typeToString False a ++ ", " ++ typeToString False b ++ ", " ++ typeToString False c ++ ")"


customTypeToTypeDecl : Type.Type -> Result String TypeDecl
customTypeToTypeDecl { name, generics, constructors } =
    let
        constructorToVariant : Type.ValueConstructor -> Result String ( String, List Type )
        constructorToVariant ctor =
            Result.map
                (Tuple.pair <| Node.value ctor.name)
                (Result.Extra.combineMap typeAnnotationToType ctor.arguments)

        inner : Result String TypeDecl
        inner =
            if List.isEmpty generics then
                constructors
                    |> Result.Extra.combineMap (Node.value >> constructorToVariant)
                    |> Result.map (Custom (Node.value name))

            else
                unsupported "generic types"
    in
    addNameToError name inner


unsupported : String -> Result String x
unsupported kind =
    Err <| "Codec generation not supported for " ++ kind


addNameToError : Node String -> Result String TypeDecl -> Result String TypeDecl
addNameToError name =
    Result.mapError (\e -> "Error generating Codec for " ++ Node.value name ++ ": " ++ e)

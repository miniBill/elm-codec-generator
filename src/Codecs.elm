module Codecs exposing (Config, getFile)

import Elm
import Elm.Annotation
import Elm.Case
import Elm.Op
import Gen.Codec as Codec
import Gen.Debug
import Gen.Maybe
import Model exposing (Type(..), TypeDecl(..), Variant, typeToAnnotation)
import Utils exposing (firstLower, typeToSimpleDefault)


type alias Config =
    { optimizeDefaultFields : Bool }


getFile : Config -> List (Result String TypeDecl) -> String
getFile config typeDecls =
    let
        declarations : List Elm.Declaration
        declarations =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map (typeDeclToCodecDeclaration config)

        errors : List String
        errors =
            typeDecls
                |> List.filterMap
                    (\r ->
                        case r of
                            Err e ->
                                Just e

                            Ok _ ->
                                Nothing
                    )

        comment : String
        comment =
            if List.isEmpty errors then
                ""

            else
                "\n\n-- " ++ String.join "\n-- " errors
    in
    (Elm.file [ "Codecs" ] declarations).contents
        ++ comment


isRecursive : String -> Type -> Bool
isRecursive name t =
    case t of
        Unit ->
            False

        Maybe c ->
            isRecursive name c

        List c ->
            isRecursive name c

        Array c ->
            isRecursive name c

        Set c ->
            isRecursive name c

        Dict a b ->
            isRecursive name a || isRecursive name b

        Result a b ->
            isRecursive name a || isRecursive name b

        Tuple a b ->
            isRecursive name a || isRecursive name b

        Triple a b c ->
            isRecursive name a || isRecursive name b || isRecursive name c

        Object fields ->
            List.any (\( _, ft ) -> isRecursive name ft) fields

        Named n ->
            n == name


typeToCodec : Config -> (String -> Elm.Expression) -> Type -> Elm.Expression
typeToCodec config named t =
    let
        oneChild : (Elm.Expression -> a) -> Type -> a
        oneChild ctor c =
            ctor
                (typeToCodec config named c)

        twoChildren : (Elm.Expression -> Elm.Expression -> a) -> Type -> Type -> a
        twoChildren ctor a b =
            ctor
                (typeToCodec config named a)
                (typeToCodec config named b)

        threeChildren : (Elm.Expression -> Elm.Expression -> Elm.Expression -> a) -> Type -> Type -> Type -> a
        threeChildren ctor a b c =
            ctor
                (typeToCodec config named a)
                (typeToCodec config named b)
                (typeToCodec config named c)
    in
    case t of
        Object fields ->
            let
                ctor : Elm.Expression
                ctor =
                    Elm.function
                        (List.map (\( fn, ft ) -> ( fn, Just <| typeToAnnotation ft )) fields)
                        (\args ->
                            List.map2
                                (\( fn, ft ) arg ->
                                    let
                                        val : Elm.Expression
                                        val =
                                            if config.optimizeDefaultFields then
                                                case ft of
                                                    Maybe _ ->
                                                        arg

                                                    _ ->
                                                        case typeToSimpleDefault ft of
                                                            Nothing ->
                                                                arg

                                                            Just default ->
                                                                Gen.Maybe.withDefault default arg

                                            else
                                                arg
                                    in
                                    ( fn, val )
                                )
                                fields
                                args
                                |> Elm.record
                        )

                fieldCodecs : List (Elm.Expression -> Elm.Expression)
                fieldCodecs =
                    List.map (fieldToCodec config named) fields
            in
            pipeline
                (Codec.object ctor)
                fieldCodecs
                |> Elm.Op.pipe Codec.values_.buildObject

        Array c ->
            oneChild Codec.array c

        Set c ->
            oneChild Codec.set c

        List c ->
            oneChild Codec.list c

        Maybe c ->
            oneChild Codec.maybe c

        Dict _ v ->
            oneChild Codec.dict v

        Result a b ->
            twoChildren Codec.result a b

        Tuple a b ->
            twoChildren Codec.tuple a b

        Triple a b c ->
            threeChildren Codec.triple a b c

        Unit ->
            Codec.succeed Elm.unit

        Named "String" ->
            Codec.string

        Named "Bool" ->
            Codec.bool

        Named "Int" ->
            Codec.int

        Named "Float" ->
            Codec.float

        Named "Char" ->
            Codec.char

        Named n ->
            named n


fieldToCodec : Config -> (String -> Elm.Expression) -> (( String, Type ) -> Elm.Expression -> Elm.Expression)
fieldToCodec config named =
    \( fn, ft ) ->
        case ft of
            Maybe it ->
                Codec.maybeField fn
                    (Elm.get fn)
                    (typeToCodec config named it)

            _ ->
                let
                    childCodec : Elm.Expression
                    childCodec =
                        typeToCodec config named ft
                in
                if config.optimizeDefaultFields then
                    case typeToSimpleDefault ft of
                        Nothing ->
                            Codec.field fn
                                (Elm.get fn)
                                childCodec

                        Just default ->
                            Codec.maybeField fn
                                (\v ->
                                    Elm.ifThen
                                        (Elm.Op.equal (Elm.get fn v) default)
                                        Gen.Maybe.make_.nothing
                                        (Gen.Maybe.make_.just (Elm.get fn v))
                                )
                                childCodec

                else
                    Codec.field fn
                        (Elm.get fn)
                        childCodec


pipeline : Elm.Expression -> List (Elm.Expression -> Elm.Expression) -> Elm.Expression
pipeline =
    List.foldl (\e a -> Elm.Op.pipe (Elm.functionReduced "pipeArg__" e) a)


customCodec : Config -> Elm.Annotation.Annotation -> (String -> Elm.Expression) -> List Variant -> Elm.Expression
customCodec config tipe named variants =
    Elm.Op.pipe Codec.values_.lazy <|
        Elm.fn ( "()", Nothing ) <|
            \_ ->
                case variants of
                    [ ( variantName, [ (Object _) as innerType ] ) ] ->
                        Elm.apply Codec.values_.map
                            [ Elm.value
                                { importFrom = [ "Model" ]
                                , name = variantName
                                , annotation = Nothing
                                }
                            , Elm.unwrapper [ "Model" ] variantName
                            , typeToCodec config named innerType
                            ]

                    _ ->
                        let
                            variantToCase : ( String, List a ) -> Elm.Expression -> Elm.Case.Branch
                            variantToCase ( name, args ) fn =
                                Elm.Case.branchWith
                                    name
                                    (List.length args)
                                    (Elm.apply fn)

                            variantToPipe : ( String, List Type ) -> Elm.Expression
                            variantToPipe ( name, args ) =
                                let
                                    argsCodecs : List Elm.Expression
                                    argsCodecs =
                                        List.map
                                            (typeToCodec config named)
                                            args
                                in
                                Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Codec" ]
                                        , name = "variant" ++ String.fromInt (List.length args)
                                        , annotation = Nothing
                                        }
                                    )
                                    ([ Elm.string name
                                     , Elm.value
                                        { importFrom = [ "Model" ]
                                        , name = name
                                        , annotation = Nothing
                                        }
                                     ]
                                        ++ argsCodecs
                                    )

                            variantsCodecs : List Elm.Expression
                            variantsCodecs =
                                variants
                                    |> List.map variantToPipe
                        in
                        List.foldl (\f a -> Elm.Op.pipe f a)
                            (Codec.custom
                                (Elm.function
                                    (List.map
                                        (\( name, _ ) ->
                                            ( "f" ++ firstLower name
                                            , Just <| Elm.Annotation.named [] "Irrelevant"
                                            )
                                        )
                                        variants
                                        ++ [ ( "value", Just tipe ) ]
                                    )
                                    (\fs ->
                                        case List.reverse fs of
                                            value :: rest ->
                                                Elm.Case.custom
                                                    value
                                                    tipe
                                                    (List.map2 variantToCase variants <| List.reverse rest)

                                            [] ->
                                                Gen.Debug.todo "Error: function didn't get enough args"
                                    )
                                )
                            )
                            variantsCodecs
                            |> Elm.Op.pipe Codec.values_.buildCustom


typeDeclToCodecDeclaration : Config -> TypeDecl -> Elm.Declaration
typeDeclToCodecDeclaration config decl =
    let
        ( name, codec, rec ) =
            case decl of
                Alias n t ->
                    ( n, \named -> typeToCodec config named t, isRecursive n t )

                Custom n vs ->
                    let
                        annotation : Elm.Annotation.Annotation
                        annotation =
                            Elm.Annotation.named [ "Model" ] n
                    in
                    ( n, \named -> customCodec config annotation named vs, List.any (\( _, args ) -> List.any (isRecursive n) args) vs )

        expression : Elm.Expression
        expression =
            if rec then
                Codec.recursive
                    (\child ->
                        codec
                            (\n ->
                                if n == name then
                                    child

                                else
                                    typeNameToCodec n
                            )
                    )

            else
                codec typeNameToCodec

        codecName : String
        codecName =
            firstLower name ++ "Codec"
    in
    expression
        |> Elm.withType (Codec.annotation_.codec <| Elm.Annotation.named [ "Model" ] name)
        |> Elm.declaration codecName
        |> Elm.expose


typeNameToCodec : String -> Elm.Expression
typeNameToCodec n =
    Elm.value
        { importFrom = []
        , name = firstLower n ++ "Codec"
        , annotation = Just <| Codec.annotation_.codec <| Elm.Annotation.named [ "Model" ] n
        }

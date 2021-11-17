module Codecs exposing (Config, getFile)

import Elm
import Elm.Annotation
import Elm.Gen.Codec as Codec
import Elm.Gen.Maybe
import Elm.Let
import Elm.Pattern
import Model exposing (Type(..), TypeDecl(..), Variant, typeToAnnotation)
import Utils exposing (firstLower, typeToSimpleDefault)


type alias Config =
    { optimizeDefaultFields : Bool }


getFile : Config -> List (Result String TypeDecl) -> String
getFile config typeDecls =
    let
        declarations =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map (typeDeclToCodecDeclaration config)

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
        oneChild ctor c =
            ctor
                (typeToCodec config named c)

        twoChildren ctor a b =
            ctor
                (typeToCodec config named a)
                (typeToCodec config named b)

        threeChildren ctor a b c =
            ctor
                (typeToCodec config named a)
                (typeToCodec config named b)
                (typeToCodec config named c)
    in
    case t of
        Object fields ->
            let
                fieldExprs =
                    List.map
                        (\( fn, ft ) ->
                            case ft of
                                Maybe it ->
                                    Codec.maybeField (Elm.string fn)
                                        (Elm.get fn)
                                        (typeToCodec config named it)
                                        Elm.pass

                                _ ->
                                    let
                                        childCodec =
                                            typeToCodec config named ft
                                    in
                                    if config.optimizeDefaultFields then
                                        case typeToSimpleDefault ft of
                                            Nothing ->
                                                Codec.field (Elm.string fn)
                                                    (Elm.get fn)
                                                    childCodec
                                                    Elm.pass

                                            Just default ->
                                                Codec.maybeField (Elm.string fn)
                                                    (\v ->
                                                        Elm.letIn
                                                            [ Elm.Let.value "inner" <| Elm.get fn v
                                                            ]
                                                            (Elm.ifThen
                                                                (Elm.equal (Elm.valueWith [] "inner" (typeToAnnotation ft)) default)
                                                                Elm.Gen.Maybe.make_.maybe.nothing
                                                                (Elm.Gen.Maybe.make_.maybe.just (Elm.valueWith [] "inner" (typeToAnnotation ft)))
                                                            )
                                                    )
                                                    childCodec
                                                    Elm.pass

                                    else
                                        Codec.field (Elm.string fn)
                                            (Elm.get fn)
                                            childCodec
                                            Elm.pass
                        )
                        fields
            in
            pipeline
                (Codec.object
                    (Elm.lambdaWith
                        (List.map (\( fn, ft ) -> ( Elm.Pattern.var fn, typeToAnnotation ft )) fields)
                        (Elm.record
                            (List.map
                                (\( fn, ft ) ->
                                    Elm.field fn <|
                                        case ft of
                                            Maybe _ ->
                                                Elm.valueWith [] fn <| typeToAnnotation ft

                                            _ ->
                                                if config.optimizeDefaultFields then
                                                    case typeToSimpleDefault ft of
                                                        Nothing ->
                                                            Elm.valueWith [] fn <| typeToAnnotation ft

                                                        Just default ->
                                                            Elm.Gen.Maybe.withDefault
                                                                default
                                                                (Elm.valueWith [] fn <| typeToAnnotation ft)

                                                else
                                                    Elm.valueWith [] fn <| typeToAnnotation ft
                                )
                                fields
                            )
                        )
                    )
                )
                fieldExprs
                |> Elm.pipe Codec.id_.buildObject

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


pipeline : Elm.Expression -> List Elm.Expression -> Elm.Expression
pipeline =
    List.foldl Elm.pipe


customCodec : Config -> Elm.Annotation.Annotation -> (String -> Elm.Expression) -> List Variant -> Elm.Expression
customCodec config tipe named variants =
    Elm.pipeLeft Codec.id_.lazy <|
        Elm.lambda "()" Elm.Annotation.unit <| \_ ->
        case variants of
            [ ( variantName, [ (Object _) as innerType ] ) ] ->
                Elm.apply Codec.id_.map
                    [ Elm.valueFrom [ "Model" ] variantName
                    , Elm.lambdaWith
                        [ ( Elm.Pattern.namedFrom [ "Model" ] variantName [ Elm.Pattern.var "inner" ]
                          , typeToAnnotation innerType
                          )
                        ]
                        (Elm.value "inner")
                    , typeToCodec config named innerType
                    ]

            _ ->
                let
                    variantToCase ( name, args ) =
                        ( Elm.Pattern.namedFrom [ "Model" ] name <|
                            List.indexedMap
                                (\i _ -> Elm.Pattern.var <| "arg" ++ String.fromInt i)
                                args
                        , Elm.apply
                            (Elm.value <| "f" ++ firstLower name)
                            (List.indexedMap (\i _ -> Elm.value <| "arg" ++ String.fromInt i) args)
                        )

                    variantToPipe ( name, args ) =
                        let
                            argsCodecs =
                                List.map
                                    (typeToCodec config named)
                                    args
                        in
                        Elm.apply (Elm.valueFrom [ "Codec" ] <| "variant" ++ String.fromInt (List.length args))
                            ([ Elm.string name
                             , Elm.valueFrom [ "Model" ] name
                             ]
                                ++ argsCodecs
                            )

                    variantsCodecs =
                        variants
                            |> List.map variantToPipe
                in
                List.foldl (\f a -> Elm.pipe f a)
                    (Codec.custom
                        (Elm.lambdaWith
                            (List.map
                                (\( name, _ ) ->
                                    ( Elm.Pattern.var <| "f" ++ firstLower name
                                    , Elm.Annotation.named [] "Irrelevant"
                                    )
                                )
                                variants
                                ++ [ ( Elm.Pattern.var "value", tipe )
                                   ]
                            )
                            (Elm.caseOf (Elm.value "value") (List.map variantToCase variants))
                        )
                    )
                    variantsCodecs
                    |> Elm.pipe Codec.id_.buildCustom


typeDeclToCodecDeclaration : Config -> TypeDecl -> Elm.Declaration
typeDeclToCodecDeclaration config decl =
    let
        ( name, codec, rec ) =
            case decl of
                Alias n t ->
                    ( n, \named -> typeToCodec config named t, isRecursive n t )

                Custom n vs ->
                    let
                        annotation =
                            Elm.Annotation.named [ "Model" ] n
                    in
                    ( n, \named -> customCodec config annotation named vs, List.any (\( _, args ) -> List.any (isRecursive n) args) vs )

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

        codecName =
            firstLower name ++ "Codec"

        declaration =
            expression
                |> Elm.withType (Codec.types_.codec <| Elm.Annotation.named [ "Model" ] name)
                |> Elm.declaration codecName
                |> Elm.expose
    in
    declaration


typeNameToCodec : String -> Elm.Expression
typeNameToCodec n =
    Elm.valueWith [] (firstLower n ++ "Codec") (Codec.types_.codec <| Elm.Annotation.named [ "Model" ] n)

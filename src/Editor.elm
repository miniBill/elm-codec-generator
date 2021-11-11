module Editor exposing (getFile)

import Dict
import Elm
import Elm.Annotation
import Elm.Gen.Array
import Elm.Gen.Basics
import Elm.Gen.Debug
import Elm.Gen.Dict
import Elm.Gen.Element as Element
import Elm.Gen.Element.Background as Background
import Elm.Gen.Element.Border as Border
import Elm.Gen.Element.Input as Input
import Elm.Gen.List
import Elm.Gen.List.Extra
import Elm.Gen.Maybe
import Elm.Gen.Result
import Elm.Gen.Set
import Elm.Gen.String
import Elm.Gen.Tuple
import Elm.Let
import Elm.Pattern
import FileParser exposing (typeToString)
import Gen.Theme
import Model exposing (Type(..), TypeDecl(..), Variant, typeToAnnotation)
import Utils exposing (firstLower, firstUpper)


getFile : List (Result String TypeDecl) -> String
getFile typeDecls =
    let
        declarations =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map typeDeclToEditor

        defaults =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map typeDeclToDefault

        errors =
            List.filterMap
                (\r ->
                    case r of
                        Err e ->
                            Just e

                        Ok _ ->
                            Nothing
                )
                typeDecls

        comment =
            if List.isEmpty errors then
                ""

            else
                "\n\n-- " ++ String.join "\n-- " errors

        docs groups =
            if List.isEmpty groups then
                ""

            else
                List.foldl
                    (\grouped str ->
                        str ++ "@docs " ++ String.join ", " grouped.members ++ "\n\n"
                    )
                    "\n\n"
                    groups
    in
    (Elm.fileWith [ "Editors" ]
        { docs = docs
        , aliases =
            [ ( [ "Element", "Input" ], "Input" )
            , ( [ "Element", "Border" ], "Border" )
            , ( [ "Element", "Background" ], "Background" )
            , ( [ "Element", "Font" ], "Font" )
            ]
        }
        (declarations ++ defaults ++ commonDeclarations)
    ).contents
        ++ comment


spacing : Elm.Expression
spacing =
    Elm.valueFrom [ "Theme" ] "spacing"


padding : Elm.Expression
padding =
    Elm.valueFrom [ "Theme" ] "padding"


commonDeclarations : List Elm.Declaration
commonDeclarations =
    [ intEditor
    , floatEditor
    , tupleEditor
    , maybeEditor
    , stringEditor
    , boolEditor
    , listEditor
    , dictEditor
    ]


noLabel : Elm.Expression
noLabel =
    Input.labelHidden <| Elm.string ""


getColor : Elm.Expression -> Elm.Expression
getColor l =
    Elm.apply (Elm.valueFrom [ "Theme" ] "getColor") [ l ]


styled : Maybe Elm.Expression -> List Elm.Expression
styled level =
    let
        common =
            [ spacing
            , padding
            , Element.alignTop
            , Border.width <| Elm.int 1
            , Border.rounded rythm
            ]
    in
    case level of
        Nothing ->
            common

        Just l ->
            (Background.color <| getColor l)
                :: Element.width Element.fill
                :: common


rythm : Elm.Expression
rythm =
    Elm.valueFrom [ "Theme" ] "rythm"


editorType : Elm.Annotation.Annotation -> Elm.Annotation.Annotation
editorType t =
    Elm.Annotation.function [ Elm.Annotation.int, t ]
        (Elm.Annotation.tuple
            (Element.types_.element t)
            Elm.Annotation.bool
        )


levelArg : ( String, Elm.Annotation.Annotation )
levelArg =
    ( "level", Elm.Annotation.int )


succ : Elm.Expression -> Elm.Expression
succ level =
    Elm.plus level (Elm.int 1)


intEditor : Elm.Declaration
intEditor =
    Elm.fn2 "intEditor"
        levelArg
        ( "value", Elm.Annotation.int )
        (\level value ->
            Elm.tuple
                (Element.map
                    (\newValue ->
                        newValue
                            |> Elm.pipe Elm.Gen.String.id_.toInt
                            |> Elm.pipe (Elm.apply Elm.Gen.Maybe.id_.withDefault [ value ])
                    )
                    (Input.text
                        [ Element.width <| Element.minimum (Elm.int 100) Element.fill
                        , Element.alignTop
                        , Background.color <| getColor level
                        ]
                        { label = noLabel
                        , onChange = Elm.Gen.Basics.identity
                        , text = Elm.Gen.String.fromInt value
                        , placeholder = Elm.Gen.Maybe.make_.maybe.nothing
                        }
                    )
                    |> Elm.withType (Element.types_.element Elm.Gen.Basics.types_.int)
                )
                Elm.Gen.Basics.make_.bool.true
        )


floatEditor : Elm.Declaration
floatEditor =
    Elm.fn2 "floatEditor"
        levelArg
        ( "value", Elm.Annotation.float )
        (\level value ->
            Elm.tuple
                (Element.map
                    (\newValue ->
                        newValue
                            |> Elm.pipe Elm.Gen.String.id_.toFloat
                            |> Elm.pipe (Elm.apply Elm.Gen.Maybe.id_.withDefault [ value ])
                    )
                    (Input.text
                        [ Element.width <| Element.minimum (Elm.float 100) Element.fill
                        , Element.alignTop
                        , Background.color <| getColor level
                        ]
                        { label = noLabel
                        , onChange = Elm.Gen.Basics.identity
                        , text = Elm.Gen.String.fromFloat value
                        , placeholder = Elm.Gen.Maybe.make_.maybe.nothing
                        }
                    )
                    |> Elm.withType (Element.types_.element Elm.Gen.Basics.types_.float)
                )
                Elm.Gen.Basics.make_.bool.true
        )


stringEditor : Elm.Declaration
stringEditor =
    Elm.fn2 "stringEditor"
        levelArg
        ( "value", Elm.Annotation.string )
        (\level value ->
            Elm.tuple
                (Input.text
                    [ Element.width <| Element.minimum (Elm.int 100) Element.fill
                    , Element.alignTop
                    , Background.color <| getColor level
                    ]
                    { label = noLabel
                    , onChange = Elm.Gen.Basics.identity
                    , text = value
                    , placeholder = Elm.Gen.Maybe.make_.maybe.nothing
                    }
                    |> Elm.withType (Element.types_.element Elm.Gen.String.types_.string)
                )
                Elm.Gen.Basics.make_.bool.true
        )


boolEditor : Elm.Declaration
boolEditor =
    Elm.fn2 "boolEditor"
        levelArg
        ( "value", Elm.Annotation.bool )
        (\_ value ->
            Elm.tuple
                (Input.radioRow
                    [ spacing
                    , Element.alignTop
                    ]
                    { label = noLabel
                    , onChange = Elm.Gen.Basics.identity
                    , options =
                        [ Input.option Elm.Gen.Basics.make_.bool.true <| Element.text <| Elm.string "True"
                        , Input.option Elm.Gen.Basics.make_.bool.false <| Element.text <| Elm.string "False"
                        ]
                    , selected = Elm.Gen.Maybe.make_.maybe.just value
                    }
                    |> Elm.withType (Element.types_.element Elm.Gen.Basics.types_.bool)
                )
                Elm.Gen.Basics.make_.bool.true
        )


dictEditor : Elm.Declaration
dictEditor =
    let
        keyAnnotation =
            Elm.Annotation.var "comparable"

        valueAnnotation =
            Elm.Annotation.var "v"

        dictAnnotation =
            Elm.Annotation.dict keyAnnotation valueAnnotation
    in
    Elm.fn6 "dictEditor"
        ( "keyEditor", editorType keyAnnotation )
        ( "keyDefault", keyAnnotation )
        ( "valueEditor", editorType valueAnnotation )
        ( "valueDefault", valueAnnotation )
        levelArg
        ( "value", dictAnnotation )
        (\keyEditor keyDefault valueEditor valueDefault level value ->
            let
                keysColumn =
                    Element.make_.column
                        { header = Element.none
                        , width = Element.shrink
                        , view = keysView
                        }

                valuesColumn =
                    Element.make_.column
                        { header = Element.none
                        , width = Element.fill
                        , view = valuesView
                        }

                keysView =
                    Elm.lambdaWith
                        [ ( Elm.Pattern.tuple (Elm.Pattern.var "key") (Elm.Pattern.var "memberValue")
                          , Elm.Annotation.tuple keyAnnotation valueAnnotation
                          )
                        ]
                        (let
                            key =
                                Elm.value "key"

                            memberValue =
                                Elm.value "memberValue"
                         in
                         Element.map
                            (\newKey ->
                                Elm.ifThen
                                    (Elm.and
                                        (Elm.equal newKey keyDefault)
                                        (Elm.equal memberValue valueDefault)
                                    )
                                    (Elm.Gen.Dict.remove key value)
                                    (Elm.Gen.Dict.insert
                                        newKey
                                        memberValue
                                        (Elm.Gen.Dict.remove key value)
                                    )
                            )
                            (Elm.Gen.Tuple.first <| Elm.apply keyEditor [ succ level, key ])
                        )

                valuesView =
                    Elm.lambdaWith
                        [ ( Elm.Pattern.tuple (Elm.Pattern.var "key") (Elm.Pattern.var "memberValue")
                          , Elm.Annotation.tuple keyAnnotation valueAnnotation
                          )
                        ]
                        (let
                            key =
                                Elm.value "key"

                            memberValue =
                                Elm.value "memberValue"
                         in
                         Element.map
                            (\newValue ->
                                Elm.ifThen
                                    (Elm.and
                                        (Elm.equal key keyDefault)
                                        (Elm.equal newValue valueDefault)
                                    )
                                    (Elm.Gen.Dict.remove key value)
                                    (Elm.Gen.Dict.insert key newValue value)
                            )
                            (Elm.Gen.Tuple.first <| Elm.apply valueEditor [ succ level, memberValue ])
                        )
            in
            Elm.letIn
                [ Elm.Let.value "keysColumn" keysColumn
                , Elm.Let.value "valuesColumn" valuesColumn
                ]
                (Elm.tuple
                    (Elm.apply Element.id_.table
                        [ Elm.list (styled <| Just level)
                        , Elm.record
                            [ Elm.field "data"
                                (Elm.append
                                    (Elm.Gen.Dict.toList value)
                                    (Elm.list [ Elm.tuple keyDefault valueDefault ])
                                )
                            , Elm.field "columns" <|
                                Elm.list
                                    [ Elm.value "keysColumn"
                                    , Elm.value "valuesColumn"
                                    ]
                            ]
                        ]
                        |> Elm.withType (Element.types_.element dictAnnotation)
                    )
                    Elm.Gen.Basics.make_.bool.false
                )
        )


listEditor : Elm.Declaration
listEditor =
    let
        valueAnnotation =
            Elm.Annotation.var "e"

        listAnnotation =
            Elm.Annotation.list valueAnnotation
    in
    Elm.fn5 "listEditor"
        ( "typeName", Elm.Annotation.string )
        ( "valueEditor", editorType valueAnnotation )
        ( "valueDefault", valueAnnotation )
        levelArg
        ( "value", listAnnotation )
        (\typeName valueEditor valueDefault level value ->
            let
                delButton =
                    Element.el
                        [ Element.paddingEach
                            { left = Elm.int 0
                            , right = rythm
                            , top = Elm.int 0
                            , bottom = Elm.int 0
                            }
                        , Element.alignRight
                        ]
                        (Gen.Theme.tabButton
                            (styled Nothing
                                ++ [ Background.gradient
                                        { angle = Elm.int 0
                                        , steps =
                                            [ getColor <| succ level
                                            , Gen.Theme.colors.delete
                                            , Gen.Theme.colors.delete
                                            ]
                                        }
                                   , Border.widthEach
                                        { left = Elm.int 1
                                        , top = Elm.int 1
                                        , right = Elm.int 1
                                        , bottom = Elm.int 0
                                        }
                                   , Border.roundEach
                                        { topLeft = rythm
                                        , topRight = rythm
                                        , bottomLeft = Elm.int 0
                                        , bottomRight = Elm.int 0
                                        }
                                   , Element.htmlAttribute <|
                                        Elm.apply (Elm.valueFrom [ "Html", "Attributes" ] "style")
                                            [ Elm.string "z-index"
                                            , Elm.string "1"
                                            ]
                                   ]
                            )
                            { onPress = Elm.Gen.Maybe.make_.maybe.just valueDefault
                            , label = Element.text <| Elm.string "Delete"
                            }
                        )

                rows =
                    Elm.apply Elm.Gen.List.id_.indexedMap
                        [ Elm.lambdaWith
                            [ ( Elm.Pattern.var "i", Elm.Annotation.int )
                            , ( Elm.Pattern.var "row", valueAnnotation )
                            ]
                            (Element.map
                                (\newValue ->
                                    Elm.ifThen
                                        (Elm.equal newValue valueDefault)
                                        (Elm.apply Elm.Gen.List.Extra.id_.removeAt
                                            [ Elm.value "i", value ]
                                        )
                                        (Elm.apply Elm.Gen.List.Extra.id_.setAt
                                            [ Elm.value "i"
                                            , newValue
                                            , value
                                            ]
                                        )
                                )
                                (Element.column
                                    [ Element.width Element.fill ]
                                    [ delButton
                                    , Element.el
                                        [ Element.width Element.fill
                                        , Element.moveUp (Elm.int 1)
                                        ]
                                        (Elm.Gen.Tuple.first <|
                                            Elm.apply valueEditor [ succ level, Elm.value "row" ]
                                        )
                                    ]
                                )
                            )
                        , value
                        ]
            in
            Elm.letIn [ Elm.Let.value "rows" rows ]
                (Elm.tuple
                    (Element.column [ Element.width Element.fill ]
                        [ Elm.apply Element.id_.column
                            [ Elm.list (styled <| Just level)
                            , Elm.value "rows"
                            ]
                        , Element.el
                            [ Element.paddingEach
                                { left = rythm
                                , right = rythm
                                , bottom = Elm.int 0
                                , top = Elm.int 0
                                }
                            , Element.alignRight
                            ]
                            (Gen.Theme.button
                                (styled Nothing
                                    ++ [ Background.gradient
                                            { angle = Elm.int 0
                                            , steps =
                                                [ Gen.Theme.colors.addNew
                                                , Gen.Theme.colors.addNew
                                                , Gen.Theme.colors.addNew
                                                , getColor level
                                                ]
                                            }
                                       , Border.widthEach
                                            { top = Elm.int 0
                                            , left = Elm.int 1
                                            , right = Elm.int 1
                                            , bottom = Elm.int 1
                                            }
                                       , Border.roundEach
                                            { topLeft = Elm.int 0
                                            , topRight = Elm.int 0
                                            , bottomLeft = rythm
                                            , bottomRight = rythm
                                            }
                                       , Element.moveUp <| Elm.int 1
                                       ]
                                )
                                { onPress =
                                    Elm.Gen.Maybe.make_.maybe.just <|
                                        Elm.append value (Elm.list [ valueDefault ])
                                , label = Element.text <| Elm.append (Elm.string "Add new ") typeName
                                }
                            )
                        ]
                        |> Elm.withType (Element.types_.element listAnnotation)
                    )
                    Elm.Gen.Basics.make_.bool.false
                )
        )


tupleEditor : Elm.Declaration
tupleEditor =
    let
        leftAnnotation =
            Elm.Annotation.var "l"

        rightAnnotation =
            Elm.Annotation.var "r"

        tupleAnnotation =
            Elm.Annotation.tuple leftAnnotation rightAnnotation
    in
    Elm.functionWith "tupleEditor"
        [ ( editorType leftAnnotation, Elm.Pattern.var "leftEditor" )
        , ( leftAnnotation, Elm.Pattern.wildcard )
        , ( editorType rightAnnotation, Elm.Pattern.var "rightEditor" )
        , ( rightAnnotation, Elm.Pattern.wildcard )
        , ( Elm.Annotation.int, Elm.Pattern.var "level" )
        , ( tupleAnnotation, Elm.Pattern.tuple (Elm.Pattern.var "left") (Elm.Pattern.var "right") )
        ]
        (let
            left =
                Elm.value "left"

            right =
                Elm.value "right"

            level =
                Elm.value "level"
         in
         Elm.letIn
            [ Elm.Let.destructure
                (Elm.Pattern.tuple
                    (Elm.Pattern.var "le")
                    (Elm.Pattern.var "lb")
                )
                (Elm.apply (Elm.value "leftEditor") [ succ level, left ])
            , Elm.Let.destructure
                (Elm.Pattern.tuple
                    (Elm.Pattern.var "re")
                    (Elm.Pattern.var "rb")
                )
                (Elm.apply (Elm.value "rightEditor") [ succ level, right ])
            , Elm.Let.value "editor" <|
                Elm.apply
                    (Elm.ifThen (Elm.and (Elm.value "lb") (Elm.value "rb"))
                        Element.id_.row
                        Element.id_.column
                    )
                    [ Elm.list <| styled <| Just level
                    , Elm.list
                        [ Element.map
                            (\newValue ->
                                Elm.tuple newValue right
                            )
                            (Elm.value "le")
                        , Element.map
                            (\newValue ->
                                Elm.tuple left newValue
                            )
                            (Elm.value "re")
                        ]
                    ]
            ]
         <|
            Elm.tuple
                (Elm.value "editor"
                    |> Elm.withType (Element.types_.element tupleAnnotation)
                )
                Elm.Gen.Basics.make_.bool.false
        )


maybeEditor : Elm.Declaration
maybeEditor =
    let
        valueAnnotation =
            Elm.Annotation.var "e"

        maybeAnnotation =
            Elm.Annotation.maybe valueAnnotation
    in
    Elm.fn5 "maybeEditor"
        ( "typeName", Elm.Annotation.string )
        ( "valueEditor", editorType valueAnnotation )
        ( "valueDefault", valueAnnotation )
        levelArg
        ( "value", maybeAnnotation )
        (\typeName valueEditor valueDefault level value ->
            let
                extracted =
                    [ ( Elm.Pattern.named "Nothing" []
                      , valueDefault
                      )
                    , ( Elm.Pattern.named "Just"
                            [ Elm.Pattern.var "inner" ]
                      , Elm.value "inner"
                      )
                    ]
                        |> Elm.caseOf value

                variantRow =
                    Input.radioRow [ spacing ]
                        { options = options
                        , onChange = Elm.Gen.Basics.identity
                        , selected = Elm.Gen.Maybe.make_.maybe.just value
                        , label = noLabel
                        }

                inputsRow =
                    [ ( Elm.Pattern.named "Nothing" []
                      , Element.none
                      )
                    , ( Elm.Pattern.named
                            "Just"
                            [ Elm.Pattern.var "inner" ]
                      , Element.map Elm.Gen.Maybe.make_.maybe.just
                            (Elm.Gen.Tuple.first <| Elm.apply valueEditor [ succ level, Elm.value "inner" ])
                      )
                    ]
                        |> Elm.caseOf value

                options =
                    [ Input.option
                        (Elm.value "Nothing")
                        (Element.text <| Elm.string "Nothing")
                    , Input.option
                        (Elm.Gen.Maybe.make_.maybe.just <| Elm.value "extracted")
                        (Element.text typeName)
                    ]
            in
            Elm.letIn
                [ Elm.Let.value "extracted" extracted
                , Elm.Let.value "variantRow" variantRow
                , Elm.Let.value "inputsRow" inputsRow
                ]
                (Elm.tuple
                    (Element.column (styled (Just level))
                        [ Elm.value "variantRow"
                        , Elm.value "inputsRow"
                        ]
                        |> Elm.withType (Element.types_.element maybeAnnotation)
                    )
                    Elm.Gen.Basics.make_.bool.false
                )
        )


typeDeclToEditor : TypeDecl -> Elm.Declaration
typeDeclToEditor decl =
    let
        ( name, view ) =
            case decl of
                Alias n t ->
                    ( n, typeToEditor t )

                Custom n vs ->
                    ( n, customEditor n vs )

        tipe =
            Elm.Annotation.named [ "Model" ] name

        editorName =
            firstLower name ++ "Editor"

        declaration =
            (\level value ->
                view level value
                    |> Elm.withType
                        (Elm.Annotation.tuple
                            (Element.types_.element tipe)
                            Elm.Annotation.bool
                        )
            )
                |> Elm.fn2 editorName levelArg ( "value", tipe )
                |> Elm.expose
    in
    declaration


typeDeclToDefault : TypeDecl -> Elm.Declaration
typeDeclToDefault decl =
    let
        ( name, default ) =
            case decl of
                Alias n t ->
                    ( n, typeToDefault t )

                Custom n vs ->
                    ( n, customTypeToDefault n vs )

        tipe =
            Elm.Annotation.named [ "Model" ] name
    in
    default
        |> Elm.withType tipe
        |> Elm.declaration (firstLower name ++ "Default")
        |> Elm.expose


customTypeToDefault : String -> List Variant -> Elm.Expression
customTypeToDefault name variants =
    let
        isVariantRecursive ( _, args ) =
            List.any isEmptyBecauseRecursive args

        isEmptyBecauseRecursive tipe =
            case tipe of
                Unit ->
                    False

                Maybe _ ->
                    False

                List _ ->
                    False

                Array _ ->
                    False

                Dict _ _ ->
                    False

                Set _ ->
                    False

                Tuple l r ->
                    isEmptyBecauseRecursive l || isEmptyBecauseRecursive r

                Triple l m r ->
                    isEmptyBecauseRecursive l || isEmptyBecauseRecursive m || isEmptyBecauseRecursive r

                Result _ o ->
                    isEmptyBecauseRecursive o

                Object fs ->
                    List.any (\( _, t ) -> isEmptyBecauseRecursive t) fs

                Named n ->
                    n == name
    in
    variants
        |> List.filter (not << isVariantRecursive)
        |> List.head
        |> Maybe.map
            (\( variantName, variantArgs ) ->
                variantArgs
                    |> List.map typeToDefault
                    |> Elm.apply (Elm.valueFrom [ "Model" ] variantName)
            )
        |> Maybe.withDefault
            ("It is not possible to generate a default for a custom type with no variants"
                |> Elm.string
                |> Elm.Gen.Debug.todo
            )


customEditor : String -> List Variant -> (Elm.Expression -> Elm.Expression -> Elm.Expression)
customEditor typeName variants level value =
    -- TODO: Deduplicate dis :/
    let
        extractedFields =
            variants
                |> List.foldr
                    (\( _, args ) acc ->
                        args
                            |> List.foldl
                                (\argType variablesCount ->
                                    let
                                        variable =
                                            typeToVariable argType
                                    in
                                    case Dict.get variable variablesCount of
                                        Nothing ->
                                            Dict.insert variable ( 1, argType ) variablesCount

                                        Just ( count, _ ) ->
                                            Dict.insert variable ( count + 1, argType ) variablesCount
                                )
                                Dict.empty
                            |> Dict.union acc
                    )
                    Dict.empty
                |> Dict.toList
                |> List.concatMap
                    (\( name, ( n, tipe ) ) ->
                        List.range 1 n
                            |> List.map
                                (\i ->
                                    ( if i == 1 then
                                        name ++ "Extracted"

                                      else
                                        name ++ String.fromInt i ++ "Extracted"
                                    , typeToDefault tipe
                                    )
                                )
                    )

        extractedDefault =
            extractedFields
                |> List.map (\( name, fieldDefault ) -> Elm.field name fieldDefault)
                |> Elm.record

        extractedPattern =
            extractedFields
                |> List.map Tuple.first
                |> Elm.Pattern.fields

        extractedValues =
            variants
                |> List.map
                    (\( variantName, args ) ->
                        let
                            argNames =
                                args
                                    |> List.foldl
                                        (\argType ( variables, variablesCount ) ->
                                            let
                                                variable =
                                                    typeToVariable argType
                                            in
                                            case Dict.get variable variablesCount of
                                                Nothing ->
                                                    ( variable :: variables
                                                    , Dict.insert variable 2 variablesCount
                                                    )

                                                Just count ->
                                                    ( (variable ++ String.fromInt count) :: variables
                                                    , Dict.insert variable (count + 1) variablesCount
                                                    )
                                        )
                                        ( [], Dict.empty )
                                    |> Tuple.first
                                    |> List.reverse
                        in
                        ( Elm.Pattern.namedFrom [ "Model" ]
                            variantName
                            (List.map Elm.Pattern.var argNames)
                        , if List.isEmpty argNames then
                            Elm.value "extractedDefault"

                          else
                            argNames
                                |> List.map (\name -> ( name ++ "Extracted", Elm.value name ))
                                |> Elm.updateRecord "extractedDefault"
                        )
                    )
                |> Elm.caseOf value

        variantRow =
            Input.radioRow [ spacing ]
                { options = List.map variantToRadioOption variants
                , onChange = Elm.Gen.Basics.identity
                , selected = Elm.Gen.Maybe.make_.maybe.just value
                , label = noLabel
                }

        inputsRow =
            if List.length variants == 1 then
                variants
                    |> List.map (variantToInputsRowCase <| Elm.minus level (Elm.int 1))
                    |> Elm.caseOf value

            else
                variants
                    |> List.map (variantToInputsRowCase level)
                    |> Elm.caseOf value

        variantToRadioOption ( variantName, args ) =
            let
                ctorArgs =
                    args
                        |> List.foldl
                            (\argType ( variables, variablesCount ) ->
                                let
                                    variableBasename =
                                        typeToVariable argType

                                    ( variable, newCount ) =
                                        case Dict.get variableBasename variablesCount of
                                            Nothing ->
                                                ( variableBasename ++ "Extracted"
                                                , 2
                                                )

                                            Just count ->
                                                ( variableBasename ++ String.fromInt count ++ "Extracted"
                                                , count + 1
                                                )
                                in
                                ( Elm.value variable :: variables
                                , Dict.insert variableBasename newCount variablesCount
                                )
                            )
                            ( [], Dict.empty )
                        |> Tuple.first
                        |> List.reverse
            in
            Input.option
                (if List.isEmpty ctorArgs then
                    Elm.valueFrom [ "Model" ] variantName

                 else
                    Elm.apply (Elm.valueFrom [ "Model" ] variantName) ctorArgs
                )
                (Element.text <|
                    Elm.string <|
                        splitOnUppercase <|
                            if String.startsWith typeName variantName then
                                String.dropLeft (String.length typeName) variantName

                            else
                                variantName
                )

        editor =
            if List.isEmpty extractedFields then
                Elm.letIn
                    [ Elm.Let.value "variantRow" variantRow ]
                    (Element.el (styled (Just level))
                        (Elm.value "variantRow")
                    )

            else if List.length variants == 1 then
                Elm.letIn
                    [ Elm.Let.destructure extractedPattern extractedValues
                    , Elm.Let.value "extractedDefault" extractedDefault
                    , Elm.Let.value "inputsRow" inputsRow
                    ]
                    (Elm.apply Element.id_.row
                        [ Elm.list
                            [ Element.width Element.fill
                            , spacing
                            ]
                        , Elm.value "inputsRow"
                        ]
                    )

            else
                Elm.letIn
                    [ Elm.Let.destructure extractedPattern extractedValues
                    , Elm.Let.value "extractedDefault" extractedDefault
                    , Elm.Let.value "variantRow" variantRow
                    , Elm.Let.value "inputsRow" inputsRow
                    ]
                    (Element.column (styled (Just level))
                        [ Elm.value "variantRow"
                        , Elm.apply Element.id_.row
                            [ Elm.list
                                [ Element.width Element.fill
                                , spacing
                                ]
                            , Elm.value "inputsRow"
                            ]
                        ]
                    )
    in
    Elm.tuple editor Elm.Gen.Basics.make_.bool.false


splitOnUppercase : String -> String
splitOnUppercase str =
    str
        |> String.toList
        |> List.concatMap
            (\c ->
                if Char.isUpper c then
                    [ ' ', Char.toLower c ]

                else
                    [ c ]
            )
        |> String.fromList
        |> String.trim
        |> firstUpper


typeToVariable : Type -> String
typeToVariable tipe =
    let
        innerTypeToVariable t =
            case t of
                Unit ->
                    "Unit"

                Maybe i ->
                    "Maybe" ++ innerTypeToVariable i

                List i ->
                    "List" ++ innerTypeToVariable i

                Array i ->
                    "Array" ++ innerTypeToVariable i

                Set i ->
                    "Set" ++ innerTypeToVariable i

                Dict l r ->
                    "Dict" ++ innerTypeToVariable l ++ innerTypeToVariable r

                Tuple l r ->
                    "Tuple" ++ innerTypeToVariable l ++ innerTypeToVariable r

                Result l r ->
                    "Result" ++ innerTypeToVariable l ++ innerTypeToVariable r

                Triple l m r ->
                    "Triple" ++ innerTypeToVariable l ++ innerTypeToVariable m ++ innerTypeToVariable r

                Object fields ->
                    fields
                        |> List.map
                            (\( fname, ftipe ) ->
                                fname ++ innerTypeToVariable ftipe
                            )
                        |> String.concat

                Named n ->
                    n
    in
    firstLower <| innerTypeToVariable tipe


variantToInputsRowCase : Elm.Expression -> ( String, List Type ) -> ( Elm.Pattern.Pattern, Elm.Expression )
variantToInputsRowCase level ( variantName, args ) =
    let
        argNamesAndTypes =
            args
                |> List.foldl
                    (\argType ( variables, variablesCount ) ->
                        let
                            variableBasename =
                                typeToVariable argType

                            ( variableName, newCount ) =
                                case Dict.get variableBasename variablesCount of
                                    Nothing ->
                                        ( variableBasename
                                        , 2
                                        )

                                    Just count ->
                                        ( variableBasename ++ String.fromInt count
                                        , count + 1
                                        )
                        in
                        ( ( variableName, argType ) :: variables
                        , Dict.insert variableBasename newCount variablesCount
                        )
                    )
                    ( [], Dict.empty )
                |> Tuple.first
                |> List.reverse
    in
    ( Elm.Pattern.namedFrom [ "Model" ]
        variantName
        (List.map (Tuple.first >> Elm.Pattern.var) argNamesAndTypes)
    , argNamesAndTypes
        |> List.indexedMap
            (\i ( name, tipe ) ->
                Element.map
                    (\newValue ->
                        Elm.apply (Elm.valueFrom [ "Model" ] variantName)
                            (List.indexedMap
                                (\j ( innerName, _ ) ->
                                    if i == j then
                                        newValue

                                    else
                                        Elm.value innerName
                                )
                                argNamesAndTypes
                            )
                    )
                    (Elm.Gen.Tuple.first <| typeToEditor tipe (succ level) <| Elm.value name)
            )
        |> Elm.list
    )


typeToEditor : Type -> Elm.Expression -> Elm.Expression -> Elm.Expression
typeToEditor =
    typeToEditorAndDefault >> Tuple.first


typeToDefault : Type -> Elm.Expression
typeToDefault =
    typeToEditorAndDefault >> Tuple.second


typeToEditorAndDefault : Type -> ( Elm.Expression -> Elm.Expression -> Elm.Expression, Elm.Expression )
typeToEditorAndDefault tipe =
    let
        typeToEditorNameAndDefault t =
            let
                ( ed, def ) =
                    typeToEditorAndDefault t
            in
            ( Elm.lambdaBetaReduced "level" Elm.Annotation.int <|
                \level -> Elm.lambdaBetaReduced "value" (typeToAnnotation tipe) (\value -> ed level value)
            , def
            )

        map ef df t1 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1
            in
            ( \level value ->
                Elm.apply (Elm.value ef)
                    [ Elm.string <| typeToString True t1
                    , e1
                    , d1
                    , level
                    , value
                    ]
            , df d1
            )

        map2 ef df t1 t2 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1

                ( e2, d2 ) =
                    typeToEditorNameAndDefault t2
            in
            ( \level value ->
                Elm.apply (Elm.value ef)
                    [ e1
                    , d1
                    , e2
                    , d2
                    , level
                    , value
                    ]
            , df d1 d2
            )

        map3 ef df t1 t2 t3 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1

                ( e2, d2 ) =
                    typeToEditorNameAndDefault t2

                ( e3, d3 ) =
                    typeToEditorNameAndDefault t3
            in
            ( \level value ->
                Elm.apply (Elm.value ef)
                    [ e1
                    , d1
                    , e2
                    , d2
                    , e3
                    , d3
                    , level
                    , value
                    ]
            , df d1 d2 d3
            )
    in
    case tipe of
        Unit ->
            ( \_ _ -> Element.none, Elm.unit )

        Maybe inner ->
            map "maybeEditor" (always Elm.Gen.Maybe.make_.maybe.nothing) inner

        List inner ->
            map "listEditor" (always (Elm.list [])) inner

        Array inner ->
            map "arrayEditor" (always Elm.Gen.Array.empty) inner

        Set inner ->
            map "setEditor" (always Elm.Gen.Set.empty) inner

        Dict k v ->
            map2 "dictEditor" (\_ _ -> Elm.Gen.Dict.empty) k v

        Tuple a b ->
            map2 "tupleEditor" Elm.tuple a b

        Result e o ->
            map2 "resultEditor" (\_ okDefault -> Elm.Gen.Result.make_.result.ok okDefault) e o

        Triple a b c ->
            map3 "tripleEditor" Elm.triple a b c

        Object fields ->
            objectEditorAndDefault tipe fields

        Named n ->
            ( \level value ->
                Elm.apply (Elm.value <| firstLower n ++ "Editor")
                    [ level, value ]
            , case n of
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
            )


objectEditorAndDefault :
    Type
    -> List ( String, Type )
    ->
        ( Elm.Expression -> Elm.Expression -> Elm.Expression
        , Elm.Expression
        )
objectEditorAndDefault tipe fields =
    ( \level value ->
        let
            raw =
                fields
                    |> List.map
                        (\( fieldName, fieldType ) ->
                            Elm.letIn
                                [ Elm.Let.destructure
                                    (Elm.Pattern.tuple
                                        (Elm.Pattern.var "editor")
                                        (Elm.Pattern.var "simple")
                                    )
                                    (typeToEditor fieldType
                                        (succ level)
                                        (Elm.get fieldName value)
                                    )
                                ]
                                (Elm.triple
                                    (Elm.string <| firstUpper fieldName)
                                    (Element.map
                                        (\newValue ->
                                            Elm.letIn
                                                [ Elm.Let.value "updating" value ]
                                                (Elm.updateRecord "updating"
                                                    [ ( fieldName, newValue ) ]
                                                )
                                        )
                                        (Elm.withType
                                            (Element.types_.element (typeToAnnotation fieldType))
                                            (Elm.value "editor")
                                        )
                                    )
                                    (Elm.value "simple")
                                )
                        )
                    |> Elm.list

            simples =
                Elm.value "raw"
                    |> Elm.pipe
                        (Elm.apply
                            Elm.Gen.List.id_.filterMap
                            [ Elm.lambdaWith
                                [ ( Elm.Pattern.triple
                                        (Elm.Pattern.var "fieldName")
                                        (Elm.Pattern.var "fieldEditor")
                                        (Elm.Pattern.var "simple")
                                  , Elm.Annotation.triple
                                        Elm.Annotation.string
                                        (Element.types_.element <|
                                            typeToAnnotation tipe
                                        )
                                        Elm.Annotation.bool
                                  )
                                ]
                                (Elm.ifThen (Elm.value "simple")
                                    (Elm.Gen.Maybe.make_.maybe.just <|
                                        Elm.tuple
                                            (Element.el [ Element.centerY ] <|
                                                Element.text <|
                                                    Elm.value "fieldName"
                                            )
                                            (Elm.value "fieldEditor")
                                    )
                                    Elm.Gen.Maybe.make_.maybe.nothing
                                )
                            ]
                        )

            tupleAnnotation =
                Elm.Annotation.tuple
                    (Element.types_.element <|
                        typeToAnnotation tipe
                    )
                    (Element.types_.element <|
                        typeToAnnotation tipe
                    )

            simplesTable =
                Elm.ifThen
                    (Elm.lte
                        (Elm.apply Elm.Gen.List.id_.length
                            [ Elm.value "simples" ]
                        )
                        (Elm.int 2)
                    )
                    (Elm.value "simples"
                        |> Elm.pipe
                            (Elm.apply Elm.Gen.List.id_.map
                                [ Elm.lambda "pair"
                                    tupleAnnotation
                                    (\pair ->
                                        Element.row
                                            [ spacing
                                            , Element.width Element.fill
                                            ]
                                            [ Elm.Gen.Tuple.first pair
                                            , Elm.Gen.Tuple.second pair
                                            ]
                                    )
                                ]
                            )
                        |> Elm.pipe
                            (Elm.apply Element.id_.row
                                [ Elm.list
                                    [ spacing
                                    , Element.width Element.fill
                                    ]
                                ]
                            )
                    )
                    (Elm.apply
                        Element.id_.table
                        [ Elm.list [ spacing, Element.width Element.fill ]
                        , Elm.record
                            [ Elm.field "columns"
                                (Elm.list
                                    [ Element.make_.column
                                        { header = Element.none
                                        , width = Element.shrink
                                        , view =
                                            Elm.lambda "pair"
                                                tupleAnnotation
                                                Elm.Gen.Tuple.first
                                        }
                                    , Element.make_.column
                                        { header = Element.none
                                        , width = Element.fill
                                        , view =
                                            Elm.lambda "pair"
                                                tupleAnnotation
                                                Elm.Gen.Tuple.second
                                        }
                                    ]
                                )
                            , Elm.field "data" (Elm.value "simples")
                            ]
                        ]
                    )
                    |> Elm.withType
                        (Element.types_.element <|
                            typeToAnnotation tipe
                        )

            complexes =
                Elm.value "raw"
                    |> Elm.pipe
                        (Elm.apply
                            Elm.Gen.List.id_.concatMap
                            [ Elm.lambdaWith
                                [ ( Elm.Pattern.triple
                                        (Elm.Pattern.var "fieldName")
                                        (Elm.Pattern.var "fieldEditor")
                                        (Elm.Pattern.var "simple")
                                  , Elm.Annotation.triple
                                        Elm.Annotation.string
                                        (Element.types_.element <|
                                            typeToAnnotation tipe
                                        )
                                        Elm.Annotation.bool
                                  )
                                ]
                                (Elm.ifThen (Elm.value "simple")
                                    (Elm.list [])
                                    (Elm.list
                                        [ Element.text <| Elm.value "fieldName"
                                        , Elm.value "fieldEditor"
                                        ]
                                    )
                                )
                            ]
                        )
        in
        Elm.letIn
            [ Elm.Let.value "raw" raw
            , Elm.Let.value "simples" simples
            , Elm.Let.value "simplesTable" simplesTable
            , Elm.Let.value "complexes" complexes
            ]
            (Elm.tuple
                (Elm.apply Element.id_.column
                    [ Elm.list <| (Element.width Element.fill :: styled (Just level))
                    , Elm.cons (Elm.value "simplesTable") (Elm.value "complexes")
                    ]
                )
                Elm.Gen.Basics.make_.bool.false
            )
    , fields
        |> List.map
            (\( fieldName, fieldType ) ->
                fieldType
                    |> typeToEditorAndDefault
                    |> Tuple.second
                    |> Elm.field fieldName
            )
        |> Elm.record
    )

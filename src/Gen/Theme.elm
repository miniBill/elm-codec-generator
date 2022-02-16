module Gen.Theme exposing (button, colors, customEditor, enumEditor, levelLambda, map, objectEditor, tabButton, tupleEditor, types_)

import Elm
import Elm.Annotation


fromEditorTheme : String -> Elm.Expression
fromEditorTheme name =
    Elm.value
        { importFrom = [ "Frontend", "EditorTheme" ]
        , name = name
        , annotation = Nothing
        }


colors :
    { addNew : Elm.Expression
    , delete : Elm.Expression
    }
colors =
    { addNew =
        fromEditorTheme "colors"
            |> Elm.get "addNew"
    , delete =
        fromEditorTheme "colors"
            |> Elm.get "delete"
    }


button :
    List Elm.Expression
    -> { onPress : Elm.Expression, label : Elm.Expression }
    -> Elm.Expression
button attrs { onPress, label } =
    Elm.apply (fromEditorTheme "button")
        [ Elm.list attrs
        , Elm.record
            [ Elm.field "onPress" onPress
            , Elm.field "label" label
            ]
        ]


tabButton :
    List Elm.Expression
    -> { onPress : Elm.Expression, label : Elm.Expression }
    -> Elm.Expression
tabButton attrs { onPress, label } =
    Elm.apply (fromEditorTheme "tabButton")
        [ Elm.list attrs
        , Elm.record
            [ Elm.field "onPress" onPress
            , Elm.field "label" label
            ]
        ]


objectEditor : Elm.Expression -> Elm.Expression -> Elm.Expression
objectEditor simplesTable complexes =
    Elm.apply (fromEditorTheme "objectEditor")
        [ simplesTable, complexes ]


customEditor : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
customEditor variantRow inputsRow value =
    Elm.apply (fromEditorTheme "customEditor")
        [ variantRow, inputsRow, value ]


tupleEditor : Elm.Expression -> Bool -> Elm.Expression -> Bool -> Elm.Expression -> Elm.Expression
tupleEditor leftEditor leftSimple rightEditor rightSimple value =
    Elm.apply (fromEditorTheme "tupleEditor")
        [ leftEditor, Elm.bool leftSimple, rightEditor, Elm.bool rightSimple, value ]


enumEditor : Elm.Expression -> Elm.Expression -> Elm.Expression
enumEditor variants value =
    Elm.apply (fromEditorTheme "enumEditor")
        [ variants, value ]


map : (Elm.Expression -> Elm.Expression) -> Elm.Annotation.Annotation -> Elm.Expression -> Elm.Expression
map f tipe e =
    Elm.apply (fromEditorTheme "map")
        [ Elm.functionReduced "f" tipe f
        , e
        ]


types_ :
    { editor : Elm.Annotation.Annotation -> Elm.Annotation.Annotation
    }
types_ =
    { editor = \v -> Elm.Annotation.namedWith [ "Frontend", "EditorTheme" ] "Editor" [ v ]
    }


levelLambda : (Elm.Expression -> Elm.Expression) -> Elm.Expression
levelLambda f =
    Elm.functionReduced "level" Elm.Annotation.int f

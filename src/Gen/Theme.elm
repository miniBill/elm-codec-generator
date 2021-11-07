module Gen.Theme exposing (button, tabButton)

import Elm


button :
    List Elm.Expression
    -> { onPress : Elm.Expression, label : Elm.Expression }
    -> Elm.Expression
button attrs { onPress, label } =
    Elm.apply (Elm.valueFrom [ "Theme" ] "button")
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
    Elm.apply (Elm.valueFrom [ "Theme" ] "tabButton")
        [ Elm.list attrs
        , Elm.record
            [ Elm.field "onPress" onPress
            , Elm.field "label" label
            ]
        ]

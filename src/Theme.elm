module Theme exposing (button, padding, rythm, spacing)

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Input as Input


rythm : number
rythm =
    10


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


button : { onPress : Maybe msg, label : Element msg } -> Element msg
button =
    Input.button [ Border.width 1, padding ]

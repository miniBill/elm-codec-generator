module Editor exposing (Editor, enumEditor, map, objectEditor, tupleEditor)

import Element exposing (Attribute, Element, shrink, text)
import Element.Border as Border
import Element.Input as Input


type alias Editor a =
    a -> Element a


map : (b -> a) -> (a -> b) -> Editor a -> Editor b
map there backAgain editor x =
    Element.map backAgain (editor <| there x)


tupleEditor : Editor a -> Editor b -> Editor ( a, b )
tupleEditor leftEditor rightEditor ( left, right ) =
    Element.row [ spacing ]
        [ Element.map (\newLeft -> ( newLeft, right )) <| leftEditor left
        , Element.map (\newRight -> ( left, newRight )) <| rightEditor right
        ]


spacing : Attribute msg
spacing =
    Element.spacing 10


padding : Attribute msg
padding =
    Element.padding 10


objectEditor : List ( String, Editor a ) -> List ( String, Editor a ) -> Editor a
objectEditor simples complexes value =
    Element.column [ spacing ]
        [ Element.table [ spacing ]
            { data = simples
            , columns =
                [ { header = Element.none
                  , width = shrink
                  , view = \( label, _ ) -> text label
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \( _, fieldEditor ) -> fieldEditor value
                  }
                ]
            }
        , complexes
            |> List.map
                (\( label, fieldEditor ) ->
                    Element.column [ Border.width 1, padding ]
                        [ text label
                        , fieldEditor value
                        ]
                )
            |> Element.wrappedRow [ spacing ]
        ]


enumEditor : List ( String, a ) -> Editor a
enumEditor options selected =
    Input.radioRow []
        { options =
            List.map
                (\( label, value ) -> Input.option value (text label))
                options
        , label = Input.labelHidden ""
        , onChange = identity
        , selected = Just selected
        }

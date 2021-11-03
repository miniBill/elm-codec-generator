module Model exposing (Type(..), TypeDecl(..), Variant, typeToAnnotation)

import Elm.Annotation


type TypeDecl
    = Alias String Type
    | Custom String (List Variant)


type alias Variant =
    ( String, List Type )


type Type
    = Maybe Type
    | List Type
    | Array Type
    | Dict Type Type
    | Set Type
    | Tuple Type Type
    | Triple Type Type Type
    | Result Type Type
    | Object (List ( String, Type ))
    | Unit
    | Named String


typeToAnnotation : Type -> Elm.Annotation.Annotation
typeToAnnotation t =
    case t of
        Unit ->
            Elm.Annotation.unit

        Maybe c ->
            Elm.Annotation.maybe <| typeToAnnotation c

        List c ->
            Elm.Annotation.list <| typeToAnnotation c

        Array c ->
            Elm.Annotation.namedWith [ "Array" ] "Array" [ typeToAnnotation c ]

        Set c ->
            Elm.Annotation.set <| typeToAnnotation c

        Dict k v ->
            Elm.Annotation.namedWith [ "Dict" ] "Dict" [ typeToAnnotation k, typeToAnnotation v ]

        Result e o ->
            Elm.Annotation.result (typeToAnnotation e) (typeToAnnotation o)

        Tuple a b ->
            Elm.Annotation.tuple (typeToAnnotation a) (typeToAnnotation b)

        Triple a b c ->
            Elm.Annotation.triple (typeToAnnotation a) (typeToAnnotation b) (typeToAnnotation c)

        Object fs ->
            fs
                |> List.map (Tuple.mapSecond typeToAnnotation)
                |> Elm.Annotation.record

        Named n ->
            Elm.Annotation.named [ "Model" ] n

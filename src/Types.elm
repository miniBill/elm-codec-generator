module Types exposing (Config, Type(..), TypeDecl(..), Variant, typeToAnnotation)

import Elm.Annotation
import Elm.Syntax.ModuleName exposing (ModuleName)


type alias Config =
    { optimizeDefaultFields : Bool
    , moduleName : ModuleName
    }


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


typeToAnnotation : Config -> Type -> Elm.Annotation.Annotation
typeToAnnotation config t =
    case t of
        Unit ->
            Elm.Annotation.unit

        Maybe c ->
            Elm.Annotation.maybe <| typeToAnnotation config c

        List c ->
            Elm.Annotation.list <| typeToAnnotation config c

        Array c ->
            Elm.Annotation.namedWith [ "Array" ] "Array" [ typeToAnnotation config c ]

        Set c ->
            Elm.Annotation.set <| typeToAnnotation config c

        Dict k v ->
            Elm.Annotation.namedWith [ "Dict" ] "Dict" [ typeToAnnotation config k, typeToAnnotation config v ]

        Result e o ->
            Elm.Annotation.result (typeToAnnotation config e) (typeToAnnotation config o)

        Tuple a b ->
            Elm.Annotation.tuple (typeToAnnotation config a) (typeToAnnotation config b)

        Triple a b c ->
            Elm.Annotation.triple (typeToAnnotation config a) (typeToAnnotation config b) (typeToAnnotation config c)

        Object fs ->
            fs
                |> List.map (Tuple.mapSecond (typeToAnnotation config))
                |> Elm.Annotation.record

        Named n ->
            Elm.Annotation.named config.moduleName n

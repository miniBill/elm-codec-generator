module Model exposing (Type(..), TypeDecl(..), Variant)


type TypeDecl
    = Alias String Type
    | Custom String (List Variant)


type alias Variant =
    ( String, List Type )


type Type
    = Record (List ( String, Type ))
    | Array Type
    | List Type
    | Dict Type Type
    | Named String
    | Unit
    | Tuple Type Type
    | Triple Type Type Type
    | Maybe Type
    | Result Type Type

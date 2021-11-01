module Model exposing (Type(..), TypeDecl(..), Variant)


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

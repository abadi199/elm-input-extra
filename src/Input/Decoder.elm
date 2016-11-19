module Input.Decoder exposing (eventDecoder)

import Json.Decode as Json


type alias Event =
    { keyCode : Int
    , ctrlKey : Bool
    , altKey : Bool
    , metaKey : Bool
    }


eventDecoder : Json.Decoder Event
eventDecoder =
    Json.map4 Event
        (Json.field "keyCode" Json.int)
        (Json.field "ctrlKey" Json.bool)
        (Json.field "altKey" Json.bool)
        (Json.field "metaKey" Json.bool)

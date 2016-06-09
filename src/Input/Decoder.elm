module Input.Decoder exposing (eventDecoder)

import Json.Decode as Json exposing ((:=))


type alias Event =
    { keyCode : Int
    , ctrlKey : Bool
    , altKey : Bool
    }


eventDecoder : Json.Decoder Event
eventDecoder =
    Json.object3 Event
        ("keyCode" := Json.int)
        ("ctrlKey" := Json.bool)
        ("altKey" := Json.bool)

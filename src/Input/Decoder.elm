module Input.Decoder exposing (eventDecoder)

import Json.Decode as Json exposing ((:=))


type alias Event =
    { keyCode : Int
    , ctrlKey : Bool
    , altKey : Bool
    , metaKey : Bool
    }


eventDecoder : Json.Decoder Event
eventDecoder =
    Json.object4 Event
        ("keyCode" := Json.int)
        ("ctrlKey" := Json.bool)
        ("altKey" := Json.bool)
        ("metaKey" := Json.bool)

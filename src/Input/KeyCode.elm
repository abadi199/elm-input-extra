module Input.KeyCode exposing (allowedKeyCodes)


allowedKeyCodes : List Int
allowedKeyCodes =
    [ leftArrow
    , upArrow
    , rightArrow
    , downArrow
    , backspace
    , ctrl
    , alt
    , delete
    , tab
    , enter
    , shift
    ]


shift : Int
shift =
    16


ctrl : Int
ctrl =
    17


alt : Int
alt =
    18


delete : Int
delete =
    46


backspace : Int
backspace =
    8


tab : Int
tab =
    9


enter : Int
enter =
    13


leftArrow : Int
leftArrow =
    37


upArrow : Int
upArrow =
    38


rightArrow : Int
rightArrow =
    39


downArrow : Int
downArrow =
    40

# Elm Input Extra

## Number Input

Html input component that only accept numeric values implementing [The Elm Architecture (TEA)](http://guide.elm-lang.org/architecture/index.html).

### Example
```elm
type alias Model =
    { numberInput : Number.Model
    ...
    }

init =
    { numberInput : Number.init
    ...
    }
    
view model =
    form [] 
        [ Number.input 
            { id = "NumberInputExample"
            , maxLength = Just 16
            , maxValue = Nothing
            , minValue = Nothing
            }
            [ class "number-input" ]
            model
        ...
        ]  
       
type Msg 
    = NumberInput Number.Msg
    ...
            
update msg model =
    case msg of
        NumberInput numberInputMsg ->
            let 
                ( numberModel, numberCmd ) =
                    Number.update numberInputMsg model.numberInput
                updateModel = 
                    { model | numberInput = numberModel }
                cmd =
                    Cmd.map NumberInput numberCmd
            in
                ( updatedModel, cmd )
```

### Options
 * `id` is the id of the HTML element.
 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
 * `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.

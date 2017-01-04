# Elm Input Extra

Commonly used Html element with extra functionality.
This library implements [reusable views](https://guide.elm-lang.org/reuse/more.html) instead of nested component, making it fit nicely in your `view` function, and doesn't complicate your `update` function.

### [Live Demo](https://abadi199.github.io/elm-input-extra)

See the [source code](https://github.com/abadi199/elm-input-extra/tree/master/demo) of the demo page.

## Number Input

Html input element that only accept numeric values.

>See: `Input.BigNumber` to work with big number that can't be stored using `Int`

### Options
 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `maxValue` is the maximum number value allowed in this input. Set to `Nothing` for no limit.
 * `minValue` is the minimum number value allowed in this input. Set to `Nothing` for no limit.
 * `onInput` is the Msg tagger for the onInput event.
 * `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

### Example
```elm
type Msg = InputUpdated String | FocusUpdated Bool

Input.Number.input
    { onInput = InputUpdated
    , maxLength = Nothing
    , maxValue = 1000
    , minValue = 10
    , hasFocus = Just FocusUpdated
    }
    [ class "numberInput"
    ...
    ]
    model.currentValue
```

## Text Input

Html input element with extra feature.

### Options
 * `maxLength` is the maximum number of character allowed in this input. Set to `Nothing` for no limit.
 * `onInput` is the Msg tagger for the onInput event.
 * `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

### Example
```elm
type Msg = InputUpdated String | FocusUpdated Bool

Input.Text.input
    { maxLength = 10
    , onInput = InputUpdated
    , hasFocus = Just FocusUpdated
    }
    [ class "textInput"
    ...
    ]
    model.currentValue
```

## Masked Input

Html input element with formatting.

### Options
 * `pattern` is the pattern used to format the input value. e.g.: (###) ###-####
 * `inputCharacter`: is the special character used to represent user input. Default value: `#`
 * `toMsg`: is the Msg for updating internal `State` of the element.
 * `onInput` is the Msg tagger for the onInput event.
 * `hasFocus` is an optional Msg tagger for onFocus/onBlur event.

### Example
```elm
type Msg
    = InputUpdated String
    | StateUpdated MaskedInput.State
    | FocusUpdated Bool

MaskedInput.Text.input
    { pattern = "(###) ###-####"
    , inputCharacter = '#'
    , onInput = InputUpdated
    , toMsg = StateUpdated
    , hasFocus = Just FocusUpdated
    }
    [ class "masked-input"
    ...
    ]
    model.currentState
    model.currentValue
```

## Dropdown

Html select element

### Options
 * `items` is content of the dropdown.
 * `emptyItem` is the item for when the nothing is selected. Set to `Nothing` for no empty item.
 * `onChange` is the message for when the selected value in the dropdown is changed.

### Example
```elm
type Msg = DropdownChanged String

Html.div []
    [ Dropdown.dropdown
        (Dropdown.defaultOptions DropdownChanged)
        [ class "my-dropdown" ]
        model.selectedDropdownValue
    ]
```
## Multi Select

Html select element with multiple selection

### Options
 * `items` is content of the dropdown.
 * `onChange` is the message for when the selected value in the multi-select is changed.

### Example
```elm
type Msg = MultiSelectChanged (List String)

Html.div []
    [ MultiSelect.multiSelect
        (defaultOptions MultiSelectChanged)
        [ class "my-multiSelect" ]
        model.selectedValues
    ]
```

## DatePicker

Moved to [abadi199/datetimepicker](http://package.elm-lang.org/packages/abadi199/datetimepicker/latest)

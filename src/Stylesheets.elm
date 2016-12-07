port module Stylesheets exposing (main)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import DatePicker.Css


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "styles.css", Css.File.compile [ DatePicker.Css.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure

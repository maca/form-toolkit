module Main exposing (main)

import Chapters.Customization as Customization
import Chapters.Fields as Fields
import Chapters.FormEditor as FormEditor
import Chapters.GeocodingAutocomplete as GeocodingAutocomplete
import Chapters.GettingStarted as GettingStarted
import Chapters.Internationalization as Internationalization
import Chapters.ParsingCookbook as ParsingCookbook
import Chapters.ParsingToJson as ParsingToJson
import ElmBook exposing (Book, book)
import ElmBook.Chapter as Chapter
import ElmBook.StatefulOptions as StatefulOptions


type alias Model =
    { gettingStarted : GettingStarted.Model
    , fields : Fields.Model
    , parsingCookbook : ParsingCookbook.Model
    , parsingToJson : ParsingToJson.Model
    , geocodingAutocomplete : GeocodingAutocomplete.Model
    , customization : Customization.Model
    , internationalization : Internationalization.Model
    , formEditor : FormEditor.Model
    }


init : Model
init =
    { gettingStarted = GettingStarted.init
    , fields = Fields.init
    , parsingCookbook = ParsingCookbook.init
    , parsingToJson = ParsingToJson.init
    , geocodingAutocomplete = GeocodingAutocomplete.init
    , customization = Customization.init
    , internationalization = Internationalization.init
    , formEditor = FormEditor.init
    }


main : Book Model
main =
    book "form-toolkit"
        |> ElmBook.withStatefulOptions
            [ StatefulOptions.initialState init
            ]
        |> ElmBook.withChapterGroups
            [ ( "Usage"
              , [ GettingStarted.chapter
                , Fields.chapter
                , FormEditor.chapter
                , ParsingCookbook.chapter
                , ParsingToJson.chapter
                , GeocodingAutocomplete.chapter
                , Customization.chapter
                , Internationalization.chapter
                ]
              )
            , ( "Reference"
              , [ Chapter.chapterLink
                    { title = "Api"
                    , url = "https://package.elm-lang.org/packages/maca/form-toolkit/latest/"
                    }
                , Chapter.chapterLink
                    { title = "Source"
                    , url = "https://github.com/maca/form-toolkit"
                    }
                ]
              )
            ]

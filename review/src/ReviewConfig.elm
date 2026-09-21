module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoMissingTypeAnnotation
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Documentation.CodeSnippet
import Review.NoCatchAllInCodec
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    , NoDebug.Log.rule
    , Review.Documentation.CodeSnippet.checkImplicitlyImportingEverythingFromCurrentModule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoMissingTypeAnnotation.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    , Review.NoCatchAllInCodec.rule
        [ "encodeAttribute"
        , "fieldAttributeOf"
        , "groupAttributeOf"
        , "repeatableAttributeOf"
        ]
    ]

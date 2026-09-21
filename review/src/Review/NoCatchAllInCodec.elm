module Review.NoCatchAllInCodec exposing (rule)

{-| Forbid `_ ->` catch-all branches in named functions, so that a case over a
closed union stays exhaustive.

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid `_ ->` catch-all branches in the cases of the given functions.
-}
rule : List String -> Rule
rule guardedFunctions =
    Rule.newModuleRuleSchema "NoCatchAllInCodec" []
        |> Rule.withDeclarationEnterVisitor (enterDeclaration guardedFunctions)
        |> Rule.withDeclarationExitVisitor exitDeclaration
        |> Rule.withExpressionEnterVisitor (checkExpression guardedFunctions)
        |> Rule.fromModuleRuleSchema


enterDeclaration guardedFunctions node names =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( [], Node.value (Node.value function.declaration).name :: names )

        _ ->
            ( [], names )


exitDeclaration node names =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            ( [], List.drop 1 names )

        _ ->
            ( [], names )


checkExpression guardedFunctions node names =
    case ( List.head names, Node.value node ) of
        ( Just name, Expression.CaseExpression caseBlock ) ->
            if List.member name guardedFunctions && List.any (isWildcard << Tuple.first) caseBlock.cases then
                ( [ Rule.error
                        { message = "A catch-all branch defeats the codec's exhaustive case"
                        , details =
                            [ "`"
                                ++ name
                                ++ "` is one of the cases that must stay exhaustive over a closed union,"
                            , "so that adding a variant is a compile error until it is handled here."
                            , "Spell out every variant instead of falling back with `_ ->`."
                            ]
                        }
                        (Node.range node)
                  ]
                , names
                )

            else
                ( [], names )

        _ ->
            ( [], names )


isWildcard patternNode =
    case Node.value patternNode of
        Pattern.AllPattern ->
            True

        _ ->
            False

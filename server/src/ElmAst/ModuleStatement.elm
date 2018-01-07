module ElmAst.ModuleStatement exposing (..)

import ElmAst.Helpers exposing (someWhitespace)
import ElmAst.Name exposing (qualifiedCapitalizedName)
import Parser exposing (oneOf, symbol, run, succeed, Count(AtLeast), Parser, zeroOrMore, (|.), (|=), Error)


{-| it's just the dotted module name for now
-}
type alias ModuleStatement =
    String


parseModuleStatement : String -> Result Error ModuleStatement
parseModuleStatement string =
    run importStatement string


importStatement : Parser ModuleStatement
importStatement =
    succeed identity
        |= importStatementName


importStatementName : Parser String
importStatementName =
    succeed identity
        |. oneOf [ symbol "module", symbol "port module" ]
        |. someWhitespace
        |= qualifiedCapitalizedName

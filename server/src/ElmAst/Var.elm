module ElmAst.Var exposing (..)

import Char
import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import Parser.LanguageKit as Parser
import Set


lowVar : Parser String
lowVar =
    variable Char.isLower


capVar : Parser String
capVar =
    variable Char.isUpper


qualifiedCapVar : Parser String
qualifiedCapVar =
    Parser.source <|
        capVar
            |. Parser.repeat Parser.zeroOrMore (Parser.symbol "." |. capVar)


variable : (Char -> Bool) -> Parser String
variable isFirst =
    Parser.variable isFirst isInnerVarChar Set.empty


isInnerVarChar : Char -> Bool
isInnerVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'

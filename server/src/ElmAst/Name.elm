module ElmAst.Name exposing (..)

import Char
import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import Parser.LanguageKit as Parser
import Set


lowerCaseName : Parser String
lowerCaseName =
    getNameParser Char.isLower


capitalizedName : Parser String
capitalizedName =
    getNameParser Char.isUpper


qualifiedCapitalizedName : Parser String
qualifiedCapitalizedName =
    Parser.source <|
        capitalizedName
            |. Parser.repeat Parser.zeroOrMore (Parser.symbol "." |. capitalizedName)


getNameParser : (Char -> Bool) -> Parser String
getNameParser isValidFirstChar =
    Parser.variable isValidFirstChar isInnerVarChar Set.empty


isInnerVarChar : Char -> Bool
isInnerVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'

module ElmAst.Helpers exposing (..)

import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))


someWhitespace : Parser ()
someWhitespace =
    Parser.ignore (AtLeast 1) isSpace


spaces : Parser ()
spaces =
    whitespace


isSpace : Char -> Bool
isSpace char =
    char == ' ' || char == '\n' || char == '\x0D'


whitespace : Parser ()
whitespace =
    Parser.ignore zeroOrMore isSpace

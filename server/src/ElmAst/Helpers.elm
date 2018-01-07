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


oneOrMoreSeparatedByComma : Parser a -> Parser (List a)
oneOrMoreSeparatedByComma parser =
    parser
        |> Parser.andThen (\first -> oneOrMoreSeparatedByCommaHelp parser [ first ])


oneOrMoreSeparatedByCommaHelp : Parser a -> List a -> Parser (List a)
oneOrMoreSeparatedByCommaHelp parser revList =
    Parser.oneOf
        [ commaAnd parser
            |> Parser.andThen (\a -> oneOrMoreSeparatedByCommaHelp parser (a :: revList))
        , Parser.succeed (List.reverse revList)
        ]


commaAnd : Parser a -> Parser a
commaAnd parser =
    Parser.delayedCommit spaces <|
        Parser.succeed identity
            |. comma
            |. spaces
            |= parser


comma : Parser ()
comma =
    Parser.symbol ","

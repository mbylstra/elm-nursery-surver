module ElmTypesParser exposing (..)

{-| This is specifically for handling the types that appear in
documentation generated by `elm-make`. If you are looking to parse
arbitrary type signatures with creative indentation (e.g. newlines
and comments) this library will not do what you want. Instead,
check out the source code and go from there. It's not too tough!

@docs Type, decoder

-}

import Types exposing (..)
import Char
import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import Parser.LanguageKit as Parser
import Set


-- TYPES


{-| Represent Elm types as values! Here are some examples:

    Int            ==> Type "Int" []

    a -> b         ==> Lambda (Var "a") (Var "b")

    ( a, b )       ==> Tuple [ Var "a", Var "b" ]

    Maybe a        ==> Type "Maybe" [ Var "a" ]

    { x : Float }  ==> Record [("x", Type "Float" [])] Nothing

-}
parseTipe : String -> Result Parser.Error Type
parseTipe source =
    Parser.run tipe source



-- FUNCTIONS


parseTypeAnnotation : String -> Result Parser.Error TypeAnnotation
parseTypeAnnotation string =
    Parser.run typeAnnotation string


typeAnnotation : Parser TypeAnnotation
typeAnnotation =
    Parser.succeed (,)
        |= lowVar
        |. someWhitespace
        |. Parser.symbol ":"
        |. someWhitespace
        |= tipe


tipe : Parser Type
tipe =
    (Parser.lazy <|
        \_ ->
            tipeTerm
                |> Parser.andThen tipeHelp
    )


tipeHelp : Type -> Parser Type
tipeHelp t =
    Parser.oneOf
        [ Parser.map (Lambda t) arrowAndType
        , Parser.succeed t
        ]


arrowAndType : Parser Type
arrowAndType =
    Parser.delayedCommit spaces <|
        Parser.succeed identity
            |. arrow
            |. spaces
            |= tipe


arrow : Parser ()
arrow =
    Parser.symbol "->"


tipeTerm : Parser Type
tipeTerm =
    Parser.lazy <|
        \_ ->
            Parser.oneOf
                [ Parser.map Var lowVar
                , Parser.succeed Type
                    |= qualifiedCapVar
                    |= chompArgs []
                , record
                , tuple
                ]


chompArgs : List Type -> Parser (List Type)
chompArgs revArgs =
    Parser.oneOf
        [ Parser.delayedCommit spaces term
            |> Parser.andThen (\arg -> chompArgs (arg :: revArgs))
        , Parser.succeed (List.reverse revArgs)
        ]


term : Parser Type
term =
    Parser.lazy <|
        \_ ->
            Parser.oneOf
                [ Parser.map Var lowVar
                , Parser.map (flip Type []) qualifiedCapVar
                , record
                , tuple
                ]



-- RECORDS


record : Parser Type
record =
    Parser.lazy <|
        \_ ->
            Parser.succeed (flip Record)
                |. Parser.symbol "{"
                |. spaces
                |= extension
                |= commaSep field
                |. spaces
                |. Parser.symbol "}"


extension : Parser (Maybe String)
extension =
    Parser.oneOf
        [ Parser.delayedCommitMap always ext (Parser.succeed ())
        , Parser.succeed Nothing
        ]


ext : Parser (Maybe String)
ext =
    Parser.succeed Just
        |= lowVar
        |. spaces
        |. Parser.symbol "|"
        |. spaces


field : Parser ( String, Type )
field =
    Parser.lazy <|
        \_ ->
            Parser.succeed (,)
                |= lowVar
                |. spaces
                |. Parser.symbol ":"
                |. spaces
                |= tipe



-- TUPLE


tuple : Parser Type
tuple =
    Parser.map tuplize <|
        Parser.tuple spaces tipe


tuplize : List Type -> Type
tuplize args =
    case args of
        [ tipe ] ->
            tipe

        _ ->
            Tuple args



-- VAR HELPERS


lowVar : Parser String
lowVar =
    variable Char.isLower


capVar : Parser String
capVar =
    variable Char.isUpper


isInnerVarChar : Char -> Bool
isInnerVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


qualifiedCapVar : Parser String
qualifiedCapVar =
    Parser.source <|
        capVar
            |. Parser.repeat Parser.zeroOrMore (Parser.symbol "." |. capVar)


variable : (Char -> Bool) -> Parser String
variable isFirst =
    Parser.variable isFirst isInnerVarChar Set.empty



-- HELPERS
-- spaces =
--     Parser.ignore Parser.zeroOrMore (\char -> char == ' ')
-- someWhitespace : Parser ()
-- someWhitespace =
--     Parser.succeed ()
--         |. Parser.ignore (AtLeast 1) isSpace
--         |. whitespace


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



-- Parser.whitespace
--     { allowTabs = False
--     , lineComment = Parser.LineComment "--"
--     , multiComment = Parser.NestableComment "{-" "-}"
--     }


commaSep : Parser a -> Parser (List a)
commaSep parser =
    parser
        |> Parser.andThen (\first -> commaSepHelp parser [ first ])


commaSepHelp : Parser a -> List a -> Parser (List a)
commaSepHelp parser revList =
    Parser.oneOf
        [ commaAnd parser
            |> Parser.andThen (\a -> commaSepHelp parser (a :: revList))
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



--------------------------------------------------------------------------------
-- My Stuff
--------------------------------------------------------------------------------


typeAlias : Parser TypeAliasDefinitionR
typeAlias =
    Parser.succeed TypeAliasDefinitionR
        |. Parser.symbol "type alias"
        |. someWhitespace
        |= capVar
        |. someWhitespace
        |= parseTypeVariables
        |. Parser.symbol "="
        |. someWhitespace
        |= tipe


parseTypeAlias : String -> Result Parser.Error TypeAliasDefinitionR
parseTypeAlias source =
    Parser.run typeAlias source


pipeSymbol : Parser ()
pipeSymbol =
    Parser.symbol "|"


parseUnion : String -> Result Parser.Error UnionR
parseUnion source =
    Parser.run unionType source


unionType : Parser UnionR
unionType =
    Parser.succeed UnionR
        |. Parser.symbol "type"
        |. someWhitespace
        |= capVar
        |. someWhitespace
        |= parseTypeVariables
        |. Parser.symbol "="
        |. someWhitespace
        |= typeConstructors


parseTypeVariables : Parser (List String)
parseTypeVariables =
    Parser.repeat
        zeroOrMore
        (Parser.delayedCommitMap
            (\typeVar _ -> typeVar)
            lowVar
            someWhitespace
        )


parseTypeConstructors : String -> Result Parser.Error (List TypeConstructor)
parseTypeConstructors source =
    Parser.run typeConstructors source


typeConstructors : Parser (List TypeConstructor)
typeConstructors =
    typeConstructor
        |> Parser.andThen (\first -> typeConstructorsHelp [ first ])


typeConstructorsHelp : List TypeConstructor -> Parser (List TypeConstructor)
typeConstructorsHelp revList =
    Parser.oneOf
        [ typeConstructorsExtraOne
            |> Parser.andThen (\extraOne -> typeConstructorsHelp (extraOne :: revList))
        , Parser.succeed (List.reverse revList)
        ]


typeConstructorsExtraOne : Parser TypeConstructor
typeConstructorsExtraOne =
    Parser.delayedCommit someWhitespace <|
        Parser.succeed identity
            |. pipeSymbol
            |. someWhitespace
            |= typeConstructor


parseTypeConstructor : String -> Result Parser.Error TypeConstructor
parseTypeConstructor source =
    Parser.run typeConstructor source


typeConstructor : Parser TypeConstructor
typeConstructor =
    Parser.succeed (,)
        |= capVar
        |= typeConstructorArgs



-- The next step might be to do the delayedCommit thing? I think the oneOf and succeed is key
-- Hmmm.. why is repeatZero not working???


typeConstructorArgs : Parser (List Type)
typeConstructorArgs =
    (Parser.oneOf
        [ -- (Parser.succeed identity
          --     |. someWhitespace
          --     |= typeConstructorArg
          --   )
          typeConstructorArgsExtraOne
            |> Parser.andThen (\first -> typeConstructorArgsHelp [ first ])
        , Parser.succeed []
        ]
    )


typeConstructorArgsHelp : List Type -> Parser (List Type)
typeConstructorArgsHelp revList =
    Parser.oneOf
        [ typeConstructorArgsExtraOne
            |> Parser.andThen (\extraOne -> typeConstructorArgsHelp (extraOne :: revList))
        , Parser.succeed (List.reverse revList)
        ]


typeConstructorArgsExtraOne : Parser Type
typeConstructorArgsExtraOne =
    Parser.delayedCommit someWhitespace <|
        Parser.succeed identity
            |= typeConstructorArg


typeConstructorArg : Parser Type
typeConstructorArg =
    Parser.oneOf
        [ record
        , tuple
        , lowVar |> Parser.map Var
        , qualifiedCapVar |> Parser.map (\name -> Type name [])
        ]

module ElmAst.ImportStatement exposing (..)

import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import ElmAst.Name exposing (capitalizedName, lowerCaseName, qualifiedCapitalizedName)
import ElmAst.Helpers exposing (whitespace, someWhitespace)


type alias ImportStatement =
    { dottedModulePath : String
    , maybeAlias : Maybe String
    , exposedNames : Listing
    }


type alias Listing =
    { explicits : List String
    , open : Bool
    }


parseImportStatement : String -> Result Parser.Error ImportStatement
parseImportStatement string =
    Parser.run importStatement string


importStatement : Parser ImportStatement
importStatement =
    Parser.succeed
        ImportStatement
        |= importStatementName
        |= importAlias
        |= exposedNames


importStatementName : Parser String
importStatementName =
    Parser.succeed identity
        |. Parser.symbol "import"
        |. someWhitespace
        |= qualifiedCapitalizedName


importAlias : Parser (Maybe String)
importAlias =
    let
        hasAlias =
            Parser.delayedCommit someWhitespace <|
                Parser.succeed (\name -> Just name)
                    |. Parser.symbol "as"
                    |. someWhitespace
                    |= qualifiedCapitalizedName

        noAlias =
            Parser.succeed Nothing
    in
        Parser.oneOf
            [ hasAlias
            , noAlias
            ]


exposedNames : Parser Listing
exposedNames =
    Parser.oneOf
        [ Parser.succeed identity
            |. someWhitespace
            |. Parser.symbol "exposing"
            |. someWhitespace
            |= exposedNamesList
        , Parser.succeed closedListing
        ]


exposedNamesList : Parser Listing
exposedNamesList =
    Parser.oneOf
        [ Parser.symbol "(..)" |> Parser.andThen (\_ -> Parser.succeed openListing)
        , explicitExposedNames |> Parser.andThen (\names -> Parser.succeed (listing names))
        ]


explicitExposedNames : Parser (List String)
explicitExposedNames =
    Parser.succeed (\head tail -> head :: tail)
        |. Parser.symbol "("
        |. whitespace
        |= exposedType
        |. whitespace
        |= Parser.repeat Parser.zeroOrMore
            (Parser.succeed identity
                |. Parser.symbol ","
                |. whitespace
                |= exposedType
                |. whitespace
            )
        |. whitespace
        |. Parser.symbol ")"


exposedType : Parser String
exposedType =
    Parser.oneOf [ unionTypeWithAllConstructors, unionTypeWithConstructors, lowOrCapVar ]


unionTypeWithAllConstructors : Parser String
unionTypeWithAllConstructors =
    -- you need to use delayedCommitMap instead of |. and |= if you want
    -- the parser to be used with oneOf (as |. and |= do not backtrack)
    Parser.delayedCommitMap
        (\name _ -> name)
        capitalizedName
        (Parser.symbol "(..)")


unionTypeWithConstructors : Parser String
unionTypeWithConstructors =
    Parser.delayedCommitMap
        (\name _ -> name)
        capitalizedName
        typeConstructors


{-| We don't need to know what type constructors are exposed, as they are
only ever used inside functions, not inside type annotations
-}
typeConstructors : Parser ()
typeConstructors =
    Parser.succeed ()
        |. Parser.symbol "("
        |. whitespace
        |. capitalizedName
        |. whitespace
        |. Parser.repeat Parser.zeroOrMore
            (Parser.succeed identity
                |. Parser.symbol ","
                |. whitespace
                |. capitalizedName
                |. whitespace
            )
        |. whitespace
        |. Parser.symbol ")"


lowOrCapVar : Parser String
lowOrCapVar =
    Parser.oneOf [ lowerCaseName, capitalizedName ]


openListing : Listing
openListing =
    Listing [] True


closedListing : Listing
closedListing =
    Listing [] False


listing : List String -> Listing
listing xs =
    Listing xs False

-- Lookup ----------------------------------------------------------------------
-- findModule : List ImportStatement -> QualifiedName -> String
-- findModule imports { name, modulePath } =
--     let
--         reversedImportStatements = List.reverse imports
--         _findModule : ImportStatement ->
--     importMethodn


module Main exposing (..)


type alias StructuredRawName =
    { name : String
    , modulePath : List String
    }


type alias RawDottedName =
    String


{-| Convert a string such as Json.Decode.field to a structure such as
{ name = "field"
, modulePath = ["Json", "Decode" ]
}

Note that this does not do any resolution to figure out the full path based
on import statements.
Eg: "field" will just be converted to { name = "field", modulePath = [] } even
if there is import Json.Decode exposing (field)

-}
rawNameToStructured : String -> StructuredRawName
rawNameToStructured rawName =
    rawName
        |> String.split "."
        |> List.reverse
        |> List.Extra.uncons
        |> Maybe.map (\( head, tail ) -> { name = head, modulePath = List.reverse tail })
        |> Maybe.withDefault { name = "", modulePath = [] }


isExplicitlyInImportStatement :
    String
    -> ImportStatement
    -> Maybe ( RawDottedName, { dottedModulePath : String, name : String } )
isExplicitlyInImportStatement rawDottedName { dottedModulePath, maybeAlias, exposedNames } =
    let
        return =
            if rawNameDottedModulePath == dottedModulePath then
                Just
                    ( rawDottedName
                    , { dottedModulePath = dottedModulePath
                      , name = structuredRawName.name
                      }
                    )
            else
                case maybeAlias of
                    Just theAlias ->
                        if theAlias == rawNameDottedModulePath then
                            Just
                                ( rawDottedName
                                , { dottedModulePath = dottedModulePath
                                  , name = structuredRawName.name
                                  }
                                )
                        else
                            handleAliasDoesntMatch

                    Nothing ->
                        handleAliasDoesntMatch

        rawNameDottedModulePath =
            (structuredRawName.modulePath |> toDottedPath)

        structuredRawName =
            rawNameToStructured rawDottedName

        handleAliasDoesntMatch =
            if structuredRawName.modulePath == [] then
                if exposedNames.explicits |> List.member structuredRawName.name then
                    Just
                        ( rawDottedName
                        , { dottedModulePath = dottedModulePath
                          , name = structuredRawName.name
                          }
                        )
                else
                    Nothing
            else
                Nothing
    in
        return


toDottedPath : List String -> String
toDottedPath segments =
    String.join "." segments


elmImplicitImports : List ImportStatement
elmImplicitImports =
    [ ImportStatement "Basics" Nothing (Listing [ "Order", "Never" ] False)
    , ImportStatement "Debug" Nothing closedListing
    , ImportStatement "List" Nothing (Listing [ "List" ] False)
    , ImportStatement "Maybe" Nothing (Listing [ "Maybe" ] False)
    , ImportStatement "Result" Nothing (Listing [ "Result" ] False)
    , ImportStatement "String" Nothing (Listing [ "String" ] False)
    , ImportStatement "Tuple" Nothing closedListing
    , ImportStatement "Platform" Nothing (Listing [ "Program" ] False)
    , ImportStatement "Platform.Cmd" Nothing (Listing [ "Cmd" ] False)
    , ImportStatement "Platform.Sub" Nothing (Listing [ "Sub" ] False)
    ]

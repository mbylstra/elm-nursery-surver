module ElmAst.MultiLineString exposing (..)


multiLineStringToken : String
multiLineStringToken =
    "\"\"\""


type alias InnerModel =
    { edited : String
    , remainder : String
    , inside : Bool
    }


init : String -> InnerModel
init sourceCode =
    { edited = ""
    , remainder = sourceCode
    , inside = False
    }


type Token
    = MultiLineComment
    | Other Char
    | EOF


convertMultiLineStrings : String -> String
convertMultiLineStrings sourceCode =
    sourceCode
        |> init
        |> convertMultiLineStringsInner
        |> .edited


convertMultiLineStringsInner : InnerModel -> InnerModel
convertMultiLineStringsInner model =
    let
        token : Token
        token =
            getToken model.remainder

        inside =
            case token of
                MultiLineComment ->
                    not model.inside

                _ ->
                    model.inside
    in
        case token of
            EOF ->
                model

            _ ->
                model
                    |> setInside inside
                    |> chompNextToken token
                    |> convertMultiLineStringsInner


setInside : Bool -> InnerModel -> InnerModel
setInside inside model =
    { model | inside = inside }


chompNextToken : Token -> InnerModel -> InnerModel
chompNextToken token ({ remainder, edited, inside } as model) =
    let
        chompResult =
            chompHelp token remainder

        newModel =
            { model | remainder = chompResult.remainder }

        nextChar =
            if inside then
                case chompResult.tokenStr of
                    "\n" ->
                        "\\n"

                    "\"\"\"" ->
                        "\""

                    _ ->
                        chompResult.tokenStr
            else
                case chompResult.tokenStr of
                    "\"\"\"" ->
                        "\""

                    _ ->
                        chompResult.tokenStr
    in
        { newModel | edited = model.edited ++ nextChar }


chompHelp : Token -> String -> { tokenStr : String, remainder : String }
chompHelp token sourceCode =
    case token of
        MultiLineComment ->
            { tokenStr = multiLineStringToken
            , remainder = String.dropLeft 3 sourceCode
            }

        Other c ->
            { tokenStr = String.fromChar c
            , remainder = String.dropLeft 1 sourceCode
            }

        EOF ->
            { tokenStr = ""
            , remainder = ""
            }


getToken : String -> Token
getToken remainder =
    if String.startsWith multiLineStringToken remainder then
        MultiLineComment
    else
        case String.uncons remainder of
            Just ( c, _ ) ->
                Other c

            Nothing ->
                EOF

module CommentsParser exposing (..)


removeOneLineComments : String -> String
removeOneLineComments sourceCode =
    let
        removeOneLineComment : String -> String
        removeOneLineComment line =
            line |> String.split "--" |> List.head |> Maybe.withDefault ""
    in
        sourceCode
            |> String.lines
            |> List.map removeOneLineComment
            |> String.join "\n"


commentOpener : String
commentOpener =
    "{-"



-- Note that doc comments can be treated as regular comments - the '|' is just removed anyway


commentCloser : String
commentCloser =
    "-}"


startsWithCommentOpener : String -> Bool
startsWithCommentOpener =
    String.startsWith commentOpener


startsWithCommentCloser : String -> Bool
startsWithCommentCloser =
    String.startsWith commentCloser


type alias InnerModel =
    { edited : String
    , remainder : String
    , level : Int
    }


init : String -> InnerModel
init sourceCode =
    { edited = ""
    , remainder = sourceCode
    , level = 0
    }


type Token
    = CommentOpener
    | CommentCloser
    | Other Char
    | EOF


removeMultiLineComments : String -> String
removeMultiLineComments sourceCode =
    sourceCode
        |> init
        |> removeMultiLineCommentsInner
        |> .edited


removeMultiLineCommentsInner : InnerModel -> InnerModel
removeMultiLineCommentsInner model =
    let
        token : Token
        token =
            getToken model.remainder

        chompLevel : Int
        chompLevel =
            case token of
                CommentOpener ->
                    model.level + 1

                _ ->
                    model.level

        finalLevel =
            case token of
                CommentCloser ->
                    if chompLevel > 0 then
                        chompLevel - 1
                    else
                        0

                _ ->
                    chompLevel
    in
        case token of
            EOF ->
                model

            _ ->
                model
                    |> setLevel chompLevel
                    |> chompNextToken token
                    |> setLevel finalLevel
                    |> removeMultiLineCommentsInner


setLevel : Int -> InnerModel -> InnerModel
setLevel level model =
    { model | level = level }


chompNextToken : Token -> InnerModel -> InnerModel
chompNextToken token ({ remainder, edited, level } as model) =
    let
        chompResult =
            chompHelp token remainder

        newModel =
            { model | remainder = chompResult.remainder }
    in
        if level < 1 then
            { newModel | edited = model.edited ++ chompResult.tokenStr }
        else
            newModel


chompHelp : Token -> String -> { tokenStr : String, remainder : String }
chompHelp token sourceCode =
    case token of
        CommentOpener ->
            { tokenStr = commentOpener
            , remainder = String.dropLeft 2 sourceCode
            }

        CommentCloser ->
            { tokenStr = commentCloser
            , remainder = String.dropLeft 2 sourceCode
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
    if startsWithCommentOpener remainder then
        CommentOpener
    else if startsWithCommentCloser remainder then
        CommentCloser
    else
        case String.uncons remainder of
            Just ( c, _ ) ->
                Other c

            Nothing ->
                EOF



-- WARNING: watch out for interactions between -- and --}

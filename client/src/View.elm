module View exposing (..)

import Html exposing (Html, div, h1, h2, h3, img, p, pre, span, text, ul, li)
import Html.Attributes exposing (class, src)
import Icons
import Lib
import State exposing (Msg(..))
import Types exposing (DirectoryContentsR, Model, Page(..), File, FileType(..))


view : Model -> Html Msg
view model =
    case model.page of
        Loading ->
            mainView
                model
                [ div [ class "spinner" ]
                    [ div [ class "sk-rotating-plane" ] [] ]
                ]

        Home ->
            let
                directoryContents =
                    { currentDirectory = ""
                    , files = model.sourceDirectories
                    }
            in
                mainView
                    model
                    [ fileList directoryContents ]

        DirectoryContents directoryContents ->
            mainView
                model
                (case directoryContents.files of
                    [] ->
                        [ p [] [ text "There are no Elm files in this directory" ] ]

                    _ ->
                        [ fileList directoryContents ]
                )

        ElmModuleViewFunctions { elmModulePath, viewFunctions } ->
            mainView
                model
                (case viewFunctions of
                    [] ->
                        [ p [] [ text "There are no view functions in this module" ] ]

                    _ ->
                        h2 [] [ text "View Functions" ]
                            :: List.map (viewFunctionLinkView { elmModulePath = elmModulePath }) viewFunctions
                )

        ViewFunction result ->
            case result of
                Ok code ->
                    mainView
                        model
                        [ h2 [] [ text "todo" ]
                        , viewFunctionCodeView ( "todo", code )
                        ]

                Err message ->
                    mainView
                        model
                        [ pre [ class "error" ] [ text message ] ]


viewFunctionLinkView : { elmModulePath : String } -> String -> Html Msg
viewFunctionLinkView { elmModulePath } viewFunction =
    p []
        [ Lib.link
            (ViewFunctionClicked
                { elmModulePath = elmModulePath, viewFunction = viewFunction }
            )
            [ class "file-link"
            ]
            [ span [ class "icon" ] [ Icons.eye ]
            , text viewFunction
            ]
        ]


fileList : DirectoryContentsR -> Html Msg
fileList { currentDirectory, files } =
    div [ class "files" ]
        [ ul
            []
            (List.map (fileView currentDirectory) files)
        ]


mainView : Model -> List (Html Msg) -> Html Msg
mainView _ content =
    div []
        ([ img [ src "/logo.svg" ] []
         , h1 [] [ text "Elm Nursery" ]
         ]
            ++ content
        )


fileView : String -> File -> Html Msg
fileView basePath file =
    let
        icon =
            case file.fileType of
                Directory ->
                    Icons.directory

                ElmModule ->
                    Icons.file
    in
        li []
            [ Lib.link (FileClicked basePath file)
                [ class "file-link"
                ]
                [ span [ class "icon" ] [ icon ]
                , text <| file.name
                ]
            ]


viewFunctionCodeView : ( String, String ) -> Html msg
viewFunctionCodeView ( functionName, code ) =
    div []
        [ h3 [] [ text functionName ]
        , pre [] [ text code ]
        ]

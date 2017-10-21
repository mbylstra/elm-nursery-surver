module Main exposing (..)

import Html exposing (Html, text, div, img, h1, li, a, ul, span, p, h2, h3, pre)
import Html.Attributes exposing (src, href, class)
import Http
import Json.Decode as Decode exposing (Decoder)
import Lib exposing (arrayAsTuple2)
import Navigation exposing (Location)
import Icons


---- CONSTANTS ----


apiBaseUrl : String
apiBaseUrl =
    "http://localhost:3000/"



---- MODEL ----


type alias Model =
    { sourceDirectories : List File
    , page : Page

    -- top level "private" fields -- Don't pay too much attention to these yet! Pages should not need to know about them.
    , linkClicked : Bool
    , location : Location
    }


type Page
    = Loading
    | Home
    | DirectoryContents DirectoryContentsR
    | ElmModuleViewFunctions { elmModulePath : String, viewFunctions : List String }
    | ViewFunction (Result String String)


type alias DirectoryContentsR =
    { currentDirectory : String, files : List File }


type alias File =
    { fileType : FileType
    , name : String
    }


type FileType
    = ElmModule
    | Directory


initWithPage : Location -> Page -> ( Model, Cmd Msg )
initWithPage location page =
    ( { page = page
      , sourceDirectories = []
      , linkClicked = False
      , location = location
      }
    , Http.send ReceiveElmSourceDirectories elmSourceDirectoriesRequest
    )


init : Location -> ( Model, Cmd Msg )
init location =
    let
        _ =
            Debug.log "init from inside Elm"
    in
        initWithPage location Loading


setLinkClicked : Model -> Model
setLinkClicked model =
    { model | linkClicked = True }


setToLoadingPage : Model -> Model
setToLoadingPage model =
    { model | page = Loading }


setSourceDirectories : List File -> Model -> Model
setSourceDirectories sourceDirectories model =
    { model | sourceDirectories = sourceDirectories }



---- UPDATE ----


type Msg
    = ReceiveElmSourceDirectories (Result Http.Error (List File))
    | FileClicked String File
    | ReceiveDirectoryContents (Result Http.Error DirectoryContentsR)
    | ReceiveElmModuleViewFunctions { elmModulePath : String } (Result Http.Error (List String))
    | NewLocation Location
    | ViewFunctionClicked { elmModulePath : String, viewFunction : String }
    | ReceiveViewFunctionOutput (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        ReceiveElmSourceDirectories result ->
            -- This is really the second part of the init function. This is only
            -- ever called on app initialisation once after we get the list of
            -- Elm source directories from the server. It's now that we have
            -- enough information to turn the url into state. Note that whenever
            -- the forward or back button is hit, we completely re-initialize
            -- the state of the app.
            case result of
                Ok sourceDirectories ->
                    model
                        |> setSourceDirectories sourceDirectories
                        |> deserializeStateFromUrl

                Err err ->
                    Debug.crash (toString err)

        FileClicked basePath file ->
            handleFileClicked basePath file model

        ReceiveDirectoryContents result ->
            case result of
                Ok directoryContents ->
                    ( { model | page = DirectoryContents directoryContents }
                    , Cmd.none
                    )

                Err err ->
                    Debug.crash (toString err)

        ReceiveElmModuleViewFunctions { elmModulePath } viewFunctionsResult ->
            case viewFunctionsResult of
                Ok viewFunctions ->
                    ( { model
                        | page =
                            ElmModuleViewFunctions
                                { elmModulePath = elmModulePath, viewFunctions = viewFunctions }
                      }
                    , Cmd.none
                    )

                Err err ->
                    Debug.crash (toString err)

        ReceiveViewFunctionOutput result ->
            case result of
                Ok s ->
                    ( { model | page = ViewFunction (Ok s) }
                    , Cmd.none
                    )

                Err err ->
                    case err of
                        Http.BadStatus response ->
                            ( { model | page = ViewFunction (Err response.body) }
                            , Cmd.none
                            )

                        _ ->
                            Debug.crash (toString err)

        --
        NewLocation location ->
            let
                _ =
                    Debug.log "new location" location.pathname
            in
                handleUrlChanged location model

        ViewFunctionClicked ({ elmModulePath, viewFunction } as requestParams) ->
            let
                url =
                    elmModulePath ++ "/" ++ viewFunction

                setNewUrl =
                    Navigation.newUrl url
            in
                model ! [ Http.send ReceiveViewFunctionOutput (generateViewFunctionRequest requestParams), setNewUrl ]


handleFileClicked : String -> File -> Model -> ( Model, Cmd Msg )
handleFileClicked basePath file model =
    case file.fileType of
        ElmModule ->
            let
                fullPath =
                    basePath ++ "/" ++ file.name

                setNewUrl =
                    Navigation.newUrl fullPath
            in
                ( model
                    |> setLinkClicked
                    |> setToLoadingPage
                , Cmd.batch
                    [ Http.send
                        (ReceiveElmModuleViewFunctions { elmModulePath = fullPath })
                        (elmModuleViewFunctionsRequest fullPath)
                    , setNewUrl
                    ]
                )

        Directory ->
            let
                fullPath =
                    basePath ++ "/" ++ file.name

                _ =
                    Debug.log "fullPath" fullPath

                setNewUrl =
                    Navigation.newUrl fullPath
            in
                ( model |> setLinkClicked
                , Cmd.batch
                    [ Http.send ReceiveDirectoryContents (directoryContentsRequest fullPath)
                    , setNewUrl
                    ]
                )


handleUrlChanged : Navigation.Location -> Model -> ( Model, Cmd Msg )
handleUrlChanged location model =
    if model.linkClicked == True then
        -- If we got a UrlChanged msg via a link click in our app, then we are not interested. We are
        -- handling the state of the app. The Address bar merely reflects the current state of the app
        -- for informational and bookmarking purposes. (It is not the source of truth for state)
        -- Reset linkClicked to False so we can detect future hits to the forward or back button.
        { model | linkClicked = False } ! []
    else
        -- Seeing as we are using HTML5 history for clean urls, and we know that
        -- a link wasn't clicked, we can deduce that the back or forward button
        -- must have been clicked. How do we get the state? Seeing as the Elm Navigation does
        -- not support serializing state when calling pushState (it should, but it doesn't)
        -- and we haven't implemented our own custom history API in elm (this would be possible),
        -- all we have to work with is the URL and to deduce as much state as we
        -- can from the URL. Depending on how much state we choose to store
        -- in the url we may not be able to bring the page to exactly the
        -- same state the page was in.
        initWithPage location model.page


deserializeStateFromUrl : Model -> ( Model, Cmd Msg )
deserializeStateFromUrl model =
    let
        pathname =
            model.location.pathname
    in
        if pathname == "/" then
            ( { model | page = Home }, Cmd.none )
        else
            let
                -- this will be necessary later
                ( sourceDirectory, modulePath ) =
                    splitBySourceDirectory model.sourceDirectories pathname
            in
                ( model
                , Http.send ReceiveDirectoryContents (directoryContentsRequest pathname)
                )


splitBySourceDirectory : List File -> String -> ( String, String )
splitBySourceDirectory sourceDirectories path =
    let
        sourceDirectory =
            sourceDirectories
                |> List.map .name
                |> List.filter (\sourceDirectory -> String.startsWith sourceDirectory path)
                |> List.head
                |> Maybe.withDefault ""
    in
        ( sourceDirectory, String.dropLeft (String.length sourceDirectory) path )



---- VIEW ----


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
                        ([ h2 [] [ text "View Functions" ] ]
                            ++ (List.map (viewFunctionLinkView { elmModulePath = elmModulePath }) viewFunctions)
                        )
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
mainView model content =
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

        fullFilePath =
            basePath ++ file.name
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



--- REQUESTS ----


elmSourceDirectoriesRequest : Http.Request (List File)
elmSourceDirectoriesRequest =
    Http.get
        (apiBaseUrl ++ "api/elm-source-directories/")
        (Decode.list (Decode.string |> Decode.map (\fileName -> File Directory fileName)))


directoryContentsRequest : String -> Http.Request DirectoryContentsR
directoryContentsRequest directory =
    let
        params =
            "?directory=" ++ (Http.encodeUri directory)
    in
        Http.get (apiBaseUrl ++ "api/directory-contents/" ++ params) directoryContentsDecoder


elmModuleViewFunctionsRequest : String -> Http.Request (List String)
elmModuleViewFunctionsRequest elmModulePath =
    let
        params =
            "?filePath=" ++ (Http.encodeUri elmModulePath)
    in
        Http.get (apiBaseUrl ++ "api/elm-module-view-functions/" ++ params) (Decode.list Decode.string)


generateViewFunctionRequest : { elmModulePath : String, viewFunction : String } -> Http.Request String
generateViewFunctionRequest { elmModulePath, viewFunction } =
    let
        params =
            "?elmModulePath=" ++ (Http.encodeUri elmModulePath) ++ "&viewFunction=" ++ viewFunction

        viewFunctionDecoder =
            Decode.string
    in
        Http.get (apiBaseUrl ++ "api/view-function/" ++ params) viewFunctionDecoder


fileDecoder : Decoder File
fileDecoder =
    Decode.map2
        (\fileName isDirectory ->
            let
                fileType =
                    case isDirectory of
                        True ->
                            Directory

                        False ->
                            ElmModule
            in
                File fileType fileName
        )
        (Decode.field "fileName" Decode.string)
        (Decode.field "isDirectory" Decode.bool)


directoryContentsDecoder : Decoder DirectoryContentsR
directoryContentsDecoder =
    Decode.map2 DirectoryContentsR
        (Decode.field "directory" Decode.string)
        (Decode.field "files" <| Decode.list fileDecoder)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program
        NewLocation
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

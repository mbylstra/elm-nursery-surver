module State exposing (..)

import Http
import Navigation exposing (Location)
import Types exposing (DirectoryContentsR, File, Model, Page(..), FileType(..))
import Requests exposing (..)
import QueryString
import Path.Posix as Path


type Msg
    = ReceiveElmSourceDirectories (Result Http.Error (List File))
    | FileClicked String File
    | ReceiveDirectoryContents (Result Http.Error DirectoryContentsR)
    | ReceiveElmModuleViewFunctions { elmModulePath : String } (Result Http.Error (List String))
    | NewLocation Location
    | ViewFunctionClicked { elmModulePath : String, viewFunction : String }
    | ReceiveViewFunctionOutput (Result Http.Error String)



---- MODEL ----


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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
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
                    elmModulePath ++ "?function=" ++ viewFunction

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
        path =
            model.location.pathname
        maybeFunctionName =
            model.location.search
            |> QueryString.parse
            |> QueryString.one QueryString.string "function"

        (dirPath, fileName) = Path.splitFileName path

    in
        if path == "/" then
            ( { model | page = Home }, Cmd.none )
        else
            case maybeFunctionName of
                Just functionName ->
                    ( model
                    , Http.send ReceiveViewFunctionOutput (generateViewFunctionRequest { elmModulePath = path, viewFunction = functionName })
                    )

                Nothing ->
                    if fileName == "" then
                        ( model
                        , Http.send ReceiveDirectoryContents (directoryContentsRequest dirPath)
                        )
                    else
                        ( model
                        , Http.send (ReceiveElmModuleViewFunctions  {elmModulePath = path})(elmModuleViewFunctionsRequest path  )
                        )

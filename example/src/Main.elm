----------------------------------------------------------------------
--
-- Main.elm
-- Example of using billstclair/elm-mastodon
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Main exposing (emptyUrl, main, parseQuery, receiveCodeAndState)

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dialog
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h2
        , input
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( autofocus
        , checked
        , cols
        , disabled
        , href
        , id
        , name
        , placeholder
        , rows
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (onCheck, onClick, onInput, onMouseDown)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Markdown
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity
    exposing
        ( Account
        , App
        , Authorization
        , Entity(..)
        , Privacy(..)
        )
import Mastodon.Login as Login exposing (FetchAccountOrRedirect(..))
import Mastodon.Request as Request
    exposing
        ( Error(..)
        , Paging
        , RawRequest
        , Request(..)
        , Response
        , WhichGroups
        , emptyPaging
        )
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket
import PortFunnels exposing (FunnelDict, Handler(..), State)
import String.Extra as SE
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as QP


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type alias Model =
    { token : Maybe String
    , server : String
    , loginServer : Maybe String
    , prettify : Bool
    , style : Style
    , selectedRequest : SelectedRequest
    , username : String
    , accountId : String
    , limit : String
    , accountIds : String
    , showMetadata : Bool
    , q : String
    , resolve : Bool
    , following : Bool
    , groupId : String
    , showReceived : Bool
    , showEntity : Bool
    , whichGroups : WhichGroups
    , followReblogs : Bool
    , onlyMedia : Bool
    , pinned : Bool
    , excludeReplies : Bool
    , excludeReblogs : Bool
    , paging : Maybe Paging

    -- Non-persistent below here
    , clearAllDialogVisible : Bool
    , request : Maybe RawRequest
    , response : Maybe Value
    , entity : Maybe Entity
    , metadata : Maybe Http.Metadata
    , savedModel : Maybe SavedModel
    , key : Key
    , url : Url
    , hideClientId : Bool
    , tokens : Dict String String
    , account : Maybe Account
    , displayName : String
    , note : String
    , avatarFile : Maybe File
    , headerFile : Maybe File
    , privacy : Privacy
    , sensitive : Bool
    , language : String
    , isAccountFollowed : Bool
    , msg : Maybe String
    , started : Started
    , funnelState : State
    }


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleClearAllDialog
    | OnKeyPress String
    | SetServer String
    | SetWhichGroups String
    | ClearSentReceived
    | TogglePrettify
    | ToggleShowMetadata
    | ToggleShowReceived
    | ToggleShowEntity
    | ToggleStyle
    | SetQ String
    | ToggleResolve
    | ToggleFollowing
    | ToggleFollowReblogs
    | SetSelectedRequest SelectedRequest Bool
    | ReceiveRedirect (Result ( String, Error ) ( String, App, Cmd Msg ))
    | ReceiveAuthorization (Result ( String, Error ) ( String, Authorization, Account ))
    | ReceiveInstance (Result Error Response)
    | ReceiveFetchAccount (Result ( String, Error ) ( String, String, Account ))
    | ReceiveGetVerifyCredentials (Result Error Response)
    | ReceiveAccountIdRelationships Bool (Result Error Response)
    | Process Value
    | SetLoginServer
    | Login
    | Logout
    | ClearAll
    | SetUsername String
    | SetAccountId String
    | SetLimit String
    | ToggleOnlyMedia
    | TogglePinned
    | ToggleExcludeReplies
    | ToggleExcludeReblogs
    | SetAccountIds String
    | SetDisplayName String
    | SetNote String
    | GetAvatarFile Bool
    | ReceiveAvatarFile File
    | GetHeaderFile Bool
    | ReceiveHeaderFile File
    | SetPrivacy Privacy
    | SetSensitive Bool
    | SetLanguage String
    | SetGroupId String
      -- See the `update` code for these messages for examples
      -- of using the `Request` module.
    | SendGetVerifyCredentials
    | SendGetAccountByUsername
    | SendGetAccount
    | SendGetFollowers
    | SendGetFollowing
    | SendGetStatuses
    | SendGetRelationships
    | SendGetSearchAccounts
    | SendPostFollowOrUnfollow
    | SendPatchUpdateCredentials
    | SendGetGroups
    | SendGetGroup
    | ReceiveResponse (Result Error Response)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


keyDecoder : Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.map OnKeyPress


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions Process model
        , if model.clearAllDialogVisible then
            Events.onKeyDown keyDecoder

          else
            Sub.none
        ]


emptyElement : String
emptyElement =
    "foo"


emptyUrl : Url
emptyUrl =
    { protocol = Url.Https
    , host = "example.com"
    , port_ = Nothing
    , path = "/" ++ emptyElement
    , query = Nothing
    , fragment = Nothing
    }


type alias CodeErrorState =
    { code : Maybe String
    , error : Maybe String
    , state : Maybe String
    }


parseQuery : String -> CodeErrorState
parseQuery queryString =
    let
        url =
            { emptyUrl | query = Just queryString }

        qp =
            QP.map3 CodeErrorState
                (QP.string "code")
                (QP.string "error")
                (QP.string "state")
    in
    Parser.parse (Parser.s emptyElement <?> qp) url
        |> Maybe.withDefault (CodeErrorState Nothing Nothing Nothing)


type CodeAndState
    = CodeAndState String (Maybe String)
    | CodeErrorAndState String (Maybe String)
    | NoCode


{-| This recognizes `?code=<code>&state=<state>` or `?error=<error>&state=<state>`

in the URL from the redirect from authentication.

-}
receiveCodeAndState : Url -> CodeAndState
receiveCodeAndState url =
    case url.query of
        Nothing ->
            NoCode

        Just q ->
            case parseQuery q of
                { code, error, state } ->
                    case code of
                        Just cod ->
                            case state of
                                Just st ->
                                    CodeAndState cod state

                                Nothing ->
                                    CodeErrorAndState "Missing state with code" code

                        Nothing ->
                            case error of
                                Just err ->
                                    CodeErrorAndState err state

                                Nothing ->
                                    NoCode


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        hideClientId =
            case JD.decodeValue JD.bool value of
                Err _ ->
                    False

                Ok hide ->
                    hide

        ( code, state, msg ) =
            case receiveCodeAndState url of
                CodeAndState cod stat ->
                    ( Just cod, stat, Nothing )

                CodeErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                NoCode ->
                    ( Nothing, Nothing, Nothing )
    in
    { token = Nothing
    , server = ""
    , loginServer = Nothing
    , prettify = True
    , style = LightStyle
    , selectedRequest = LoginSelected
    , username = ""
    , accountId = ""
    , limit = ""
    , accountIds = ""
    , showMetadata = False
    , q = ""
    , resolve = False
    , following = False
    , groupId = ""
    , showReceived = True
    , showEntity = False
    , whichGroups = Request.MemberGroups
    , followReblogs = True
    , onlyMedia = False
    , pinned = False
    , excludeReplies = False
    , excludeReblogs = False
    , paging = Nothing

    -- Non-persistent below here
    , clearAllDialogVisible = False
    , request = Nothing
    , response = Nothing
    , entity = Nothing
    , metadata = Nothing
    , savedModel = Nothing
    , key = key
    , url = url
    , hideClientId = hideClientId
    , tokens = Dict.empty
    , account = Nothing
    , displayName = ""
    , note = ""
    , avatarFile = Nothing
    , headerFile = Nothing
    , privacy = PublicPrivacy
    , sensitive = False
    , language = ""
    , isAccountFollowed = False
    , msg = msg
    , started = NotStarted
    , funnelState = initialFunnelState
    }
        -- As soon as the localStorage module reports in,
        -- we'll load the saved model,
        -- and then all the saved tokens.
        -- See `storageHandler` below, `get pk.model`.
        |> withCmds
            [ Navigation.replaceUrl key url.path
            , case ( code, state ) of
                ( Just cod, Just st ) ->
                    Login.getTokenTask { code = cod, state = st }
                        |> Task.attempt ReceiveAuthorization

                _ ->
                    Cmd.none
            ]


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if
                        LocalStorage.isLoaded state.storage
                            && (model.started == NotStarted)
                    then
                        StartedReadingModel

                    else
                        model.started
            }

        cmd =
            if
                (mdl.started == StartedReadingModel)
                    && (model.started == NotStarted)
            then
                Cmd.batch
                    [ get pk.model
                    , listKeysLabeled pk.token (pk.token ++ ".")
                    ]

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            handleGetResponse label key value mdl

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys model

        _ ->
            mdl |> withCmd cmd


getInstance : Model -> Cmd Msg
getInstance model =
    let
        serverInfo =
            { server = model.server
            , token = Nothing
            }
    in
    Request.serverRequest (\id -> ReceiveInstance)
        []
        serverInfo
        ()
        InstanceRequest


getVerifyCredentials : Model -> Cmd Msg
getVerifyCredentials model =
    case model.loginServer of
        Nothing ->
            Cmd.none

        Just server ->
            case model.token of
                Nothing ->
                    sendRequest InstanceRequest model
                        |> Tuple.second

                Just token ->
                    Request.serverRequest (\_ -> ReceiveGetVerifyCredentials)
                        []
                        { server = server
                        , token = Just token
                        }
                        ()
                    <|
                        AccountsRequest Request.GetVerifyCredentials


handleListKeysResponse : Maybe String -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse maybeLabel prefix keys model =
    case maybeLabel of
        Nothing ->
            model |> withNoCmd

        Just label ->
            -- label will be pk.token,
            -- but we won't care about that until the value comes in
            -- to handleGetResponse below.
            model |> withCmds (List.map (getLabeled label) keys)


handleGetModel : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModel maybeValue model =
    case maybeValue of
        Nothing ->
            { model
                | started = Started
                , msg = Nothing
            }
                |> withNoCmd

        Just value ->
            case JD.decodeValue savedModelDecoder value of
                Err err ->
                    { model
                        | started = Started
                        , msg =
                            Just <|
                                Debug.log "Error decoding SavedModel"
                                    (JD.errorToString err)
                    }
                        |> withNoCmd

                Ok savedModel ->
                    let
                        mdl =
                            Debug.log "savedModelToModel" <|
                                savedModelToModel savedModel model
                    in
                    { mdl
                        | started = Started
                        , msg = Nothing
                    }
                        |> withCmd
                            (if mdl.loginServer == Nothing then
                                Task.perform SetServer <| Task.succeed mdl.server

                             else
                                getVerifyCredentials mdl
                            )


handleGetToken : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetToken key value model =
    case JD.decodeValue JD.string value of
        Err err ->
            let
                ignore =
                    Debug.log ("Error decoding " ++ key) err
            in
            model |> withNoCmd

        Ok token ->
            let
                tokens =
                    model.tokens

                server =
                    Debug.log "Received token for server" <|
                        tokenStorageKeyServer key
            in
            { model | tokens = Dict.insert server token tokens }
                |> withNoCmd


handleGetResponse : Maybe String -> String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse maybeLabel key maybeValue model =
    case maybeLabel of
        Nothing ->
            if key == pk.model then
                handleGetModel maybeValue model

            else
                model |> withNoCmd

        Just label ->
            case maybeValue of
                Nothing ->
                    model |> withNoCmd

                Just value ->
                    if label == pk.token then
                        handleGetToken key value model

                    else
                        model |> withNoCmd


socketHandler : WebSocket.Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        WebSocket.ErrorResponse error ->
            case error of
                WebSocket.SocketAlreadyOpenError _ ->
                    socketHandler
                        (WebSocket.ConnectedResponse { key = "", description = "" })
                        state
                        model

                _ ->
                    { model | msg = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse received ->
            model |> withNoCmd

        WebSocket.ClosedResponse { expected, reason } ->
            model
                |> withNoCmd

        WebSocket.ConnectedResponse _ ->
            model |> withNoCmd

        _ ->
            model |> withNoCmd


updatePatchCredentialsInputs : Model -> Model
updatePatchCredentialsInputs model =
    case model.account of
        Nothing ->
            { model
                | displayName = ""
                , note = ""
                , privacy = PublicPrivacy
                , sensitive = False
                , language = ""
            }

        Just account ->
            let
                ( ( privacy, sensitive, language ), ( note, fields ) ) =
                    case account.source of
                        Nothing ->
                            ( ( PublicPrivacy, False, "" )
                            , ( "", [] )
                            )

                        Just source ->
                            ( ( source.privacy
                              , source.sensitive
                              , Maybe.withDefault "" source.language
                              )
                            , ( source.note, source.fields )
                            )
            in
            { model
                | displayName = account.display_name
                , note = note
                , privacy = privacy
                , sensitive = sensitive
                , language = language
            }


imageMimeTypes : List String
imageMimeTypes =
    [ "image/jpeg"
    , "image/gif"
    , "image/png"
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model2, cmd ) =
            updateInternal msg model

        savedModel =
            modelToSavedModel model2

        needsSaving =
            if model2.started /= Started then
                False

            else
                case model2.savedModel of
                    Nothing ->
                        True

                    Just sm ->
                        savedModel /= sm
    in
    { model2
        | savedModel =
            if needsSaving then
                Just savedModel

            else
                model2.savedModel
    }
        |> withCmds
            [ cmd
            , if needsSaving then
                put pk.model (Just <| encodeSavedModel savedModel)

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        OnUrlRequest _ ->
            model |> withNoCmd

        OnUrlChange _ ->
            model |> withNoCmd

        ToggleClearAllDialog ->
            { model | clearAllDialogVisible = not model.clearAllDialogVisible }
                |> withCmd
                    (if model.clearAllDialogVisible then
                        Cmd.none

                     else
                        Task.attempt (\_ -> Noop) <|
                            Dom.focus cancelButtonId
                    )

        OnKeyPress key ->
            { model
                | clearAllDialogVisible =
                    if key == "Escape" then
                        False

                    else
                        model.clearAllDialogVisible
            }
                |> withNoCmd

        SetServer server ->
            let
                mdl =
                    { model | server = server }
            in
            mdl
                |> withCmd
                    (if String.contains "." server then
                        getInstance mdl

                     else
                        Cmd.none
                    )

        SetWhichGroups s ->
            let
                whichGroups =
                    case s of
                        "Featured" ->
                            Request.FeaturedGroups

                        "Admin" ->
                            Request.AdminGroups

                        _ ->
                            Request.MemberGroups
            in
            { model | whichGroups = whichGroups }
                |> withNoCmd

        ClearSentReceived ->
            { model
                | request = Nothing
                , response = Nothing
                , metadata = Nothing
            }
                |> withNoCmd

        TogglePrettify ->
            { model | prettify = not model.prettify }
                |> withNoCmd

        ToggleShowMetadata ->
            { model | showMetadata = not model.showMetadata }
                |> withNoCmd

        ToggleShowReceived ->
            { model | showReceived = not model.showReceived }
                |> withNoCmd

        ToggleShowEntity ->
            { model | showEntity = not model.showEntity }
                |> withNoCmd

        ToggleStyle ->
            { model
                | style =
                    if model.style == LightStyle then
                        DarkStyle

                    else
                        LightStyle
            }
                |> withNoCmd

        SetQ q ->
            { model | q = q }
                |> withNoCmd

        ToggleResolve ->
            { model | resolve = not model.resolve }
                |> withNoCmd

        ToggleFollowing ->
            { model | following = not model.following }
                |> withNoCmd

        ToggleFollowReblogs ->
            { model | followReblogs = not model.followReblogs }
                |> withNoCmd

        SetSelectedRequest selectedRequest selected ->
            { model
                | selectedRequest =
                    if selected then
                        selectedRequest

                    else
                        model.selectedRequest
            }
                |> withNoCmd

        ReceiveRedirect result ->
            case result of
                Err ( server, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( server, app, cmd ) ->
                    { model | msg = Nothing }
                        |> withCmd cmd

        ReceiveAuthorization result ->
            case result of
                Err ( server, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( server, authorization, account ) ->
                    let
                        ( mdl, cmd ) =
                            saveAuthorization server authorization model

                        serverInfo =
                            { server = server
                            , token = Just authorization.token
                            }

                        mdl2 =
                            { mdl
                                | msg = Nothing
                                , token = Just authorization.token
                                , loginServer = Just server
                                , account = Just account
                                , request =
                                    -- Fake the request
                                    Just <|
                                        Request.requestToRawRequest []
                                            serverInfo
                                            (AccountsRequest Request.GetVerifyCredentials)
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updatePatchCredentialsInputs
                    in
                    mdl2
                        |> withCmds [ cmd, getAccountIdRelationships False mdl ]

        ReceiveFetchAccount result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok ( loginServer, token, account ) ->
                    let
                        serverInfo =
                            { server = loginServer
                            , token = Just token
                            }

                        request =
                            -- Fake the request
                            Request.requestToRawRequest []
                                serverInfo
                                (AccountsRequest Request.GetVerifyCredentials)

                        mdl =
                            { model
                                | msg = Nothing
                                , server = loginServer
                                , loginServer = Just loginServer
                                , token = Just token
                                , account = Just account
                                , request = Just request
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updatePatchCredentialsInputs
                    in
                    mdl
                        |> withCmd (getAccountIdRelationships False mdl)

        ReceiveInstance result ->
            case result of
                Err _ ->
                    -- We'll get lots of errors, for non-existant domains
                    model |> withNoCmd

                Ok response ->
                    case response.entity of
                        InstanceEntity instance ->
                            { model
                                | msg = Nothing
                                , request = Just response.rawRequest
                                , metadata = Just response.metadata
                                , response = Just instance.v
                                , entity = Just response.entity
                            }
                                |> withNoCmd

                        _ ->
                            model |> withNoCmd

        ReceiveGetVerifyCredentials result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok response ->
                    case response.entity of
                        AccountEntity account ->
                            let
                                mdl =
                                    { model
                                        | msg = Nothing
                                        , request = Just response.rawRequest
                                        , metadata = Just response.metadata
                                        , response = Just account.v
                                        , entity = Just response.entity
                                        , account = Just account
                                    }
                                        |> updatePatchCredentialsInputs
                            in
                            mdl
                                |> withCmd (getAccountIdRelationships False mdl)

                        _ ->
                            model |> withNoCmd

        ReceiveAccountIdRelationships showResult result ->
            case result of
                Err _ ->
                    ( if showResult then
                        { model
                            | metadata = Nothing
                            , request = Nothing
                            , response = Nothing
                            , entity = Nothing
                        }

                      else
                        model
                    , Cmd.none
                    )

                Ok response ->
                    case response.entity of
                        RelationshipListEntity relationships ->
                            case
                                LE.find (\r -> r.id == model.accountId)
                                    relationships
                            of
                                Nothing ->
                                    model |> withNoCmd

                                Just relationship ->
                                    -- Maybe we should handle blocked_by
                                    let
                                        mdl =
                                            if not showResult then
                                                model

                                            else
                                                { model
                                                    | metadata =
                                                        Just response.metadata
                                                    , request =
                                                        Just response.rawRequest
                                                    , response =
                                                        Just relationship.v
                                                    , entity =
                                                        Just response.entity
                                                }
                                    in
                                    { mdl
                                        | isAccountFollowed =
                                            relationship.following
                                    }
                                        |> withNoCmd

                        _ ->
                            model |> withNoCmd

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok res ->
                    res

        SetLoginServer ->
            if model.server == "" then
                { model
                    | msg = Nothing
                    , loginServer = Nothing
                    , request = Nothing
                    , response = Nothing
                    , entity = Nothing
                    , metadata = Nothing
                }
                    |> withNoCmd

            else
                let
                    mdl =
                        { model
                            | loginServer = Just model.server
                            , token = Nothing
                            , account = Nothing
                        }
                            |> updatePatchCredentialsInputs
                in
                sendRequest InstanceRequest mdl

        Login ->
            let
                url =
                    model.url

                sau =
                    { client_name = "mammudeck"
                    , server = model.server
                    , applicationUri =
                        { url
                            | fragment = Nothing
                            , query = Nothing
                        }
                            |> Url.toString
                    }
            in
            case Login.loginTask sau <| Dict.get model.server model.tokens of
                Redirect task ->
                    ( model, Task.attempt ReceiveRedirect task )

                FetchAccount task ->
                    ( model, Task.attempt ReceiveFetchAccount task )

        Logout ->
            case model.loginServer of
                Nothing ->
                    model |> withNoCmd

                Just server ->
                    { model
                        | server = ""
                        , loginServer = Nothing
                        , account = Nothing
                        , tokens = Dict.remove server model.tokens
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , metadata = Nothing
                        , msg = Nothing
                    }
                        |> updatePatchCredentialsInputs
                        |> withCmd (putToken server Nothing)

        ClearAll ->
            let
                mdl =
                    { model
                        | clearAllDialogVisible = False
                        , tokens = Dict.empty
                        , server = ""
                        , loginServer = Nothing
                        , account = Nothing
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , metadata = Nothing
                        , msg = Nothing
                    }
                        |> updatePatchCredentialsInputs
            in
            { mdl | savedModel = Just <| modelToSavedModel mdl }
                |> withCmd clear

        SetUsername username ->
            { model | username = username }
                |> withNoCmd

        SetAccountId accountId ->
            let
                mdl =
                    { model | accountId = accountId }
            in
            mdl
                |> withCmd (getAccountIdRelationships True mdl)

        SetLimit limit ->
            { model | limit = limit }
                |> withNoCmd

        ToggleOnlyMedia ->
            { model | onlyMedia = not model.onlyMedia }
                |> withNoCmd

        TogglePinned ->
            { model | pinned = not model.pinned }
                |> withNoCmd

        ToggleExcludeReplies ->
            { model | excludeReplies = not model.excludeReplies }
                |> withNoCmd

        ToggleExcludeReblogs ->
            { model | excludeReblogs = not model.excludeReblogs }
                |> withNoCmd

        SetAccountIds accountIds ->
            { model | accountIds = accountIds }
                |> withNoCmd

        SetDisplayName displayName ->
            { model | displayName = displayName }
                |> withNoCmd

        SetNote note ->
            { model | note = note }
                |> withNoCmd

        GetAvatarFile clearAvatar ->
            if clearAvatar then
                { model | avatarFile = Nothing }
                    |> withNoCmd

            else
                model
                    |> withCmd (File.Select.file imageMimeTypes ReceiveAvatarFile)

        ReceiveAvatarFile file ->
            { model | avatarFile = Just file }
                |> withNoCmd

        GetHeaderFile clearHeader ->
            if clearHeader then
                { model | headerFile = Nothing }
                    |> withNoCmd

            else
                model
                    |> withCmd (File.Select.file imageMimeTypes ReceiveHeaderFile)

        ReceiveHeaderFile file ->
            { model | headerFile = Just file }
                |> withNoCmd

        SetPrivacy privacy ->
            { model | privacy = privacy }
                |> withNoCmd

        SetSensitive sensitive ->
            { model | sensitive = sensitive }
                |> withNoCmd

        SetLanguage language ->
            { model | language = language }
                |> withNoCmd

        SetGroupId groupId ->
            { model | groupId = groupId }
                |> withNoCmd

        SendGetVerifyCredentials ->
            sendRequest (AccountsRequest Request.GetVerifyCredentials) model

        SendGetAccountByUsername ->
            sendRequest
                (AccountsRequest <|
                    Request.GetAccountByUsername
                        { username = getUsername model }
                )
                model

        SendGetAccount ->
            sendRequest
                (AccountsRequest <|
                    Request.GetAccount
                        { id = getAccountId model }
                )
                model

        SendGetFollowers ->
            sendRequest
                (AccountsRequest <|
                    Request.GetFollowers
                        { id = getAccountId model
                        , limit = String.toInt model.limit
                        }
                )
                model

        SendGetFollowing ->
            sendRequest
                (AccountsRequest <|
                    Request.GetFollowing
                        { id = getAccountId model
                        , limit = String.toInt model.limit
                        }
                )
                model

        SendGetStatuses ->
            sendRequest
                (AccountsRequest <|
                    Request.GetStatuses
                        { id = model.accountId
                        , only_media = model.onlyMedia
                        , pinned = model.pinned
                        , exclude_replies = model.excludeReplies
                        , paging =
                            case String.toInt model.limit of
                                Nothing ->
                                    Nothing

                                Just limit ->
                                    Just
                                        { emptyPaging
                                            | limit = Just limit
                                        }
                        , exclude_reblogs = model.excludeReblogs
                        }
                )
                model

        SendGetRelationships ->
            sendRequest
                (AccountsRequest <|
                    Request.GetRelationships
                        { ids =
                            String.split "," model.accountIds
                                |> List.map String.trim
                        }
                )
                model

        SendGetSearchAccounts ->
            sendRequest
                (AccountsRequest <|
                    Request.GetSearchAccounts
                        { q = model.q
                        , limit = String.toInt model.limit
                        , resolve = model.resolve
                        , following = model.following
                        }
                )
                model

        SendPostFollowOrUnfollow ->
            sendRequest
                (AccountsRequest <|
                    if model.isAccountFollowed then
                        Request.PostUnfollow { id = model.accountId }

                    else
                        Request.PostFollow
                            { id = model.accountId
                            , reblogs = model.followReblogs
                            }
                )
                model

        SendPatchUpdateCredentials ->
            -- TODO
            model |> withNoCmd

        SendGetGroups ->
            sendRequest
                (GroupsRequest <| Request.GetGroups { tab = model.whichGroups })
                model

        SendGetGroup ->
            sendRequest
                (GroupsRequest <| Request.GetGroup { id = model.groupId })
                model

        ReceiveResponse result ->
            receiveResponse result model


getAccountIdRelationships : Bool -> Model -> Cmd Msg
getAccountIdRelationships showResult model =
    case model.account of
        Nothing ->
            Cmd.none

        Just account ->
            case model.loginServer of
                Nothing ->
                    Cmd.none

                Just server ->
                    case model.accountId of
                        "" ->
                            Cmd.none

                        accountId ->
                            Request.serverRequest ReceiveAccountIdRelationships
                                []
                                { server = server
                                , token = model.token
                                }
                                showResult
                                (AccountsRequest <|
                                    Request.GetRelationships
                                        { ids = [ accountId ] }
                                )


getUsername : Model -> String
getUsername model =
    let
        username =
            model.username
    in
    if username /= "" then
        username

    else
        case model.account of
            Just account ->
                account.username

            Nothing ->
                ""


getAccountId : Model -> String
getAccountId model =
    let
        id =
            model.accountId
    in
    if id /= "" then
        id

    else
        case model.account of
            Just account ->
                account.id

            Nothing ->
                ""


receiveResponse : Result Error Response -> Model -> ( Model, Cmd Msg )
receiveResponse result model =
    case result of
        Err err ->
            { model
                | msg = Just <| Debug.toString err
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
            }
                |> withNoCmd

        Ok response ->
            let
                mdl =
                    applyResponseSideEffects response model
            in
            { mdl
                | msg = Nothing
                , metadata = Just response.metadata
                , response = Just <| ED.entityValue response.entity
                , entity = Just response.entity
            }
                |> withNoCmd


applyResponseSideEffects : Response -> Model -> Model
applyResponseSideEffects response model =
    case response.request of
        AccountsRequest Request.GetVerifyCredentials ->
            case response.entity of
                AccountEntity account ->
                    updatePatchCredentialsInputs model

                _ ->
                    model

        AccountsRequest (Request.GetAccountByUsername _) ->
            case response.entity of
                AccountEntity { id } ->
                    { model | accountId = id }

                _ ->
                    model

        AccountsRequest (Request.PostFollow _) ->
            { model | isAccountFollowed = True }

        AccountsRequest (Request.PostUnfollow _) ->
            { model | isAccountFollowed = False }

        _ ->
            model


sendRequest : Request -> Model -> ( Model, Cmd Msg )
sendRequest request model =
    case model.loginServer of
        Nothing ->
            model |> withNoCmd

        Just server ->
            let
                rawRequest =
                    Request.requestToRawRequest []
                        { server = server
                        , token = model.token
                        }
                        request
            in
            { model
                | request = Just rawRequest
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
            }
                |> withCmd
                    (Request.rawRequestToCmd ReceiveResponse rawRequest)


saveAuthorization : String -> Authorization -> Model -> ( Model, Cmd Msg )
saveAuthorization server authorization model =
    let
        tokens =
            model.tokens
    in
    { model
        | tokens =
            Dict.insert server
                authorization.token
                tokens
    }
        |> withCmd (putToken server <| Just authorization.token)


serverOption : String -> String -> Html Msg
serverOption currentServer server =
    option
        [ value server
        , selected <| server == currentServer
        ]
        [ text server ]


serverSelect : Model -> Html Msg
serverSelect model =
    let
        currentServer =
            case model.loginServer of
                Nothing ->
                    ""

                Just server ->
                    server
    in
    select [ onInput SetServer ]
        (option [ value "" ]
            [ text "-- select a server --" ]
            :: (List.map (serverOption currentServer) <| Dict.keys model.tokens)
        )


whichGroupsSelect : Model -> Html Msg
whichGroupsSelect model =
    let
        whichGroups =
            model.whichGroups
    in
    select [ onInput SetWhichGroups ] <|
        [ option
            [ value "Member"
            , selected <| Request.MemberGroups == whichGroups
            ]
            [ text "Member" ]
        , option
            [ value "Featured"
            , selected <| Request.FeaturedGroups == whichGroups
            ]
            [ text "Featured" ]
        , option
            [ value "Admin"
            , selected <| Request.AdminGroups == whichGroups
            ]
            [ text "Admin" ]
        ]


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


type alias StyleProperties =
    { backgroundColor : String
    , color : String
    }


type Style
    = DarkStyle
    | LightStyle


styles :
    { dark : StyleProperties
    , light : StyleProperties
    }
styles =
    { dark =
        { backgroundColor = "#222"
        , color = "#eee"
        }
    , light =
        { backgroundColor = "white"
        , color = "black"
        }
    }


getStyle : Style -> StyleProperties
getStyle style =
    case style of
        DarkStyle ->
            styles.dark

        LightStyle ->
            styles.light


theStyle : Style
theStyle =
    DarkStyle


type SelectedRequest
    = LoginSelected
    | AccountsSelected
    | BlocksSelected
    | GroupsSelected


encodeSelectedRequest : SelectedRequest -> Value
encodeSelectedRequest selectedRequest =
    JE.string <| selectedRequestToString selectedRequest


selectedRequestToString : SelectedRequest -> String
selectedRequestToString selectedRequest =
    case selectedRequest of
        LoginSelected ->
            "login"

        AccountsSelected ->
            "AccountsRequest"

        BlocksSelected ->
            "BlocksRequest"

        GroupsSelected ->
            "GroupsRequest"


selectedRequestDecoder : Decoder SelectedRequest
selectedRequestDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                JD.succeed <|
                    selectedRequestFromString s
            )


selectedRequestFromString : String -> SelectedRequest
selectedRequestFromString s =
    case s of
        "AccountsRequest" ->
            AccountsSelected

        "BlocksRequest" ->
            BlocksSelected

        "GroupsRequest" ->
            GroupsSelected

        _ ->
            LoginSelected


radioNames =
    { selectedRequest = "selectedRequest"
    , privacy = "privacy"
    }


selectedRequestRadio : SelectedRequest -> Model -> Html Msg
selectedRequestRadio selectedRequest model =
    let
        string =
            selectedRequestToString selectedRequest
    in
    span [ onClick <| SetSelectedRequest selectedRequest True ]
        [ input
            [ type_ "radio"
            , name radioNames.selectedRequest
            , value string
            , checked <| model.selectedRequest == selectedRequest
            , onCheck (SetSelectedRequest selectedRequest)
            ]
            []
        , text " "
        , text string
        ]


selectedRequestHtml : SelectedRequest -> Model -> (Model -> Html Msg) -> Html Msg
selectedRequestHtml selectedRequest model body =
    span []
        [ selectedRequestRadio selectedRequest model
        , br
        , if selectedRequest /= model.selectedRequest then
            text ""

          else
            body model
        ]


privacyRadio : Privacy -> String -> Privacy -> Html Msg
privacyRadio privacy label currentPrivacy =
    span []
        [ input
            [ type_ "radio"
            , name radioNames.privacy
            , checked <| currentPrivacy == privacy
            , onCheck <|
                \checked ->
                    if checked then
                        SetPrivacy privacy

                    else
                        Noop
            ]
            []
        , text label
        ]


link : String -> String -> Html Msg
link label url =
    a
        [ href url
        , blankTarget
        ]
        [ text label ]


blankTarget : Attribute msg
blankTarget =
    target "_blank"


{-| The Material Design CSS puts paragraph spacing BELOW the paragraph.

Use this to make a paragraph worth of vertical white space.

-}
pspace : Html msg
pspace =
    p [] [ text "" ]


view : Model -> Document Msg
view model =
    let
        { backgroundColor, color } =
            getStyle model.style
    in
    { title = "Mastodon API Explorer"
    , body =
        [ clearAllDialog model
        , div
            [ style "background-color" backgroundColor
            , style "padding" "1em 0 0 0"
            , style "margin" "0"
            , style "width" "auto"
            ]
            [ div
                [ style "color" color
                , style "background-color" backgroundColor
                , style "padding" "1em 3em 1em 3em"
                , style "max-width" "fill-available"
                , style "width" "40em"
                , style "margin" "auto"
                ]
                [ div []
                    [ h2 [] [ text "Mastodon API Explorer" ]
                    , case model.loginServer of
                        Nothing ->
                            text ""

                        Just server ->
                            p []
                                [ b "Use API for: "
                                , link server <| "https://" ++ server
                                , text " "
                                , button [ onClick Logout ]
                                    [ text "Logout" ]
                                , br
                                , case model.account of
                                    Nothing ->
                                        text ""

                                    Just account ->
                                        span []
                                            [ b "Username: "
                                            , text account.display_name
                                            , text " ("
                                            , link account.username account.url
                                            , text ")"
                                            ]
                                ]
                    , p []
                        [ selectedRequestHtml LoginSelected model loginSelectedUI
                        , selectedRequestHtml AccountsSelected model accountsSelectedUI
                        , selectedRequestHtml BlocksSelected model blocksSelectedUI
                        , selectedRequestHtml GroupsSelected model groupsSelectedUI
                        ]
                    , p [ style "color" "red" ]
                        [ Maybe.withDefault "" model.msg |> text ]
                    , p []
                        [ span [ onClick TogglePrettify ]
                            [ input
                                [ type_ "checkbox"
                                , checked model.prettify
                                ]
                                []
                            , b "Prettify"
                            , text " (easier to read, may no longer be valid JSON)"
                            ]
                        , text " "
                        , button [ onClick ClearSentReceived ]
                            [ text "Clear" ]
                        ]
                    , p [] [ b "Sent:" ]
                    , pre []
                        [ case model.request of
                            Nothing ->
                                text ""

                            Just request ->
                                span []
                                    [ text request.method
                                    , text " "
                                    , text request.url
                                    , case request.jsonBody of
                                        Nothing ->
                                            text ""

                                        Just value ->
                                            pre []
                                                [ text <|
                                                    encodeWrap model.prettify value
                                                ]
                                    ]
                        ]
                    , p [] <|
                        if model.showMetadata then
                            [ b "Headers: "
                            , button [ onClick ToggleShowMetadata ]
                                [ text "Hide" ]
                            ]

                        else
                            [ b "Headers "
                            , button [ onClick ToggleShowMetadata ]
                                [ text "Show" ]
                            ]
                    , if not model.showMetadata then
                        text ""

                      else
                        case model.metadata of
                            Nothing ->
                                text ""

                            Just metadata ->
                                p []
                                    [ renderHeaders model.prettify color metadata ]
                    , p [] <|
                        if model.showReceived then
                            [ b "Received:"
                            , button [ onClick ToggleShowReceived ]
                                [ text "Hide" ]
                            ]

                        else
                            [ b "Received "
                            , button [ onClick ToggleShowReceived ]
                                [ text "Show" ]
                            ]
                    , if not model.showReceived then
                        text ""

                      else
                        pre []
                            [ case model.response of
                                Nothing ->
                                    text ""

                                Just value ->
                                    text <|
                                        encodeWrap model.prettify value
                            ]
                    , p [] <|
                        if model.showEntity then
                            [ b "Decoded:"
                            , button [ onClick ToggleShowEntity ]
                                [ text "Hide" ]
                            ]

                        else
                            [ b "Decoded "
                            , button [ onClick ToggleShowEntity ]
                                [ text "Show" ]
                            ]
                    , if not model.showEntity then
                        text ""

                      else
                        pre []
                            [ case model.entity of
                                Nothing ->
                                    text ""

                                Just entity ->
                                    entity
                                        |> ED.encodeEntity
                                        |> encodeWrap model.prettify
                                        |> text
                            ]
                    , div []
                        [ help model ]
                    , br
                    , p []
                        [ button [ onClick ToggleClearAllDialog ]
                            [ text "Clear All Persistent State" ]
                        ]
                    , br
                    , p [ onClick ToggleStyle ]
                        [ input
                            [ type_ "checkbox"
                            , checked <| model.style == DarkStyle
                            ]
                            []
                        , b "Dark Mode"
                        ]
                    , p []
                        [ text <| "Copyright " ++ special.copyright ++ " 2019, Bill St. Clair"
                        , br
                        , text "API Docs: "
                        , link "docs.joinmastodon.org"
                            "https://docs.joinmastodon.org/api/guidelines"
                        , br
                        , text "Source code: "
                        , link "GitHub"
                            "https://github.com/billstclair/elm-mastodon"
                        ]
                    ]
                ]
            ]
        ]
    }


blocksSelectedUI : Model -> Html Msg
blocksSelectedUI model =
    p []
        [ pspace
        , text "Blocks requests go here."
        ]


groupsSelectedUI : Model -> Html Msg
groupsSelectedUI model =
    p []
        [ pspace
        , whichGroupsSelect model
        , text " "
        , button [ onClick SendGetGroups ]
            [ text "GetGroups" ]
        , br
        , b "id: "
        , input
            [ size 20
            , onInput SetGroupId
            , value model.groupId
            ]
            []
        , text " "
        , button [ onClick SendGetGroup ]
            [ text "GetGroup" ]
        ]


renderChooseFile : String -> Maybe File -> (Bool -> Msg) -> Html Msg
renderChooseFile label maybeFile getter =
    span []
        [ b label
        , case maybeFile of
            Nothing ->
                text "No file selected "

            Just file ->
                let
                    name =
                        File.name file

                    size =
                        File.size file
                in
                span []
                    [ text <| String.fromInt size
                    , text " "
                    , text name
                    , text " "
                    , button [ onClick <| getter True ]
                        [ text "Clear" ]
                    , text " "
                    ]
        , button [ onClick <| getter False ]
            [ text "Choose File" ]
        ]


accountsSelectedUI : Model -> Html Msg
accountsSelectedUI model =
    p []
        [ pspace
        , button [ onClick SendGetVerifyCredentials ]
            [ text "GetVerifyCredentials" ]
        , br
        , b "username: "
        , input
            [ size 30
            , onInput SetUsername
            , value model.username
            ]
            []
        , text " "
        , button [ onClick SendGetAccountByUsername ]
            [ text "GetAccountByUsername" ]
        , br
        , b "id: "
        , input
            [ size 20
            , onInput SetAccountId
            , value model.accountId
            ]
            []
        , text " "
        , button [ onClick SendGetAccount ]
            [ text "GetAccount" ]
        , br
        , b "limit: "
        , input
            [ size 10
            , onInput SetLimit
            , value model.limit
            ]
            []
        , text " "
        , button [ onClick SendGetFollowers ]
            [ text "GetFollowers" ]
        , text " "
        , button [ onClick SendGetFollowing ]
            [ text "GetFollowing" ]
        , br
        , span [ onClick ToggleOnlyMedia ]
            [ input
                [ type_ "checkbox"
                , checked model.onlyMedia
                ]
                []
            , b "only media"
            ]
        , text " "
        , span [ onClick TogglePinned ]
            [ input
                [ type_ "checkbox"
                , checked model.pinned
                ]
                []
            , b "pinned"
            ]
        , text " "
        , span [ onClick ToggleExcludeReplies ]
            [ input
                [ type_ "checkbox"
                , checked <| not model.excludeReplies
                ]
                []
            , b "replies"
            ]
        , text " "
        , span [ onClick ToggleExcludeReblogs ]
            [ input
                [ type_ "checkbox"
                , checked <| not model.excludeReblogs
                ]
                []
            , b "reblogs"
            ]
        , text " "
        , button [ onClick SendGetStatuses ]
            [ text "GetStatuses" ]
        , br
        , b "ids (1,2,...): "
        , input
            [ size 40
            , onInput SetAccountIds
            , value model.accountIds
            ]
            []
        , text " "
        , button [ onClick SendGetRelationships ]
            [ text "GetRelationships" ]
        , br
        , b "q: "
        , input
            [ size 40
            , onInput SetQ
            , value model.q
            ]
            []
        , br
        , b "limit: "
        , input
            [ size 10
            , onInput SetLimit
            , value model.limit
            ]
            []
        , text " "
        , span [ onClick ToggleResolve ]
            [ input
                [ type_ "checkbox"
                , checked model.resolve
                ]
                []
            ]
        , b " resolve "
        , span [ onClick ToggleFollowing ]
            [ input
                [ type_ "checkbox"
                , checked model.following
                ]
                []
            , b " following "
            ]
        , button [ onClick SendGetSearchAccounts ]
            [ text "GetSearchAccounts" ]
        , br
        , text "-- writes below here --"
        , br
        , b "id: "
        , input
            [ size 20
            , onInput SetAccountId
            , value model.accountId
            ]
            []
        , text " "
        , span [ onClick ToggleFollowReblogs ]
            [ input
                [ type_ "checkbox"
                , checked model.followReblogs
                , disabled model.isAccountFollowed
                ]
                []
            , b " reblogs "
            ]
        , button [ onClick SendPostFollowOrUnfollow ]
            [ text <|
                if model.isAccountFollowed then
                    "PostUnfollow"

                else
                    "PostFollow"
            ]
        , br
        , b "Display name: "
        , input
            [ size 40
            , onInput SetDisplayName
            , value model.displayName
            ]
            []
        , br
        , b "Note: "
        , textarea
            [ rows 4
            , cols 50
            , onInput SetNote
            ]
            [ text model.note ]
        , br
        , renderChooseFile "Avatar: " model.avatarFile GetAvatarFile
        , br
        , renderChooseFile "Header: " model.headerFile GetHeaderFile
        , br
        , b "Privacy: "
        , privacyRadio PublicPrivacy "public " model.privacy
        , privacyRadio UnlistedPrivacy "unlisted " model.privacy
        , privacyRadio PrivatePrivacy "private " model.privacy
        , br
        , b "Sensitive: "
        , input
            [ type_ "checkbox"
            , checked model.sensitive
            , onCheck SetSensitive
            ]
            []
        , b " Language: "
        , input
            [ size 2
            , onInput SetLanguage
            , value model.language
            ]
            []
        , br
        , button [ onClick SendPatchUpdateCredentials ]
            [ text "PatchUpdateCredentials" ]

        -- avatar (File), header (File), privacy (Privacy), sensitive (Bool), language (Maybe String)
        ]


loginSelectedUI : Model -> Html Msg
loginSelectedUI model =
    p []
        [ pspace
        , b "Server: "
        , input
            [ size 30
            , onInput SetServer
            , value model.server
            , placeholder "mastodon.social"
            ]
            []
        , text " "
        , serverSelect model
        , br
        , button
            [ onClick Login
            , disabled <| model.server == ""
            ]
            [ text "Login" ]
        , text " "
        , button
            [ onClick SetLoginServer ]
            [ text "Set Server" ]
        ]


renderHeaders : Bool -> String -> Http.Metadata -> Html Msg
renderHeaders prettify color metadata =
    let
        fold k v res =
            let
                vline =
                    if not prettify then
                        v

                    else
                        wrapJsonLine wrapColumns v
                            |> String.join "\n"
            in
            tr []
                [ td [] [ pre [] [ text k ] ]
                , td [] [ pre [] [ text vline ] ]
                ]
                :: res
    in
    Dict.foldr fold [] metadata.headers
        |> table
            [ style "color" color
            , style "font-size" "14px"
            ]


cancelButtonId : String
cancelButtonId =
    "cancelButton"


clearAllDialog : Model -> Html Msg
clearAllDialog model =
    Dialog.render
        { styles = [ ( "width", "40%" ) ]
        , title = "Confirm"
        , content = [ text "Do you really want to erase everything?" ]
        , actionBar =
            [ button
                [ onClick ToggleClearAllDialog
                , id cancelButtonId
                ]
                [ text "Cancel" ]
            , text <| String.repeat 4 special.nbsp
            , button [ onClick ClearAll ]
                [ text "Erase" ]
            ]
        }
        model.clearAllDialogVisible


help : Model -> Html Msg
help model =
    Markdown.toHtml [] <|
        if model.selectedRequest == AccountsSelected then
            """
**AccountsRequest Help**

The "GetVerifyCredentials" button fetches the `Account` entity for the logged-in user.

The "GetAccountByUsername" button fetches the `Account` entity for the user with the given "username". If the "username" is blank, it uses the username of the logged in user, or sends blank, which will result in an error, if not logged in. If successful, it fills in the "id" with that user's account ID. This is a Gab-only feature.

The "GetAccount" button fetches the `Account` entity for the user with the given "id". If "id" is blank, uses the id of the logged in user, or sends blank, which will result in an error, if not logged in.

The "GetFollowers" and "GetFollowing" buttons fetch lists of the associated `Account` entities. If "limit" is non-blank, it is the maximum number of entities to return.

The "GetRelationships" button returns a list of `Relationship` entities, one for each of the (comma-separated) "ids".

The "GetSearchAccounts" button returns a list of `Account` entities that match "q", "resolve", and "following". If "limit" is non-blank, it is the maximum number of entities to return.

The "Postfollow / PostUnfollow" button either follows or unfollows the account with the given "id". If following, will show reblogs if and only if "reblogs" is checked. In order to decide whether to follow or unfollow when you click the button, every change to the "id" causes A `GetRelationships` request to be sent.
            """

        else if model.selectedRequest == BlocksSelected then
            """
**BlocksRequest Help**

Blocks help goes here.
            """

        else if model.selectedRequest == GroupsSelected then
            """
**GroupsRequest Help**

Groups are a Gab-only feature.

Click "GetGroups" to get the list of groups in the "Member", "Featured", or "Admin" selector.

Click "GetGroup" to get information about the group with the given "id".
            """

        else
            """
**General and Login Help**

Click a radio button to choose the user interface for that section of the API. The names match the variant names in [Mastodon.Request](https://github.com/billstclair/elm-mastodon/blob/master/src/Mastodon/Request.elm)'s `xxxReq` types.

Type a server name, e.g. `mastodon.social`, in the "Server" box at the top of the screen. As soon as you finish typing the name of a real Mastodon server, it will show its `Instance` entity.

The selector to the right of the "Server" input area shows the servers to which you have successfully logged in. Tokens are saved for each, so you don't need to visit the server authentication page again to login to that account. Selecting one of the servers here changes the "Server" input box, and looks up that server's `Instance` entity, but does NOT change the "Use API for" setting. You need to click "Login" or "Set Server" to do that.

The "Login" button logs in to the displayed "Server". This will use a saved token, if there is one, or redirect to the server's authorization page, where you will need to enter your userid/email and password, or, if there are cookies for that in your browser, just click to approve access. Your `Account` entity will be fetched and displayed.

The "Set Server" button uses the "Server" for API requests without logging in. Only a few API requests work without logging in, but this lets you do some exploration of a server without having an account there. The server's `Instance` entity will be fetched and displayed.

The "Logout" button logs out of the "Use API for" server. This will remove it from the server selector and clear its persistent token, requiring you to reauthenticate if you login again.

The "Prettify" checkbox controls whether the JSON output lines are wrapped to fit the screen. If selected, then the output will not necessarily be valid JSON. If NOT selected, then it will, and you can copy and paste it into environments that expect JSON.

The "Headers" section, if enabled, shows the headers received from the server for the last request.

The "Received" section, if enabled, shows the JSON received from the server for the last request.

The "Decoded" section, if enabled, shows the decoded JSON received from the server for the last request. If it differs from "Received", there is either a decoder bug, one or more fields are not yet supported, or the received JSON uses defaults for one or more fields.

The "Clear" button on the same line as the "Prettify" checkbox clears the "Sent", "Received", and "Decoded" sections, making this help easier to see.

The "Clear All Persistent State" button near the bottom of the page does that, after you click "Erase" on a confirmation dialog.

The "Dark Mode" checkbox toggles between light and dark mode.

If you look at the [code for this module](https://github.com/billstclair/elm-mastodon/blob/master/example/src/Main.elm), and search for `SendGetVerifyCredentials`, you'll see examples of using the `Mastodon.Request` module.
    """


convertJsonNewlines : String -> String
convertJsonNewlines json =
    String.replace "\\r" "" json
        |> String.replace "\\n" "\n"


wrapJsonLine : Int -> String -> List String
wrapJsonLine width line =
    let
        body =
            String.trimLeft line

        indentN =
            String.length line - String.length body + 2

        initialIndent =
            String.repeat (indentN - 2) " "

        indent =
            String.repeat indentN " "

        wrapped =
            convertJsonNewlines body
                |> String.split "\n"
                |> List.map (SE.softWrap <| max 20 (width - indentN))
                |> String.join "\n"

        lines =
            String.split "\n" wrapped
    in
    case lines of
        [] ->
            []

        first :: rest ->
            (initialIndent ++ first)
                :: List.map ((++) indent) rest


wrapJsonLines : Int -> String -> String
wrapJsonLines width string =
    String.split "\n" string
        |> List.concatMap (wrapJsonLine width)
        |> String.join "\n"


wrapColumns : Int
wrapColumns =
    80


encodeWrap : Bool -> Value -> String
encodeWrap prettify value =
    JE.encode 2 value
        |> (if prettify then
                wrapJsonLines wrapColumns

            else
                identity
           )



---
--- Persistence
---


type alias SavedModel =
    { loginServer : Maybe String
    , token : Maybe String
    , server : String
    , prettify : Bool
    , style : Style
    , selectedRequest : SelectedRequest
    , username : String
    , accountId : String
    , limit : String
    , accountIds : String
    , showMetadata : Bool
    , q : String
    , resolve : Bool
    , following : Bool
    , groupId : String
    , showReceived : Bool
    , showEntity : Bool
    , whichGroups : WhichGroups
    , followReblogs : Bool
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { loginServer = model.loginServer
    , token = model.token
    , server = model.server
    , prettify = model.prettify
    , style = model.style
    , selectedRequest = model.selectedRequest
    , username = model.username
    , accountId = model.accountId
    , limit = model.limit
    , accountIds = model.accountIds
    , showMetadata = model.showMetadata
    , q = model.q
    , resolve = model.resolve
    , following = model.following
    , groupId = model.groupId
    , showReceived = model.showReceived
    , showEntity = model.showEntity
    , whichGroups = model.whichGroups
    , followReblogs = model.followReblogs
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | loginServer = savedModel.loginServer
        , token = savedModel.token
        , server = savedModel.server
        , prettify = savedModel.prettify
        , style = savedModel.style
        , selectedRequest = savedModel.selectedRequest
        , username = savedModel.username
        , accountId = savedModel.accountId
        , limit = savedModel.limit
        , accountIds = savedModel.accountIds
        , showMetadata = savedModel.showMetadata
        , q = savedModel.q
        , resolve = savedModel.resolve
        , following = savedModel.following
        , groupId = savedModel.groupId
        , showReceived = savedModel.showReceived
        , showEntity = savedModel.showEntity
        , whichGroups = savedModel.whichGroups
        , followReblogs = savedModel.followReblogs
    }


encodeWhichGroups : WhichGroups -> Value
encodeWhichGroups whichGroups =
    JE.string <|
        case whichGroups of
            Request.MemberGroups ->
                "MemberGroups"

            Request.FeaturedGroups ->
                "FeaturedGroups"

            Request.AdminGroups ->
                "AdminGroups"


whichGroupsDecoder : Decoder WhichGroups
whichGroupsDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "MemberGroups" ->
                        JD.succeed Request.MemberGroups

                    "FeaturedGroups" ->
                        JD.succeed Request.FeaturedGroups

                    "AdminGroups" ->
                        JD.succeed Request.AdminGroups

                    _ ->
                        JD.fail <| "Unknown WhichGroups value: " ++ s
            )


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "loginServer", ED.encodeMaybe JE.string savedModel.loginServer )
        , ( "token", ED.encodeMaybe JE.string savedModel.token )
        , ( "server", JE.string savedModel.server )
        , ( "prettify", JE.bool savedModel.prettify )
        , ( "darkstyle", JE.bool <| savedModel.style == DarkStyle )
        , ( "selectedRequest", encodeSelectedRequest savedModel.selectedRequest )
        , ( "username", JE.string savedModel.username )
        , ( "accountId", JE.string savedModel.accountId )
        , ( "limit", JE.string savedModel.limit )
        , ( "accountIds", JE.string savedModel.accountIds )
        , ( "showMetadata", JE.bool savedModel.showMetadata )
        , ( "q", JE.string savedModel.q )
        , ( "resolve", JE.bool savedModel.resolve )
        , ( "following", JE.bool savedModel.following )
        , ( "groupId", JE.string savedModel.groupId )
        , ( "showReceived", JE.bool savedModel.showReceived )
        , ( "showEntity", JE.bool savedModel.showEntity )
        , ( "whichGroups", encodeWhichGroups savedModel.whichGroups )
        , ( "followReblogs", JE.bool savedModel.followReblogs )
        ]


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "loginServer" (JD.nullable JD.string) Nothing
        |> optional "token" (JD.nullable JD.string) Nothing
        |> required "server" JD.string
        |> optional "prettify" JD.bool True
        |> optional "darkstyle"
            (JD.bool
                |> JD.andThen
                    (\x ->
                        JD.succeed <|
                            if x then
                                DarkStyle

                            else
                                LightStyle
                    )
            )
            LightStyle
        |> optional "selectedRequest" selectedRequestDecoder LoginSelected
        |> optional "username" JD.string ""
        |> optional "accountId" JD.string ""
        |> optional "limit" JD.string ""
        |> optional "accountIds" JD.string ""
        |> optional "showMetadata" JD.bool False
        |> optional "q" JD.string ""
        |> optional "resolve" JD.bool False
        |> optional "following" JD.bool False
        |> optional "groupId" JD.string ""
        |> optional "showReceived" JD.bool True
        |> optional "showEntity" JD.bool False
        |> optional "whichGroups" whichGroupsDecoder Request.MemberGroups
        |> optional "followReblogs" JD.bool True


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


tokenStorageKey : String -> String
tokenStorageKey server =
    pk.token ++ "." ++ server


tokenStorageKeyServer : String -> String
tokenStorageKeyServer key =
    String.dropLeft (String.length pk.token + 1) key


getToken : String -> Cmd Msg
getToken server =
    getLabeled pk.token <| tokenStorageKey server


putToken : String -> Maybe String -> Cmd Msg
putToken server token =
    put (tokenStorageKey server) <|
        case token of
            Nothing ->
                Nothing

            Just tok ->
                Just <| JE.string tok


clear : Cmd Msg
clear =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "mammudeck"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send (getCmdPort WebSocket.moduleName ()) <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , token = "token"
    }



---
--- Special characters
---


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    }

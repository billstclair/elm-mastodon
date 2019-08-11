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


module Main exposing (dollarButtonNameToMsg, dollarButtonNameToSendName, emptyUrl, main, parseQuery, receiveCodeAndState, replaceSendButtonNames, sendButtonName)

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import CustomElement.WriteClipboard as WriteClipboard
import Dialog
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html
    exposing
        ( Attribute
        , Html
        , a
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
        , hidden
        , href
        , id
        , name
        , placeholder
        , readonly
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
import JsonTree exposing (TaggedValue(..))
import List.Extra as LE
import Markdown
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity
    exposing
        ( Account
        , App
        , Authorization
        , Entity(..)
        , Field
        , Focus
        , Privacy(..)
        , Visibility(..)
        )
import Mastodon.Login as Login exposing (FetchAccountOrRedirect(..))
import Mastodon.Request as Request
    exposing
        ( Error(..)
        , FieldUpdate
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
import Regex
import String.Extra as SE
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as QP


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type alias PagingInput =
    { max_id : String
    , since_id : String
    , min_id : String
    , limit : String
    }


emptyPagingInput : PagingInput
emptyPagingInput =
    { max_id = ""
    , since_id = ""
    , min_id = ""
    , limit = ""
    }


type Dialog
    = NoDialog
    | ConfirmDialog String String Msg


type alias FocusInput =
    { x : String, y : String }


type alias Model =
    { token : Maybe String
    , server : String
    , loginServer : Maybe String
    , prettify : Bool
    , style : Style
    , selectedRequest : SelectedRequest
    , username : String
    , accountId : String
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
    , pagingInput : PagingInput
    , local : Bool
    , hashtag : String
    , listId : String
    , smartPaging : Bool
    , showJsonTree : Bool
    , showUpdateCredentials : Bool
    , statusId : String
    , useElmButtonNames : Bool
    , showPostStatus : Bool

    -- Non-persistent below here
    , dialog : Dialog
    , altKeyDown : Bool
    , request : Maybe RawRequest
    , response : Maybe Value
    , entity : Maybe Entity
    , responseTree : Result JD.Error JsonTree.Node
    , responseState : JsonTree.State
    , entityTree : Result JD.Error JsonTree.Node
    , entityState : JsonTree.State
    , selectedKeyPath : JsonTree.KeyPath
    , selectedKeyValue : String
    , clipboardValue : String
    , clipboardCount : Int
    , metadata : Maybe Http.Metadata
    , savedModel : Maybe SavedModel
    , key : Key
    , url : Url
    , hideClientId : Bool
    , tokens : Dict String String
    , account : Maybe Account
    , displayName : String
    , note : String
    , fields : List Field
    , avatarFile : Maybe File
    , headerFile : Maybe File
    , locked : Bool
    , privacy : Privacy
    , sensitive : Bool
    , language : String
    , isAccountFollowed : Bool
    , status : String
    , in_reply_to_id : String
    , quote_of_id : String
    , spoiler_text : String
    , visibility : Maybe Visibility
    , scheduled_at : String
    , idempotencyKey : String
    , mediaFile : Maybe File
    , mediaDescription : String
    , mediaFocus : FocusInput
    , media_ids : String
    , media_id : String

    -- Not input state
    , msg : Maybe String
    , started : Started
    , funnelState : State
    }


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetResponseState JsonTree.State
    | SetEntityState JsonTree.State
    | ExpandAll WhichJson
    | CollapseAll WhichJson
    | SelectTreeNode WhichJson JsonTree.KeyPath
    | SetDialog Dialog
    | OnKeyPress String
    | OnAltKey Bool
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
    | SetSelectedRequest SelectedRequest
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
      -- UI input setters
    | SetUsername String
    | SetAccountId String
    | SetMaxId String
    | SetSinceId String
    | SetMinId String
    | SetLimit String
    | ToggleSmartPaging
    | ToggleShowJsonTree
    | ToggleUseElmButtonNames
    | ToggleShowUpdateCredentials
    | ToggleOnlyMedia
    | TogglePinned
    | ToggleExcludeReplies
    | ToggleExcludeReblogs
    | SetAccountIds String
    | SetDisplayName String
    | SetNote String
    | SetField Int Bool String
    | GetAvatarFile Bool
    | ReceiveAvatarFile File
    | GetHeaderFile Bool
    | ReceiveHeaderFile File
    | SetPrivacy Privacy
    | ToggleLocked
    | ToggleSensitive
    | SetLanguage String
    | SetGroupId String
    | SetStatusId String
    | ToggleShowPostStatus
    | SetStatus String
    | SetInReplyToId String
    | SetInQuoteOfId String
    | SetSpoilerText String
    | SetVisibility (Maybe Visibility)
    | SetScheduledAt String
    | SetIdempotencyKey String
    | GetMediaFile Bool
    | ReceiveMediaFile File
    | SetMediaDescription String
    | SetMediaFocusX String
    | SetMediaFocusY String
    | SetMediaId String
    | SetMediaIds String
    | ToggleLocal
    | SetHashtag String
    | SetListId String
      -- Messages from buttons that send requests over the wire.
      -- See the `update` code for these messages for examples
      -- of using the `Request` module.
      -- If you add a message here, it should also be added to the
      -- `buttonNameAlist` and `dollarButtonNameDict` values near the
      -- bottom of the file.
    | SendGetInstance
    | SendGetTrends
    | SendGetCustomEmojies
    | SendGetVerifyCredentials
    | SendGetAccountByUsername
    | SendGetAccount
    | SendGetFollowers
    | SendGetFollowing
    | SendGetStatuses
    | SendGetRelationships
    | SendGetSearchAccounts
    | SendPostFollow
    | SendPostUnfollow
    | SendPatchUpdateCredentials
    | SendGetGroups
    | SendGetGroup
    | SendGetStatus
    | SendGetStatusContext
    | SendGetStatusCard
    | SendGetStatusRebloggedBy
    | SendGetStatusFavouritedBy
    | SendDeleteStatus
    | SendPostReblogStatus
    | SendPostUnreblogStatus
    | SendPostPinStatus
    | SendPostUnpinStatus
    | SendPostMedia
    | SendPutMedia
    | SendPostStatus
    | SendGetHomeTimeline
    | SendGetConversations
    | SendGetPublicTimeline
    | SendGetTagTimeline
    | SendGetListTimeline
    | SendGetGroupTimeline
      -- All of the `SendXXX` messages receive their results via `ReceiveResponse`.
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


altKeyDecoder : Bool -> Decoder Msg
altKeyDecoder down =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                if key == "Alt" then
                    JD.succeed <| OnAltKey down

                else
                    JD.fail "Not Alt key"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions Process model
        , if model.dialog /= NoDialog then
            Events.onKeyDown keyDecoder

          else
            Sub.batch
                [ Events.onKeyDown <| altKeyDecoder True
                , Events.onKeyUp <| altKeyDecoder False
                ]
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
    , pagingInput = emptyPagingInput
    , local = False
    , hashtag = ""
    , listId = ""
    , smartPaging = False
    , showJsonTree = True
    , showUpdateCredentials = False
    , showPostStatus = False
    , statusId = ""
    , useElmButtonNames = False

    -- Non-persistent below here
    , dialog = NoDialog
    , altKeyDown = False
    , request = Nothing
    , response = Nothing
    , entity = Nothing
    , responseTree = emptyJsonTree
    , responseState = JsonTree.defaultState
    , entityTree = emptyJsonTree
    , entityState = JsonTree.defaultState
    , selectedKeyPath = ""
    , selectedKeyValue = ""
    , clipboardValue = ""
    , clipboardCount = 0
    , metadata = Nothing
    , savedModel = Nothing
    , key = key
    , url = url
    , hideClientId = hideClientId
    , tokens = Dict.empty
    , account = Nothing
    , displayName = ""
    , note = ""
    , fields = []
    , avatarFile = Nothing
    , headerFile = Nothing
    , locked = False
    , privacy = PublicPrivacy
    , sensitive = False
    , language = ""
    , isAccountFollowed = False
    , status = ""
    , in_reply_to_id = ""
    , quote_of_id = ""
    , spoiler_text = ""
    , visibility = Nothing
    , scheduled_at = ""
    , idempotencyKey = ""
    , mediaFile = Nothing
    , mediaDescription = ""
    , mediaFocus = { x = "", y = "" }
    , media_ids = ""
    , media_id = ""
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
        (InstanceRequest Request.GetInstance)


getVerifyCredentials : Model -> Cmd Msg
getVerifyCredentials model =
    case model.loginServer of
        Nothing ->
            Cmd.none

        Just server ->
            case model.token of
                Nothing ->
                    sendRequest (InstanceRequest Request.GetInstance) model
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


emptyJsonTree : Result JD.Error JsonTree.Node
emptyJsonTree =
    JsonTree.parseString "[]"


updateJsonTrees : Model -> Model
updateJsonTrees model =
    let
        parse : Maybe Value -> ( Result JD.Error JsonTree.Node, JsonTree.State )
        parse value =
            case value of
                Nothing ->
                    ( emptyJsonTree, JsonTree.defaultState )

                Just v ->
                    let
                        result =
                            JsonTree.parseValue v
                    in
                    ( result
                    , case result of
                        Err _ ->
                            JsonTree.defaultState

                        Ok root ->
                            JsonTree.collapseToDepth 1 root JsonTree.defaultState
                    )

        ( responseTree, responseState ) =
            parse model.response

        ( entityTree, entityState ) =
            case model.entity of
                Nothing ->
                    parse Nothing

                Just entity ->
                    parse (Just <| ED.encodeEntity entity)
    in
    { model
        | responseTree = responseTree
        , responseState = responseState
        , entityTree = entityTree
        , entityState = entityState
        , selectedKeyPath = ""
        , selectedKeyValue = ""
    }


updatePatchCredentialsInputs : Model -> Model
updatePatchCredentialsInputs mdl =
    let
        model =
            updateJsonTrees mdl
    in
    case model.account of
        Nothing ->
            { model
                | displayName = ""
                , note = ""
                , locked = False
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

                locked =
                    account.locked
            in
            { model
                | displayName = account.display_name
                , note = note
                , fields = extendFields fields
                , locked = locked
                , privacy = privacy
                , sensitive = sensitive
                , language = language
            }


extendFields : List Field -> List Field
extendFields fields =
    List.concat
        [ fields
        , List.repeat (4 - List.length fields) emptyField
        ]


emptyField : Field
emptyField =
    { name = ""
    , value = ""
    , verified_at = Nothing
    }


imageMimeTypes : List String
imageMimeTypes =
    [ "image/jpeg"
    , "image/gif"
    , "image/png"
    ]


nothingIfBlank : String -> Maybe String
nothingIfBlank x =
    if x == "" then
        Nothing

    else
        Just x


pagingInputToPaging : PagingInput -> Maybe Paging
pagingInputToPaging pagingInput =
    let
        { max_id, since_id, min_id, limit } =
            pagingInput

        ilimit =
            Maybe.withDefault -1 <| String.toInt limit
    in
    if max_id == "" && since_id == "" && min_id == "" && ilimit < 0 then
        Nothing

    else
        Just
            { max_id = nothingIfBlank max_id
            , since_id = nothingIfBlank since_id
            , min_id = nothingIfBlank min_id
            , limit =
                if ilimit < 0 then
                    Nothing

                else
                    Just ilimit
            }


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

        SetResponseState state ->
            { model | responseState = state }
                |> withNoCmd

        SetEntityState state ->
            { model | entityState = state }
                |> withNoCmd

        ExpandAll whichJson ->
            (case whichJson of
                ResponseJson ->
                    { model
                        | responseState =
                            JsonTree.expandAll model.responseState
                    }

                DecodedJson ->
                    { model
                        | entityState =
                            JsonTree.expandAll model.entityState
                    }
            )
                |> withNoCmd

        CollapseAll whichJson ->
            (case whichJson of
                ResponseJson ->
                    case model.responseTree of
                        Ok root ->
                            { model
                                | responseState =
                                    JsonTree.collapseToDepth 1 root model.responseState
                            }

                        Err _ ->
                            model

                DecodedJson ->
                    case model.entityTree of
                        Ok root ->
                            { model
                                | entityState =
                                    JsonTree.collapseToDepth 1 root model.entityState
                            }

                        Err _ ->
                            model
            )
                |> withNoCmd

        SelectTreeNode whichJson keyPath ->
            let
                result =
                    case whichJson of
                        ResponseJson ->
                            model.responseTree

                        DecodedJson ->
                            model.entityTree

                value =
                    case result of
                        Err _ ->
                            ""

                        Ok root ->
                            case findKeyPath keyPath root of
                                Nothing ->
                                    ""

                                Just taggedValue ->
                                    taggedValueToString taggedValue
            in
            { model
                | selectedKeyPath = keyPath
                , selectedKeyValue = value
                , clipboardValue = value
                , clipboardCount = model.clipboardCount + 1
            }
                |> withNoCmd

        SetDialog dialog ->
            { model | dialog = dialog }
                |> withCmd
                    (if model.dialog == NoDialog then
                        Cmd.none

                     else
                        Task.attempt (\_ -> Noop) <|
                            Dom.focus cancelButtonId
                    )

        OnKeyPress key ->
            { model
                | dialog =
                    if key == "Escape" then
                        NoDialog

                    else
                        model.dialog
            }
                |> withNoCmd

        OnAltKey isDown ->
            { model
                | altKeyDown = isDown
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
                , entity = Nothing
                , metadata = Nothing
                , selectedKeyPath = ""
                , selectedKeyValue = ""
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

        SetSelectedRequest selectedRequest ->
            { model | selectedRequest = selectedRequest }
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
                                |> updateJsonTrees
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
                            , selectedKeyPath = ""
                            , selectedKeyValue = ""
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
                                                    |> updateJsonTrees
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
                    , selectedKeyPath = ""
                    , selectedKeyValue = ""
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
                sendRequest (InstanceRequest Request.GetInstance) mdl

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
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
                        , msg = Nothing
                    }
                        |> updatePatchCredentialsInputs
                        |> withCmd (putToken server Nothing)

        ClearAll ->
            let
                mdl =
                    { model
                        | dialog = NoDialog
                        , tokens = Dict.empty
                        , server = ""
                        , loginServer = Nothing
                        , account = Nothing
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
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

        SetMaxId maxId ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | max_id = maxId } }
                |> withNoCmd

        SetSinceId sinceId ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | since_id = sinceId } }
                |> withNoCmd

        SetMinId minId ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | min_id = minId } }
                |> withNoCmd

        SetLimit limit ->
            let
                pagingInput =
                    model.pagingInput
            in
            { model | pagingInput = { pagingInput | limit = limit } }
                |> withNoCmd

        ToggleSmartPaging ->
            { model | smartPaging = not model.smartPaging }
                |> withNoCmd

        ToggleShowJsonTree ->
            let
                mdl =
                    if model.showJsonTree then
                        { model
                            | selectedKeyPath = ""
                            , selectedKeyValue = ""
                        }

                    else
                        model
            in
            { mdl | showJsonTree = not model.showJsonTree }
                |> withNoCmd

        ToggleUseElmButtonNames ->
            { model | useElmButtonNames = not model.useElmButtonNames }
                |> withNoCmd

        ToggleShowUpdateCredentials ->
            { model | showUpdateCredentials = not model.showUpdateCredentials }
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

        SetField index updateValue string ->
            let
                mdl =
                    case LE.getAt index model.fields of
                        Nothing ->
                            model

                        Just field ->
                            let
                                fld =
                                    if updateValue then
                                        { field | value = string }

                                    else
                                        { field | name = string }
                            in
                            { model | fields = LE.setAt index fld model.fields }
            in
            mdl |> withNoCmd

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

        ToggleLocked ->
            { model | locked = not model.locked }
                |> withNoCmd

        ToggleSensitive ->
            { model | sensitive = not model.sensitive }
                |> withNoCmd

        SetLanguage language ->
            { model | language = language }
                |> withNoCmd

        SetGroupId groupId ->
            { model | groupId = groupId }
                |> withNoCmd

        SetStatusId statusId ->
            { model | statusId = statusId }
                |> withNoCmd

        ToggleShowPostStatus ->
            { model | showPostStatus = not model.showPostStatus }
                |> withNoCmd

        SetStatus status ->
            { model | status = status }
                |> withNoCmd

        SetInReplyToId in_reply_to_id ->
            { model | in_reply_to_id = in_reply_to_id }
                |> withNoCmd

        SetInQuoteOfId quote_of_id ->
            { model | quote_of_id = quote_of_id }
                |> withNoCmd

        SetSpoilerText spoiler_text ->
            { model | spoiler_text = spoiler_text }
                |> withNoCmd

        SetVisibility visibility ->
            { model | visibility = visibility }
                |> withNoCmd

        SetScheduledAt scheduled_at ->
            { model | scheduled_at = scheduled_at }
                |> withNoCmd

        SetIdempotencyKey idempotencyKey ->
            { model | idempotencyKey = idempotencyKey }
                |> withNoCmd

        GetMediaFile clearMedia ->
            if clearMedia then
                { model | mediaFile = Nothing }
                    |> withNoCmd

            else
                model
                    |> withCmd (File.Select.file imageMimeTypes ReceiveMediaFile)

        ReceiveMediaFile file ->
            { model | mediaFile = Just file }
                |> withNoCmd

        SetMediaDescription mediaDescription ->
            { model | mediaDescription = mediaDescription }
                |> withNoCmd

        SetMediaFocusX x ->
            let
                mediaFocus =
                    model.mediaFocus
            in
            { model | mediaFocus = { mediaFocus | x = x } }
                |> withNoCmd

        SetMediaFocusY y ->
            let
                mediaFocus =
                    model.mediaFocus
            in
            { model | mediaFocus = { mediaFocus | y = y } }
                |> withNoCmd

        SetMediaId media_id ->
            { model | media_id = media_id }
                |> withNoCmd

        SetMediaIds media_ids ->
            { model | media_ids = media_ids }
                |> withNoCmd

        ToggleLocal ->
            { model | local = not model.local }
                |> withNoCmd

        SetHashtag hashtag ->
            { model | hashtag = hashtag }
                |> withNoCmd

        SetListId listId ->
            { model | listId = listId }
                |> withNoCmd

        SendGetInstance ->
            sendRequest (InstanceRequest Request.GetInstance) model

        SendGetTrends ->
            sendRequest (TrendsRequest Request.GetTrends) model

        SendGetCustomEmojies ->
            sendRequest (CustomEmojisRequest Request.GetCustomEmojis) model

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
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendGetFollowing ->
            sendRequest
                (AccountsRequest <|
                    Request.GetFollowing
                        { id = getAccountId model
                        , limit = String.toInt model.pagingInput.limit
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
                        , paging = pagingInputToPaging model.pagingInput
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
                        , limit = String.toInt model.pagingInput.limit
                        , resolve = model.resolve
                        , following = model.following
                        }
                )
                model

        SendPostFollow ->
            sendRequest
                (AccountsRequest <|
                    Request.PostFollow
                        { id = model.accountId
                        , reblogs = model.followReblogs
                        }
                )
                model

        SendPostUnfollow ->
            sendRequest
                (AccountsRequest <|
                    Request.PostUnfollow { id = model.accountId }
                )
                model

        SendPatchUpdateCredentials ->
            sendPatchUpdateCredentials model

        SendGetGroups ->
            sendRequest
                (GroupsRequest <| Request.GetGroups { tab = model.whichGroups })
                model

        SendGetGroup ->
            sendRequest
                (GroupsRequest <| Request.GetGroup { id = model.groupId })
                model

        SendGetStatus ->
            sendRequest
                (StatusesRequest <| Request.GetStatus { id = model.statusId })
                model

        SendGetStatusContext ->
            sendRequest
                (StatusesRequest <| Request.GetStatusContext { id = model.statusId })
                model

        SendGetStatusCard ->
            sendRequest
                (StatusesRequest <| Request.GetStatusCard { id = model.statusId })
                model

        SendGetStatusRebloggedBy ->
            sendRequest
                (StatusesRequest <|
                    Request.GetStatusRebloggedBy
                        { id = model.statusId
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendGetStatusFavouritedBy ->
            sendRequest
                (StatusesRequest <|
                    Request.GetStatusFavouritedBy
                        { id = model.statusId
                        , limit = String.toInt model.pagingInput.limit
                        }
                )
                model

        SendDeleteStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.DeleteStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostReblogStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostReblogStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostUnreblogStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostUnreblogStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostPinStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostPinStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostUnpinStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostUnpinStatus { id = model.statusId }
                )
                { model | dialog = NoDialog }

        SendPostStatus ->
            sendRequest
                (StatusesRequest <|
                    Request.PostStatus
                        { status = nothingIfBlank model.status
                        , in_reply_to_id = nothingIfBlank model.in_reply_to_id
                        , group_id = nothingIfBlank model.groupId
                        , quote_of_id = nothingIfBlank model.quote_of_id
                        , media_ids =
                            splitMediaIds model.media_ids
                        , poll = Nothing --TODO
                        , spoiler_text = nothingIfBlank model.spoiler_text
                        , visibility = model.visibility
                        , scheduled_at = nothingIfBlank model.scheduled_at
                        , language = nothingIfBlank model.language
                        , idempotencyKey = nothingIfBlank model.idempotencyKey
                        }
                )
                { model | dialog = NoDialog }

        SendPostMedia ->
            case ( model.mediaFile, parseFocus model.mediaFocus ) of
                ( Just file, Just focus ) ->
                    sendRequest
                        (MediaAttachmentsRequest <|
                            Request.PostMedia
                                { file = file
                                , description = nothingIfBlank model.mediaDescription
                                , focus = focus
                                }
                        )
                        model

                _ ->
                    { model | msg = Just "Missing media file or malformed focus." }
                        |> withNoCmd

        SendPutMedia ->
            case parseFocus model.mediaFocus of
                Nothing ->
                    model |> withNoCmd

                Just focus ->
                    sendRequest
                        (MediaAttachmentsRequest <|
                            Request.PutMedia
                                { id = model.media_id
                                , description = nothingIfBlank model.mediaDescription
                                , focus = focus
                                }
                        )
                        model

        SendGetHomeTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetHomeTimeline
                        { paging = pagingInputToPaging model.pagingInput }
                )
                model

        SendGetConversations ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetConversations
                        { paging = pagingInputToPaging model.pagingInput }
                )
                model

        SendGetPublicTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetPublicTimeline
                        { local = model.local
                        , only_media = model.onlyMedia
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        SendGetTagTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetTagTimeline
                        { hashtag = model.hashtag
                        , local = model.local
                        , only_media = model.onlyMedia
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        SendGetListTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetListTimeline
                        { list_id = model.listId
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        SendGetGroupTimeline ->
            sendRequest
                (TimelinesRequest <|
                    Request.GetGroupTimeline
                        { group_id = model.groupId
                        , paging = pagingInputToPaging model.pagingInput
                        }
                )
                model

        ReceiveResponse result ->
            receiveResponse result model


taggedValueToString : TaggedValue -> String
taggedValueToString taggedValue =
    case taggedValue of
        TString string ->
            string

        TFloat float ->
            String.fromFloat float

        TBool bool ->
            if bool then
                "true"

            else
                "false"

        TNull ->
            "null"

        _ ->
            ""


findKeyPath : String -> JsonTree.Node -> Maybe JsonTree.TaggedValue
findKeyPath keyPath node =
    if keyPath == node.keyPath then
        Just node.value

    else
        let
            loop nodes =
                case nodes of
                    [] ->
                        Nothing

                    first :: rest ->
                        case findKeyPath keyPath first of
                            Nothing ->
                                loop rest

                            res ->
                                res
        in
        case node.value of
            TList nodes ->
                loop nodes

            TDict dict ->
                Dict.values dict
                    |> loop

            _ ->
                Nothing


sendPatchUpdateCredentials model =
    case model.account of
        Nothing ->
            model |> withNoCmd

        Just account ->
            let
                { displayName, note, avatarFile, headerFile } =
                    model

                { locked, privacy, sensitive, language } =
                    model

                ( maybeSourceUpdate, sourceNote, fields ) =
                    case account.source of
                        -- May need to compare with model fields.
                        -- If so, need to source from them as well.
                        Nothing ->
                            ( Nothing, "", [] )

                        Just source ->
                            let
                                sourceUpdate =
                                    { privacy =
                                        ifNotEqual privacy source.privacy
                                    , sensitive =
                                        ifNotEqual sensitive source.sensitive
                                    , language =
                                        let
                                            lang =
                                                if language == "" then
                                                    Nothing

                                                else
                                                    Just language
                                        in
                                        ifNotEqual lang source.language
                                    }
                            in
                            if
                                (sourceUpdate.privacy == Nothing)
                                    && (sourceUpdate.sensitive == Nothing)
                                    && (sourceUpdate.language == Nothing)
                            then
                                ( Nothing, source.note, source.fields )

                            else
                                ( Just sourceUpdate, source.note, source.fields )

                request =
                    AccountsRequest <|
                        Request.PatchUpdateCredentials
                            { display_name =
                                ifNotEqual displayName account.display_name
                            , note =
                                ifNotEqual note sourceNote
                            , avatar =
                                model.avatarFile
                            , header =
                                model.headerFile
                            , locked =
                                ifNotEqual locked account.locked
                            , source =
                                maybeSourceUpdate
                            , fields_attributes =
                                let
                                    flds =
                                        extendFields fields
                                in
                                if Debug.log "fields" flds == Debug.log "model.fields" model.fields then
                                    Nothing

                                else
                                    Just <|
                                        List.map fieldToUpdate model.fields
                            }
            in
            -- This sends the request even if nothing is updated.
            -- Maybe it shouldn't.
            sendRequest request model


fieldToUpdate : Field -> FieldUpdate
fieldToUpdate { name, value } =
    { name = name
    , value = value
    }


ifNotEqual : a -> a -> Maybe a
ifNotEqual new old =
    if new == old then
        Nothing

    else
        Just new


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
            let
                threeStrikes =
                    ( Nothing, Nothing, Nothing )

                ( msg, ( response, entity, metadata ) ) =
                    case err of
                        BadUrl url ->
                            ( "BadUrl: " ++ url, threeStrikes )

                        Timeout ->
                            ( "Timeout", threeStrikes )

                        NetworkError ->
                            ( "Network Error", threeStrikes )

                        BadStatus meta status ->
                            ( "Bad status: " ++ status, ( Nothing, Nothing, Just meta ) )

                        BadBody meta jderr json ->
                            let
                                res =
                                    case JD.decodeString JD.value json of
                                        Err _ ->
                                            Nothing

                                        Ok v ->
                                            Just v

                                m =
                                    "BadBody: " ++ decodeErrorToString jderr
                            in
                            ( m, ( res, Nothing, Just meta ) )
            in
            { model
                | msg = Just msg
                , response = response
                , entity = entity
                , metadata = metadata
                , selectedKeyPath = ""
                , selectedKeyValue = ""
            }
                |> updateJsonTrees
                |> withNoCmd

        Ok response ->
            let
                mdl =
                    applyResponseSideEffects response model
            in
            { mdl
                | msg = Nothing
                , metadata = Just response.metadata
                , response = Just <| ED.entityValue (Debug.log "entity" response.entity)
                , entity = Just response.entity
            }
                |> updateJsonTrees
                |> withNoCmd


decodeErrorToString : JD.Error -> String
decodeErrorToString error =
    case error of
        JD.Field field err ->
            "Error on field \"" ++ field ++ "\": " ++ decodeErrorToString err

        JD.Index idx err ->
            "Error on index " ++ String.fromInt idx ++ "\": " ++ decodeErrorToString err

        JD.OneOf errors ->
            case errors of
                [] ->
                    "OneOf []"

                err :: _ ->
                    decodeErrorToString err

        JD.Failure fail value ->
            -- TODO: encode some part of `value`.
            fail


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

        AccountsRequest (Request.PatchUpdateCredentials _) ->
            case response.entity of
                AccountEntity account ->
                    { model
                        | account = Just account
                        , avatarFile = Nothing
                        , headerFile = Nothing
                    }
                        |> updatePatchCredentialsInputs

                _ ->
                    model

        AccountsRequest (Request.GetStatuses { paging }) ->
            statusSmartPaging response.entity paging model

        StatusesRequest (Request.PostStatus _) ->
            case response.entity of
                StatusEntity { id } ->
                    { model
                        | statusId = id
                        , status = ""
                        , in_reply_to_id = ""
                        , quote_of_id = ""
                        , spoiler_text = ""
                        , scheduled_at = ""
                        , idempotencyKey = ""
                    }

                _ ->
                    model

        MediaAttachmentsRequest mediaReq ->
            case response.entity of
                AttachmentEntity { id } ->
                    let
                        mdl =
                            case mediaReq of
                                Request.PostMedia _ ->
                                    { model
                                        | media_id = id
                                        , media_ids =
                                            splitMediaIds model.media_ids
                                                |> (\ids ->
                                                        List.concat [ ids, [ id ] ]
                                                   )
                                                |> String.join ","
                                    }

                                _ ->
                                    model
                    in
                    { mdl
                        | mediaFile = Nothing
                        , mediaDescription = ""
                        , mediaFocus = { x = "", y = "" }
                    }

                _ ->
                    model

        TimelinesRequest req ->
            case req of
                Request.GetHomeTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetConversations { paging } ->
                    case response.entity of
                        ConversationListEntity conversations ->
                            smartPaging conversations .id paging model

                        _ ->
                            model

                Request.GetPublicTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetTagTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetListTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetGroupTimeline { paging } ->
                    statusSmartPaging response.entity paging model

        _ ->
            model


splitMediaIds : String -> List String
splitMediaIds string =
    let
        s =
            String.trim string
    in
    if s == "" then
        []

    else
        String.split "," s
            |> List.map String.trim


statusSmartPaging : Entity -> Maybe Paging -> Model -> Model
statusSmartPaging entity paging model =
    case entity of
        StatusListEntity statuses ->
            smartPaging statuses .id paging model

        _ ->
            model


smartPaging : List a -> (a -> String) -> Maybe Paging -> Model -> Model
smartPaging entities getid paging model =
    let
        ( limit, ( max_id, min_id, since_id ) ) =
            case paging of
                Nothing ->
                    -- use the API default here?
                    ( 1, ( "", "", "" ) )

                Just pag ->
                    ( Maybe.withDefault 1 pag.limit
                    , ( Maybe.withDefault "" pag.max_id
                      , Maybe.withDefault "" pag.min_id
                      , Maybe.withDefault "" pag.since_id
                      )
                    )

        pagingInput =
            model.pagingInput
    in
    if not model.smartPaging then
        model

    else if since_id /= "" then
        model

    else if min_id /= "" then
        if max_id /= "" then
            model

        else
            case List.head entities of
                Nothing ->
                    model

                Just e ->
                    { model | pagingInput = { pagingInput | min_id = getid e } }

    else if limit <= List.length entities then
        case LE.last entities of
            Nothing ->
                model

            Just e ->
                { model | pagingInput = { pagingInput | max_id = getid e } }

    else
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
                , selectedKeyPath = ""
                , selectedKeyValue = ""
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
    | InstanceSelected
    | AccountsSelected
    | BlocksSelected
    | GroupsSelected
    | StatusesSelected
    | TimelinesSelected


encodeSelectedRequest : SelectedRequest -> Value
encodeSelectedRequest selectedRequest =
    JE.string <| selectedRequestToString selectedRequest


selectedRequestToString : SelectedRequest -> String
selectedRequestToString selectedRequest =
    case selectedRequest of
        LoginSelected ->
            "login"

        InstanceSelected ->
            "Instance Information"

        AccountsSelected ->
            "AccountsRequest"

        BlocksSelected ->
            "BlocksRequest"

        GroupsSelected ->
            "GroupsRequest"

        StatusesSelected ->
            "StatusesRequest"

        TimelinesSelected ->
            "TimelinesRequest"


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
        "Instance Information" ->
            InstanceSelected

        "AccountsRequest" ->
            AccountsSelected

        "BlocksRequest" ->
            BlocksSelected

        "GroupsRequest" ->
            GroupsSelected

        "StatusesRequest" ->
            StatusesSelected

        "TimelinesRequest" ->
            TimelinesSelected

        _ ->
            LoginSelected


radioNames =
    { selectedRequest = "selectedRequest"
    , privacy = "privacy"
    , visibility = "visibility"
    }


radioButton : { buttonValue : a, radioValue : a, radioName : String, setter : Msg, label : String } -> Html Msg
radioButton { buttonValue, radioValue, radioName, setter, label } =
    span []
        [ input
            [ type_ "radio"
            , name radioName
            , value label
            , checked <| buttonValue == radioValue
            , onCheck <|
                \checked ->
                    if checked then
                        setter

                    else
                        Noop
            ]
            []
        , text label
        ]


selectedRequestRadio : SelectedRequest -> Model -> Html Msg
selectedRequestRadio selectedRequest model =
    let
        string =
            selectedRequestToString selectedRequest
    in
    radioButton
        { buttonValue = selectedRequest
        , radioValue = model.selectedRequest
        , radioName = string
        , setter = SetSelectedRequest selectedRequest
        , label = string
        }


selectedRequestHtml : SelectedRequest -> String -> Model -> (Model -> Html Msg) -> Html Msg
selectedRequestHtml selectedRequest url model body =
    span []
        [ selectedRequestRadio selectedRequest model
        , if url == "" then
            text ""

          else
            span []
                [ text " ("
                , link "docs" url
                , text ")"
                ]
        , br
        , if selectedRequest /= model.selectedRequest then
            text ""

          else
            body model
        ]


privacyRadio : Privacy -> String -> Model -> Html Msg
privacyRadio privacy label model =
    radioButton
        { buttonValue = privacy
        , radioValue = model.privacy
        , radioName = radioNames.privacy
        , setter = SetPrivacy privacy
        , label = label
        }


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


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        ]
        [ b label ]


button : Msg -> String -> Html Msg
button =
    enabledButton True


view : Model -> Document Msg
view model =
    let
        { backgroundColor, color } =
            getStyle model.style
    in
    { title = "Mastodon API Explorer"
    , body =
        [ renderDialog model
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
                                , button Logout "Logout"
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
                        [ selectedRequestHtml LoginSelected
                            "https://docs.joinmastodon.org/api/authentication/"
                            model
                            loginSelectedUI
                        , selectedRequestHtml InstanceSelected
                            "https://docs.joinmastodon.org/api/rest/instances/"
                            model
                            instanceSelectedUI
                        , selectedRequestHtml AccountsSelected
                            "https://docs.joinmastodon.org/api/rest/accounts/"
                            model
                            accountsSelectedUI
                        , selectedRequestHtml BlocksSelected
                            "https://docs.joinmastodon.org/api/rest/blocks/"
                            model
                            blocksSelectedUI
                        , selectedRequestHtml GroupsSelected
                            ""
                            model
                            groupsSelectedUI
                        , selectedRequestHtml StatusesSelected
                            "https://docs.joinmastodon.org/api/rest/statuses/"
                            model
                            statusesSelectedUI
                        , selectedRequestHtml TimelinesSelected
                            "https://docs.joinmastodon.org/api/rest/timelines/"
                            model
                            timelinesSelectedUI
                        ]
                    , p [ style "color" "red" ]
                        [ Maybe.withDefault "" model.msg |> text ]
                    , p []
                        [ span [ hidden <| model.selectedKeyValue == "" ]
                            [ b "selected path: "
                            , text model.selectedKeyPath
                            , case Url.fromString model.selectedKeyValue of
                                Nothing ->
                                    text ""

                                Just _ ->
                                    span []
                                        [ br
                                        , a
                                            [ href model.selectedKeyValue
                                            , target "_blank"
                                            ]
                                            [ text "open URL in new tab" ]
                                        ]
                            , br
                            , textarea
                                [ id "selectedKeyValue"
                                , rows 4
                                , cols 80
                                , readonly True
                                , value model.selectedKeyValue
                                ]
                                []
                            , br
                            ]
                        , WriteClipboard.writeClipboard
                            [ WriteClipboard.write
                                { id =
                                    if model.altKeyDown then
                                        ""

                                    else
                                        "selectedKeyValue"
                                , text = model.clipboardValue
                                , count = model.clipboardCount
                                }
                            ]
                            []
                        ]
                    , checkBox ToggleShowJsonTree model.showJsonTree "show tree"
                    , if model.showJsonTree then
                        text ""

                      else
                        span []
                            [ text " "
                            , checkBox TogglePrettify model.prettify "prettify"
                            ]
                    , text " "
                    , checkBox ToggleUseElmButtonNames model.useElmButtonNames "elm button names"
                    , text " "
                    , button ClearSentReceived "Clear"
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
                            , button ToggleShowMetadata "Hide"
                            ]

                        else
                            [ b "Headers "
                            , button ToggleShowMetadata "Show"
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
                            , button ToggleShowReceived "Hide"
                            ]

                        else
                            [ b "Received "
                            , button ToggleShowReceived "Show"
                            ]
                    , renderJson ResponseJson
                        model
                        model.showReceived
                        model.response
                        Nothing
                    , p [] <|
                        if model.showEntity then
                            [ b "Decoded:"
                            , button ToggleShowEntity "Hide"
                            ]

                        else
                            [ b "Decoded "
                            , button ToggleShowEntity "Show"
                            ]
                    , renderJson DecodedJson
                        model
                        model.showEntity
                        Nothing
                        model.entity
                    , div []
                        [ help model ]
                    , br
                    , p []
                        [ button
                            (SetDialog <|
                                ConfirmDialog
                                    "Do you really want to erase everything?"
                                    "Erase"
                                    ClearAll
                            )
                            "Clear All Persistent State"
                        ]
                    , br
                    , p
                        [ onClick ToggleStyle
                        , style "cursor" "default"
                        ]
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


renderJson : WhichJson -> Model -> Bool -> Maybe Value -> Maybe Entity -> Html Msg
renderJson whichJson model enabled value entity =
    if not enabled then
        text ""

    else
        let
            mv =
                case value of
                    Just val ->
                        Just val

                    Nothing ->
                        case entity of
                            Nothing ->
                                Nothing

                            Just e ->
                                Just <| ED.encodeEntity e
        in
        case mv of
            Nothing ->
                text ""

            Just v ->
                if model.showJsonTree then
                    renderJsonTree whichJson model v

                else
                    pre []
                        [ text <| encodeWrap model.prettify v ]


type WhichJson
    = ResponseJson
    | DecodedJson


jsonTreeColors =
    let
        default =
            JsonTree.defaultColors
    in
    { light = default
    , dark = { default | number = "turquoise" }
    }


renderJsonTree : WhichJson -> Model -> Value -> Html Msg
renderJsonTree whichJson model value =
    let
        ( setter, treeResponse, state ) =
            case whichJson of
                ResponseJson ->
                    ( SetResponseState, model.responseTree, model.responseState )

                DecodedJson ->
                    ( SetEntityState, model.entityTree, model.entityState )

        config =
            { colors =
                if model.style == DarkStyle then
                    jsonTreeColors.dark

                else
                    jsonTreeColors.light

            -- Should copy the text to a <TextArea> on select.
            , onSelect = Just <| SelectTreeNode whichJson
            , toMsg = setter
            }
    in
    case treeResponse of
        Err _ ->
            text ""

        Ok root ->
            span []
                [ button (ExpandAll whichJson) "Expand All"
                , text " "
                , button (CollapseAll whichJson) "Collapse All"
                , br
                , JsonTree.view root config state
                ]


blocksSelectedUI : Model -> Html Msg
blocksSelectedUI model =
    p []
        [ pspace
        , text "Blocks requests go here."
        ]


maybeLabeledTextInput : Maybe String -> Int -> (String -> Msg) -> String -> Html Msg
maybeLabeledTextInput label inputSize wrapper string =
    let
        inputBox =
            input
                [ size inputSize
                , onInput wrapper
                , value string
                ]
                []
    in
    case label of
        Nothing ->
            inputBox

        Just lab ->
            span []
                [ b lab
                , inputBox
                ]


textInput : String -> Int -> (String -> Msg) -> String -> Html Msg
textInput label =
    maybeLabeledTextInput (Just label)


unlabeledTextInput : Int -> (String -> Msg) -> String -> Html Msg
unlabeledTextInput =
    maybeLabeledTextInput Nothing


checkBox : Msg -> Bool -> String -> Html Msg
checkBox msg isChecked label =
    span
        [ onClick msg
        , style "cursor" "default"
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            ]
            []
        , b label
        ]


groupsSelectedUI : Model -> Html Msg
groupsSelectedUI model =
    p []
        [ pspace
        , whichGroupsSelect model
        , text " "
        , sendButton SendGetGroups model
        , br
        , textInput "group id: " 20 SetGroupId model.groupId
        , text " "
        , sendButton SendGetGroup model
        ]


statusesSelectedUI : Model -> Html Msg
statusesSelectedUI model =
    p []
        [ pspace
        , textInput "status id: " 25 SetStatusId model.statusId
        , br
        , sendButton SendGetStatus model
        , text " "
        , sendButton SendGetStatusContext model
        , text " "
        , sendButton SendGetStatusCard model
        , br
        , textInput "limit: " 10 SetLimit model.pagingInput.limit
        , br
        , sendButton SendGetStatusRebloggedBy model
        , text " "
        , sendButton SendGetStatusFavouritedBy model
        , br
        , text "-- writes below here --"
        , br
        , textInput "status id: " 25 SetStatusId model.statusId
        , br
        , button (SetDialog <| ConfirmDialog "Delete Status?" "OK" SendDeleteStatus) <|
            sendButtonName model.useElmButtonNames SendDeleteStatus
        , text " (after confirmation)"
        , br
        , sendButton SendPostReblogStatus model
        , text " "
        , sendButton SendPostUnreblogStatus model
        , br
        , sendButton SendPostPinStatus model
        , text " "
        , sendButton SendPostUnpinStatus model
        , br
        , if not model.showPostStatus then
            let
                buttonName =
                    sendButtonName model.useElmButtonNames
                        SendPostStatus
            in
            button ToggleShowPostStatus <| "Show '" ++ buttonName ++ "'"

          else
            span []
                [ b "Status:"
                , br
                , textarea
                    [ onInput SetStatus
                    , value model.status
                    , rows 4
                    , cols 80
                    ]
                    []
                , br
                , textInput "in reply to id: " 25 SetInReplyToId model.in_reply_to_id
                , br
                , textInput "group id: " 20 SetGroupId model.groupId
                , br
                , textInput "quote of id: " 25 SetInQuoteOfId model.quote_of_id
                , br
                , textInput "spoiler text: " 40 SetSpoilerText model.spoiler_text
                , br
                , b "visibility: "
                , visibilityRadio Nothing model
                , visibilityRadio (Just PublicVisibility) model
                , visibilityRadio (Just UnlistedVisibility) model
                , visibilityRadio (Just PrivateVisibility) model
                , visibilityRadio (Just DirectVisibility) model
                , br
                , textInput "scheduled at: " 24 SetScheduledAt model.scheduled_at
                , br
                , textInput "language: " 2 SetLanguage model.language
                , br
                , textInput "idempotency key: " 20 SetIdempotencyKey model.idempotencyKey
                , br
                , textInput "media ids (a,b,...): " 50 SetMediaIds model.media_ids
                , br
                , button ToggleShowPostStatus "Hide"
                , text " "
                , sendButton SendPostStatus model
                , br
                , text "-- new media --"
                , span []
                    [ text " ("
                    , link "docs" "https://docs.joinmastodon.org/api/rest/media/"
                    , text ")"
                    ]
                , br
                , renderChooseFile "media: " model.mediaFile GetMediaFile
                , br
                , textInput "description: " 50 SetMediaDescription model.mediaDescription
                , br
                , textInput "focus x: " 5 SetMediaFocusX model.mediaFocus.x
                , textInput " y: " 5 SetMediaFocusY model.mediaFocus.y
                , text " "
                , enabledSendButton (isEnabledPostMedia model) SendPostMedia model
                , br
                , textInput "media id: " 20 SetMediaId model.media_id
                , text " "
                , enabledSendButton (isEnabledPutMedia model) SendPutMedia model
                ]
        ]


parseFocus : FocusInput -> Maybe (Maybe Focus)
parseFocus { x, y } =
    if x == "" && y == "" then
        Just Nothing

    else
        case ( String.toFloat x, String.toFloat y ) of
            ( Just fx, Just fy ) ->
                Just <| Just { x = fx, y = fy }

            _ ->
                Nothing


isEnabledPostMedia : Model -> Bool
isEnabledPostMedia model =
    case parseFocus model.mediaFocus of
        Nothing ->
            False

        Just _ ->
            model.mediaFile /= Nothing


isEnabledPutMedia : Model -> Bool
isEnabledPutMedia model =
    case parseFocus model.mediaFocus of
        Nothing ->
            False

        Just _ ->
            model.media_id /= ""


visibilityToString : Maybe Visibility -> String
visibilityToString visibility =
    case visibility of
        Nothing ->
            "none "

        Just vis ->
            case vis of
                PublicVisibility ->
                    "public "

                UnlistedVisibility ->
                    "unlisted "

                PrivateVisibility ->
                    "private "

                DirectVisibility ->
                    "direct "


visibilityRadio : Maybe Visibility -> Model -> Html Msg
visibilityRadio visibility model =
    let
        label =
            visibilityToString visibility
    in
    radioButton
        { buttonValue = visibility
        , radioValue = model.visibility
        , radioName = radioNames.privacy
        , setter = SetVisibility visibility
        , label = label
        }


timelinesSelectedUI : Model -> Html Msg
timelinesSelectedUI model =
    p []
        [ pspace
        , textInput "limit: " 10 SetLimit model.pagingInput.limit
        , text " "
        , checkBox ToggleSmartPaging model.smartPaging "smart paging "
        , br
        , textInput "max id: " 25 SetMaxId model.pagingInput.max_id
        , text " "
        , textInput "min id: " 25 SetMinId model.pagingInput.min_id
        , br
        , textInput "since id: " 25 SetSinceId model.pagingInput.since_id
        , br
        , sendButton SendGetHomeTimeline model
        , text " "
        , sendButton SendGetConversations model
        , br
        , checkBox ToggleLocal model.local "local "
        , checkBox ToggleOnlyMedia model.onlyMedia "media only "
        , sendButton SendGetPublicTimeline model
        , br
        , textInput "hashtag: " 30 SetHashtag model.hashtag
        , text " "
        , sendButton SendGetTagTimeline model
        , br
        , textInput "list id: " 20 SetListId model.listId
        , text " "
        , sendButton SendGetListTimeline model
        , br
        , textInput "group id: " 20 SetGroupId model.groupId
        , text " "
        , sendButton SendGetGroupTimeline model
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
                    , button (getter True) "Clear"
                    , text " "
                    ]
        , button (getter False) "Choose File"
        ]


accountIsVerified : Model -> Bool
accountIsVerified model =
    case model.account of
        Nothing ->
            False

        Just account ->
            account.is_verified


instanceSelectedUI : Model -> Html Msg
instanceSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetInstance model
        , text " "
        , sendButton SendGetTrends model
        , text " "
        , sendButton SendGetCustomEmojies model
        ]


accountsSelectedUI : Model -> Html Msg
accountsSelectedUI model =
    p []
        [ pspace
        , sendButton SendGetVerifyCredentials model
        , br
        , textInput "username: " 30 SetUsername model.username
        , text " "
        , sendButton SendGetAccountByUsername model
        , br
        , textInput "account id: " 20 SetAccountId model.accountId
        , text " "
        , sendButton SendGetAccount model
        , br
        , textInput "limit: " 10 SetLimit model.pagingInput.limit
        , text " "
        , sendButton SendGetFollowers model
        , text " "
        , sendButton SendGetFollowing model
        , br
        , textInput "max id: " 25 SetMaxId model.pagingInput.max_id
        , text " "
        , textInput "min id: " 25 SetMinId model.pagingInput.min_id
        , br
        , textInput "since id: " 25 SetSinceId model.pagingInput.since_id
        , text " "
        , checkBox ToggleSmartPaging model.smartPaging "smart paging "
        , br
        , checkBox ToggleOnlyMedia model.onlyMedia "media only"
        , text " "
        , checkBox TogglePinned model.pinned "pinned"
        , text " "
        , checkBox ToggleExcludeReplies (not model.excludeReplies) "replies"
        , text " "
        , checkBox ToggleExcludeReblogs (not model.excludeReblogs) "reblogs"
        , text " "
        , sendButton SendGetStatuses model
        , br
        , textInput "ids (1,2,...): " 40 SetAccountIds model.accountIds
        , text " "
        , sendButton SendGetRelationships model
        , br
        , textInput "q: " 40 SetQ model.q
        , br
        , textInput "limit: " 10 SetLimit model.pagingInput.limit
        , text " "
        , checkBox ToggleResolve model.resolve " resolve "
        , checkBox ToggleFollowing model.following " following "
        , sendButton SendGetSearchAccounts model
        , br
        , text "-- writes below here --"
        , br
        , textInput "account id: " 20 SetAccountId model.accountId
        , text " "
        , checkBox ToggleFollowReblogs model.followReblogs "reblogs "
        , if model.isAccountFollowed then
            sendButton SendPostUnfollow model

          else
            sendButton SendPostFollow model
        , br
        , if not <| accountIsVerified model then
            text ""

          else
            span []
                [ text "[account is verified. Changing display name will error]"
                , br
                ]
        , if not model.showUpdateCredentials then
            let
                buttonName =
                    sendButtonName model.useElmButtonNames
                        SendPatchUpdateCredentials
            in
            button ToggleShowUpdateCredentials <| "Show '" ++ buttonName ++ "'"

          else
            span []
                [ textInput "display name: " 40 SetDisplayName model.displayName
                , br
                , b "note:"
                , br
                , textarea
                    [ rows 4
                    , cols 50
                    , onInput SetNote
                    , value model.note
                    ]
                    []
                , br
                , renderChooseFile "avatar: " model.avatarFile GetAvatarFile
                , br
                , renderChooseFile "header: " model.headerFile GetHeaderFile
                , br
                , b "privacy: "
                , privacyRadio PublicPrivacy "public " model
                , privacyRadio UnlistedPrivacy "unlisted " model
                , privacyRadio PrivatePrivacy "private " model
                , br
                , checkBox ToggleLocked model.locked "blocked"
                , checkBox ToggleSensitive model.sensitive "sensitive "
                , br
                , textInput "Language: " 2 SetLanguage model.language
                , br
                , b "Profile metadata:"
                , br
                , List.map2 fieldEditorUI
                    model.fields
                    (List.range 0 <| List.length model.fields)
                    |> List.intersperse [ br ]
                    |> List.concat
                    |> span []
                , br
                , button ToggleShowUpdateCredentials "Hide"
                , text " "
                , sendButton SendPatchUpdateCredentials model
                ]
        ]


fieldEditorUI : Field -> Int -> List (Html Msg)
fieldEditorUI field idx =
    [ input
        [ size 20
        , onInput <| SetField idx False
        , value field.name
        , placeholder "name"
        ]
        []
    , text " "
    , input
        [ size 30
        , onInput <| SetField idx True
        , value field.value
        , placeholder "value"
        ]
        []
    ]


loginSelectedUI : Model -> Html Msg
loginSelectedUI model =
    p []
        [ pspace
        , b "server: "
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
        , Html.button
            [ onClick Login
            , disabled <| model.server == ""
            ]
            [ b "Login" ]
        , text " "
        , button SetLoginServer "Set Server"
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
    span []
        [ b "status code: "
        , text <| String.fromInt metadata.statusCode
        , br
        , b "status text: "
        , text metadata.statusText
        , Dict.foldr fold [] metadata.headers
            |> table
                [ style "color" color
                , style "font-size" "14px"
                ]
        ]


cancelButtonId : String
cancelButtonId =
    "cancelButton"


renderDialog : Model -> Html Msg
renderDialog model =
    let
        ( content, okButtonText, msg ) =
            case model.dialog of
                NoDialog ->
                    ( "You should never see this.", "OK", Noop )

                ConfirmDialog cont ok m ->
                    ( cont, ok, m )
    in
    Dialog.render
        { styles = [ ( "width", "40%" ) ]
        , title = "Confirm"
        , content = [ text content ]
        , actionBar =
            [ Html.button
                [ onClick <| SetDialog NoDialog
                , id cancelButtonId
                ]
                [ b "Cancel" ]
            , text <| String.repeat 4 special.nbsp
            , button msg okButtonText
            ]
        }
        (model.dialog /= NoDialog)


dollarButtonNameToSendName : Bool -> String -> String
dollarButtonNameToSendName useElmButtonNames dollarButtonName =
    let
        name =
            dollarButtonNameToMsg dollarButtonName
                |> sendButtonName useElmButtonNames
    in
    if name == unknownButtonName then
        dollarButtonName

    else
        name


replaceSendButtonNames : Bool -> String -> String
replaceSendButtonNames useElmButtonNames string =
    case Regex.fromString "\\$[A-Za-z]+" of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex
                (.match >> dollarButtonNameToSendName useElmButtonNames)
                string


{-| `$Foo` will be replaced in strings below by `replaceSendButtonNames`.
-}
help : Model -> Html Msg
help model =
    Markdown.toHtml [] <|
        replaceSendButtonNames model.useElmButtonNames <|
            case model.selectedRequest of
                InstanceSelected ->
                    """
**Instance Information Help**

The "$GetInstance" button fetches the `Instance` entity for the "Use API for" instance.

The "$GetTrends" button fetches a list of `Tag` entities, containing information about trending hashtags. Some servers always return an empty list for this.

The "$GetCustomEmojies" button fetches a list of `Emoji` entities.
                """

                AccountsSelected ->
                    """
**AccountsRequest Help**

The "$GetVerifyCredentials" button fetches the `Account` entity for the logged-in user.

The "$GetAccountByUsername" button fetches the `Account` entity for the user with the given "username". If the "username" is blank, it uses the username of the logged in user, or sends blank, which will result in an error, if not logged in. If successful, it fills in the "account id" with that user's account ID. This is a Gab-only feature.

The "$GetAccount" button fetches the `Account` entity for the user with the given "account id". If "account id" is blank, uses the id of the logged in user, or sends blank, which will result in an error, if not logged in.

The "$GetFollowers" and "$GetFollowing" buttons fetch lists of the associated `Account` entities. If "limit" is non-blank, it is the maximum number of entities to return.

The "$GetRelationships" button returns a list of `Relationship` entities, one for each of the (comma-separated) "ids".

The "$GetSearchAccounts" button returns a list of `Account` entities that match "q", "resolve", and "following". If "limit" is non-blank, it is the maximum number of entities to return.

The "$PostFollow" / "$PostUnfollow" button either follows or unfollows the account with the given "account id". If following, will show reblogs if and only if "reblogs" is checked. In order to decide whether to follow or unfollow when you click the button, every change to the "account id" causes A `GetRelationships` request to be sent.

Since the parameters to "$PatchUpdateCredentials" take a lot of screen space, that section is initially invisible. Click the "Show '$PatchUpdateCredentials'" button to reveal it.

The "$PatchUpdateCredentials" button changes account profile information from "Display name", "Note", "Avatar", "Header", "Privacy", "Locked", "Sensitive", "Language", and "Profile MetaData". Only the changed fields are sent to the server. The displayed field values are updated whenever a request returns the logged-in user's `Account` entity, e.g. at login or when you press the "$GetVerifyCredentials" button.

The "Hide" button to the left of "$PatchUpdateCredentials" hides that section of the user interface again.
              """

                BlocksSelected ->
                    """
**BlocksRequest Help**

Blocks help goes here.
              """

                GroupsSelected ->
                    """
**GroupsRequest Help**

Groups are a Gab-only feature.

Click "$GetGroups" to get the list of groups in the "Member", "Featured", or "Admin" selector.

Click "$GetGroup" to get information about the group with the given "group id".
            """

                StatusesSelected ->
                    """
**StatusesRequest Help**

Click "$GetStatus" to get the `Status` entity for "status id".

Click "$GetStatusContext" to get the `Context` entity for "status id".

Click "$GetStatusCard" to get the `Card` entity for "status id". A card is generated in the background from the first link in a status. Returns are variously `HTTP 404 status`, `{}`, or `null` when the card doesn't exist.

Click "$GetStatusRebloggedBy" to get a list of `Account` entities that reblogged "status id". "limit" controls the maximum number of results.

Click "$GetStatusFavouritedBy" to get a list of `Account` entities that favorited "status id". "limit" controls the maximum number of results.

Click "$DeleteStatus" to delete "status id", after confirmation.

Click "$PostReblogStatus" to reblog "status id".

Click "$PostUnreblogStatus" to unreblog "status id".

Click "$PostPinStatus" to pin "status id".

Click "$PostUnpinStatus" to unpin "status id".

Click "$PostStatus" to create a new status, using "status", "in reply to id", "group id", "quote of id", "spoiler text", "visibility", "scheduled at", "language", "idempotency key", and "media ids".

If you want to add media to a status, you can do that in the "-- new media --" section. Click "Choose File" to read a media file. Optionally fill in a "description" and ""focus x" and "y". Click "$PostMedia" to send it to the server. The "id" from the received `Attachment` entity will set the "media id" and be added to the comma-separated list of "media ids".

To edit the description or focus of a "media id", fill in one or both of those, and click "$PutMedia".
                """

                TimelinesSelected ->
                    """
**TimelinesRequest Help**

The paging parameters, "limit", "max id", "min id", and "since id" are used for all the timelines requests. The ids are `Status` ids for `GetXXXTimeline` and `Conversation` ids for `GetConversations`.

If "smart paging" is checked, does its best to be smart about changing the paging parameters after a request. If fewer entities are returned than the "limit", and "since id" & "min id" are blank, will set "max id" to the id of the last entity returned. If "min id" is non-blank, and "max id" and "since id" are blank, will set "min id" to the id of the first entity returned.

The "$GetHomeTimeline" button returns the statuses for those you follow.

The "$GetConversations" button returns a list of conversations. I don't know what a conversation is, possibly the PM feature.

The "$GetPublicTimeline" button returns the public timeline, with local posts only if "local" is checked, and with only posts containing media if "media only" is checked.

The "$GetTagTimeline" button returns posts containing the given "hashtag", with local posts only if "local" is checked, and with only posts containing media if "media only" is checked.

The "$GetListTimeline" button returns posts for the list with the given "list id".

The "$GetGroupTimeline" button returns posts for the given "group id".
              """

                _ ->
                    """
**General and Login Help**

Click a radio button to choose the user interface for that section of the API. The names (mostly) match the variant names in the [`Mastodon.Request`](https://github.com/billstclair/elm-mastodon/blob/master/src/Mastodon/Request.elm)`.Request` type.

The "docs" links open, in a new tab, the relevant section of the documentation at docs.joinmastodon.org.

Type a server name, e.g. `mastodon.social`, in the "Server" box at the top of the screen. As soon as you finish typing the name of a real Mastodon server, it will show its `Instance` entity.

The selector to the right of the "Server" input area shows the servers to which you have successfully logged in. Tokens are saved for each, so you don't need to visit the server authentication page again to login to that account. Selecting one of the servers here changes the "Server" input box, and looks up that server's `Instance` entity, but does NOT change the "Use API for" setting. You need to click "Login" or "Set Server" to do that.

The "Login" button logs in to the displayed "Server". This will use a saved token, if there is one, or redirect to the server's authorization page, where you will need to enter your userid/email and password, or, if there are cookies for that in your browser, just click to approve access. Your `Account` entity will be fetched and displayed.

The "Set Server" button uses the "Server" for API requests without logging in. Only a few API requests work without logging in, but this lets you do some exploration of a server without having an account there. The server's `Instance` entity will be fetched and displayed.

The "Logout" button logs out of the "Use API for" server. This will remove it from the server selector and clear its persistent token, requiring you to reauthenticate if you login again.

The "show tree" checkbox controls whether the "Received" and "Decoded" sections are shown as preformatted text or as expandable trees. If trees are shown, clicking on a string, number, or boolean in the tree will copy its path and value to "selected path" and a textarea, which will appear above the "show tree" checkbox. It also copies the value to the clipboard. This makes it easy to paste values, e.g. IDs, and to view them with line-wrap.

If you hold down the "Alt" key ("Option" on Macintosh) while clicking on a tree value, the value will be copied into the selected path and to the clipboard, but the selected path textarea will not be focused or selected, nor will it be scrolled into view. You can use this when you want to paste somewhere other than a field on this page, and don't want the scroll position to change.

If the "selected path" textarea shows a URL, there will be an "open URL in new tab" link which will do that.

The "prettify" checkbox, which is only shown if "show tree" is not checked, controls whether the JSON output lines are wrapped to fit the screen. If selected, then the non-tree output will not necessarily be valid JSON. If NOT selected, then it will, and you can copy and paste it into environments that expect JSON.

The "elm button names" checkbox controls whether the buttons are labelled with HTTP methods and URLs (with the "/api/v1/" prefix elided) or with the names of the Elm type variants.

The "Headers" section, if enabled, shows the headers received from the server for the last request.

The "Received" section, if enabled, shows the JSON received from the server for the last request.

The "Decoded" section, if enabled, shows the decoded JSON received from the server for the last request. If it differs from "Received", there is either a decoder bug, one or more fields are not yet supported, or the received JSON uses defaults for one or more fields.

The "Clear" button on the same line as the "Prettify" checkbox clears the "Sent", "Received", and "Decoded" sections, making this help easier to see.

This page saves state in the JavaScript `localStorage` database. The "Clear All Persistent State" button near the bottom of the page removes all that state, after you click "Erase" on a confirmation dialog.

This page does NOT use cookies, but logging in to a Mastodon/Pleroma server will set cookies for that server, so if you want to switch users on a server, you need to "Logout" here, go to the Mastodon/Pleroma server's web page, logout there, and "Login" again here.

The "Dark Mode" checkbox toggles between light and dark mode.

If you look at the [code for this page](https://github.com/billstclair/elm-mastodon/blob/master/example/src/Main.elm), and search for `SendGetVerifyCredentials`, you'll see examples of using the `Mastodon.Request` module.
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
    , pagingInput : PagingInput
    , local : Bool
    , hashtag : String
    , listId : String
    , smartPaging : Bool
    , showJsonTree : Bool
    , showUpdateCredentials : Bool
    , statusId : String
    , useElmButtonNames : Bool
    , showPostStatus : Bool
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
    , onlyMedia = model.onlyMedia
    , pinned = model.pinned
    , excludeReplies = model.excludeReplies
    , excludeReblogs = model.excludeReblogs
    , pagingInput = model.pagingInput
    , local = model.local
    , hashtag = model.hashtag
    , listId = model.listId
    , smartPaging = model.smartPaging
    , showJsonTree = model.showJsonTree
    , showUpdateCredentials = model.showUpdateCredentials
    , statusId = model.statusId
    , useElmButtonNames = model.useElmButtonNames
    , showPostStatus = model.showPostStatus
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
        , onlyMedia = savedModel.onlyMedia
        , pinned = savedModel.pinned
        , excludeReplies = savedModel.excludeReplies
        , excludeReblogs = savedModel.excludeReblogs
        , pagingInput = savedModel.pagingInput
        , local = savedModel.local
        , hashtag = savedModel.hashtag
        , listId = savedModel.listId
        , smartPaging = savedModel.smartPaging
        , showJsonTree = savedModel.showJsonTree
        , showUpdateCredentials = savedModel.showUpdateCredentials
        , statusId = savedModel.statusId
        , useElmButtonNames = savedModel.useElmButtonNames
        , showPostStatus = savedModel.showPostStatus
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


{-| Encode `Paging` into `Value`
-}
encodePagingInput : PagingInput -> Value
encodePagingInput { max_id, since_id, min_id, limit } =
    JE.object
        [ ( "max_id", JE.string max_id )
        , ( "since_id", JE.string since_id )
        , ( "min_id", JE.string min_id )
        , ( "limit", JE.string limit )
        ]


{-| Decode `PagingInput`
-}
pagingInputDecoder : Decoder PagingInput
pagingInputDecoder =
    JD.succeed PagingInput
        |> required "max_id" JD.string
        |> required "since_id" JD.string
        |> required "min_id" JD.string
        |> required "limit" JD.string


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
        , ( "onlyMedia", JE.bool savedModel.onlyMedia )
        , ( "pinned", JE.bool savedModel.pinned )
        , ( "excludeReplies", JE.bool savedModel.excludeReplies )
        , ( "excludeReblogs", JE.bool savedModel.excludeReblogs )
        , ( "pagingInput", encodePagingInput savedModel.pagingInput )
        , ( "local", JE.bool savedModel.local )
        , ( "hashtag", JE.string savedModel.hashtag )
        , ( "listId", JE.string savedModel.listId )
        , ( "smartPaging", JE.bool savedModel.smartPaging )
        , ( "showJsonTree", JE.bool savedModel.showJsonTree )
        , ( "showUpdateCredentials", JE.bool savedModel.showUpdateCredentials )
        , ( "statusId", JE.string savedModel.statusId )
        , ( "useElmButtonNames", JE.bool savedModel.useElmButtonNames )
        , ( "showPostStatus", JE.bool savedModel.showPostStatus )
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
        |> optional "onlyMedia" JD.bool False
        |> optional "pinned" JD.bool False
        |> optional "excludeReplies" JD.bool False
        |> optional "excludeReblogs" JD.bool False
        |> optional "pagingInput" pagingInputDecoder emptyPagingInput
        |> optional "local" JD.bool False
        |> optional "hashtag" JD.string ""
        |> optional "listId" JD.string ""
        |> optional "smartPaging" JD.bool False
        |> optional "showJsonTree" JD.bool True
        |> optional "showUpdateCredentials" JD.bool False
        |> optional "statusId" JD.string ""
        |> optional "useElmButtonNames" JD.bool False
        |> optional "showPostStatus" JD.bool False


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



---
--- Button names
---


dollarButtonNameDict : Dict String Msg
dollarButtonNameDict =
    Dict.fromList
        [ ( "GetGroups", SendGetGroups )
        , ( "GetGroup", SendGetGroup )
        , ( "GetStatus", SendGetStatus )
        , ( "GetStatusContext", SendGetStatusContext )
        , ( "GetStatusCard", SendGetStatusCard )
        , ( "GetStatusRebloggedBy", SendGetStatusRebloggedBy )
        , ( "GetStatusFavouritedBy", SendGetStatusFavouritedBy )
        , ( "DeleteStatus", SendDeleteStatus )
        , ( "PostReblogStatus", SendPostReblogStatus )
        , ( "PostUnreblogStatus", SendPostUnreblogStatus )
        , ( "PostPinStatus", SendPostPinStatus )
        , ( "PostUnpinStatus", SendPostUnpinStatus )
        , ( "PostStatus", SendPostStatus )
        , ( "PostMedia", SendPostMedia )
        , ( "PutMedia", SendPutMedia )
        , ( "PostStatus", SendPostStatus )
        , ( "GetConversations", SendGetConversations )
        , ( "GetPublicTimeline", SendGetPublicTimeline )
        , ( "GetTagTimeline", SendGetTagTimeline )
        , ( "GetListTimeline", SendGetListTimeline )
        , ( "GetGroupTimeline", SendGetGroupTimeline )
        , ( "GetInstance", SendGetInstance )
        , ( "GetTrends", SendGetTrends )
        , ( "GetCustomEmojies", SendGetCustomEmojies )
        , ( "GetVerifyCredentials", SendGetVerifyCredentials )
        , ( "GetAccountByUsername", SendGetAccountByUsername )
        , ( "GetAccount", SendGetAccount )
        , ( "GetFollowers", SendGetFollowers )
        , ( "GetFollowing", SendGetFollowing )
        , ( "GetStatuses", SendGetStatuses )
        , ( "GetRelationships", SendGetRelationships )
        , ( "GetSearchAccounts", SendGetSearchAccounts )
        , ( "PostFollow", SendPostFollow )
        , ( "PostUnfollow", SendPostUnfollow )
        , ( "PatchUpdateCredentials", SendPatchUpdateCredentials )
        ]


dollarButtonNameToMsg : String -> Msg
dollarButtonNameToMsg dollarButtonName =
    Dict.get (String.dropLeft 1 dollarButtonName) dollarButtonNameDict
        |> Maybe.withDefault Noop


buttonNameAlist : List ( Msg, ( String, String ) )
buttonNameAlist =
    [ ( SendGetGroups, ( "GetGroups", "GET groups" ) )
    , ( SendGetGroup, ( "GetGroup", "GET groups/:id" ) )
    , ( SendGetStatus, ( "GetStatus", "GET statuses/:id" ) )
    , ( SendGetStatusContext, ( "GetStatusContext", "GET statuses/:id/context" ) )
    , ( SendGetStatusCard, ( "GetStatusCard", "GET statuses/:id/card" ) )
    , ( SendGetStatusRebloggedBy, ( "GetStatusRebloggedBy", "GET statuses/:id/reblogged_by" ) )
    , ( SendGetStatusFavouritedBy, ( "GetStatusFavouritedBy", "GET statuses/:id/favourited_by" ) )
    , ( SendDeleteStatus, ( "DeleteStatus", "DELETE statuses/:id" ) )
    , ( SendPostReblogStatus, ( "PostReblogStatus", "POST statuses/:id/reblog" ) )
    , ( SendPostUnreblogStatus, ( "PostUnreblogStatus", "POST statuses/:id/unreblog" ) )
    , ( SendPostPinStatus, ( "PostPinStatus", "POST statuses/:id/pin" ) )
    , ( SendPostUnpinStatus, ( "PostUnpinStatus", "POST statuses/:id/unpin" ) )
    , ( SendPostStatus, ( "PostStatus", "POST statuses" ) )
    , ( SendPostMedia, ( "PostMedia", "POST media" ) )
    , ( SendPutMedia, ( "PutMedia", "PUT media" ) )
    , ( SendGetHomeTimeline, ( "GetHomeTimeline", "GET timelines/home" ) )
    , ( SendGetConversations, ( "GetConversations", "GET conversations" ) )
    , ( SendGetPublicTimeline, ( "GetPublicTimeline", "GET timelines/public" ) )
    , ( SendGetTagTimeline, ( "GetTagTimeline", "GET timelines/tag/:tag" ) )
    , ( SendGetListTimeline, ( "GetListTimeline", "GET timeslines/list/:id" ) )
    , ( SendGetGroupTimeline, ( "GetGroupTimeline", "GET timelines/group/:id" ) )
    , ( SendGetInstance, ( "GetInstance", "GET instance" ) )
    , ( SendGetTrends, ( "GetTrends", "GET trends" ) )
    , ( SendGetCustomEmojies, ( "GetCustomEmojies", "GET custom_emojis" ) )
    , ( SendGetVerifyCredentials, ( "GetVerifyCredentials", "GET accounts/verify_credentials" ) )
    , ( SendGetAccountByUsername, ( "GetAccountByUsername", "GET account_by_username/:username" ) )
    , ( SendGetAccount, ( "GetAccount", "GET accounts/:id" ) )
    , ( SendGetFollowers, ( "GetFollowers", "GET accounts/:id/followers" ) )
    , ( SendGetFollowing, ( "GetFollowing", "GET accounts/:id/following" ) )
    , ( SendGetStatuses, ( "GetStatuses", "GET accounts/:id/statuses" ) )
    , ( SendGetRelationships, ( "GetRelationships", "GET accounts/relationships" ) )
    , ( SendGetSearchAccounts, ( "GetSearchAccounts", "GET accounts/search" ) )
    , ( SendPostFollow, ( "PostFollow", "POST accounts/:id/follow" ) )
    , ( SendPostUnfollow, ( "PostUnfollow", "POST accounts/:id/unfollow" ) )
    , ( SendPatchUpdateCredentials, ( "PatchUpdateCredentials", "PATCH accounts/update_credentials" ) )
    ]


tupleToButtonName : Bool -> ( String, String ) -> String
tupleToButtonName useElmButtonNames ( elm, http ) =
    if useElmButtonNames then
        elm

    else
        http


unknownButtonName : String
unknownButtonName =
    "**Unknown**"


sendButtonName : Bool -> Msg -> String
sendButtonName useElmButtonNames msg =
    case LE.find (\( m, _ ) -> m == msg) buttonNameAlist of
        Nothing ->
            unknownButtonName

        Just ( _, tuple ) ->
            tupleToButtonName useElmButtonNames tuple


enabledSendButton : Bool -> Msg -> Model -> Html Msg
enabledSendButton enabled msg model =
    enabledButton enabled msg <| sendButtonName model.useElmButtonNames msg


sendButton : Msg -> Model -> Html Msg
sendButton =
    enabledSendButton True

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
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
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
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( checked
        , disabled
        , href
        , name
        , placeholder
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Markdown
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity exposing (Account, App, Authorization, Entity(..))
import Mastodon.Login as Login exposing (FetchAccountOrRedirect(..))
import Mastodon.Request as Request
    exposing
        ( Error(..)
        , RawRequest
        , Request(..)
        , Response
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
    { server : String
    , loginServer : Maybe String
    , prettify : Bool
    , style : Style
    , selectedRequest : SelectedRequest
    , username : String
    , accountId : String
    , limit : String
    , accountIds : String

    -- Non-persistent below here
    , token : Maybe String
    , request : Maybe RawRequest
    , response : Maybe Value
    , savedModel : Maybe SavedModel
    , key : Key
    , url : Url
    , hideClientId : Bool
    , tokens : Dict String String
    , account : Maybe Account
    , msg : Maybe String
    , started : Started
    , funnelState : State
    }


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetServer String
    | ClearSentReceived
    | TogglePrettify
    | ToggleStyle
    | SetSelectedRequest SelectedRequest Bool
    | ReceiveRedirect (Result ( String, Error ) ( String, App, Cmd Msg ))
    | ReceiveAuthorization (Result ( String, Error ) ( String, Authorization, Account ))
    | ReceiveInstance (Result Error Response)
    | ReceiveFetchAccount (Result ( String, Error ) ( String, String, Account ))
    | ReceiveAccount (Result Error Response)
    | Process Value
    | Login
    | Logout
    | ClearAll
    | SendVerifyCredentials
    | SetUsername String
    | SetAccountId String
    | SetLimit String
    | SetAccountIds String
    | SendGetAccountByUsername
    | SendGetAccount
    | SendGetFollowers
    | SendGetFollowing
    | SendGetRelationships
    | ReceiveResponse (Result Error Response)
    | SetLoginServer


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions Process model
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
    , prettify = True
    , style = LightStyle
    , selectedRequest = LoginSelected
    , username = ""
    , accountId = ""
    , limit = ""
    , accountIds = ""

    -- Non-persistent below here
    , request = Nothing
    , response = Nothing
    , savedModel = Nothing
    , key = key
    , url = url
    , hideClientId = hideClientId
    , tokens = Dict.empty
    , loginServer = Nothing
    , account = Nothing
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
                    Request.serverRequest (\_ -> ReceiveAccount)
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
        OnUrlRequest _ ->
            model |> withNoCmd

        OnUrlChange _ ->
            model |> withNoCmd

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

        ClearSentReceived ->
            { model
                | request = Nothing
                , response = Nothing
            }
                |> withNoCmd

        TogglePrettify ->
            { model | prettify = not model.prettify }
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
                    in
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
                    }
                        |> withCmd cmd

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
                    in
                    { model
                        | msg = Nothing
                        , server = loginServer
                        , loginServer = Just loginServer
                        , token = Just token
                        , account = Just account
                        , request = Just request
                        , response = Just account.v
                    }
                        |> withNoCmd

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
                                , response = Just instance.v
                            }
                                |> withNoCmd

                        _ ->
                            model |> withNoCmd

        ReceiveAccount result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok response ->
                    case response.entity of
                        AccountEntity account ->
                            { model
                                | msg = Nothing
                                , request = Just response.rawRequest
                                , response = Just account.v
                                , account = Just account
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
                        , msg = Nothing
                    }
                        |> withCmd (putToken server Nothing)

        ClearAll ->
            let
                mdl =
                    { model
                        | tokens = Dict.empty
                        , server = ""
                        , loginServer = Nothing
                        , account = Nothing
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , msg = Nothing
                    }
            in
            { mdl | savedModel = Just <| modelToSavedModel mdl }
                |> withCmd clear

        SendVerifyCredentials ->
            sendRequest (AccountsRequest Request.GetVerifyCredentials) model

        SetUsername username ->
            { model | username = username }
                |> withNoCmd

        SetAccountId accountId ->
            { model | accountId = accountId }
                |> withNoCmd

        SetLimit limit ->
            { model | limit = limit }
                |> withNoCmd

        SetAccountIds accountIds ->
            { model | accountIds = accountIds }
                |> withNoCmd

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

        ReceiveResponse result ->
            receiveResponse result model

        SetLoginServer ->
            if model.server == "" then
                { model
                    | msg = Nothing
                    , loginServer = Nothing
                    , request = Nothing
                    , response = Nothing
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
                in
                sendRequest InstanceRequest mdl


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
                , request = Nothing
                , response = Nothing
            }
                |> withNoCmd

        Ok response ->
            let
                mdl =
                    applyResponseSideEffects response model
            in
            { mdl
                | msg = Nothing
                , request = Just response.rawRequest
                , response = Just <| ED.entityValue response.entity
            }
                |> withNoCmd


applyResponseSideEffects : Response -> Model -> Model
applyResponseSideEffects response model =
    case response.request of
        AccountsRequest (Request.GetAccountByUsername _) ->
            case response.entity of
                AccountEntity { id } ->
                    { model | accountId = id }

                _ ->
                    model

        _ ->
            model


sendRequest : Request -> Model -> ( Model, Cmd Msg )
sendRequest request model =
    case model.loginServer of
        Nothing ->
            model |> withNoCmd

        Just server ->
            model
                |> withCmd
                    (Request.serverRequest (\_ -> ReceiveResponse)
                        []
                        { server = server
                        , token = model.token
                        }
                        ()
                        request
                    )


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


encodeSelectedRequest : SelectedRequest -> Value
encodeSelectedRequest selectedRequest =
    JE.string <| selectedRequestToString selectedRequest


selectedRequestToString : SelectedRequest -> String
selectedRequestToString selectedRequest =
    case selectedRequest of
        LoginSelected ->
            "login"

        AccountsSelected ->
            "accounts"

        BlocksSelected ->
            "blocks"


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
        "accounts" ->
            AccountsSelected

        "block" ->
            BlocksSelected

        _ ->
            LoginSelected


selectedRequestRadioName : String
selectedRequestRadioName =
    "selectedRequest"


selectedRequestRadio : SelectedRequest -> Model -> Html Msg
selectedRequestRadio selectedRequest model =
    let
        string =
            selectedRequestToString selectedRequest
    in
    span [ onClick <| SetSelectedRequest selectedRequest True ]
        [ input
            [ type_ "radio"
            , name selectedRequestRadioName
            , value string
            , checked <| model.selectedRequest == selectedRequest
            , onCheck (SetSelectedRequest selectedRequest)
            ]
            []
        , text " "
        , text string
        ]


selectedRequestHtml : SelectedRequest -> Model -> (() -> Html Msg) -> Html Msg
selectedRequestHtml selectedRequest model body =
    span []
        [ selectedRequestRadio selectedRequest model
        , br
        , if selectedRequest /= model.selectedRequest then
            text ""

          else
            body ()
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


view : Model -> Document Msg
view model =
    let
        { backgroundColor, color } =
            getStyle model.style
    in
    { title = "Mastodon API Explorer"
    , body =
        [ div
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
                        [ selectedRequestHtml LoginSelected
                            model
                            (\_ ->
                                p []
                                    [ b "Server: "
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
                            )
                        , selectedRequestHtml AccountsSelected
                            model
                            (\_ ->
                                p []
                                    [ button [ onClick SendVerifyCredentials ]
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
                                    , b "ids (1,2,...): "
                                    , input
                                        [ size 40
                                        , onInput SetAccountIds
                                        , value model.accountIds
                                        ]
                                        []
                                    , br
                                    , button [ onClick SendGetRelationships ]
                                        [ text "GetRelationships" ]
                                    ]
                            )
                        , selectedRequestHtml BlocksSelected
                            model
                            (\_ ->
                                p []
                                    [ text "Blocks requests go here." ]
                            )
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
                            , b " Prettify"
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

                                    -- Need a jsonBody property
                                    ]
                        ]
                    , p []
                        [ b "Received:" ]
                    , pre []
                        [ case model.response of
                            Nothing ->
                                text ""

                            Just value ->
                                text <|
                                    encodeWrap model.prettify value
                        ]
                    , p [ onClick ToggleStyle ]
                        [ input
                            [ type_ "checkbox"
                            , checked <| model.style == DarkStyle
                            ]
                            []
                        , b " Dark Mode"
                        ]
                    , div []
                        [ help model ]
                    , br
                    , p []
                        [ button [ onClick ClearAll ]
                            [ text "Clear All Persistent State" ]
                        ]
                    , br
                    , p []
                        [ text <| "Copyright " ++ special.copyright ++ " 2019, Bill St. Clair"
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


help : Model -> Html Msg
help model =
    Markdown.toHtml [] <|
        if model.selectedRequest == AccountsSelected then
            """
**Accounts Help**

The "GetVerifyCredentials" button fetches the `Account` entity for the logged-in user.

The "GetAccountByUsername" button fetches the `Account` entity for the user with the given "username". If the "username" is blank, it uses the username of the logged in user, or sends blank, which will result in an error, if not logged in. If successful, it fills in the "id" with that user's account ID.

The "GetAccount" button fetches the `Account` entity for the user with the given "id". If "id" is blank, uses the id of the logged in user, or sends blank, which will result in an error, if not logged in.

The "GetFollowers" and "GetFollowing" buttons fetch lists of the associated `Account` entities. If "limit" is non-blank, it should be the maximum number of entities to return.

The "GetRelationships" button returns a list of `Relationship` entities, one for each of the (comma-separated) "ids".
            """

        else if model.selectedRequest == BlocksSelected then
            """
**Blocks Help**

Blocks help goes here.
            """

        else
            """
**Login Help**

Click a radio button to choose the user interface for that section of the API.

Type a server name, e.g. `mastodon.social`, in the "Server" box at the top of the screen. As soon as you finish typing the name of a real Mastodon server, it will show its `Instance` entity.

The selector to the right of the "Server" input area shows the servers into which you have successfully logged in. Tokens are saved for each, so you don't need to visit the server authentication page again to login to that account. Selecting one of the servers here changes the "Server" input box, and looks up that server's `Instance` entity, but does NOT change the "Use API for" setting.

The "Login" button logs in to the displayed "Server". This will redirect to the server's authorization page, where you will need to enter your userid/email and password, or, if there are cookies for that in your browser, just click to approve access. Your `Account` entity will be fetched and displayed.

The "Set Server" button uses the "Server" for API requests without logging in. Only a few API request work without logging in, but this lets you do some exploration of a server without having an account there. The server's `Instance` entity will be fetched and displayed.

The selector to the right of the "Server" type-in box shows all the servers that you have successfully logged in to. Choose one to copy its name into the "Server" box and fetch its `Instance` entity. Click the "Login" button to fetch your `Account` entity there. No authentication will be necessary, since the access token is persistent.

The "Logout" button logs out of the "Use API for" server. This will remove it from the server selector and clear its persistent token, requiring you to reauthenticate if you login again.

The "Prettify" checkbox controls whether the JSON output lines are wrapped to fit the screen. If selected, then the output will not necessarily be valid JSON. If NOT selected, then it will, and you can copy and paste it into environments that expect JSON.

The "Clear" button on the same line as the "Prettify" checkbox clears the "Sent" and "Received" sections, making this help easier to see.

The "Dark Mode" checkbox toggles between light and dark mode.

The "Clear All Persistent State" button at the bottom of the page does that, with no confirmation. Be careful. This is a sharp tool.
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


encodeWrap : Bool -> Value -> String
encodeWrap prettify value =
    JE.encode 2 value
        |> (if prettify then
                wrapJsonLines 80

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
    }


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

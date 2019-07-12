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
        , text
        )
import Html.Attributes
    exposing
        ( href
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Mastodon.EncodeDecode as ED
import Mastodon.Entity as Entity exposing (Account, Authorization, Entity(..))
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
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as QP


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type alias Model =
    { authorization : Maybe Authorization
    , provider : String

    -- Non-persistent below here
    , request : Maybe RawRequest
    , response : Maybe Value
    , savedModel : Maybe SavedModel
    , key : Key
    , url : Url
    , hideClientId : Bool
    , account : Maybe Account
    , state : Maybe String
    , msg : Maybe String
    , started : Started
    , funnelState : State
    }


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetProvider String
    | ReceiveRedirect (Result Error (Cmd Msg))
    | ReceiveAuthorization (Result Error ( Authorization, Account ))
    | ReceiveInstance (Result Error Response)
    | Login
    | Process Value


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
    { authorization = Nothing
    , provider = "mastodon.social"

    -- Non-persistent below here
    , request = Nothing
    , response = Nothing
    , savedModel = Nothing
    , key = key
    , url = url
    , hideClientId = hideClientId
    , account = Nothing
    , state = state
    , msg = msg
    , started = NotStarted
    , funnelState = initialFunnelState
    }
        -- As soon as the localStorage module reports in,
        -- we'll load the saved model,
        -- and then all the saved authorizations.
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
                get pk.model

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            handleGetResponse key value mdl

        _ ->
            mdl |> withCmd cmd


getInstance : Model -> Cmd Msg
getInstance model =
    let
        serverInfo =
            { server = model.provider
            , authorization = Nothing
            }
    in
    Request.serverRequest (\id -> ReceiveInstance)
        []
        serverInfo
        ()
        InstanceRequest


getVerifyCredentials : Model -> Cmd Msg
getVerifyCredentials model =
    Cmd.none


handleGetResponse : String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse key maybeValue model =
    if key == pk.model then
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
                                (getVerifyCredentials mdl)

    else
        model
            |> withNoCmd


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
            ( model
            , Cmd.none
            )

        OnUrlChange _ ->
            ( model
            , Cmd.none
            )

        SetProvider provider ->
            let
                mdl =
                    { model | provider = provider }
            in
            mdl
                |> withCmd
                    (if String.contains "." provider then
                        getInstance mdl

                     else
                        Cmd.none
                    )

        Login ->
            let
                url =
                    model.url

                sau =
                    { client_name = "elm-mastodon"
                    , server = model.provider
                    , applicationUri =
                        { url
                            | fragment = Nothing
                            , query = Nothing
                        }
                            |> Url.toString
                    }
            in
            case Login.loginTask sau Nothing of
                Redirect task ->
                    ( model, Task.attempt ReceiveRedirect task )

                _ ->
                    ( model, Cmd.none )

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    -- Maybe I should display an error here,
                    -- but I don't think it will ever happen.
                    model |> withNoCmd

                Ok res ->
                    res

        ReceiveRedirect result ->
            case result of
                Err err ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok cmd ->
                    ( { model | msg = Nothing }
                    , cmd
                    )

        ReceiveAuthorization result ->
            case result of
                Err err ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( authorization, account ) ->
                    ( { model
                        | authorization = Debug.log "authorization" <| Just authorization
                        , account = Just account
                      }
                    , Cmd.none
                    )

        ReceiveInstance result ->
            case result of
                Err _ ->
                    -- We'll get lots of errors, for non-existant domains
                    model |> withNoCmd

                Ok res ->
                    case res.entity of
                        InstanceEntity instance ->
                            { model
                                | request = Just res.rawRequest
                                , response = Just instance.v
                            }
                                |> withNoCmd

                        _ ->
                            model |> withNoCmd


providerOption : String -> String -> Html Msg
providerOption currentProvider provider =
    option
        [ value provider
        , selected <| provider == currentProvider
        ]
        [ text provider ]


providerSelect : Model -> Html Msg
providerSelect model =
    input
        [ size 30
        , onInput SetProvider
        , value model.provider
        ]
        []


b : String -> Html msg
b string =
    Html.b [] [ text string ]


view : Model -> Document Msg
view model =
    { title = "Mastodon API Explorer"
    , body =
        [ div
            [ style "margin-left" "3em"
            ]
            [ h2 [] [ text "Mastodon API Explorer" ]
            , p []
                [ text "Provider: "
                , providerSelect model
                ]
            , p []
                [ button [ onClick Login ]
                    [ text "Login" ]
                ]
            , p [ style "color" "red" ]
                [ Maybe.withDefault "" model.msg |> text ]
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
            , p [] [ b "Received:" ]
            , pre []
                [ case model.response of
                    Nothing ->
                        text ""

                    Just v ->
                        text <| JE.encode 2 v
                ]
            , p []
                [ text "Source code: "
                , a
                    [ href "https://github.com/billstclair/elm-mastodon"
                    , target "_blank"
                    ]
                    [ text "GitHub" ]
                ]
            ]
        ]
    }



---
--- Persistence
---


type alias SavedModel =
    { authorization : Maybe Authorization
    , provider : String
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { authorization = model.authorization
    , provider = model.provider
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | authorization = savedModel.authorization
        , provider = savedModel.provider
    }


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "authorization"
          , ED.encodeMaybe ED.encodeAuthorization savedModel.authorization
          )
        , ( "provider", JE.string savedModel.provider )
        ]


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "authorization" (JD.nullable ED.authorizationDecoder)
        |> required "provider" JD.string


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key)


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
    , server = "server"
    }

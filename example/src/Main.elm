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


module Main exposing (Model, Msg(..), getUser, init, lookupProvider, main, providerOption, providerSelect, update, userAgentHeader, view)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h2
        , option
        , p
        , pre
        , select
        , text
        )
import Html.Attributes
    exposing
        ( selected
        , style
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import OAuthMiddleware
    exposing
        ( Authorization
        , ResponseToken
        , TokenAuthorization
        , TokenState(..)
        , authorize
        , getAuthorizations
        , locationToRedirectBackUri
        , receiveTokenAndState
        , use
        )
import OAuthMiddleware.EncodeDecode
    exposing
        ( authorizationsEncoder
        , responseTokenEncoder
        )
import Url exposing (Url)


type alias Model =
    { key : Key
    , authorization : Maybe Authorization
    , token : Maybe ResponseToken
    , state : Maybe String
    , msg : Maybe String
    , replyType : String
    , reply : Maybe Value
    , redirectBackUri : String
    , provider : String
    , authorizations : Dict String Authorization
    , tokenAuthorization : Maybe TokenAuthorization
    , getUserApi : Maybe String
    }


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ReceiveAuthorizations (Result Http.Error (List Authorization))
    | ChangeProvider String
    | Login
    | GetUser
    | ReceiveUser (Result Http.Error Value)


{-| GitHub requires the "User-Agent" header.
-}
userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Xossbow"


getUserApi : String
getUserApi =
    "accounts/verify_credentials"


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( token, state, msg ) =
            case receiveTokenAndState url of
                TokenAndState tok stat ->
                    ( Just tok, stat, Nothing )

                TokenErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                TokenDecodeError m ->
                    ( Nothing, Nothing, Just m )

                NoToken ->
                    ( Nothing, Nothing, Nothing )
    in
    ( { key = key
      , authorization = Nothing
      , token = token
      , state = state
      , msg = msg
      , replyType = "Token"
      , reply =
            case token of
                Nothing ->
                    Nothing

                Just tok ->
                    Just <| responseTokenEncoder tok
      , redirectBackUri = locationToRedirectBackUri url
      , authorizations = Dict.empty
      , provider =
            case state of
                Just p ->
                    p

                Nothing ->
                    "Gab"
      , tokenAuthorization = Nothing
      , getUserApi = Nothing
      }
    , Cmd.batch
        [ Http.request <|
            getAuthorizations ReceiveAuthorizations False "authorizations.json"
        , Navigation.replaceUrl key "#"
        ]
    )


getUser : Model -> ( Model, Cmd Msg )
getUser model =
    case model.token of
        Nothing ->
            ( { model
                | msg = Just "You must login before getting user information."
              }
            , Cmd.none
            )

        Just token ->
            case ( model.getUserApi, model.authorization ) of
                ( Just api, Just auth ) ->
                    let
                        url =
                            auth.apiUri ++ api

                        req =
                            Http.request
                                { method = "GET"
                                , headers = use token [ userAgentHeader ]
                                , url = url
                                , body = Http.emptyBody
                                , expect = Http.expectJson ReceiveUser JD.value
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    in
                    ( model, req )

                _ ->
                    ( { model | msg = Just "No known API." }
                    , Cmd.none
                    )


lookupProvider : Model -> Model
lookupProvider model =
    let
        authorization =
            case Dict.get model.provider model.authorizations of
                Nothing ->
                    case List.head <| Dict.toList model.authorizations of
                        Nothing ->
                            Nothing

                        Just ( _, auth ) ->
                            Just auth

                Just auth ->
                    Just auth
    in
    case authorization of
        Nothing ->
            model

        Just auth ->
            case List.head <| Dict.toList auth.scopes of
                Nothing ->
                    model

                Just ( _, scope ) ->
                    let
                        provider =
                            auth.name
                    in
                    { model
                        | provider = provider
                        , tokenAuthorization =
                            Just
                                { authorization = auth
                                , scope = [ scope ]
                                , state = Just model.provider
                                , redirectBackUri = model.redirectBackUri
                                }
                        , getUserApi = Just getUserApi
                        , authorization = authorization
                    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest _ ->
            ( model
            , Cmd.none
            )

        OnUrlChange _ ->
            ( model
            , Cmd.none
            )

        ReceiveAuthorizations result ->
            case result of
                Err err ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok authorizations ->
                    let
                        ( replyType, reply ) =
                            case ( model.token, model.msg ) of
                                ( Nothing, Nothing ) ->
                                    ( "Authorizations"
                                    , Just <|
                                        authorizationsEncoder
                                            authorizations
                                    )

                                _ ->
                                    ( model.replyType, model.reply )
                    in
                    ( lookupProvider
                        { model
                            | authorizations =
                                Dict.fromList <|
                                    List.map (\a -> ( a.name, a )) authorizations
                            , replyType = replyType
                            , reply = reply
                        }
                    , Cmd.none
                    )

        ChangeProvider provider ->
            ( lookupProvider
                { model | provider = provider }
            , Cmd.none
            )

        Login ->
            case model.tokenAuthorization of
                Nothing ->
                    ( { model | msg = Just "No provider selected." }
                    , Cmd.none
                    )

                Just authorization ->
                    case authorize authorization of
                        Nothing ->
                            ( { model
                                | msg = Just "Bad Uri in authorizations.json."
                              }
                            , Cmd.none
                            )

                        Just url ->
                            ( model
                            , Navigation.load <| Url.toString url
                            )

        GetUser ->
            getUser model

        ReceiveUser result ->
            case result of
                Err err ->
                    ( { model
                        | reply = Nothing
                        , msg = Just <| Debug.toString err
                      }
                    , Cmd.none
                    )

                Ok reply ->
                    ( { model
                        | replyType = "API Response"
                        , reply = Just reply
                        , msg = Nothing
                      }
                    , Cmd.none
                    )


providerOption : String -> String -> Html Msg
providerOption currentProvider provider =
    option
        [ value provider
        , selected <| provider == currentProvider
        ]
        [ text provider ]


providerSelect : Model -> Html Msg
providerSelect model =
    select [ onInput ChangeProvider ]
        (Dict.toList model.authorizations
            |> List.map Tuple.second
            |> List.map .name
            |> List.map (providerOption model.provider)
        )


view : Model -> Document Msg
view model =
    { title = "OAuthMiddleware Example"
    , body =
        [ div
            [ style "margin-left" "3em"
            ]
            [ h2 [] [ text "OAuthMiddleware Example" ]
            , p []
                [ text "Provider: "
                , providerSelect model
                ]
            , p []
                [ button [ onClick Login ]
                    [ text "Login" ]
                , text " "
                , button [ onClick GetUser ]
                    [ text "Get User" ]
                ]
            , pre []
                [ case ( model.msg, model.reply ) of
                    ( Just msg, _ ) ->
                        text <| Debug.toString msg

                    ( _, Just reply ) ->
                        text <| model.replyType ++ ":\n" ++ JE.encode 2 reply

                    _ ->
                        text "Nothing to report"
                ]
            ]
        ]
    }

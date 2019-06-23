module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Mastodon.EncodeDecode as ED exposing (encodeEntity, entityDecoder)
import Mastodon.Entities as Entities
    exposing
        ( Account
        , Entity(..)
        , WrappedAccount(..)
        )
import Maybe exposing (withDefault)
import Set exposing (Set)
import Test exposing (..)


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map Debug.toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


all : Test
all =
    Test.concat <|
        List.concat
            [ testMap entityTest entityData
            ]


expectResult : Result JD.Error thing -> Result JD.Error thing -> Expectation
expectResult sb was =
    case was of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false (JD.errorToString msg) True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


stripAccount : Account -> Account
stripAccount account =
    let
        moved =
            case account.moved of
                Nothing ->
                    Nothing

                Just (WrappedAccount wa) ->
                    Just <| WrappedAccount (stripAccount wa)
    in
    { account
        | moved = moved
        , v = JE.null
    }


stripEntity : Entity -> Entity
stripEntity entity =
    case entity of
        AccountEntity account ->
            AccountEntity <| stripAccount account

        _ ->
            entity


entityTest : Entity -> String -> Test
entityTest entity name =
    test ("entityTest \"" ++ name ++ "\"")
        (\_ ->
            let
                value =
                    encodeEntity entity

                result =
                    case JD.decodeValue (entityDecoder entity) value of
                        Err e ->
                            Err e

                        Ok en ->
                            Ok <| stripEntity en
            in
            expectResult (Ok entity) result
        )


entityData : List Entity
entityData =
    [ AccountEntity account1
    , AccountEntity account2
    ]


account1 : Account
account1 =
    { id = "id"
    , username = "username"
    , acct = "acct"
    , display_name = "display_name"
    , locked = True
    , created_at = "created_at"
    , followers_count = 1000
    , following_count = 100
    , statuses_count = 500
    , note = "note"
    , url = "url"
    , avatar = "avatar"
    , avatar_static = "avatar_static"
    , header = "header"
    , header_static = "header_static"
    , emojis =
        [ { shortcode = "shortcode"
          , static_url = "static_url"
          , url = "url2"
          , visible_in_picker = False
          }
        , { shortcode = "shortcode 2"
          , static_url = "static_url 2"
          , url = "url3"
          , visible_in_picker = True
          }
        ]
    , moved = Nothing
    , fields =
        [ { name = "name"
          , value = "value"
          , verified_at = Just "verified_at"
          }
        , { name = "name2"
          , value = "value2"
          , verified_at = Nothing
          }
        ]
    , bot = True
    , v = JE.null
    }


account2 : Account
account2 =
    { id = "id2"
    , username = "username2"
    , acct = "acct2"
    , display_name = "display_name2"
    , locked = True
    , created_at = "created_at2"
    , followers_count = 1001
    , following_count = 101
    , statuses_count = 501
    , note = "note2"
    , url = "url2"
    , avatar = "avatar2"
    , avatar_static = "avatar_static2"
    , header = "header2"
    , header_static = "header_static2"
    , emojis = []
    , moved = Just (WrappedAccount account1)
    , fields = [] -- List Field
    , bot = True
    , v = JE.null
    }

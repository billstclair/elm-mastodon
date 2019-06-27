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
        , Attachment
        , AttachmentType(..)
        , Card
        , CardType(..)
        , Entity(..)
        , Field
        , Focus
        , ImageMetaFields
        , ImageMetaInfo
        , Meta(..)
        , VideoMetaFields
        , VideoMetaInfo
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

        SourceEntity source ->
            SourceEntity { source | v = JE.null }

        TokenEntity token ->
            TokenEntity { token | v = JE.null }

        ApplicationEntity application ->
            ApplicationEntity { application | v = JE.null }

        AttachmentEntity attachment ->
            AttachmentEntity { attachment | v = JE.null }

        CardEntity card ->
            CardEntity { card | v = JE.null }

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
                    case JD.decodeValue entityDecoder value of
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
    , SourceEntity
        { privacy = Just "privacy"
        , sensitive = True
        , language = Just "language"
        , note = "note"
        , fields = [ field1, field2 ]
        , v = JE.null
        }
    , SourceEntity
        { privacy = Nothing
        , sensitive = False
        , language = Nothing
        , note = "note2"
        , fields = []
        , v = JE.null
        }
    , TokenEntity
        { access_token = "access_token"
        , token_type = "token_type"
        , scope = "scope"
        , created_at = 1234
        , v = JE.null
        }
    , ApplicationEntity
        { name = "name"
        , website = Nothing
        , v = JE.null
        }
    , ApplicationEntity
        { name = "name2"
        , website = Just "website"
        , v = JE.null
        }
    , AttachmentEntity
        { id = "id1"
        , type_ = UnknownAttachment
        , url = "url"
        , remote_url = Just "remote_url"
        , preview_url = "preview_url"
        , text_url = Just "text_url"
        , meta = Nothing
        , description = "description"
        , v = JE.null
        }
    , AttachmentEntity
        { id = "id2"
        , type_ = ImageAttachment
        , url = "url2"
        , remote_url = Nothing
        , preview_url = "preview_url2"
        , text_url = Nothing
        , meta = Just imageMeta1
        , description = "description2"
        , v = JE.null
        }
    , AttachmentEntity
        { id = "id3"
        , type_ = ImageAttachment
        , url = "url3"
        , remote_url = Nothing
        , preview_url = "preview_url3"
        , text_url = Just "text_url3"
        , meta = Just imageMeta2
        , description = "description3"
        , v = JE.null
        }
    , AttachmentEntity
        { id = "id4"
        , type_ = VideoAttachment
        , url = "url4"
        , remote_url = Nothing
        , preview_url = "preview_url4"
        , text_url = Nothing
        , meta = Just videoMeta1
        , description = "description4"
        , v = JE.null
        }
    , AttachmentEntity
        { id = "id5"
        , type_ = VideoAttachment
        , url = "url5"
        , remote_url = Nothing
        , preview_url = "preview_url5"
        , text_url = Nothing
        , meta = Just videoMeta2
        , description = "description5"
        , v = JE.null
        }
    , CardEntity
        { url = "url"
        , title = "title"
        , description = "description"
        , image = Nothing
        , type_ = LinkCard
        , author_name = Nothing
        , author_url = Nothing
        , provider_name = Nothing
        , provider_url = Nothing
        , html = Just "html"
        , width = Nothing
        , height = Nothing
        , v = JE.null
        }
    , CardEntity
        { url = "url2"
        , title = "title2"
        , description = "description2"
        , image = Just "image"
        , type_ = PhotoCard
        , author_name = Nothing
        , author_url = Nothing
        , provider_name = Nothing
        , provider_url = Nothing
        , html = Nothing
        , width = Just 1024
        , height = Just 768
        , v = JE.null
        }
    , CardEntity
        { url = "url3"
        , title = "title3"
        , description = "description3"
        , image = Just "image"
        , type_ = VideoCard
        , author_name = Nothing
        , author_url = Nothing
        , provider_name = Nothing
        , provider_url = Nothing
        , html = Nothing
        , width = Just 1536
        , height = Just 1024
        , v = JE.null
        }
    , CardEntity
        { url = "url"
        , title = "title"
        , description = "description"
        , image = Just "image"
        , type_ = RichCard
        , author_name = Just "author_name"
        , author_url = Just "author_url"
        , provider_name = Just "provider_name"
        , provider_url = Just "provider_url"
        , html = Nothing
        , width = Nothing
        , height = Nothing
        , v = JE.null
        }
    ]


imageMeta1 : Meta
imageMeta1 =
    ImageMeta
        { small = Just imageMetaInfo1
        , original = Nothing
        , focus = Just focus
        }


imageMeta2 : Meta
imageMeta2 =
    ImageMeta
        { small = Nothing
        , original = Just imageMetaInfo2
        , focus = Nothing
        }


imageMetaInfo1 : ImageMetaInfo
imageMetaInfo1 =
    { width = Just 1
    , height = Just 2
    , size = Nothing
    , aspect = Nothing
    }


imageMetaInfo2 : ImageMetaInfo
imageMetaInfo2 =
    { width = Nothing
    , height = Nothing
    , size = Just 128
    , aspect = Just 0.5
    }


videoMeta1 : Meta
videoMeta1 =
    VideoMeta
        { small = Just videoMetaInfo1
        , original = Nothing
        , focus = Just focus
        }


videoMeta2 : Meta
videoMeta2 =
    VideoMeta
        { small = Nothing
        , original = Just videoMetaInfo2
        , focus = Nothing
        }


videoMetaInfo1 : VideoMetaInfo
videoMetaInfo1 =
    { width = Just 3
    , height = Just 4
    , frame_rate = Nothing
    , duration = Nothing
    , bitrate = Nothing
    }


videoMetaInfo2 : VideoMetaInfo
videoMetaInfo2 =
    { width = Nothing
    , height = Nothing
    , frame_rate = Just 30
    , duration = Just 12.3
    , bitrate = Just 10000
    }


focus : Focus
focus =
    { x = 0.5
    , y = 0.5
    }


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
        [ field1
        , field2
        ]
    , bot = True
    , v = JE.null
    }


field1 : Field
field1 =
    { name = "name"
    , value = "value"
    , verified_at = Just "verified_at"
    }


field2 : Field
field2 =
    { name = "name2"
    , value = "value2"
    , verified_at = Nothing
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
    , fields = []
    , bot = True
    , v = JE.null
    }

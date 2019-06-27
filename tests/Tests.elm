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
        , Application
        , Attachment
        , AttachmentType(..)
        , Card
        , CardType(..)
        , Context
        , Emoji
        , Entity(..)
        , Field
        , Filter
        , FilterContext(..)
        , Focus
        , History
        , ImageMetaFields
        , ImageMetaInfo
        , Instance
        , Mention
        , Meta(..)
        , Poll
        , PollOption
        , Stats
        , Status
        , Tag
        , URLs
        , VideoMetaFields
        , VideoMetaInfo
        , Visibility(..)
        , WrappedAccount(..)
        , WrappedStatus(..)
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


stripStatus : Status -> Status
stripStatus status =
    let
        account =
            status.account

        application =
            status.application
    in
    { status
        | account = stripAccount account
        , reblog =
            case status.reblog of
                Nothing ->
                    Nothing

                Just (WrappedStatus ws) ->
                    Just <| WrappedStatus (stripStatus ws)
        , card =
            case status.card of
                Nothing ->
                    Nothing

                Just card ->
                    Just { card | v = JE.null }
        , application =
            { application | v = JE.null }
        , v = JE.null
    }


stripContext : Context -> Context
stripContext { ancestors, descendants } =
    { ancestors = List.map stripStatus ancestors
    , descendants = List.map stripStatus descendants
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

        CardEntity card ->
            CardEntity { card | v = JE.null }

        ContextEntity context ->
            ContextEntity <| stripContext context

        StatusEntity status ->
            StatusEntity <| stripStatus status

        FilterEntity filter ->
            FilterEntity { filter | v = JE.null }

        InstanceEntity instance ->
            let
                contact_account =
                    instance.contact_account
            in
            InstanceEntity
                { instance
                    | contact_account =
                        case contact_account of
                            Nothing ->
                                Nothing

                            Just account ->
                                Just <| stripAccount account
                    , v = JE.null
                }

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
    , ApplicationEntity application1
    , ApplicationEntity application2
    , CardEntity card1
    , CardEntity card2
    , CardEntity card3
    , CardEntity card4
    , StatusEntity status1
    , StatusEntity status2
    , StatusEntity status3
    , StatusEntity status4
    , ContextEntity
        { ancestors = [ status1 ]
        , descendants = [ status2, status3, status4 ]
        }
    , FilterEntity filter1
    , FilterEntity filter2
    , InstanceEntity instance1
    , InstanceEntity instance2
    , ListEntityEntity { id = "id", title = "title" }
    ]


instance1 : Instance
instance1 =
    { uri = "uri"
    , title = "title"
    , description = "description"
    , email = "email"
    , version = "version"
    , thumbnail = Just "thumbnail"
    , urls = urls
    , stats = stats
    , languages = [ "l1", "l2", "l3" ]
    , contact_account = Just account1
    , v = JE.null
    }


instance2 : Instance
instance2 =
    { instance1
        | thumbnail = Nothing
        , languages = []
        , contact_account = Nothing
    }


urls : URLs
urls =
    { streaming_api = "streaming_api" }


stats : Stats
stats =
    { user_count = 1
    , status_count = 2
    , domain_count = 3
    }


filter1 : Filter
filter1 =
    { id = "id"
    , phrase = "phrase"
    , context = [ HomeContext, NotificationsContext ]
    , expires_at = Just "expires_at"
    , irreversible = False
    , whole_word = False
    , v = JE.null
    }


filter2 : Filter
filter2 =
    { id = "id2"
    , phrase = "phrase2"
    , context = [ PublicContext, ThreadContext ]
    , expires_at = Nothing
    , irreversible = True
    , whole_word = True
    , v = JE.null
    }


card1 : Card
card1 =
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


card2 : Card
card2 =
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


card3 : Card
card3 =
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


card4 : Card
card4 =
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


application1 : Application
application1 =
    { name = "name"
    , website = Nothing
    , v = JE.null
    }


application2 : Application
application2 =
    { name = "name2"
    , website = Just "website"
    , v = JE.null
    }


status1 : Status
status1 =
    { id = "id"
    , uri = "uri"
    , url = Just "url"
    , account = account1
    , in_reply_to_id = Just "in_reply_to_id"
    , in_reply_to_account_id = Just "in_reply_to_account_id"
    , reblog = Just <| WrappedStatus status2
    , content = "content"
    , created_at = "created_at"
    , emojis = [ emoji1, emoji2 ]
    , replies_count = 100
    , reblogs_count = 10
    , favourites_count = 1000
    , reblogged = True
    , favourited = True
    , muted = False
    , sensitive = False
    , spoiler_text = "spoiler_text"
    , visibility = PublicVisibility
    , media_attachments = [ attachment1, attachment2 ]
    , mentions = [ mention1, mention2 ]
    , tags = [ tag1, tag2 ]
    , card = Just card1
    , poll = Just poll1
    , application = application1
    , language = Just "language"
    , pinned = True
    , v = JE.null
    }


status2 : Status
status2 =
    { id = "id2"
    , uri = "uri2"
    , url = Nothing
    , account = account2
    , in_reply_to_id = Nothing
    , in_reply_to_account_id = Nothing
    , reblog = Nothing
    , content = "content2"
    , created_at = "created_at2"
    , emojis = []
    , replies_count = 0
    , reblogs_count = 0
    , favourites_count = 0
    , reblogged = False
    , favourited = False
    , muted = True
    , sensitive = True
    , spoiler_text = "spoiler_text2"
    , visibility = UnlistedVisibility
    , media_attachments = []
    , mentions = []
    , tags = []
    , card = Just card2
    , poll = Just poll2
    , application = application2
    , language = Nothing
    , pinned = False
    , v = JE.null
    }


status3 : Status
status3 =
    { status2
        | visibility = PrivateVisibility
        , card = Nothing
    }


status4 : Status
status4 =
    { status2
        | visibility = DirectVisibility
        , card = Nothing
    }


poll1 : Poll
poll1 =
    { id = "id"
    , expires_at = Just "expires_at"
    , expired = False
    , multiple = False
    , votes_count = 10
    , options = [ pollOption1, pollOption2 ]
    , voted = False
    }


poll2 : Poll
poll2 =
    { id = "id2"
    , expires_at = Nothing
    , expired = True
    , multiple = True
    , votes_count = 100
    , options = [ pollOption1, pollOption2 ]
    , voted = True
    }


pollOption1 : PollOption
pollOption1 =
    { title = "Yes"
    , votes_count = 10
    }


pollOption2 : PollOption
pollOption2 =
    { title = "No"
    , votes_count = 1000
    }


tag1 : Tag
tag1 =
    { name = "name"
    , url = "url"
    , history = [ history1, history2 ]
    }


tag2 : Tag
tag2 =
    { name = "name2"
    , url = "url2"
    , history = []
    }


history1 : History
history1 =
    { day = "day"
    , uses = 1
    , accounts = 2
    }


history2 : History
history2 =
    { day = "day2"
    , uses = 3
    , accounts = 4
    }


mention1 : Mention
mention1 =
    { url = "url"
    , username = "username"
    , acct = "acct"
    , id = "id"
    }


mention2 : Mention
mention2 =
    { url = "url2"
    , username = "username2"
    , acct = "acct2"
    , id = "id2"
    }


attachment1 : Attachment
attachment1 =
    { id = "id1"
    , type_ = UnknownAttachment
    , url = "url"
    , remote_url = Just "remote_url"
    , preview_url = "preview_url"
    , text_url = Just "text_url"
    , meta = Nothing
    , description = "description"
    }


attachment2 : Attachment
attachment2 =
    { id = "id2"
    , type_ = ImageAttachment
    , url = "url2"
    , remote_url = Nothing
    , preview_url = "preview_url2"
    , text_url = Nothing
    , meta = Just imageMeta1
    , description = "description2"
    }


attachment3 : Attachment
attachment3 =
    { id = "id3"
    , type_ = ImageAttachment
    , url = "url3"
    , remote_url = Nothing
    , preview_url = "preview_url3"
    , text_url = Just "text_url3"
    , meta = Just imageMeta2
    , description = "description3"
    }


attachment4 : Attachment
attachment4 =
    { id = "id4"
    , type_ = VideoAttachment
    , url = "url4"
    , remote_url = Nothing
    , preview_url = "preview_url4"
    , text_url = Nothing
    , meta = Just videoMeta1
    , description = "description4"
    }


attachment5 : Attachment
attachment5 =
    { id = "id5"
    , type_ = VideoAttachment
    , url = "url5"
    , remote_url = Nothing
    , preview_url = "preview_url5"
    , text_url = Nothing
    , meta = Just videoMeta2
    , description = "description5"
    }


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
    , emojis = [ emoji1, emoji2 ]
    , moved = Nothing
    , fields =
        [ field1
        , field2
        ]
    , bot = True
    , v = JE.null
    }


emoji1 : Emoji
emoji1 =
    { shortcode = "shortcode"
    , static_url = "static_url"
    , url = "url2"
    , visible_in_picker = False
    }


emoji2 : Emoji
emoji2 =
    { shortcode = "shortcode 2"
    , static_url = "static_url 2"
    , url = "url3"
    , visible_in_picker = True
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

module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Mastodon.EncodeDecode as ED exposing (encodeEntity)
import Mastodon.Entity as Entity
    exposing
        ( Account
        , Activity
        , App
        , Application
        , Attachment
        , AttachmentType(..)
        , Card
        , CardType(..)
        , Context
        , Conversation
        , Emoji
        , Entity(..)
        , Field
        , Filter
        , FilterContext(..)
        , Focus
        , Group
        , GroupRelationship
        , History
        , ImageMetaFields
        , ImageMetaInfo
        , Instance
        , Mention
        , Meta(..)
        , Notification
        , NotificationType(..)
        , Poll
        , PollOption
        , Privacy(..)
        , PushSubscription
        , Relationship
        , Results
        , ScheduledStatus
        , Source
        , Stats
        , Status
        , StatusParams
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


testEmptyInstance : Test
testEmptyInstance =
    test "emptyInstanceTest"
        (\_ ->
            let
                value =
                    JE.object []

                result =
                    case JD.decodeValue entityDecoder value of
                        Err e ->
                            Err e

                        Ok en ->
                            Ok <| stripEntity en
            in
            expectResult (Ok <| InstanceEntity emptyInstance) result
        )


all : Test
all =
    Test.concat <|
        List.concat
            [ testMap entityTest entityData
            , [ testEmptyInstance ]
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

        source =
            case account.source of
                Nothing ->
                    Nothing

                Just s ->
                    Just { s | v = JE.null }
    in
    { account
        | moved = moved
        , source = source
        , v = JE.null
    }


stripAttachment : Attachment -> Attachment
stripAttachment attachment =
    { attachment | v = JE.null }


stripStatus : Status -> Status
stripStatus status =
    let
        account =
            status.account
    in
    { status
        | account = stripAccount account
        , reblog =
            case status.reblog of
                Nothing ->
                    Nothing

                Just (WrappedStatus ws) ->
                    Just <| WrappedStatus (stripStatus ws)
        , media_attachments =
            List.map stripAttachment status.media_attachments
        , card =
            case status.card of
                Nothing ->
                    Nothing

                Just card ->
                    Just { card | v = JE.null }
        , poll =
            case status.poll of
                Nothing ->
                    Nothing

                Just poll ->
                    Just { poll | v = JE.null }
        , application =
            case status.application of
                Nothing ->
                    Nothing

                Just application ->
                    Just { application | v = JE.null }
        , group =
            case status.group of
                Nothing ->
                    Nothing

                Just group ->
                    Just { group | v = JE.null }
        , quote =
            case status.quote of
                Nothing ->
                    Nothing

                Just (WrappedStatus qs) ->
                    Just (WrappedStatus <| stripStatus qs)
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

        AppEntity app ->
            AppEntity { app | v = JE.null }

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

        ActivityEntity activity ->
            ActivityEntity <| { activity | v = JE.null }

        AttachmentEntity attachment ->
            AttachmentEntity <| stripAttachment attachment

        NotificationEntity notification ->
            let
                account =
                    notification.account

                status =
                    notification.status
            in
            NotificationEntity
                { notification
                    | account = stripAccount account
                    , status =
                        case status of
                            Nothing ->
                                Nothing

                            Just s ->
                                Just <| stripStatus s
                    , v = JE.null
                }

        PushSubscriptionEntity pushSubscription ->
            PushSubscriptionEntity { pushSubscription | v = JE.null }

        RelationshipEntity relationship ->
            RelationshipEntity { relationship | v = JE.null }

        ResultsEntity results ->
            ResultsEntity
                { results
                    | accounts = List.map stripAccount results.accounts
                    , statuses = List.map stripStatus results.statuses
                    , v = JE.null
                }

        ScheduledStatusEntity scheduledStatus ->
            ScheduledStatusEntity
                { scheduledStatus
                    | media_attachments =
                        List.map stripAttachment scheduledStatus.media_attachments
                    , v = JE.null
                }

        ConversationEntity conversation ->
            let
                status =
                    conversation.last_status
            in
            ConversationEntity
                { conversation
                    | accounts = List.map stripAccount conversation.accounts
                    , last_status =
                        case status of
                            Nothing ->
                                Nothing

                            Just s ->
                                Just <| stripStatus s
                    , v = JE.null
                }

        GroupEntity group ->
            GroupEntity { group | v = JE.null }

        GroupRelationshipEntity group ->
            GroupRelationshipEntity { group | v = JE.null }

        _ ->
            entity


unnamedHost : String
unnamedHost =
    "the-server-that-shall-not-be-named.com"


unnamedUrl : String
unnamedUrl =
    "https://" ++ unnamedHost


entityDecoder : Decoder Entity
entityDecoder =
    ED.entityDecoder unnamedUrl


emptyInstance : Instance
emptyInstance =
    { uri = unnamedUrl
    , title = unnamedHost
    , description = unnamedHost
    , email = unnamedHost
    , version = "unknown"
    , thumbnail = Nothing
    , urls = Just { streaming_api = "wss://" ++ unnamedHost }
    , stats =
        { domain_count = 0
        , status_count = 0
        , user_count = 0
        }
    , max_toot_chars = Just 3000
    , languages = []
    , contact_account = Nothing
    , v = JE.null
    }


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
    [ AccountEntity account1 --1
    , AccountEntity account2 --2
    , AppEntity app1 --3
    , AppEntity app2 --4
    , SourceEntity source1 --5
    , SourceEntity source2 --6
    , SourceEntity source3 --7
    , TokenEntity
        --8
        { access_token = "access_token"
        , token_type = "token_type"
        , scope = "scope"
        , created_at = 1234
        , v = JE.null
        }
    , ApplicationEntity application1 --9
    , ApplicationEntity application2 --10
    , CardEntity card1 --11
    , CardEntity card2 --12
    , CardEntity card3 --13
    , CardEntity card4 --14
    , StatusEntity status1 --15
    , StatusEntity status2 --16
    , StatusEntity status3 --17
    , StatusEntity status4 --18
    , ContextEntity
        --19
        { ancestors = [ status1 ]
        , descendants = [ status2, status3, status4 ]
        }
    , FilterEntity filter1 --20
    , FilterEntity filter2 --21
    , InstanceEntity instance1 --22
    , InstanceEntity instance2 --23
    , ActivityEntity activity1 --24
    , ActivityEntity activity2 --25
    , ListEntityEntity { id = "id", title = "title" } --26
    , NotificationEntity notification1 --27
    , NotificationEntity notification2 --28
    , NotificationEntity notification3 --29
    , NotificationEntity notification4 --30
    , NotificationEntity notification5 --31
    , NotificationEntity notification6 --32
    , NotificationEntity notification7 --33
    , PushSubscriptionEntity pushSubscription1 --34
    , RelationshipEntity relationship1 --35
    , RelationshipEntity relationship2 --36
    , ResultsEntity results1 --37
    , ScheduledStatusEntity scheduledStatus1 --38
    , ScheduledStatusEntity scheduledStatus2 --39
    , ScheduledStatusEntity scheduledStatus3 --40
    , ScheduledStatusEntity scheduledStatus4 --41
    , ConversationEntity conversation1 --42
    , ConversationEntity conversation2 --43
    , GroupEntity group1 --44
    , GroupEntity group2 --45
    , GroupRelationshipEntity groupRelationship1 --46
    , GroupRelationshipEntity groupRelationship2 --47
    , NotificationEntity notification8 --48
    ]


source1 : Source
source1 =
    { privacy = PublicPrivacy
    , sensitive = True
    , language = Just "language"
    , note = "note"
    , fields = [ field1, field2 ]
    , v = JE.null
    }


source2 : Source
source2 =
    { privacy = UnlistedPrivacy
    , sensitive = False
    , language = Nothing
    , note = "note2"
    , fields = []
    , v = JE.null
    }


source3 : Source
source3 =
    { privacy = PrivatePrivacy
    , sensitive = False
    , language = Nothing
    , note = "note3"
    , fields = []
    , v = JE.null
    }


group1 : Group
group1 =
    { id = "id"
    , title = "title"
    , description = "description"
    , cover_image_url = "cover_image_url"
    , is_archived = False
    , member_count = 10
    , v = JE.null
    }


group2 : Group
group2 =
    { id = "id2"
    , title = "title2"
    , description = "description2"
    , cover_image_url = "cover_image_url2"
    , is_archived = True
    , member_count = 20
    , v = JE.null
    }


groupRelationship1 : GroupRelationship
groupRelationship1 =
    { id = "id"
    , member = True
    , admin = False
    , unread_count = 10
    , v = JE.null
    }


groupRelationship2 : GroupRelationship
groupRelationship2 =
    { id = "id2"
    , member = False
    , admin = True
    , unread_count = 20
    , v = JE.null
    }


conversation1 : Conversation
conversation1 =
    { id = "id"
    , accounts = [ account1, account2 ]
    , last_status = Just status1
    , unread = True
    , v = JE.null
    }


conversation2 : Conversation
conversation2 =
    { id = "id2"
    , accounts = []
    , last_status = Nothing
    , unread = False
    , v = JE.null
    }


statusParams1 : StatusParams
statusParams1 =
    { text = "text"
    , in_reply_to_id = Just "in_reply_to_id"
    , media_ids = [ "id1", "id2", "id3" ]
    , sensitive = True
    , spoiler_text = Just "spoiler_text"
    , visibility = Just PublicVisibility
    , scheduled_at = Just "scheduled_at"
    , application_id = "application_id"
    }


statusParams2 : StatusParams
statusParams2 =
    { statusParams1
        | in_reply_to_id = Nothing
        , media_ids = []
        , sensitive = False
        , spoiler_text = Nothing
        , visibility = Just UnlistedVisibility
        , scheduled_at = Nothing
    }


statusParams3 : StatusParams
statusParams3 =
    { statusParams2 | visibility = Just PrivateVisibility }


statusParams4 : StatusParams
statusParams4 =
    { statusParams2 | visibility = Just DirectVisibility }


scheduledStatus1 : ScheduledStatus
scheduledStatus1 =
    { id = "id"
    , scheduled_at = "scheduled_at"
    , params = statusParams1
    , media_attachments = [ attachment1, attachment2 ]
    , v = JE.null
    }


scheduledStatus2 : ScheduledStatus
scheduledStatus2 =
    { scheduledStatus1
        | params = statusParams2
        , media_attachments = []
    }


scheduledStatus3 : ScheduledStatus
scheduledStatus3 =
    { scheduledStatus2 | params = statusParams3 }


scheduledStatus4 : ScheduledStatus
scheduledStatus4 =
    { scheduledStatus2 | params = statusParams4 }


results1 : Results
results1 =
    { accounts = [ account1, account2 ]
    , statuses = [ status1, status2 ]
    , hashtags = [ "tag1", "tag2" ]
    , groups = []
    , v = JE.null
    }


relationship1 : Relationship
relationship1 =
    { id = "id"
    , following = False
    , followed_by = False
    , blocking = False
    , muting = False
    , muting_notifications = False
    , requested = False
    , domain_blocking = False
    , showing_reblogs = False
    , endorsed = False
    , v = JE.null
    }


relationship2 : Relationship
relationship2 =
    { id = "id"
    , following = True
    , followed_by = True
    , blocking = True
    , muting = True
    , muting_notifications = True
    , requested = True
    , domain_blocking = True
    , showing_reblogs = True
    , endorsed = True
    , v = JE.null
    }


pushSubscription1 : PushSubscription
pushSubscription1 =
    { id = "id"
    , endpoint = "endpoint"
    , server_key = "server_key"
    , alerts = JE.string "alerts"
    , v = JE.null
    }


notification1 : Notification
notification1 =
    { id = "id"
    , type_ = FollowNotification
    , created_at = "created_at"
    , account = account1
    , emoji = Nothing
    , status = Just status1
    , v = JE.null
    }


notification2 : Notification
notification2 =
    { notification1
        | type_ = MentionNotification
        , status = Nothing
    }


notification3 : Notification
notification3 =
    { notification2
        | type_ = ReblogNotification
    }


notification4 : Notification
notification4 =
    { notification2
        | type_ = FavouriteNotification
    }


notification5 : Notification
notification5 =
    { notification2
        | type_ = PollNotification
    }


notification6 : Notification
notification6 =
    { notification2
        | type_ = FollowRequestNotification
    }


notification7 : Notification
notification7 =
    { notification2
        | type_ = UnknownNotification "foo"
    }


notification8 : Notification
notification8 =
    { notification2
        | type_ = Pleroma_EmojiReactionNotification
        , emoji = Just "X"
    }


instance1 : Instance
instance1 =
    { uri = "uri"
    , title = "title"
    , description = "description"
    , email = "email"
    , version = "version"
    , thumbnail = Just "thumbnail"
    , urls = Just urls
    , stats = stats
    , max_toot_chars = Just 300
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


activity1 : Activity
activity1 =
    { week = 1565568000
    , statuses = 85012
    , logins = Just 16446
    , registrations = 2171
    , v = JE.null
    }


activity2 : Activity
activity2 =
    { activity1 | logins = Nothing }


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
    , plain_markdown = Nothing
    , plain_text = Nothing
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
    , application = Just application1
    , language = Just "language"
    , pinned = True
    , group = Nothing
    , quote_of_id = Nothing
    , quote = Nothing
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
    , content = "<b>content2</b>"
    , plain_markdown = Just "**content2**"
    , plain_text = Just "content2"
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
    , application = Just application2
    , language = Nothing
    , pinned = False
    , group = Just group1
    , quote_of_id = Nothing
    , quote = Nothing
    , v = JE.null
    }


status3 : Status
status3 =
    { status2
        | id = "id3"
        , visibility = PrivateVisibility
        , card = Nothing
        , application = Nothing
        , quote_of_id = Just "id2"
        , quote = Just <| WrappedStatus status2
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
    , voters_count = Nothing
    , options = [ pollOption1, pollOption2 ]
    , emojis = []
    , voted = False
    , own_votes = []
    , v = JE.null
    }


poll2 : Poll
poll2 =
    { id = "id2"
    , expires_at = Nothing
    , expired = True
    , multiple = True
    , votes_count = 100
    , voters_count = Just 12
    , options = [ pollOption1, pollOption2 ]
    , emojis = [ emoji1, emoji2 ]
    , voted = True
    , own_votes = [ 0 ]
    , v = JE.null
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
    , preview_url = Just "preview_url"
    , text_url = Just "text_url"
    , meta = Nothing
    , description = Nothing
    , v = JE.null
    }


attachment2 : Attachment
attachment2 =
    { id = "id2"
    , type_ = ImageAttachment
    , url = "url2"
    , remote_url = Nothing
    , preview_url = Nothing
    , text_url = Nothing
    , meta = Just imageMeta1
    , description = Just "description2"
    , v = JE.null
    }


attachment3 : Attachment
attachment3 =
    { id = "id3"
    , type_ = ImageAttachment
    , url = "url3"
    , remote_url = Nothing
    , preview_url = Just "preview_url3"
    , text_url = Just "text_url3"
    , meta = Just imageMeta2
    , description = Just "description3"
    , v = JE.null
    }


attachment4 : Attachment
attachment4 =
    { id = "id4"
    , type_ = VideoAttachment
    , url = "url4"
    , remote_url = Nothing
    , preview_url = Just "preview_url4"
    , text_url = Nothing
    , meta = Just videoMeta1
    , description = Nothing
    , v = JE.null
    }


attachment5 : Attachment
attachment5 =
    { id = "id5"
    , type_ = VideoAttachment
    , url = "url5"
    , remote_url = Nothing
    , preview_url = Just "preview_url5"
    , text_url = Nothing
    , meta = Just videoMeta2
    , description = Just "description5"
    , v = JE.null
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
    , size = Just "128"
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
    , frame_rate = Just "30"
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
    , source = Just source1
    , is_pro = True
    , is_verified = True
    , is_donor = True
    , is_investor = True
    , v = JE.null
    }


app1 : App
app1 =
    { id = "id"
    , name = "name"
    , website = Just "website"
    , redirect_uri = "redirect_uri"
    , client_id = "client_id"
    , client_secret = "client_secret"
    , vapid_key = Just "vapid_key"
    , v = JE.null
    }


app2 : App
app2 =
    { app1
        | website = Nothing
        , vapid_key = Nothing
    }


emoji1 : Emoji
emoji1 =
    { shortcode = "shortcode"
    , static_url = "static_url"
    , url = "url2"
    , visible_in_picker = False
    , category = Nothing
    }


emoji2 : Emoji
emoji2 =
    { shortcode = "shortcode 2"
    , static_url = "static_url 2"
    , url = "url3"
    , visible_in_picker = True
    , category = Just "foo"
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
    , source = Nothing
    , is_pro = False
    , is_verified = False
    , is_donor = False
    , is_investor = False
    , v = JE.null
    }

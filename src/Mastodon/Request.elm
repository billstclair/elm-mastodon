---------------------------------------------------------------------
--
-- Request.elm
-- Mastodon API requests.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Mastodon.Request exposing
    ( ServerInfo, Request(..), Response, Error(..)
    , serverRequest
    , AccountsReq(..), AppsReq(..), BlocksReq(..), FavouritesReq(..)
    , FollowReq(..), MediaAttachmentsReq(..), MutesReq(..), NotificationsReq(..)
    , PollsReq(..), ReportsReq(..), SearchReq(..), StatusesReq(..), TimelinesReq(..)
    , Paging, SourceUpdate, PollDefinition
    , userAgentHeader, idempotencyKeyHeader
    , RawRequest, requestToRawRequest, rawRequestToCmd, rawRequestToTask
    , emptyRawRequest, emptyServerInfo
    )

{-| Types to represent the Mastodon REST API.

Funcion to generate a request and parse the return.

Documentation starts at <https://docs.joinmastodon.org/api/rest/accounts>


# Basic Types

@docs ServerInfo, Request, Response, Error


# Creating an HTTP request

@docs serverRequest


# Request details

@docs AccountsReq, AppsReq, BlocksReq, FavouritesReq
@docs FollowReq, MediaAttachmentsReq, MutesReq, NotificationsReq
@docs PollsReq, ReportsReq, SearchReq, StatusesReq, TimelinesReq


# Non-atomic data in requests

@docs Paging, SourceUpdate, PollDefinition


# Utility

@docs userAgentHeader, idempotencyKeyHeader


# Low-level request creation

@docs RawRequest, requestToRawRequest, rawRequestToCmd, rawRequestToTask


# Testing

@docs emptyRawRequest, emptyServerInfo

-}

import File exposing (File)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mastodon.EncodeDecode as ED exposing (encodeMaybe)
import Mastodon.Entity as Entity
    exposing
        ( Entity(..)
        , FilterContext(..)
        , UnixTimestamp
        )
import Task exposing (Task)
import Url.Builder as Builder exposing (QueryParameter, relative)


{-| An API request.

Broken down as in the documentation.

The result types are mostly documented in the individual `Request` element types,
with these exceptions:

`InstanceRequest` results in an `InstanceEntity`.

`CustomEmojisRequest` results in an `EmojiListEntity`.

`TrendsRequest` results in a `TagListEntity`.

`InstanceRequest` and `CustomEmojisRequest` do not require an authentication token.

-}
type Request
    = AccountsRequest AccountsReq
    | AppsRequest AppsReq
    | BlocksRequest BlocksReq
    | CustomEmojisRequest
    | DomainBlocksRequest DomainBlocksReq
    | EndorsementsRequest EndorsementsReq
    | FavouritesRequest FavouritesReq
    | FiltersRequest FiltersReq
    | FollowRequest FollowReq
    | FollowSuggestionsRequest FollowSuggestionsReq
    | InstanceRequest
    | ListsRequest ListsReq
    | MediaAttachmentsRequest MediaAttachmentsReq
    | MutesRequest MutesReq
    | NotificationsRequest NotificationsReq
    | PollsRequest PollsReq
    | ReportsRequest ReportsReq
    | ScheduledStatusesRequest ScheduledStatusesReq
    | SearchRequest SearchReq
    | StatusesRequest StatusesReq
    | TimelinesRequest TimelinesReq
    | TrendsRequest


{-| Parameters to control paging for requests that return lists.
-}
type alias Paging =
    { max_id : Maybe String
    , since_id : Maybe String
    , min_id : Maybe String
    , limit : Maybe Int
    }


{-| Updated account `Source` information
-}
type alias SourceUpdate =
    { privacy : Maybe String
    , sensitive : Bool
    , language : Maybe Entity.ISO6391
    }


{-| GET/POST/PATCH /api/v1/accounts

`GetAccount` does not require an authentication token.

`GetAccount`, `GetVerifyCredentials`, and `PatchUpdateCredentials` result in an `AccountEntity`.

`GetFollowers`, `GetFollowing`, and `GetSearchAccounts` resul in an `AccountListEntity`.

`GetStatuses` results in a `StatusListEntity`.

`PostFollow` and `PostUnfollow` result in a `RelationshipEntity`.

`GetRelationships` results in a `RelationshipListEntity`.

-}
type AccountsReq
    = GetAccount { id : String }
    | GetVerifyCredentials
    | PatchUpdateCredentials
        { display_name : Maybe String
        , note : Maybe String
        , avatar : Maybe File
        , header : Maybe File
        , locked : Bool
        , source : Maybe SourceUpdate
        , fields_attributes : Maybe (List Entity.Field)
        }
    | GetFollowers { id : String, limit : Maybe Int }
    | GetFollowing { id : String, limit : Maybe Int }
    | GetStatuses
        { id : String
        , only_media : Bool
        , pinned : Bool
        , exclude_replies : Bool
        , paging : Maybe Paging
        , exclude_reblogs : Bool
        }
    | PostFollow { id : String, reblogs : Bool }
    | PostUnfollow { id : String }
    | GetRelationships { ids : List String }
    | GetSearchAccounts
        { q : String
        , limit : Maybe Int
        , resolve : Bool
        , following : Bool
        }


{-| GET/POST /api/v1/apps

`PostApp` results in an `AppEntity`.

-}
type AppsReq
    = PostApp
        { client_name : String
        , redirect_uris : String
        , scopes : List String
        , website : Maybe String
        }
    | GetVerifyAppCredentials


{-| GET/POST /api/v1/blocks

`GetBlocks` results in an `AccountListEntity`.

`PostBlock` and `PostUnblock` result in a `RelationshipEntity`.

-}
type BlocksReq
    = GetBlocks { limit : Maybe Int }
    | PostBlock { id : String }
    | PostUnblock { id : String }


{-| GET/POST /api/v8/domain\_blocks

`GetDomainBlocks` results in a `StringListEntity`, a list of domain names.

`PostDomainBlock` and `DeleteDomainBlock` result in `NoEntity`.

-}
type DomainBlocksReq
    = GetDomainBlocks { limit : Maybe Int }
    | PostDomainBlock { domain : String }
    | DeleteDomainBlock { domain : String }


{-| GET/POST /api/v1/endorsements

`GetEndorsements` results in an `AccountListEntity`.

`PostPinAccount` and `PostUnpinAccount` result in a `RelationshipEntity`.

-}
type EndorsementsReq
    = GetEndorsements
    | PostPinAccount { id : String }
    | PostUnpinAccount { id : String }


{-| GET/POST /api/v1/favourites

`GetFavourites` results in a `StatusListEntity`.

`PostFavourite` and `PostUnfavorite` result in a `StatusEntity`.

-}
type FavouritesReq
    = GetFavourites { limit : Maybe Int }
    | PostFavourite { id : String }
    | PostUnfavourite { id : String }


{-| GET/POST/PUT /api/v1/filters

`GetFilters` results in a `FilterListEntity`.

`PostFilter`, `GetFilter`, and `PutFilter` result in a `FilterEntity`.

`DeleteFilter` results in `NoEntity`.

-}
type FiltersReq
    = GetFilters
    | PostFilter
        { phrase : String
        , context : List FilterContext
        , irreversible : Bool
        , whole_word : Bool
        , expires_in : Maybe Int
        }
    | GetFilter { id : String }
    | PutFilter
        { id : String
        , phrase : String
        , context : List FilterContext
        , irreversible : Bool
        , whole_word : Bool
        , expires_in : Maybe Int
        }
    | DeleteFilter { id : String }


{-| GET/POST /api/v1/follow\_requests

`GetFollowRequest` results in an `AccountEntity`.

`PostAuthorizeFollow` and `PostRejectFollow` result in `NoEntity`.

-}
type FollowReq
    = GetFollowRequest { limit : Maybe Int }
    | PostAuthorizeFollow { id : String }
    | PostRejectFollow { id : String }


{-| GET/DELETE /api/v1/suggestions

`GetFollowSuggestions` results in an `AccountListEntity`.

`DeleteFollowSuggestions` results in `NoEntity`.

-}
type FollowSuggestionsReq
    = GetFollowSuggestions
    | DeleteFollowSuggestions { account_id : String }


{-| GET/POST/PUT/DELETE /api/v1/lists

`GetLists` and `GetAccountLists` result in a `ListEntityEntity`.

`GetListAccounts` results in an `AccountListEntity`.

`GetList`, `PostList`, and `PutList` result in a `ListEntity`.

`PostListAccounts` and `DeleteListAccounts` result in `NoEntity`.

-}
type ListsReq
    = GetLists
    | GetAccountLists { id : String }
    | GetListAccounts
        { id : String
        , limit : Maybe Int
        }
    | GetList { id : String }
    | PostList { title : String }
    | PutList
        { id : String
        , title : String
        }
    | DeleteList { id : String }
    | PostListAccounts
        { id : String
        , account_ids : List String
        }
    | DeleteListAccounts
        { id : String
        , account_ids : List String
        }


{-| GET/POST /api/v1/media

`PostMedia` and `PutMedia` result in an `AttachmentEntity`.

-}
type MediaAttachmentsReq
    = PostMedia
        { file : File
        , description : Maybe String
        , focus : Maybe Entity.Focus
        }
    | PutMedia
        { id : String
        , description : Maybe String
        , focus : Maybe Entity.Focus
        }


{-| GET/POST /api/v1/mutes

`GetAccountMutes` results in an `AccountListEntity`.

`PostAccountMute` and `PostAccountUnmute` result in a `RelationshipEntity`.

`PostStatusMute` and `PostStatusUnmute` result in a `StatusEntity`.

-}
type MutesReq
    = GetAccountMutes { limit : Maybe Int }
    | PostAccountMute
        { id : String
        , notifications : Bool
        }
    | PostAccountUnmute { id : String }
    | PostStatusMute { id : String }
    | PostStatusUnmute { id : String }


{-| GET/POST /api/v1/notifications

`GetNotifications` results in a `NotificationListEntity`.

`GetNotification` results in `NotificationEntity`.

`PostClearNotifications` and `PostDismissNotifications` result in `NoEntity`.

This doesn't yet define requests for "POST /api/v1/push/subscription",
"GET /api/v1/push/subscription", "PUT /api/v1/push/subscription", or
"DELETE /api/v1/push/subscription".

-}
type NotificationsReq
    = GetNotifications
        { paging : Maybe Paging
        , exclude_types : List Entity.NotificationType
        , account_id : Maybe String
        }
    | GetNotification { id : String }
    | PostClearNotifications
    | PostDismissNotification { id : String }



--- TODO:
--- | PostPushSubscription, GetPushSubscription
--- | PutPushSubscription, DeletePushSubscription


{-| GET/POST /api/v1/polls

`GetPoll` and `PostVotes` result in a `PollInstance`.

`GetPoll` does not require an authentication token.

-}
type PollsReq
    = GetPoll { id : String }
    | PostVotes
        { id : String
        , choices : List Int
        }


{-| POST /api/v1/reports

`PostReports` results in `NoInstance`.

-}
type ReportsReq
    = PostReports
        { account_id : String
        , status_ids : List String
        , comment : Maybe String
        , forward : Bool
        }


{-| GET/PUT /api/v1/scheduled\_statuses

`GetScheduledStatuses` results in a `ScheduledStatusListInstance`.

`GetScheduledStatus` and `PutScheduledStatus` result in a `ScheduledStatusInstance`.

`DeleteScheduledStatus` results in `NoInstance`.

-}
type ScheduledStatusesReq
    = GetScheduledStatuses
    | GetScheduledStatus { id : String }
    | PutScheduledStatus
        { id : String
        , scheduled_at : Maybe UnixTimestamp
        }
    | DeleteScheduledStatus { id : String }


{-| GET/POST /api/v1/search

`GetSearch` results in a `ResultsInstance`.

-}
type SearchReq
    = GetSearch
        { q : String
        , resolve : Bool
        , limit : Maybe Int
        , offset : Maybe Int
        , following : Bool
        }


{-| Define a Poll as part of a posted new Status
-}
type alias PollDefinition =
    { options : List String
    , expires_in : Int
    , multiple : Bool
    , hide_totals : Bool
    }


{-| GET/POST /api/v1/statuses

`GetStatus`, `PostStatus`, `PostReblogStatus`, `PostUnreblogStatus`, `PostPinStatus`, and `PostUnpinStatus` result in a `StatusEntity`.

`GetStatusContext` results in a `ContextEntity`.

`GetStatusCard` results in a `CardEntity`.

`GetStatusRebloggedBy` and `GetStatusFavouritedBy` result in an `AccountListEntity`.

`DeleteStatus` results in `NoEntity`.

The `GetXxx` requests require no authentication token.

`PostStatus` should be accompanied by an `Idempotency-Key` header,
which you can create with `IdempotencyKeyHeader`.
See <https://stripe.com/blog/idempotency>.

-}
type StatusesReq
    = GetStatus { id : String }
    | GetStatusContext { id : String }
    | GetStatusCard { id : String }
    | GetStatusRebloggedBy
        { id : String
        , limit : Maybe Int
        }
    | GetStatusFavouritedBy
        { id : String
        , limit : Maybe Int
        }
      -- Need to handle unique Idempotency-Key header
    | PostStatus
        { status : Maybe String
        , in_reply_to_id : Maybe String
        , media_ids : List String
        , poll : Maybe PollDefinition

        -- If included, then sensitive will be passed as true
        , spoiler_text : Maybe String
        , visibility : Entity.Visibility
        , scheduled_at : Maybe Entity.Datetime
        , language : Maybe Entity.ISO6391
        }
    | DeleteStatus { id : String }
    | PostReblogStatus { id : String }
    | PostUnreblogStatus { id : String }
    | PostPinStatus { id : String }
    | PostUnpinStatus { id : String }


{-| GET/POST /api/v1/timelines

`GetHomeTimeline`, `GetPublicTimeline`, `GetTagTimeline`, and `GetListTimeline` result in a `StatusListEntity`.

`GetConversations` results in a `ConversationListEntity`.

`GetPublicTimeline` and `GetTagTimeline` do not require an authentication token.

-}
type TimelinesReq
    = GetHomeTimeline
        { paging : Maybe Paging
        }
    | GetConversations
        { paging : Maybe Paging
        }
    | GetPublicTimeline
        { local : Bool
        , only_media : Bool
        , paging : Maybe Paging
        }
    | GetTagTimeline
        { hashtag : String
        , local : Bool
        , only_media : Bool
        , paging : Maybe Paging
        }
    | GetListTimeline
        { list_id : String
        , paging : Maybe Paging
        }


{-| A response from an API request.

The `request` is a copy of the `Request` that was sent over the wire,
for cases where that isn't obvious from the `Entity` tag.

The `metadata` is `Http.Metadata` for a successful request, mostly so
you can get to the headers, if you need them.

-}
type alias Response =
    { request : Request
    , metadata : Http.Metadata
    , entity : Entity
    }


{-| Used to create the HTTP URL and fill in its authentication token.

It's the host name for the URL.

Example `server`: "mastodon.social".

A few requests do not require a token. Most do, and will error if you don't include one.

-}
type alias ServerInfo =
    { server : String
    , authorization : Maybe String
    }


apiUrlPrefix : String
apiUrlPrefix =
    "/api/v1/"


{-| Prevent misspellings of the URL components.
-}
apiReq =
    { accounts = "accounts"
    , apps = "apps"
    , blocks = "blocks"
    , custom_emojis = "custom_emojis"
    , domain_blocks = "domain_blocks"
    , endorsements = "endorsements"
    , favourites = "favourites"
    , filters = "filters"
    , follow_requests = "follow_requests"
    , suggestions = "suggestions"
    , instance = "instance"
    , lists = "lists"
    , media = "media"
    , mutes = "mutes"
    , notifications = "notifications"
    , polls = "polls"
    , reports = "reports"
    , scheduled_statuses = "scheduled_statuses"
    , search = "search"
    , statuses = "statuses"
    , timelines = "timelines"
    , trends = "trends"
    , conversations = "conversations"
    }


{-| Encodes an error from the server request.

Same as `Http.Error`, but includes `Http.Metadata` when it's available.

-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody Http.Metadata String


{-| Represent an HTTP request.

Usually, you will let `serverRequest` create one of these internally.

Sometimes, however, you need to create one yourself, or call
`requestToRawRequest` to make one, make changes to it, and then call
`rawRequestToCmd`.

-}
type alias RawRequest =
    { method : String
    , authorization : Maybe String
    , url : String
    , headers : List Http.Header
    , body : Http.Body
    , request : Request
    , decoder : Decoder Entity
    }


{-| Create an HTTP request for the server.

The `id` is whatever you need, besides the `Request`, to identify the returned
`Error` or `Response`.

You will often pass `[]` for headers, but including a "User-Agent" header
is usually a good idea. For example, <https://mammudeck.com> uses:

    [ Mastodon.Request.userAgentHeader "Mammudeck" ]

-}
serverRequest : (id -> Result Error Response -> msg) -> List Http.Header -> ServerInfo -> id -> Request -> Cmd msg
serverRequest tagger headers serverInfo id request =
    requestToRawRequest headers serverInfo request
        |> rawRequestToCmd (tagger id)


{-| Convert a `RawRequest` into a `Cmd`.

You will usually not call this yourself, but let `serverRequest` do it internally.

Sometimes, however, you need to create a `RawRequest`, by hand or by calling
`requestToRawRequest`, then pass it here to turn it into a `Cmd`.

-}
rawRequestToCmd : (Result Error Response -> msg) -> RawRequest -> Cmd msg
rawRequestToCmd tagger rawRequest =
    Task.attempt tagger <| rawRequestToTask rawRequest


{-| Same as rawRequestToCmd, but returns a `Task`.

`rawRequestToCmd` could be defined as:

    rawRequestToCmd : (Result Error Response -> msg) -> RawRequest -> Cmd msg
    rawRequestToCmd tagger rawRequest =
        Task.attempt tagger <|
            rawRequestToTask rawRequest

-}
rawRequestToTask : RawRequest -> Task Error Response
rawRequestToTask rawRequest =
    if rawRequest.url == "" then
        Task.fail <| BadUrl "Empty URL"

    else
        Http.task
            { method = rawRequest.method
            , headers =
                case rawRequest.authorization of
                    Nothing ->
                        rawRequest.headers

                    Just auth ->
                        Http.header "Authorization" auth
                            :: rawRequest.headers
            , url = rawRequest.url
            , body = rawRequest.body
            , resolver = Http.stringResolver <| processResponse rawRequest
            , timeout = Nothing
            }


processResponse : RawRequest -> Http.Response String -> Result Error Response
processResponse rawRequest response =
    case response of
        Http.BadUrl_ s ->
            Err <| BadUrl s

        Http.Timeout_ ->
            Err <| Timeout

        Http.NetworkError_ ->
            Err <| NetworkError

        Http.BadStatus_ metadata body ->
            Err <| BadStatus metadata body

        Http.GoodStatus_ metadata body ->
            case JD.decodeString rawRequest.decoder body of
                Err _ ->
                    Err <| BadBody metadata ("JSON decoding error on: " ++ body)

                Ok entity ->
                    Ok
                        { request = rawRequest.request
                        , metadata = metadata
                        , entity = entity
                        }


{-| Only required by GitHub that I know of, but can't hurt.

Pass whatever string describes your user agent. Or fake a common one.

-}
userAgentHeader : String -> Http.Header
userAgentHeader userAgent =
    Http.header "User-Agent" userAgent


{-| Create an "Idempotency-Key" header for use with `PostStatus`.
-}
idempotencyKeyHeader : String -> Http.Header
idempotencyKeyHeader key =
    Http.header "Idempotency-Key" key


{-| An empty raw request.

Exposed only for testing in `elm repl`.

-}
emptyRawRequest : RawRequest
emptyRawRequest =
    { method = m.get
    , authorization = Nothing
    , url = ""
    , headers = []
    , body = Http.emptyBody
    , request = InstanceRequest
    , decoder = JD.fail "Unspecified decoder"
    }


{-| For testing in `elm repl`.
-}
emptyServerInfo : ServerInfo
emptyServerInfo =
    { server = "mastodon.social"
    , authorization = Nothing
    }


{-| Convert a Request into a RawRequest.

You will usually not call this yourself, but let `serverRequest` do it internally.

Sometimes, however, you need to call this to create a `RawRequest`, modify it,
and then call `rawRequestToCmd` to turn it into a `Cmd`.

-}
requestToRawRequest : List Http.Header -> ServerInfo -> Request -> RawRequest
requestToRawRequest headers serverInfo request =
    let
        raw =
            { emptyRawRequest
                | authorization = serverInfo.authorization
                , headers = headers
                , request = request
            }

        res =
            case request of
                AccountsRequest req ->
                    accountsReq req raw

                AppsRequest req ->
                    appsReq req raw

                BlocksRequest req ->
                    blocksReq req raw

                CustomEmojisRequest ->
                    customEmojisRequest raw

                DomainBlocksRequest req ->
                    domainBlocksReq req raw

                EndorsementsRequest req ->
                    endorsementsReq req raw

                FavouritesRequest req ->
                    favouritesReq req raw

                FiltersRequest req ->
                    filtersReq req raw

                FollowRequest req ->
                    followReq req raw

                FollowSuggestionsRequest req ->
                    followSuggestionsReq req raw

                InstanceRequest ->
                    instanceReq raw

                ListsRequest req ->
                    listsReq req raw

                MediaAttachmentsRequest req ->
                    mediaAttachmentsReq req raw

                MutesRequest req ->
                    mutesReq req raw

                NotificationsRequest req ->
                    notificationsReq req raw

                PollsRequest req ->
                    pollsReq req raw

                ReportsRequest req ->
                    reportsReq req raw

                ScheduledStatusesRequest req ->
                    scheduledStatusesReq req raw

                SearchRequest req ->
                    searchReq req raw

                StatusesRequest req ->
                    statusesReq req raw

                TimelinesRequest req ->
                    timelinesReq req raw

                TrendsRequest ->
                    trendsReq raw
    in
    { res
        | url = "https://" ++ serverInfo.server ++ apiUrlPrefix ++ res.url
    }



---
--- Some utility functions
---


qps : List (Maybe QueryParameter) -> List QueryParameter
qps params =
    List.filterMap identity params


sp : String -> Maybe String -> Maybe QueryParameter
sp name value =
    case value of
        Nothing ->
            Nothing

        Just v ->
            Just <| Builder.string name v


ip : String -> Maybe Int -> Maybe QueryParameter
ip name value =
    case value of
        Nothing ->
            Nothing

        Just v ->
            Just <| Builder.int name v


bp : String -> Bool -> Maybe QueryParameter
bp name value =
    if value then
        Just <| Builder.string name "true"

    else
        Nothing


bpt : String -> Bool -> Maybe QueryParameter
bpt name value =
    if value then
        Nothing

    else
        Just <| Builder.string name "false"


m =
    { get = "GET"
    , post = "POST"
    , delete = "DELETE"
    , put = "PUT"
    , patch = "PATCH"
    }


pagingParameters : Maybe Paging -> List QueryParameter
pagingParameters paging =
    case paging of
        Nothing ->
            []

        Just { max_id, since_id, min_id, limit } ->
            qps
                [ sp "max_id" max_id
                , sp "since_id" since_id
                , sp "min_id" min_id
                , ip "limit" limit
                ]


idListValue : List String -> String
idListValue ids =
    let
        idlist =
            List.map (\s -> "'" ++ s ++ "'") ids
                |> String.join ","
    in
    "[" ++ idlist ++ "]"


decoders =
    { account = ED.accountDecoder |> JD.map AccountEntity
    , accountList = JD.list ED.accountDecoder |> JD.map AccountListEntity
    , status = ED.statusDecoder |> JD.map StatusEntity
    , statusList = JD.list ED.statusDecoder |> JD.map StatusListEntity
    , relationship = ED.relationshipDecoder |> JD.map RelationshipEntity
    , relationshipList =
        JD.list ED.relationshipDecoder
            |> JD.map RelationshipListEntity
    , app = ED.appDecoder |> JD.map AppEntity
    , emoji = ED.emojiDecoder |> JD.map EmojiEntity
    , emojiList = JD.list ED.emojiDecoder |> JD.map EmojiListEntity
    , stringList = JD.list JD.string |> JD.map StringListEntity
    , ignore = JD.succeed NoEntity
    , filter = ED.filterDecoder |> JD.map FilterEntity
    , filterList = JD.list ED.filterDecoder |> JD.map FilterListEntity
    , instance = ED.instanceDecoder |> JD.map InstanceEntity
    , listEntity = ED.listEntityDecoder |> JD.map ListEntityEntity
    , listEntityList = JD.list ED.listEntityDecoder |> JD.map ListEntityListEntity
    , attachment = ED.attachmentDecoder |> JD.map AttachmentEntity
    , notification = ED.notificationDecoder |> JD.map NotificationEntity
    , notificationList =
        JD.list ED.notificationDecoder
            |> JD.map NotificationListEntity
    , poll = ED.pollDecoder |> JD.map PollEntity
    , scheduledStatus = ED.scheduledStatusDecoder |> JD.map ScheduledStatusEntity
    , scheduledStatusList =
        JD.list ED.scheduledStatusDecoder
            |> JD.map ScheduledStatusListEntity
    , results = ED.resultsDecoder |> JD.map ResultsEntity
    , context = ED.contextDecoder |> JD.map ContextEntity
    , card = ED.cardDecoder |> JD.map CardEntity
    , conversationList =
        JD.list ED.conversationDecoder
            |> JD.map ConversationListEntity
    , tagList =
        JD.list ED.tagDecoder |> JD.map TagListEntity
    }



---
--- Modify the incoming RawRequest to perform the requested operation.
--- This means at least setting the `method`, `url`, and `decoder`.
--- For POST, PUT, and PATCH requests, this also sets the `body`,
--- usually to a JSON string.
--- The `url` value is just the part after "https://<server>/api/v1/"
---


accountsReq : AccountsReq -> RawRequest -> RawRequest
accountsReq req res =
    let
        r =
            apiReq.accounts
    in
    case req of
        GetAccount { id } ->
            { res
                | url = relative [ r, id ] []
                , decoder = decoders.account
            }

        GetVerifyCredentials ->
            { res
                | url = relative [ r, "verify_credentials" ] []
                , decoder = decoders.account
            }

        PatchUpdateCredentials { display_name, note, avatar, header, locked, source, fields_attributes } ->
            { res
                | method = m.patch
                , url = relative [ r, "update_credentials" ] []
                , body =
                    Http.multipartBody <|
                        List.concat
                            [ case display_name of
                                Nothing ->
                                    []

                                Just name ->
                                    [ Http.stringPart "display_name" name ]
                            , case note of
                                Nothing ->
                                    []

                                Just n ->
                                    [ Http.stringPart "note" n ]
                            , case avatar of
                                Nothing ->
                                    []

                                Just av ->
                                    [ Http.filePart "avatar" av ]
                            , case header of
                                Nothing ->
                                    []

                                Just head ->
                                    [ Http.filePart "header" head ]
                            , [ Http.stringPart "locked" <|
                                    if locked then
                                        "true"

                                    else
                                        "false"
                              ]
                            , case source of
                                Nothing ->
                                    []

                                Just src ->
                                    List.concat
                                        [ case src.privacy of
                                            Nothing ->
                                                []

                                            Just privacy ->
                                                [ Http.stringPart "source[privacy]"
                                                    privacy
                                                ]
                                        , [ Http.stringPart "source[sensitive]" <|
                                                if src.sensitive then
                                                    "true"

                                                else
                                                    "false"
                                          ]
                                        , case src.language of
                                            Nothing ->
                                                []

                                            Just language ->
                                                [ Http.stringPart "source[language]"
                                                    language
                                                ]
                                        ]
                            , case fields_attributes of
                                Nothing ->
                                    []

                                Just attributes ->
                                    [ Http.stringPart "fields_attributes" <|
                                        (JE.list ED.encodeField attributes
                                            |> JE.encode 0
                                        )
                                    ]
                            ]
                , decoder = decoders.account
            }

        GetFollowers { id, limit } ->
            { res
                | url =
                    relative [ r, id, "followers" ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        GetFollowing { id, limit } ->
            { res
                | url =
                    relative [ r, id, "following" ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        GetStatuses { id, only_media, pinned, exclude_replies, paging, exclude_reblogs } ->
            { res
                | url =
                    relative [ r, id, "statuses" ] <|
                        List.concat
                            [ qps
                                [ bp "only_media" only_media
                                , bp "pinned" pinned
                                , bp "exclude_replies" exclude_replies
                                , bp "exclude_reblogs" exclude_reblogs
                                ]
                            , pagingParameters paging
                            ]
                , decoder = decoders.statusList
            }

        PostFollow { id, reblogs } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "follow" ] []
                , body =
                    if not reblogs then
                        -- Defaults to false
                        res.body

                    else
                        Http.jsonBody <|
                            JE.object
                                [ ( "reblogs", JE.bool reblogs ) ]
                , decoder = decoders.relationship
            }

        PostUnfollow { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "unfollow" ] []
                , decoder = decoders.relationship
            }

        -- This is sending ?id=['1'] (with the value url-encoded)
        -- That's how I read the documentation.
        -- The web clients I have send ?id[]=1
        -- I don't know how to get them to request multiple ids.
        -- So this may not work.
        GetRelationships { ids } ->
            { res
                | url =
                    relative [ r, "relationships" ]
                        [ Builder.string "id" <| idListValue ids ]
                , decoder = decoders.relationshipList
            }

        GetSearchAccounts { q, limit, resolve, following } ->
            { res
                | url =
                    relative [ r, "search" ] <|
                        qps
                            [ sp "q" <| Just q
                            , ip "limit" limit
                            , bp "resolve" resolve
                            , bp "following" following
                            ]
                , decoder = decoders.accountList
            }


appsReq : AppsReq -> RawRequest -> RawRequest
appsReq req res =
    let
        r =
            apiReq.apps
    in
    case req of
        PostApp { client_name, redirect_uris, scopes, website } ->
            { res
                | method = m.post
                , url =
                    relative [ r ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            (List.concat
                                [ [ ( "client_name", JE.string client_name )
                                  , ( "redirect_uris", JE.string redirect_uris )
                                  , ( "scopes"
                                    , JE.string <| String.join " " scopes
                                    )
                                  ]
                                , case website of
                                    Nothing ->
                                        []

                                    Just w ->
                                        [ ( "website", JE.string w ) ]
                                ]
                            )
                , decoder = decoders.app
            }

        GetVerifyAppCredentials ->
            { res
                | url =
                    relative [ r, "verify_credentials" ] []
                , decoder = decoders.app
            }


blocksReq : BlocksReq -> RawRequest -> RawRequest
blocksReq req res =
    let
        r =
            apiReq.blocks
    in
    case req of
        GetBlocks { limit } ->
            { res
                | url =
                    relative [ r ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        PostBlock { id } ->
            { res
                | method = m.post
                , url = relative [ r, id, "block" ] []
                , decoder = decoders.relationship
            }

        PostUnblock { id } ->
            { res
                | method = m.post
                , url = relative [ r, id, "unblock" ] []
                , decoder = decoders.relationship
            }


customEmojisRequest : RawRequest -> RawRequest
customEmojisRequest res =
    let
        r =
            apiReq.custom_emojis
    in
    { res
        | url = relative [ r ] []
        , decoder = decoders.emojiList
    }


domainBlocksReq : DomainBlocksReq -> RawRequest -> RawRequest
domainBlocksReq req res =
    let
        r =
            apiReq.domain_blocks
    in
    case req of
        GetDomainBlocks { limit } ->
            { res
                | url =
                    relative [ r ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.stringList
            }

        PostDomainBlock { domain } ->
            { res
                | method = m.post
                , url =
                    relative [ r ]
                        [ Builder.string "domain" domain ]
                , decoder = decoders.ignore
            }

        DeleteDomainBlock { domain } ->
            { res
                | method = m.delete
                , url =
                    relative [ r ]
                        [ Builder.string "domain" domain ]
                , decoder = decoders.ignore
            }


endorsementsReq : EndorsementsReq -> RawRequest -> RawRequest
endorsementsReq req res =
    let
        r =
            apiReq.endorsements
    in
    case req of
        GetEndorsements ->
            { res
                | url =
                    relative [ r ] []
                , decoder = decoders.accountList
            }

        PostPinAccount { id } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.accounts, id, "pin" ] []
                , decoder = decoders.relationship
            }

        PostUnpinAccount { id } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.accounts, id, "unpin" ] []
                , decoder = decoders.relationship
            }


favouritesReq : FavouritesReq -> RawRequest -> RawRequest
favouritesReq req res =
    let
        r =
            apiReq.favourites
    in
    case req of
        GetFavourites { limit } ->
            { res
                | url =
                    relative [ r ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.statusList
            }

        PostFavourite { id } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.statuses, id, "favourite" ] []
                , decoder = decoders.status
            }

        PostUnfavourite { id } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.statuses, id, "unfavourite" ] []
                , decoder = decoders.status
            }


filtersReq : FiltersReq -> RawRequest -> RawRequest
filtersReq req res =
    let
        r =
            apiReq.filters
    in
    case req of
        GetFilters ->
            { res
                | url =
                    relative [ r ] []
                , decoder = decoders.filterList
            }

        PostFilter { phrase, context, irreversible, whole_word, expires_in } ->
            { res
                | method = m.post
                , url =
                    relative [ r ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "phrase", JE.string phrase )
                            , ( "context", JE.list ED.encodeFilterContext context )
                            , ( "irreversible", JE.bool irreversible )
                            , ( "whole_word", JE.bool whole_word )
                            , ( "expires_in", encodeMaybe JE.int expires_in )
                            ]
                , decoder = decoders.filter
            }

        GetFilter { id } ->
            { res
                | url =
                    relative [ r, id ] []
                , decoder = decoders.filter
            }

        PutFilter { id, phrase, context, irreversible, whole_word, expires_in } ->
            { res
                | method = m.put
                , url =
                    relative [ r, id ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "phrase", JE.string phrase )
                            , ( "context", JE.list ED.encodeFilterContext context )
                            , ( "irreversible", JE.bool irreversible )
                            , ( "whole_word", JE.bool whole_word )
                            , ( "expires_in", encodeMaybe JE.int expires_in )
                            ]
                , decoder = decoders.filter
            }

        DeleteFilter { id } ->
            { res
                | method = m.delete
                , url =
                    relative [ r, id ] []
                , decoder = decoders.ignore
            }


followReq : FollowReq -> RawRequest -> RawRequest
followReq req res =
    let
        r =
            apiReq.follow_requests
    in
    case req of
        GetFollowRequest { limit } ->
            { res
                | url =
                    relative [ r ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        PostAuthorizeFollow { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "authorize" ] []
                , decoder = decoders.ignore
            }

        PostRejectFollow { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "reject" ] []
                , decoder = decoders.ignore
            }


followSuggestionsReq : FollowSuggestionsReq -> RawRequest -> RawRequest
followSuggestionsReq req res =
    let
        r =
            apiReq.suggestions
    in
    case req of
        GetFollowSuggestions ->
            { res
                | url =
                    relative [ r ] []
                , decoder = decoders.accountList
            }

        DeleteFollowSuggestions { account_id } ->
            { res
                | method = m.delete
                , url =
                    relative [ r, account_id ] []
                , decoder = decoders.ignore
            }


instanceReq : RawRequest -> RawRequest
instanceReq res =
    let
        r =
            apiReq.instance
    in
    { res
        | url =
            relative [ r ] []
        , decoder = decoders.instance
    }


listsReq : ListsReq -> RawRequest -> RawRequest
listsReq req res =
    let
        r =
            apiReq.lists
    in
    case req of
        GetLists ->
            { res
                | url =
                    relative [ r ] []
                , decoder = decoders.listEntityList
            }

        GetAccountLists { id } ->
            { res
                | url =
                    relative [ apiReq.accounts, id, "lists" ] []
                , decoder = decoders.listEntityList
            }

        GetListAccounts { id, limit } ->
            { res
                | url =
                    relative [ r, id, "accounts" ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        GetList { id } ->
            { res
                | url =
                    relative [ r, id ] []
                , decoder = decoders.listEntity
            }

        PostList { title } ->
            { res
                | method = m.post
                , url =
                    relative [ r ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "title", JE.string title ) ]
                , decoder = decoders.listEntity
            }

        PutList { id, title } ->
            { res
                | method = m.put
                , url =
                    relative [ r, id ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "title", JE.string title ) ]
                , decoder = decoders.listEntity
            }

        DeleteList { id } ->
            { res
                | method = m.delete
                , url =
                    relative [ r, id ] []
                , decoder = decoders.ignore
            }

        PostListAccounts { id, account_ids } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "accounts" ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "account_ids"
                              , JE.list JE.string account_ids
                              )
                            ]
                , decoder = decoders.ignore
            }

        DeleteListAccounts { id, account_ids } ->
            { res
                | method = m.delete
                , url =
                    relative [ r, id, "accounts" ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "account_ids"
                              , JE.list JE.string account_ids
                              )
                            ]
                , decoder = decoders.ignore
            }


mediaAttachmentsReq : MediaAttachmentsReq -> RawRequest -> RawRequest
mediaAttachmentsReq req res =
    let
        r =
            apiReq.media
    in
    case req of
        PostMedia { file, description, focus } ->
            { res
                | method = m.post
                , url =
                    relative [ r ] []
                , body =
                    Http.multipartBody <|
                        List.concat
                            [ [ Http.filePart "file" file ]
                            , case description of
                                Nothing ->
                                    []

                                Just desc ->
                                    [ Http.stringPart "description" desc ]
                            , case focus of
                                Nothing ->
                                    []

                                Just { x, y } ->
                                    [ Http.stringPart "focus" <|
                                        String.fromFloat x
                                            ++ ","
                                            ++ String.fromFloat y
                                    ]
                            ]
                , decoder =
                    decoders.attachment
            }

        PutMedia { id, description, focus } ->
            { res
                | method = m.put
                , url =
                    relative [ r, id ] []
                , body =
                    Http.multipartBody <|
                        List.concat
                            [ case description of
                                Nothing ->
                                    []

                                Just desc ->
                                    [ Http.stringPart "description" desc ]
                            , case focus of
                                Nothing ->
                                    []

                                Just { x, y } ->
                                    [ Http.stringPart "focus" <|
                                        String.fromFloat x
                                            ++ ","
                                            ++ String.fromFloat y
                                    ]
                            ]
                , decoder =
                    decoders.attachment
            }


mutesReq : MutesReq -> RawRequest -> RawRequest
mutesReq req res =
    let
        r =
            apiReq.mutes
    in
    case req of
        GetAccountMutes { limit } ->
            { res
                | url =
                    relative [ r ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        PostAccountMute { id, notifications } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.accounts, id, "mute" ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "notifications", JE.bool notifications ) ]
                , decoder = decoders.relationship
            }

        PostAccountUnmute { id } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.accounts, id, "unmute" ] []
                , decoder = decoders.relationship
            }

        PostStatusMute { id } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.statuses, id, "mute" ] []
                , decoder = decoders.status
            }

        PostStatusUnmute { id } ->
            { res
                | method = m.post
                , url =
                    relative [ apiReq.statuses, id, "unmute" ] []
                , decoder = decoders.status
            }


notificationsReq : NotificationsReq -> RawRequest -> RawRequest
notificationsReq req res =
    let
        r =
            apiReq.notifications
    in
    case req of
        GetNotifications { paging, exclude_types, account_id } ->
            { res
                | url =
                    relative [ r ] <|
                        List.concat
                            [ pagingParameters paging
                            , qps
                                [ case exclude_types of
                                    [] ->
                                        Nothing

                                    _ ->
                                        exclude_types
                                            |> List.map
                                                (ED.encodeNotificationType
                                                    >> JE.encode 0
                                                )
                                            |> idListValue
                                            |> Builder.string "exclude_types"
                                            |> Just
                                , sp "account_id" account_id
                                ]
                            ]
                , decoder = decoders.notificationList
            }

        GetNotification { id } ->
            { res
                | url =
                    relative [ r, id ] []
                , decoder = decoders.notification
            }

        PostClearNotifications ->
            { res
                | method = m.post
                , url =
                    relative [ r, "clear" ] []
                , decoder = decoders.ignore
            }

        PostDismissNotification { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, "dismiss" ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "id", JE.string id ) ]
                , decoder = decoders.ignore
            }


pollsReq : PollsReq -> RawRequest -> RawRequest
pollsReq req res =
    let
        r =
            apiReq.polls
    in
    case req of
        GetPoll { id } ->
            { res
                | url =
                    relative [ r, id ] []
                , decoder = decoders.poll
            }

        PostVotes { id, choices } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "votes" ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            [ ( "choices", JE.list JE.int choices ) ]
                , decoder = decoders.poll
            }


reportsReq : ReportsReq -> RawRequest -> RawRequest
reportsReq req res =
    let
        r =
            apiReq.reports
    in
    case req of
        PostReports { account_id, status_ids, comment, forward } ->
            { res
                | method = m.post
                , url =
                    relative [ r ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            (List.concat
                                [ [ ( "account_id", JE.string account_id ) ]
                                , if status_ids == [] then
                                    []

                                  else
                                    [ ( "status_ids", JE.list JE.string status_ids ) ]
                                , case comment of
                                    Nothing ->
                                        []

                                    Just c ->
                                        [ ( "comment", JE.string c ) ]
                                , if forward then
                                    [ ( "forward", JE.bool True ) ]

                                  else
                                    []
                                ]
                            )
                , decoder = decoders.ignore
            }


scheduledStatusesReq : ScheduledStatusesReq -> RawRequest -> RawRequest
scheduledStatusesReq req res =
    let
        r =
            apiReq.scheduled_statuses
    in
    case req of
        GetScheduledStatuses ->
            { res
                | url =
                    relative [ r ] []
                , decoder = decoders.scheduledStatusList
            }

        GetScheduledStatus { id } ->
            { res
                | url =
                    relative [ r, id ] []
                , decoder = decoders.scheduledStatus
            }

        PutScheduledStatus { id, scheduled_at } ->
            { res
                | method = m.put
                , url =
                    relative [ r, id ] []
                , body =
                    case scheduled_at of
                        Nothing ->
                            Http.emptyBody

                        Just sa ->
                            Http.jsonBody <|
                                JE.object
                                    [ ( "scheduled_at", JE.string sa ) ]
                , decoder = decoders.scheduledStatus
            }

        DeleteScheduledStatus { id } ->
            { res
                | method = m.delete
                , url =
                    relative [ r, id ] []
                , decoder = decoders.ignore
            }


searchReq : SearchReq -> RawRequest -> RawRequest
searchReq req res =
    let
        r =
            apiReq.search
    in
    case req of
        GetSearch { q, resolve, limit, offset, following } ->
            { res
                | url =
                    relative [ r ] <|
                        qps
                            [ bp "resolve" resolve
                            , ip "limit" limit
                            , ip "offset" offset
                            , bp "following" following
                            ]
                , decoder = decoders.results
            }


statusesReq : StatusesReq -> RawRequest -> RawRequest
statusesReq req res =
    let
        r =
            apiReq.statuses
    in
    case req of
        GetStatus { id } ->
            { res
                | url =
                    relative [ r, id ] []
                , decoder = decoders.status
            }

        GetStatusContext { id } ->
            { res
                | url =
                    relative [ r, id, "context" ] []
                , decoder = decoders.context
            }

        GetStatusCard { id } ->
            { res
                | url =
                    relative [ r, id, "card" ] []
                , decoder = decoders.card
            }

        GetStatusRebloggedBy { id, limit } ->
            { res
                | url =
                    relative [ r, id, "reblogged_by" ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        GetStatusFavouritedBy { id, limit } ->
            { res
                | url =
                    relative [ r, id, "favourited_by" ] <|
                        qps [ ip "limit" limit ]
                , decoder = decoders.accountList
            }

        -- Caller needs to handle unique Idempotency-Key header
        PostStatus { status, in_reply_to_id, media_ids, poll, spoiler_text, visibility, scheduled_at, language } ->
            -- If spoiler_text is included, then sensitive will be passed as true
            { res
                | method = m.post
                , url = relative [ r ] []
                , body =
                    Http.jsonBody <|
                        JE.object
                            (List.concat
                                [ case status of
                                    Nothing ->
                                        []

                                    Just s ->
                                        [ ( "status", JE.string s ) ]
                                , case in_reply_to_id of
                                    Nothing ->
                                        []

                                    Just id ->
                                        [ ( "in_reply_to_id", JE.string id ) ]
                                , if media_ids == [] then
                                    []

                                  else
                                    [ ( "media_ids", JE.list JE.string media_ids ) ]
                                , case poll of
                                    Nothing ->
                                        []

                                    Just p ->
                                        [ ( "poll"
                                          , JE.object
                                                [ ( "options", JE.list JE.string p.options )
                                                , ( "expires_in", JE.int p.expires_in )
                                                , ( "multiple", JE.bool p.multiple )
                                                , ( "hide_totals", JE.bool p.hide_totals )
                                                ]
                                          )
                                        ]
                                , case spoiler_text of
                                    Nothing ->
                                        []

                                    Just text ->
                                        [ ( "sensitive", JE.bool True )
                                        , ( "spoiler_text", JE.string text )
                                        ]
                                , [ ( "visibility", ED.encodeVisibility visibility ) ]
                                , case scheduled_at of
                                    Nothing ->
                                        []

                                    Just timestamp ->
                                        [ ( "scheduled_at", JE.string timestamp ) ]
                                , case language of
                                    Nothing ->
                                        []

                                    Just lang ->
                                        [ ( "language", JE.string lang ) ]
                                ]
                            )
                , decoder = decoders.status
            }

        DeleteStatus { id } ->
            { res
                | method = m.delete
                , url =
                    relative [ r, id ] []
                , decoder = decoders.ignore
            }

        PostReblogStatus { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "reblog" ] []
                , decoder = decoders.status
            }

        PostUnreblogStatus { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "unreblog" ] []
                , decoder = decoders.status
            }

        PostPinStatus { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "pin" ] []
                , decoder = decoders.status
            }

        PostUnpinStatus { id } ->
            { res
                | method = m.post
                , url =
                    relative [ r, id, "unpin" ] []
                , decoder = decoders.status
            }


timelinesReq : TimelinesReq -> RawRequest -> RawRequest
timelinesReq req res =
    let
        r =
            apiReq.timelines
    in
    case req of
        GetHomeTimeline { paging } ->
            { res
                | url =
                    relative [ r, "home" ] <|
                        pagingParameters paging
                , decoder = decoders.statusList
            }

        GetConversations { paging } ->
            { res
                | url =
                    relative [ apiReq.conversations ] <|
                        pagingParameters paging
                , decoder = decoders.conversationList
            }

        GetPublicTimeline { local, only_media, paging } ->
            { res
                | url =
                    relative [ r, "public" ] <|
                        List.concat
                            [ qps
                                [ bp "local" local
                                , bp "only_media" only_media
                                ]
                            , pagingParameters paging
                            ]
                , decoder = decoders.statusList
            }

        GetTagTimeline { hashtag, local, only_media, paging } ->
            { res
                | url =
                    relative [ r, "tag", hashtag ] <|
                        List.concat
                            [ qps
                                [ bp "local" local
                                , bp "only_media" only_media
                                ]
                            , pagingParameters paging
                            ]
                , decoder = decoders.statusList
            }

        GetListTimeline { list_id, paging } ->
            { res
                | url =
                    relative [ r, "list", list_id ] <|
                        pagingParameters paging
                , decoder = decoders.statusList
            }


{-| This one wasn't documented. I had to reverse engineer it.
-}
trendsReq : RawRequest -> RawRequest
trendsReq res =
    let
        r =
            apiReq.trends
    in
    { res
        | url =
            relative [ r ] []
        , decoder = decoders.tagList
    }
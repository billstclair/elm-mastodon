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


module Mastodon.Requests exposing
    ( ServerInfo, Request(..), Response, Error(..)
    , serverRequest
    , AccountsReq(..), AppsReq(..), BlocksReq(..), FavouritesReq(..)
    , FollowReq(..), MediaAttachmentsReq(..), MutesReq(..), NotificationsReq(..)
    , PollsReq(..), ReportsReq(..), SearchReq(..), StatusesReq(..), TimelinesReq(..)
    , Paging, SourceUpdate, PollDefinition
    , emptyRawRequest, emptyServerInfo, requestToRawRequest
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


# Testing

@docs emptyRawRequest, emptyServerInfo, requestToRawRequest

-}

import File exposing (File)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mastodon.EncodeDecode as ED exposing (encodeMaybe)
import Mastodon.Entities as Entities
    exposing
        ( Entity(..)
        , FilterContext(..)
        , UnixTimestamp
        )
import OAuthMiddleware exposing (ResponseToken)
import Task
import Url.Builder as Builder exposing (QueryParameter, relative)


{-| An API request.

Broken down as in the documentation.

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
    , sensitive : Maybe Bool
    , language : Maybe Entities.ISO6391
    }


{-| GET/POST/PATCH /api/v1/accounts

`GetAccount` does not require an authentication token.

-}
type AccountsReq
    = GetAccount { id : String }
    | GetVerifyCredentials
    | PatchUpdateCredentials
        { display_name : Maybe String
        , note : Maybe String
        , avatar : Maybe File
        , header : Maybe File
        , locked : Maybe Bool
        , source : Maybe SourceUpdate
        , fields_attributes : Maybe (List Entities.Field)
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
-}
type BlocksReq
    = GetBlocks { limit : Maybe Int }
    | PostBlock { id : String }
    | PostUnblock { id : String }


{-| GET/POST /api/v8/domain\_blocks
-}
type DomainBlocksReq
    = GetDomainBlocks { limit : Maybe Int }
    | PostDomainBlock { domain : String }
    | DeleteDomainBlock { domain : String }


{-| GET/POST /api/v1/endorsements
-}
type EndorsementsReq
    = GetEndorsements
    | PostPinAccount { id : String }
    | PostUnpinAccount { id : String }


{-| GET/POST /api/v1/favourites
-}
type FavouritesReq
    = GetFavourites { limit : Maybe Int }
    | PostFavourite { id : String }
    | PostUnfavourite { id : String }


{-| GET/POST/PUT /api/v1/filters
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
-}
type FollowReq
    = GetFollowRequest { limit : Maybe Int }
    | PostAuthorizeFollow { id : String }
    | PostRejectFollow { id : String }


{-| GET/DELETE /api/v1/suggestions
-}
type FollowSuggestionsReq
    = GetFollowSuggestions
    | DeleteFollowSuggestions { account_id : String }


{-| GET/POST/PUT/DELETE /api/v1/lists
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
-}
type MediaAttachmentsReq
    = PostMedia
        { file : File
        , description : Maybe String
        , focus : Maybe Entities.Focus
        }
    | PutMedia
        { id : String
        , description : Maybe String
        , focus : Maybe Entities.Focus
        }


{-| GET/POST /api/v1/mutes
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



-- TODO:
-- | PostPushSubscription, GetPushSubscription
-- | PutPushSubscription, DeletePushSubscription


{-| GET/POST /api/v1/notifications
-}
type NotificationsReq
    = GetNotifications
        { paging : Maybe Paging
        , exclude_types : List Entities.NotificationType
        , account_id : Maybe String
        }
    | GetNotification { id : String }
    | PostClearNotifications
    | PostDismissNotification { id : String }


{-| GET/POST /api/v1/polls

`GetPoll` does not require an authentication token.

-}
type PollsReq
    = GetPoll { id : String }
    | PostVotes
        { id : String
        , choices : List Int
        }


{-| POST /api/v1/reports
-}
type ReportsReq
    = PostReports
        { account_id : String
        , status_ids : List String
        , comment : String
        , forward : Bool
        }


{-| GET/PUT /api/v1/scheduled\_statuses
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

The `GetXxx` requests require no authentication token, unless the status has `Private` visibility.

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
        , visibility : Entities.Visibility
        , scheduled_at : Maybe Entities.Datetime
        , language : Maybe Entities.ISO6391
        }
    | DeleteStatus { id : String }
    | PostReblogStatus { id : String }
    | PostUnreblogStatus { id : String }
    | PostPinStatus { id : String }
    | PostUnpinStatus { id : String }


{-| GET/POST /api/v1/timelines

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
        { local : Bool
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

Example `server`: "mastodon.social". It's the host name for the URL.

A few requests do not require a token. Most do, and will error if you don't include one.

-}
type alias ServerInfo =
    { server : String
    , token : Maybe ResponseToken
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


type alias RawRequest =
    { method : String
    , token : Maybe ResponseToken
    , url : String
    , body : Http.Body
    , request : Request
    , decoder : Decoder Entity
    }


{-| Create an HTTP request for the server.

The `id` is whatever you need, besides the `Request`, to identify the returned
`Error` or `Response`

-}
serverRequest : (id -> Result Error Response -> msg) -> ServerInfo -> id -> Request -> Cmd msg
serverRequest tagger serverInfo id request =
    requestToRawRequest serverInfo request
        |> rawRequestToCmd (tagger id)


rawRequestToCmd : (Result Error Response -> msg) -> RawRequest -> Cmd msg
rawRequestToCmd tagger rawRequest =
    if rawRequest.method == "" then
        Cmd.none

    else
        Http.request
            { method = rawRequest.method
            , headers =
                case rawRequest.token of
                    Nothing ->
                        []

                    Just token ->
                        OAuthMiddleware.use token [ userAgentHeader ]
            , url = rawRequest.url
            , body = rawRequest.body
            , expect =
                Http.expectStringResponse tagger <| processResponse rawRequest
            , timeout = Nothing
            , tracker = Nothing
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
-}
userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Mammudeck"


{-| An empty raw request.

Exposed only for testing in `elm repl`.

-}
emptyRawRequest : RawRequest
emptyRawRequest =
    { method = ""
    , token = Nothing
    , url = ""
    , body = Http.emptyBody
    , request = InstanceRequest
    , decoder = JD.fail "Unspecified decoder"
    }


{-| For testing in `elm repl`.
-}
emptyServerInfo : ServerInfo
emptyServerInfo =
    { server = "mastodon.social"
    , token = Nothing
    }


{-| Exposed for testing in `elm repl`.
-}
requestToRawRequest : ServerInfo -> Request -> RawRequest
requestToRawRequest serverInfo request =
    let
        raw =
            { emptyRawRequest
                | token = serverInfo.token
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

                _ ->
                    -- TODO, all the other `Request` types
                    raw
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
    }



---
--- Modify the incoming RawRequest to perform the requested operation.
--- This means at least setting the `method`, `url`, and `decoder`.
--- For POST, PUT, and PATCH requests, this also sets the `body`,
--- usually to a JSON string.
--- The `url` value is just the part after "https://<server>/api/v1/"
---


accountsReq : AccountsReq -> RawRequest -> RawRequest
accountsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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

        PatchUpdateCredentials _ ->
            -- TODO
            rawreq

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
appsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
blocksReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
customEmojisRequest rawreq =
    let
        res =
            { rawreq | method = m.get }

        r =
            apiReq.custom_emojis
    in
    { res
        | url = relative [ r ] []
        , decoder = decoders.emojiList
    }


domainBlocksReq : DomainBlocksReq -> RawRequest -> RawRequest
domainBlocksReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
endorsementsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
favouritesReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
filtersReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
followReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
followSuggestionsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
instanceReq rawreq =
    let
        res =
            { rawreq | method = m.get }

        r =
            apiReq.instance
    in
    { res
        | url =
            relative [ r ] []
        , decoder = decoders.instance
    }


listsReq : ListsReq -> RawRequest -> RawRequest
listsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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
mediaAttachmentsReq req rawreq =
    let
        res =
            { rawreq | method = m.get }

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

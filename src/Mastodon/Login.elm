module Mastodon.Login exposing (login)

{-| Support for creating an `App` and logging in to a server.
-}

import Mastodon.Entity as Entity exposing (Account, Authorization)
import Mastodon.Request as Request exposing (Error(..))


{-| Get a token and, if possible, the logged in `Account` from the server.

Arguments are:

    login tagger server maybeAuthorization

If `maybeAuthorization` is not `Nothing`, will attempt to get an
`Account` using `authorization.token` from there. If that fails, will
attempt to mint a new token, using `authorization.clientId` and
`authorization.clientSecret`. If that fails, or if
`maybeAuthorization` is `Nothing`, will create a new `App` first.
If it succeeds in getting a token, the tagged `Result` will be `Ok`,
but its `Maybe Account` component may be `Nothing`, if that could not
be fetched with the token.

Usually, you will get an `Authorization` from persistent `localStorage`,
pass that here, and successfully receive the `Authorization` back, along
with the logged-in users's `Account`.

The full login procedure is as follows:

1.  Send "POST /api/v1/apps" to the Mastodon server, via a `PostApp` request.

2.  Receive the returned `client_id` and `client_secret`

3.  Redirect to `<server_url>/oauth/authorize?client_id=<client_id>&redirect_uri=<client_uri>&response_type=code&scope=<scopes>&state=<state>`

4.  The user enters login authorization information to the server web site.

5.  The server web site redirects to `<redirect_uri>?code=<code>&state=<state>`

6.  The `<state>` encodes the server uri, <client\_id>, <client\_secret>, and <scopes>.
    Use that to POST to `<server_url>/oauth/token`:

        POST /oauth/token HTTP/1.1
        Host: <server_url>
        Authorization: Basic `(Base64.encode (<client_id> ++ ":" ++ <client_secret>))`
        Content-Type: application/x-www-form-urlencoded

        grant_type=authorization_code&code=<code>&redirect_uri=<client_uri>

7.  Receive back token information, JSON-encoded:

        { "access_token":"cptGSO8ff7xKbBXlTTxH15bnxQS5b9erVUWi_n0_EGc",
          "token_type":"Bearer",
          "scope":"read",
          "created_at":1561845912
        }

-}
login : (Result Error ( Authorization, Maybe Account ) -> msg) -> String -> Maybe Authorization -> Cmd msg
login tagger server authorization =
    Cmd.none

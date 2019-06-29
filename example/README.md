This directory contains an example of using [`billstclair/elm-mastodon`](http://package.elm-lang.org/packages/billstclair/elm-mastodon/latest) package. To run the example, you first need to run a server, at a `redirectUri` that you've registered with an OAuth provider for the OAuth Authorization Code Grant Flow.

First, clone the code on your computer:

    cd ...
    git clone git@github.com:billstclair/elm-oauth-middleware.git
    cd elm-oauth-middleware/example

Learn how to configure and run the server in the [elm-oauth-middleware/server](https://github.com/billstclair/elm-oauth-middleware/tree/master/server) directory's README. Part of doing that will be to create an Authorization Code Grant Flow account with one or more OAuth providers. I have tested this code with mastodon.social, [some Pleroma instance], and develop.gab.com, but it should work with any Mastodon or Pleroma instance that supports Authorization Code Grant Flow.

If you will run the example only on a web server at a domain configured for your server, you're golden. To run from your development machine, you have to invent an unused domain name, enter it in the `redirectBackHosts` in the server's `config.json` file, add it to `/etc/hosts` on your development machine, [as instructed](https://github.com/billstclair/elm-oauth-middleware#development) in the top-level README, and use it in the `elm-reactor` startup below, where I'm calling it `oauth-client-dev.com`.

Next, you need to create an API configuration file for the example. Copy [`authorizations.json.template`](authorizations.json.template) to `authorizations.json`, and edit that file to contain your OAuth server(s) information.

    [ { "comment": "Any object containing one of more 'comment' properites is ignored."
        "comment": "To use one of the settings below, remove the"
        "comment": "'comment' line and fill in 'clientId' and 'redirectUri'.",
      },
      { "name": "OAuth Provider Name",
        "authorizationUri" : "https://example.com/outh/authorize",
        "tokenUri" : "https://example.com/oauth/access_token",
        "clientId" : "The client Id assigned to you by the OAuth Provider",
        "redirectUri" : "The redirect (callback) Uri you provided for that client Id",
        "scopes" : {"scope name 1": "scope 1",
                    "scope name 2": "scope 2"
                   }
      }
    ]

If there is a `comment` property in an object in the list, that entire object will be ignored. This allows you to comment your settings, and also allows commenting out an individual setting that you're not using. The provided `authorizations.json.template` contains commented-out real settings that you can use by removing the `comment` property, and filling in your `clientId` and `redirectUri`.

`name` appears in the example's "Provider" selector on the example web page.

`authorizationUri` is for the authorization provider. For mastodon.social, it is `https://github.com/login/oauth/authorize`.

`tokenUri` is also for the authorization provider. For mastodon.social, it is `https://github.com/login/oauth/access_token`.

`clientId` is the identifier assigned by the provider to your OAuth application. It is sort of your OAuth user name. Note that the client SECRET is NOT here. It is only on your token server, so that it will never be seen in a user's browser.

`redirectUri` is the web address of your server. It must exactly match the Uri registered with your OAuth provider.

`scopes` is a list of `name/scope` pairs, encoded as a JSON object. the `scope` part of those pairs is sent to the `authorizationUri` to request particular access permissions. The example currently uses only the first scope it encounters, so entering more than one isn't useful. I will probably eventually let you pick which scope(s) to use.

Now you can run the example with `elm-reactor`:

    cd .../elm-mastodon/example
    elm reactor -a oauth-client-dev.com
    
Aim your browser at http://oauth-client-dev.com:8000/example.elm

You can select a "Provider", and click "Login" to authenticate and get a token from your callback server. Then you can click "Get User" to send an API request for user information.

If you use a provider other than the four I put in `authorizations.json.template`, you'll have to add a `getUser` setting for your provider to the `apis` constant in `example.elm`.

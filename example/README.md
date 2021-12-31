This directory contains an example of using the [`billstclair/elm-mastodon`](http://package.elm-lang.org/packages/billstclair/elm-mastodon/latest) package.

The example is live at https://mammudeck.com/api

To run the example locally, first create a dummy domain, pointing to your local machine, in `/etc/hosts`:

    127.0.0.1	dev.mammudeck.com
    
Then, in a Bash shell:

    $ cd .../elm-mastodon/example
    $ elm reactor

Then aim your web browser at http://dev.mammudeck.com:8000/site/index.html

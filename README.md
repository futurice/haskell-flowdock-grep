# flowdock-grep

> Grep Flowdock logs!

## Installing

- install [`stack`](https://github.com/commercialhaskell/stack), [Downloads wiki page](https://github.com/commercialhaskell/stack/wiki/Downloads).
- clone this repository:

    ```
    git clone https://github.com/futurice/haskell-flowdock-grep.git
    cd haskell-flowdock-grep
    ```

- Build and install using stack:

    ```
    stack install
    ```

## Basic usage

On the first use, you'd need to tell `flowdock-grep` your [Flowdock token](https://flowdock.com/account/tokens):

```
flowdock-grep --token deadbeef my-org someflow something
```

Consequently you can omit the `--token` parameter.

Token and logs are stored in `$HOME/.flowdock-grep`.

### Example

```
flowdock-grep futurice open-source chilicorn | tail
...
13:28:53 <Susanne> :thumbsup: Thanks for remembering all sites also. :chilicorn: is awesome!
11:28:19 <ttur> (need a sad chilicorn emoticon)
11:28:30 <juhovh> chilicorn is never sad
```

# gimmel

UDP chat server
---

UDP packets that are received are decoded as JSON and then sent to every address that has
previously successfully delivered a message.

# howto

## terminal 1

`cd src/gimmel`
`stack run`

## terminal 2

### commands

`jo to=2 msg=hi | nc localhost -u 1337`

### output log (before 3)

`{"to":1,"msg":"hi"}`

### output log (after 3)

`{"to":1,"msg":"hi"}{"to":2,"msg":"hiback"}`

## terminal 3

### commands

`jo to=1 msg='hiback' | nc localhost -u 1337`

### output log

`{"to":1,"msg":"hiback"}`

# test dependencies

* https://github.com/jpmens/jo
* https://en.wikipedia.org/wiki/Netcat

# build dependencies

* https://docs.haskellstack.org/en/stable/README/

# dev dependencies

* https://code.visualstudio.com/
* https://www.haskell.org/ghcup/
* https://github.com/haskell/vscode-haskell
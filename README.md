# gimmel

UDP chat server

UDP packets that are received are decoded as JSON and then sent to every address that has
previously successfully delivered a message.

## howto

To run it:

1. **Start the server** in one terminal:
   ```bash
   stack run
   ```

2. **Send UDP packets** from a separate terminal. On Windows with Nmap's `ncat` (assuming IPv6):
   ```bash
   C:\Users\me>ncat -v -u -6 localhost 1337
   ```

   You can then type JSON messages like:
   ```json
   {"msg":"hello","to":0}
   ```

3. **Server operator can respond** via the TUI - simply type your message in the input box and press Enter to send it to all connected peers.

## test dependencies

* <https://github.com/jpmens/jo>
* <https://en.wikipedia.org/wiki/Netcat>

## build dependencies

* <https://docs.haskellstack.org/en/stable/README/>

## dev dependencies

* <https://code.visualstudio.com/>
* <https://www.haskell.org/ghcup/>
* <https://github.com/haskell/vscode-haskell>

[![Haskell CI](https://github.com/maxdeliso/gimmel/actions/workflows/ci.yml/badge.svg)](https://github.com/maxdeliso/gimmel/actions/workflows/ci.yml)

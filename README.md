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

   Or use the smart build script (automatically uses static builds on Linux with Nix):

   ```bash
   ./build.sh && stack exec gimmel-exe
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

## building static binaries

The build system automatically detects Nix on Linux and uses static builds when available. On Windows or Linux without Nix, it falls back to regular dynamic builds.

### Automatic static builds (recommended)

Simply use the smart build script:

```bash
./build.sh
```

This will:
- Use static builds on Linux when Nix is available
- Use regular builds on Windows or Linux without Nix

### Manual static builds

If you want to force a static build:

1. **Using the dedicated static build script** (recommended):

   ```bash
   ./build-static.sh
   ```

   This script uses [haskell.nix](https://github.com/input-output-hk/haskell.nix) with musl to build a fully static binary.

2. **Using Nix flake directly**:

   ```bash
   nix build .#static
   ```

   The binary will be in `result/bin/gimmel-exe`

3. **Using the static development shell**:

   ```bash
   nix develop .#static
   ```

   This gives you a shell with all static build dependencies configured.

The static build uses [haskell.nix](https://github.com/input-output-hk/haskell.nix) with `pkgsMusl` (musl-based) to create a fully static binary with no dynamic library dependencies. The binary will be located at:

```text
result/bin/gimmel-exe  # or the path printed by nix build
```

You can verify it's static with:

```bash
ldd result/bin/gimmel-exe
```

This should show "not a dynamic executable" for a fully static binary.

## dev dependencies

* <https://code.visualstudio.com/>
* <https://www.haskell.org/ghcup/>
* <https://github.com/haskell/vscode-haskell>

[![Haskell CI](https://github.com/maxdeliso/gimmel/actions/workflows/ci.yml/badge.svg)](https://github.com/maxdeliso/gimmel/actions/workflows/ci.yml)

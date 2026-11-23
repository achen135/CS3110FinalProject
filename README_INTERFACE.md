# QuickSketch — Interface/Setup

## Setup
 
These steps assume you already have the CS 3110 OPAM switch installed
(OCaml, `dune`, `ounit2`). The only extra library is
[`graphics`](https://github.com/ocaml/graphics), which opens the game
window. It needs an X11 display, so install the X11 pieces for your system
**first**, then install `graphics`.
 
> The automated tests (`dune test`) do not open a window. If you only need
> to run the tests, you still need `graphics` installed to build, but you
> do not need a working display.
 
### macOS
 
1. Install XQuartz (the X11 display for macOS), either with Homebrew:
```bash
brew install --cask xquartz
```
   or from <https://www.xquartz.org>.
2. **Log out of your Mac and log back in.** This is required: it is what
   sets the `DISPLAY` environment variable.
3. Install `pkg-config` (used to find the X11 libraries):
```bash
brew install pkg-config
```
4. Install `graphics`. If you installed it before XQuartz, use
   `opam reinstall graphics` instead.
```bash
opam install graphics
```
5. Open a **new** terminal and check that a display is set:
```bash
echo $DISPLAY
```
   It should print something like
   `/private/tmp/com.apple.launchd.XXXX/org.xquartz:0`.
 
### Linux (Ubuntu/Debian)
 
```bash
sudo apt install libx11-dev pkg-config
opam install graphics
```
 
Run the game from a terminal on your desktop, not over SSH.
 
### Windows (WSL)
 
1. Make sure WSL is up to date so it can show Linux windows (WSLg). In
   PowerShell:
```powershell
wsl --update
wsl --shutdown
```
   Then reopen your WSL terminal. On Windows 11, `echo $DISPLAY` should
   print `:0`. (Windows 10 has no WSLg; install and start an X server such
   as VcXsrv instead.)
2. In WSL:
```bash
sudo apt install libx11-dev pkg-config
opam install graphics
```
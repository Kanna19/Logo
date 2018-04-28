# Logo with Haskell
This project is a haskell implementation of the popular educational programming language logo.

You can learn more about logo in this wikipedia [article](https://en.wikipedia.org/wiki/Logo_(programming_language)).

# Installation Instructions
  Install Haskell Platform.
## Installing haskell platform (for Ubuntu)
```
$ sudo apt-get install haskell-platform
```

For other distributions or operating systems refer to this haskell [page](https://www.haskell.org/platform/).

*Note that all work on this project (testing and developing) is currently being done only in Ubuntu 16.04 or higher.*

## Installing Gtk2hs (for Ubuntu)
Gtk2Hs is a GUI library for Haskell based on GTK+. GTK+ is an extensive and mature multi-platform toolkit for creating graphical user interfaces.

This project uses the newer gtk+3 bindings
```
$ sudo apt-get install libghc-gtk3-dev
```
Clone the repo
```
$ git clone https://github.com/IITH-SBJoshi/haskell-1.git
```
Change the working directory

```
$ cd haskell-1
```

## Building with Cabal
To configure and build using cabal, first download the repo and cd into it. Then execute the following commands

```
$ cabal configure
$ cabal build
```
Once build is successfull, installation can be done using Cabal

```
$ cabal install
```

The executable ./logo can be found in the /dist/build/logo directory

```
$ cd dist/build/logo
```

Execute using
```
$ ./logo
```

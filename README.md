# leblog

![GitHub Workflow Status](https://img.shields.io/github/workflow/status/Phosphorus15/leblog/Haskell%20CI?logo=github)
![Travis (.com)](https://img.shields.io/travis/com/Phosphorus15/leblog?label=Travis%20CI&logo=travis)

Public bloggin' system for everyone, powered by Haskell Spock.

**Face the RealWorld**

## Build instruction

To build this blogging system, you'll need:

+ Haskell stack (Recommended `2.1.3` with ghc `8.6.5` )
+ Node.js & npm
+ libpq-dev (For building postgresql interfaces)

Following step shows how a full build can be completed:

```bash
git clone https://github.com/Phosphorus15/leblog.git
# clone this repository
git submodule init & git submodule update
# initialize and update submodule
make
# compile front-end and integrate with back-end (see `Makefile`)
stack run
# optional - run the server just built 
```

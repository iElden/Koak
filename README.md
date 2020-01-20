# Introduction

Working with LLVM in Haskell can be a bit tricky, as one needs to get:

- A proper LLVM suite.
- *Matching* Haskell bindings.

This repository demonstrates how to achieve this using the Nix package manager.

# Getting started

1. [Install nix](https://nixos.org/nix/download.html). The simplest way to do so
   is to paste `curl https://nixos.org/nix/install | sh` in a terminal and
   follow the instructions.
2. Clone this repository and `cd` into it:
   ```
   git clone https://github.com/gdeest/llvm-demo.git
   cd llvm-demo
   ```
3. Type `nix-shell` to enter a shell with all required dependencies:
   ```
   nix-shell
   ```
3. Type `cabal build` to build the project.
4. Type `cabal run` to run the sample executable.

That's all folks ! Modify to your needs.

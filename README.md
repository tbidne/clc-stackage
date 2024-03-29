# `clc-stackage`

## How to?

This is a meta-package to facilitate impact assessment for [CLC proposals](https://github.com/haskell/core-libraries-committee). The package `clc-stackage.cabal` lists almost entire Stackage as `build-depends`, so that `cabal build` transitively compiles them all.

An impact assessment is due when

1. Proposal makes a breaking change according to [PVP](https://pvp.haskell.org/).
2. Proposal exports a new entity from `Prelude` or other modules, described in [Haskell Report](https://www.haskell.org/onlinereport/haskell2010/haskellpa2.html#x20-192000II).
3. On discretion of CLC members.

The procedure is as follows:

1. Rebase changes, mandated by your proposal, atop of `ghc-9.8` branch.
2. Compile a patched GHC, say, `~/ghc/_build/stage1/bin/ghc`.
3. `git clone https://github.com/Bodigrim/clc-stackage`, then `cd clc-stackage`.
4. Run `cabal build clc-stackage -w ~/ghc/_build/stage1/bin/ghc --keep-going` and wait for a long time.
  * On a recent Macbook Air it takes around 12 hours, YMMV.
  * You can interrupt `cabal` at any time and rerun again later.
  * Consider setting `--jobs` to retain free CPU cores for other tasks.
  * Full build requires roughly 7 Gb of free disk space.
5. If any packages fail to compile:
  * copy them locally using `cabal unpack`,
  * patch to confirm with your proposal,
  * link them from `packages` section of `cabal.project`,
  * return to Step 4.
6. When everything finally builds, get back to CLC with a list of packages affected and patches required.

## Getting dependencies via `nix`
For Linux based systems, there's a provided `flake.nix` and `shell.nix` to get a nix shell
with an approximation of the required dependencies (cabal itself, C libs) to build `clc-stackage`.

Note that it is not actively maintained, so it may require some tweaking to get working, and conversely, it may have some redundant dependencies.

## Misc

* Your custom GHC will need to be on the PATH to build the `stack` library i.e.

  ```
  export PATH=/path/to/custom/ghc/stage1/bin/:$PATH
  ```

  Nix users can uncomment (and modify) this line in the `flake.nix`.

## Sequential Builds

There is an exe `sequential` that can be used to build all packages on a level more granular than everything at once. This still requires building everything so it does not save work (in fact it can be significantly slower). Nevertheless it exists for when building every package simultaneously is not feasible (e.g. lack of memory, nix issues).

First build the exe. Ideally you will want to use a standard, non-custom GHC, as using a custom GHC can take quite long to build all dependencies. Nix users can use the `with-ghc` shell to get GHC (`nix develop .#with-ghc`). Moreover, installing the exe is a better idea than using `cabal run`, as `build/install/run` requires running `cabal`'s constraint solver, which takes several minutes.

```sh
$ cabal install sequential --installdir=bin
```

Once `sequential` is built, make the following changes to `cabal.project` (or add to `cabal.project.local`):

- Append the entirety of `clc-stackage.cabal`'s `build-depends` to the `constraints` section. This ensures the right transitive dependencies will always be used.
- Add the path to your ghc. Note that this should be an absolute path e.g.

    ```
    with-compiler: /path/to/ghc/_build/stage1/bin/ghc
    ```

Nix users will want to switch to the default shell (`nix develop`). Then run e.g.

```sh
$ ./bin/sequential
```

This will:

- Build every package one at a time, saving the results in `output/report.json`.
- Save the current progress to `output/cache.json`. This allows us to interrupt the program (e.g. `CTRL-C`) and then pick up where we left off. The cache can be disabled with `--no-cache`.
- Write failing stdout/stderr to `output/logs`. This can be adjusted with the `--write-logs` option.

There is a `--batch N` option that will build `N` packages at the same time, rather than individually. This may help performance (e.g. `--batch 20`).

Finally, if you are on `ghc >= 9.8` and `cabal >= 3.12`, the `--jobs` option can be used with cabal's semaphore functionality, for a potentially faster build: `--jobs semaphore`.

For details, see

```sh
$ sequential --help
```

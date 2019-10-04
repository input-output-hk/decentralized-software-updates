<h1 align="center">On decentralized software updates for blockchain systems.</h1>

## Executable specification

The executable specifications for the update mechanism can be found in the
[`executable-spec`](executable-spec/) folder.

To test the executable specifications run:

```sh
stack test --nix --file-watch
```

To play with the generated traces you can use the `ghci` repl. For instance, if
using `stack` you can enter the repl as follows:

```sh
stack --nix repl decentralized-updates
```

Once in the repl, one can obtain a random trace as follows:

```sh
λ > import  Control.State.Transition.Generator
λ > :set -XTypeApplications
λ > import Cardano.Crypto.Hash.Short (ShortHash)
λ > randomTrace @(IDEATION ShortHash) 100
```

### Contributing

Make sure:

- your editor supports [`editorconfig`](https://editorconfig.org/).
- you have the latest version of
  [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell/) installed
  (we use features that are not in Hackage yet, so make sure to build from
  source).
- Make sure your editor runs `stylish-haskell` on save.

We try to adhere to the coding standard explained
[here](https://github.com/input-output-hk/cardano-wallet/wiki/Coding-Standards)

#### On nix

The use of [`nix`](https://nixos.org/nix/download.html) is not required, but
recommended, since it allows to achieve reproducible builds across developers
machines.

When using `nix` it is recommended that you setup the cache, so that it can
reuse built artifacts, reducing the compilation times dramatically:

If you are using [NixOS](https://nixos.org/) add the snippet below to your
`/etc/nixos/configuration.nix`:

```
nix.binaryCaches = [
  "https://cache.nixos.org"
  "https://hydra.iohk.io"
];

nix.binaryCachePublicKeys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
```

If you are using the `nix` package manager next to another operating system put
the following in `/etc/nix/nix.conf` if you have a system-wide `nix`
installation , or in `~/.config/nix/nix.conf` if you have a local installation:

```
substituters        = https://hydra.iohk.io https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

hwifi-dev
=========

# Pre-requisites

- makefile
- cabal
- haskell-platform

Ensure a make is available and for debian-based platform:

```sh
make install
```

# Bootstrap

Bootstrap your environment.

```sh
make sandbox-init init
```

# Build

```sh
make build
```

# Test

```sh
make test
```

# Run

With a running environment using nmcli:

```sh
make run
```

# Nix

If you are using nix package manager, you can use a dedicated sandbox environment.

```sh
make env-sandbox
```

*Note* This will use the hwifi.nix in the repository.

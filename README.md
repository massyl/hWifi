hWifi [![Build Status](https://travis-ci.org/lambdatree/hWifi.png?branch=master)](https://travis-ci.org/lambdatree/hWifi)
=====================================================================================================================

A simple wifi haskell executable to auto-connect to available Wifi.

# What

This will:
- Scan the current available wifis
- Compute your autoconnect list of wifis
- Elect the one with the most powerful signal and try to connect to it

# How

Embedding this in a script:

```sh
runhaskell Main.hs
```

or

```sh
cabal run
```

Possible output:

```text
Scanned wifi:
- some-ssid
- some-other-ssid

Elect the most powerful wifi signal.

Connection if possible.

Successfully connected to wifi 'some-other-ssid'!
```

# Pre-requisite

## Packages

- haskell-platform
- nmcli (which is packaged with NetworkManager tools in debian-based machine).

## Wifi setup

You already have some connection settings applied to your machine.

# platform

Tested on:
- debian-based (ubuntu, linux mint)
- nixos

# Tested use case

## no concurrency

- Disconnect your wifi
- Run the command
- This will connect you

## concurrency

- Let your current wifi connected
- Activate the tethering on your mobile phone and let your mobile phone near your computer (this should be the most powerful signal)
- Run the command
- This will connect you to the tethering connection over your other wifi connections

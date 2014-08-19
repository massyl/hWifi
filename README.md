hWifi [![Build Status](https://travis-ci.org/lambdatree/hWifi.png?branch=master)](https://travis-ci.org/lambdatree/hWifi)
=====================================================================================================================

A simple wifi haskell executable to connect to available Wifi.

The main attraction about hWifi is the auto-connect policy to the most powerful amongst known ssid signal.

# Use

## Usage

```text
Usage: hwifi [OPTION...] files...
  -V, -?        --version                   Show version number
  -a            --auto                      Standard auto-connect policy. This is the default behavior
  -s SSID       --ssid=SSID                 wifi SSID to connect to.
  -c <wpa|wep>  --connect-policy=<wpa|wep>  The connection policy (wep or wpa)
  -p <psk>      --psk=<psk>                 Pre-Shared Key to connect to the ssid.
```

## Example

### Auto-connect

```sh
hWifi
```
or
```sh
hWifi -a
```

This is the default option (-a is optional) to try and connect to the most powerful known ssid.
This will:
- Scan the current available wifis
- Compute your autoconnect wifis list
- Elect the most powerful signal one and connect to it

### Manual mode

```sh
hWifi -s tatooine -c wpa -p password
```

In manual mode, the user has to provide 3 information:
- -s ssid
- -c connect-policy (which is wep or wpa)
- -p pre-shared key to connect to such ssid

This will:
- create a new nmcli entry
- connect to such wifi

Then, next time, you could simply use the auto-connect policy.

# Pre-requisite

## Packages

- haskell-platform
- nmcli (which is packaged with `NetworkManager` tools in debian-based machine).
- checkbox (which is in package `checkbox` in debian-based machine).

## Wifi setup

You already have some connection settings applied to your machine.

# platform

Tested on:
- debian-based (ubuntu, linux mint)
- nixos

# Tested use case

## Auto-connect

### no concurrency

- Disconnect your wifi
- Run the command
- This will connect you

### concurrency

- Let your current wifi connected
- Activate the tethering on your mobile phone and let your mobile phone near your computer (this should be the most powerful signal)
- Run the command
- This will connect you to the tethering connection over your other wifi connections

## Manual

Destroy one wifi entry (`make clean-wifi`).
Create a new entry using manual mode described earlier and be connected.
Next reconnect using auto-connect mode.

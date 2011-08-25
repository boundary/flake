#!/bin/sh

rebar clean compile generate
./rel/flake/bin/flake

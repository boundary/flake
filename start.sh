#!/bin/sh

rebar get-deps clean compile generate && ./rel/flake/bin/flake

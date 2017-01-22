#!/bin/sh

# takes an arg and passes it to xsel.  No idea why xsel lacks this behavior.

echo "$@" | xsel

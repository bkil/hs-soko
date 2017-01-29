#!/bin/bash
export LINES=`tput lines`
export COLUMNS=`tput cols`
exec runhaskell exercise1.hs "$@"

# works with both ghc and hugs:
# exec runghc exercise1.hs "$@"
# exec runhugs exercise1.hs "$@"

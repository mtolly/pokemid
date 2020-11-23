FROM ubuntu:14.04
# needed for final Haskell linking
# (these are installed by stack script but we're not doing that)
RUN apt-get update && apt-get -y install g++ libgmp-dev
# needed for Stack to unpack GHC
RUN apt-get update && apt-get -y install make xz-utils

<p align="center"> <img src="longshot.png" height="200"/> </p>

> Is it really a __long shot__ to hope that the COVID-19 pandemic will end?



[![Build Status](https://api.travis-ci.com/thyeem/longshot.svg?branch=master)](https://app.travis-ci.com/thyeem/longshot) [![Hackage](https://img.shields.io/hackage/v/longshot)](https://hackage.haskell.org/package/longshot)

# longshot

__Search for preimage__ from a given hash value using _Brute-force_ method based on _parallelism_

* Support various __search lengths__, __character sets__ and __hashers__.
* Strict mode: searches only for a given _exact length_
* Deep mode: **Incrementally searches** when you *do not know the exact length of search*
* Use `CPUs` as _much_ as possible. __Get the most out of them!__
* Use, however, `memory` as _little_ as possible.


Enjoy `longshot`. Good luck!



```plain
longshot - Fast Brute-force search using parallelism

Usage:
  longshot run        [--deep | -n SIZE] [-c CHARS] [-a HASHER] HEX
  longshot image      [-a HASHER] KEY

Commands:
  run                 Brute-force search with given hexstring and options
  image               Generate image from given key string and hash algorithm

Arguments:
  HEX                 Specify target hexstring to search
  KEY                 Specify key string as a preimage

Options:
  -h --help           Show this
  --deep              Deep search by increasing length of search
                      Use when you do not know the exact length of preimage
  -n SIZE             Specify search length  [default: 8]
  -c CHARS            Specify characters available in preimage  [default: 0123456789]
  -a HASHER           Specify hash algorithm  [default: sha256]
                      HASHER available below:
                      md5           sha1          ripemd160     whirlpool
                      sha256        sha3_256      sha3_384      sha3_512
                      blake2s_256   blake2b_256   blake2b_384   blake2b_512
                      blake3_256    blake3_384    blake3_512
                      keccak_256    keccak_384    keccak_512
                      skein_256     skein_384     skein_512
```

## How to build
```shell
## Install 'stack' if not any
$ brew install stack

$ git clone https://github.com/thyeem/longshot.git

$ make build

## Optional: test if installed properly using quickcheck
$ make test

## You can see an executable 'longshot' here
$ cd app
```

## Quick start
```shell
## Refer to the following usage:
## Note that the order of arguments and options matter.
$ ./longshot -h

## Generate test hash values
$ ./longshot image 12345
5994471abb01112afcc18159f6cc74b4f511b99806da59b3caf5a9c173cacfc5

$ ./longshot image 12345678
ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f

## Brute-force search with default options:
## The same as: ./longshot -n 8 -c 0123456789 -a sha256 ef797c..98a64f
$ ./longshot run ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f
Found  12345678

## Search below fails because default search length is 8.
## In default mode, it searches by the exact length of key.
$ ./longshot run 5994471abb01112afcc18159f6cc74b4f511b99806da59b3caf5a9c173cacfc5
Not found

## If you don't know the exact key length, use deep search. (--deep)
## In most cases, it is difficult to know the length of the preimage.
## It will try to search by increasing length of search
$ ./longshot run --deep 5994471abb01112afcc18159f6cc74b4f511b99806da59b3caf5a9c173cacfc5
Found  12345
```
These are all about how to use `longshot`.
See below for more interesting detailed examples.

## More examples
```shell
## Generate a example image using Blake2b hash algorithm
## Blake2b-hashed sofia (my first daughter!)
$ ./longshot image -a blake2b sofia
bb40f637bb211532318965627f15bb165f701230fd83e0adf8cd673a6ee02830

## Need different character set. Don't forget --deep
$ ./longshot run --deep -c 'abcdefghijklmnopqrstuvwxyz' -a blake2b bb40f6..e02830
Found  sofia

## You should consider the following if there might be more characters in preimage.
## More characters, much longer time to find!
$ ./longshot run --deep -c 'abcdefghijklmnopqrstuvwxyz0123456789' -a blake2b bb40f6..e02830

## Roughly, time spending is proportional to (Number of char available) ^ (char length).
## Exponent 'char length' is surely more dominant! Use long-long password as always!

## Longshot is very efficient and get the most of CPU's power in parallel.
## But this kind of work would need a lot of time even for longshot
## due to exponentially increased search space.
$ ./longshot run -deep -c 'abcdefghijklmnopqrstuvwxyz0123456789`~!@#$%^&*()-=_+[]{}\|' \
                 -a blake2b bb40f6..e02830 +RTS -s

## '+RTS -s' the end of line is optional, and that is for a summary of CPU time and memory.
```

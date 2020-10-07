<p align="center"> <img src="longshot.png" height="200"/> </p>

> Is it really a __long shot__ to hope that the COVID-19 pandemic will end?

# LongShot

__Search for preimage__ from a given hash value using _Brute-force_ method based on _parallelism_

* Support various __search lengths__, __character sets__ and __hashers__.
* Strict mode: searches only for a given _exact length_
* Deep mode: searches _everything less than or equal_ to a given length.
* Use `CPUs` as _much_ as possible. __Get the most out of them!__
* Use, however, `memory` as _little_ as possible.
  

Enjoy `longshot`. Good luck!



```plain
longshot - Fast and concise Brute-force search

Usage:
  longshot run        [-n SIZE] [-c CHARS] [-a HASHER] [--deep] HEX
  longshot image      [-a HASHER] KEY

Commands:
  run                 Brute-force search with given hexstring and options
  image               Generate image from given key string and hash algorithm

Arguments:
  HEX                 Specify target hexstring to search
  KEY                 Specify key string as a preimage

Options:
  -h --help           Show this
  -n SIZE             Specify search length  [default: 8]   
  -c CHARS            Specify characters available in preimage  [default: 0123456789]
  -a HASHER           Specify hash algorithm  [default: sha256]
                      Available HASHER: sha256 | blake2b | keccak256
  --deep              Search deeply including less than a given search length
```

## How to build
```bash
$ git clone https://github.com/thyeem/LongShot.git

$ make build 

## You can see an executable 'longshot' here
$ cd app
```

## Quick start
```bash
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

## If you don't know the key length, use deep search. (--deep)
## It will try to search with less than or equal to the given length.
$ ./longshot run --deep 5994471abb01112afcc18159f6cc74b4f511b99806da59b3caf5a9c173cacfc5
Found 12345
```
These are all about how to use `longshot`.  
See below for more interesting detailed examples.

## More examples
```bash
## Generate a example image using Blake2b hash algorithm
## Blake2b-hashed sofia (my first daughter!)
$ ./longshot image -a blake2b sofia
bb40f637bb211532318965627f15bb165f701230fd83e0adf8cd673a6ee02830

## Need different character set. Don't forget --deep
$ ./longshot run -c 'abcdefghijklmnopqrstuvwxyz' -a blake2b --deep bb40f6..e02830
Found sofia

## You should consider the following if there might be more characters in preimage.
## More characters, much longer time to find!
$ ./longshot run -c 'abcdefghijklmnopqrstuvwxyz0123456789' -a blake2b --deep bb40f6..e02830

## Roughly, time spending is proportional to (Number of char available) ^ (char length).
## Exponent 'char length' is surely more dominant! Use long-long password as always!

## longshot is very efficient and get the most of CPU's power in parallel.
## But this kind of work would be very painful time even for longshot.
$ ./longshot run -n 12 -c 'abcdefghijklmnopqrstuvwxyz0123456789`~!@#$%^&*()-=_+[]{}\|' \
                 -a blake2b --deep bb40f6..e02830 +RTS -s

## '+RTS -s' the end of line is optional, and that is for a summary of CPU time and memory.
```

<p align="center"> <img src="longshot.png" height="200"/> </p>


> Is it really a __long shot__ to hope that the COVID-19 pandemic will end?

# LongShot

Search for a given hash value using _Brute-force_ method based on _parallelism_

* Support various __search lengths__, __character sets__ and __hashers__.
* Soft mode: searches only for a given _exact length_
* Deep mode: searches _everything less than or equal_ to a given length.
* Use `CPUs` as _much_ as possible
* Use, however, `memory` as _little_ as possible

Enjoy it!

```plain
longshot - Fast and concise Brute-force search

Usage:
  longshot run        [-n SIZE] [-c CHARS] [-a HASHER] [--deep] HEX
  longshot image      [-a HASHER] KEY

Commands:
  run                   Brute-force search with given hexstring and options
  image                 Generate image from given key string and hash algorithm

Arguments:
  HEX                   Specify target hexstring to search
  KEY                   Specify key string as a preimage

Options:
  -h --help             Show this
  -n SIZE               Specify search length  [default: 8]   
  -c CHARS              Specify characters in preimage  [default: 0123456789]
  -a HASHER             Specify hash algorithm  [default: sha256]
                        Available HASHER: sha256 | blake2b | keccak256
  --deep                Search deeply including less than a given search length
```

## How to build
```bash
$ git clone https://github.com/thyeem/LongShot.git

$ make build 

## You can see an executable 'longshot' here
$ cd app

## And refer to the following usage
## NOTE: the order of optional parameters matters.
$ app/longshot -h
```

## Examples
```bash

```


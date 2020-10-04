# BruteForce
Search by Brute-force method using parallelism

* Support various search lengths, charset and hashers.
* Use CPUs _as much as possible_
* Use, however, memory _as little as possible_
* Enjoy it!

```plain
bruteforce - Fast and concise Brute-force search

Usage:
  bruteforce run        [-n SIZE] [-c CHARS] [-a HASHER] [--deep] HEX
  bruteforce image      [-a HASHER] KEY

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
$ git clone https://github.com/thyeem/BruteForce.git

$ make build 

## You can see an executable 'bruteforce' here
$ cd app

## And refer to the following usage
$ app/bruteforce -h
```

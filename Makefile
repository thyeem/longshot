bin := longshot
path := $(shell stack path --local-install-root)/bin
n := 10
len := 8
algo := sha256
chars := '0123456789'
key := 20201010

.PHONY: build test clean strict deep
build: 
	@echo $(path)
	stack build 
	cp -f $(path)/$(bin) app
	/usr/bin/strip app/$(bin)

clean:
	rm -f app/$(bin)

# Quick-check property test. Use 'n=..' to specify the number of tests
# make test n=10
test:
	@stack test --test-arguments="--quickcheck-tests $(n)"

# Simple operation check in strict mode for a specific case
# make strict key=20201010 algo=blake2b_256 chars='0123456789' len=8
strict:
	@app/$(bin) image -a $(algo) $(key)
	@app/$(bin) image -a $(algo) $(key) | xargs app/$(bin) run -n $(len) -c $(chars) -a $(algo)

# Simple operation check in deep mode for a specific case
# make strict key=20201010 algo=blake2b_256 chars='0123456789'
deep:
	@app/$(bin) image -a $(algo) $(key)
	@app/$(bin) image -a $(algo) $(key) | xargs app/$(bin) run --deep -c $(chars) -a $(algo)

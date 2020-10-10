bin := longshot
path := $(shell stack path --local-install-root)/bin
n := 10
len := 8
algo := sha256
chars := '0123456789'
key := 20201010
deep :=

.PHONY: build test quick clean
build: 
	@echo $(path)
	stack build 
	cp -f $(path)/$(bin) app
	/usr/bin/strip app/$(bin)

test:
	@stack test --test-arguments="--quickcheck-tests $(n)"

quick:
	@app/$(bin) image -a $(algo) $(key)
	@app/$(bin) image -a $(algo) $(key) | xargs app/$(bin) run -n $(len) -c $(chars) -a $(algo) $(deep)
	
clean:
	rm -f app/$(bin)

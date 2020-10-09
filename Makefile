bin := longshot
path := $(shell stack path --local-install-root)/bin
n := 10

.PHONY: build test clean
build: 
	@echo $(path)
	stack build 
	cp -f $(path)/$(bin) app
	/usr/bin/strip app/$(bin)

test:
	stack test --test-arguments="--quickcheck-tests $(n)"
	
clean:
	rm -f app/$(bin)


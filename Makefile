bin := longshot
path := $(shell stack path --local-install-root)/bin

.PHONY: build test clean
build: 
	@echo $(path)
	stack build 
	cp -f $(path)/$(bin) app
	/usr/bin/strip app/$(bin)

test:
	stack test --test-arguments="--quickcheck-tests 10"
	
clean:
	rm -f app/$(bin)


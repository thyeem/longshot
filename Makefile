bin := bruteforce
path := $(shell stack path --local-install-root)/bin

.PHONY: build test clean
build: 
	@echo $(path)
	stack build 
	cp -f $(path)/$(bin) app
	/usr/bin/strip app/$(bin)

test:
	app/$(bin) +RTS -s
	
clean:
	rm -f app/$(bin)


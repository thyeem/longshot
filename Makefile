bin := bruteforce
path := $(shell stack path --local-install-root)/bin

.PHONY: build test clean
build: 
	@echo $(path)
	stack build 
	cp -f $(path)/$(bin) .
	/usr/bin/strip ./$(bin)

test:
	./$(bin) +RTS -s
	
clean:
	rm -f $(bin)


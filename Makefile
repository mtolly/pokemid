version := $(shell release/version)

.PHONY: build

build:
	stack build
	rm -rf release/${version} release/${version}.zip
	mkdir release/${version}
	cp $(shell stack exec which pokemid) release/${version}/
	strip release/${version}/pokemid*
	cp README.md release/${version}/README.txt
	cd release && zip -r ${version}.zip ${version}

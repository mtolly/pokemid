version := $(shell release/version)

.PHONY: build docker

build:
	stack build
	rm -rf release/${version} release/${version}.zip
	mkdir release/${version}
	stack exec which pokemid | xargs -I{} cp {} release/${version}/
	strip release/${version}/pokemid*
	cp README.md release/${version}/README.txt
	cd release && zip -r ${version}.zip ${version}

docker:
	docker build -t onyxite/pokemid .
	stack --docker --docker-image=onyxite/pokemid build
	rm -rf release/${version} release/${version}.zip
	mkdir release/${version}
	stack --docker --docker-image=onyxite/pokemid exec which pokemid | xargs -I{} cp {} release/${version}/
	strip release/${version}/pokemid*
	cp README.md release/${version}/README.txt
	cd release && zip -r ${version}.zip ${version}

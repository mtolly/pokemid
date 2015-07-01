# Builds win/mac/linux binaries from a Mac with Vagrant.

release := $(shell release/version)

.PHONY: all osx linux win32
all: release/${release}-osx-x64.zip release/${release}-linux-x64.tar.gz release/${release}-win32-x86.zip
osx: release/${release}-osx-x64.zip
linux: release/${release}-linux-x64.tar.gz
win32: release/${release}-win32-x86.zip

release/${release}-osx-x64.zip:
	stack setup
	stack build
	cp .stack-work/install/x86_64-osx/*/*/bin/pokemid pokemid
	strip pokemid
	zip $@ pokemid README.md
	rm pokemid

release/${release}-linux-x64.tar.gz:
	vagrant up linux
	vagrant ssh linux -c "cd /vagrant && stack setup && stack build"
	cp .stack-work/install/x86_64-linux/*/*/bin/pokemid pokemid
	vagrant ssh linux -c "cd /vagrant && strip pokemid"
	tar -cvzf $@ pokemid README.md
	rm pokemid

release/${release}-win32-x86.zip:
	vagrant up wine
	vagrant ssh wine -c "cd /vagrant && wine stack setup && wine stack build"
	cp .stack-work/install/i386-windows/*/*/bin/pokemid.exe pokemid.exe
	# vagrant ssh wine -c "cd /vagrant && wine stack exec -- strip pokemid.exe"
	zip $@ pokemid.exe README.md
	rm pokemid.exe

release := $(shell release/version)

all: release/${release}-osx-x64.zip release/${release}-linux-x86.tar.gz release/${release}-win32-x86.zip

release/${release}-osx-x64.zip:
	rm -rf dist/
	release/build
	cp dist/build/pokemid/pokemid pokemid
	zip $@ pokemid README.md
	rm pokemid

release/${release}-linux-x86.tar.gz:
	rm -rf dist/
	vagrant up
	vagrant ssh -c "cd /vagrant; release/build"
	cp dist/build/pokemid/pokemid pokemid
	tar -cvzf $@ pokemid README.md
	rm pokemid

release/${release}-win32-x86.zip:
	rm -rf dist/
	vagrant up
	vagrant ssh -c "cd /vagrant; release/build-wine"
	cp dist/build/pokemid/pokemid.exe pokemid.exe
	zip $@ pokemid.exe README.md
	rm pokemid.exe

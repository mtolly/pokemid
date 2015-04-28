# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "boxcutter/ubuntu1504-i386"
  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y ghc cabal-install alex happy
  SHELL
  config.vm.provision "shell", privileged: false, inline: <<-SHELL
    cd /vagrant
    cabal sandbox init
    cabal update
    cabal install --only-dependencies
    cabal configure
    cabal build
    strip dist/build/pokemid/pokemid
  SHELL
end

# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "hashicorp/precise32"
  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y ghc cabal-install alex happy
    cabal update
    cd /vagrant
    cabal install --only-dependencies
    cabal configure
    cabal build
  SHELL
end

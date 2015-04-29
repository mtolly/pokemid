# -*- mode: ruby -*-
# vi: set ft=ruby :

# Sets up a box that can compile for both Linux and Windows.
Vagrant.configure(2) do |config|
  config.vm.box = "boxcutter/ubuntu1504-i386"
  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y ghc cabal-install alex happy
    apt-get install -y wine || apt-get install -y wine
  SHELL
  config.vm.provision "shell", privileged: false, inline: <<-SHELL
    cabal update
    wget https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.8.4-i386.exe
    wine minghc-7.8.4-i386.exe /S
    wine cabal update
  SHELL
end

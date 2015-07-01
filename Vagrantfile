# -*- mode: ruby -*-
# vi: set ft=ruby :

# Sets up boxes for both Linux and Windows (Wine).
Vagrant.configure(2) do |config|
  config.vm.box = 'boxcutter/ubuntu1504'
  config.vm.define 'linux' do |linux|
    linux.vm.provision 'shell', inline: <<-SHELL
      wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
      echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main' | tee /etc/apt/sources.list.d/fpco.list
      apt-get update
      apt-get install -y stack
    SHELL
  end
  config.vm.define 'wine' do |wine|
    wine.vm.provision "shell", inline: <<-SHELL
      apt-get update
      echo ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula "select" true | debconf-set-selections
      apt-get install -y wine
    SHELL
    wine.vm.provision "shell", privileged: false, inline: <<-SHELL
      wget https://github.com/commercialhaskell/stack/releases/download/v0.1.1.0/stack-0.1.1.0-i386-windows.zip
      unzip stack-0.1.1.0-i386-windows.zip
      wine wineboot
      mv stack.exe /vagrant/stack.exe
    SHELL
  end
end

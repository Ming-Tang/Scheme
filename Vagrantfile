# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # All Vagrant configuration is done here. The most common configuration
  # options are documented and commented below. For a complete reference,
  # please see the online documentation at vagrantup.com.

  # Every Vagrant virtual environment requires a box to build off of.
  config.vm.box = "vStone/centos-7.x-puppet.3.x"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # If true, then any SSH connections made will enable agent forwarding.
  # Default value: false
  # config.ssh.forward_agent = true

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  config.vm.provider "virtualbox" do |vb|
    # vb.gui = true

    vb.customize ["modifyvm", :id, "--cpus", "2"]
    vb.customize ["modifyvm", :id, "--memory", "768"]

    # Disable USB (or else VM won't boot)
    vb.customize ["modifyvm", :id, "--usb", "off"]
    vb.customize ["modifyvm", :id, "--usbehci", "off"]
  end

  config.vm.provision "shell", inline: <<-end
    # Install F#
    curl -sS https://packagecloud.io/install/repositories/haf/oss/script.rpm | bash
    yum -y install fsharp-3.1.1.5-2.x86_64

    # Install NuGet
    curl -sS https://api.nuget.org/downloads/nuget.exe >> /usr/bin/nuget.exe
    echo -e '#!/bin/bash'"\n"'mono /usr/bin/nuget.exe "$@"' > /usr/bin/nuget
    chmod +x /usr/bin/nuget

    # Install nuget certs
    # http://stackoverflow.com/questions/15181888/nuget-on-linux-error-getting-response-stream
    mozroots --import --machine --sync
    yes | certmgr -ssl -m https://go.microsoft.com
    yes | certmgr -ssl -m https://nugetgallery.blob.core.windows.net
    yes | certmgr -ssl -m https://nuget.org
  end
end

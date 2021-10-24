# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "debian/bullseye64"
  config.vm.synced_folder ".", "/vagrant", type: "rsync"

  config.vm.provider :libvirt do |v|
    v.cpus = 2
    v.memory = 2048
  end

  config.vm.provision :ansible do |a|
    a.playbook = "books/new.yml"
  end
end

# laptop.nix
{ config, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hw-laptop.nix
      ./configuration.nix
    ];

  boot.loader= {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;

    grub = {
      enable = false;
      version = 2;
      devices = [ "nodev" ];
      efiSupport = true;
      configurationLimit = 10;
    };
  };
  boot.blacklistedKernelModules = [ "radeon" ];

  networking = {
    networkmanager.enable = true;
    networkmanager.insertNameservers = config.networking.nameservers;
    hostName = "kusanagi"; # Define your hostname.
    hostId = "8425e349"; # Required by zfs.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp1s0.useDHCP = true;
    };
  };

  # List services that you want to enable:
  services = {
  # Enable the X11 windowing system.
    xserver.libinput = {
      enable = true;
      touchpad.middleEmulation = true;
      touchpad.tapping = true;
    };
    sshd.enable = true;
  };
  programs.qt5ct.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}

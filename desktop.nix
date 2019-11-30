{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hw-desktop.nix
      ./configuration.nix
    ];

  networking.hostName = "seiketsu"; # Define your hostname.

  # disks
  boot.initrd.luks.devices = {
    "nixos" = {
      #device = "/dev/disk/by-uuid/55a8d607-cdc5-425c-bb88-397b9a658168";
      preLVM = false;
      allowDiscards = true;
    };
  };
  services = {
    xserver.xrandrHeads = [
      "DVI-0"
      { output="HDMI-3"; primary=true;}
      { output="DisplayPort-4";
        monitorConfig=''
          Option "Rotate" "Right"
          Option "Position" "3840 -420"
          Option "Mode" "1920x1080"
        '';
        # Modeline "1920x1080_60"  148.50  1920 2008 2052 2200  1080 1084 1089 1125 +hsync +vsync
      }
    ];
    plex.enable = true;
    openssh.permitRootLogin = "no";
  };

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}

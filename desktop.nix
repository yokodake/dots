# desktop.nix
{ config, pkgs, lib, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hw-desktop.nix
      ./configuration.nix
    ];

  networking.hostName = "seiketsu"; # Define your hostname.

  boot.initrd.kernelModules = [ "amdgpu" ];

  # disks
  boot.initrd.luks.devices = {
    "nixos" = {
      #device = "/dev/disk/by-uuid/55a8d607-cdc5-425c-bb88-397b9a658168";
      preLVM = false;
      allowDiscards = true;
    };
  };
  services = {
    xserver = rec {
      videoDrivers = [ "ati" ];
      resolutions = [
        { x=1920; y=1080; }
        { x=1920; y=1080; }
        { x=1920; y=1080; }
      ];
      xrandrHeads = [
        { output="DVI-0"; }
        { output="HDMI-3"; primary=true; }
        { output="DisplayPort-4";
          monitorConfig=''
            Option "Rotate" "Left"
            Option "Position" "3840 -420"
            Option "Mode" "1920x1080"
          '';
        }
      ];
      displayManager.sessionCommands = let
        bgs = lib.imap0 (i: _: "--bg-fill $HOME/.background-image" + toString i) xrandrHeads;
      in ''
                ${pkgs.feh}/bin/feh ${toString bgs}
         '';
    };
    plex.enable = false;
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

# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot= {
    loader= {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    tmpOnTmpfs = true;
  # disks
    initrd.luks.devices = {
      "nixos" = {
        #device = "/dev/disk/by-uuid/55a8d607-cdc5-425c-bb88-397b9a658168";
        preLVM = false;
        allowDiscards = true;
      };
    };
  };

  systemd.generator-packages = [ pkgs.systemd-cryptsetup-generator ];
  environment.etc = {
    "crypttab" = {
      enable = true;
      text = ''
home /dev/mapper/vg0-home /luks-keys/home luks
      '';
     };
   };

  networking.hostName = "seiketsu"; # Define your hostname.

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  hardware = {
    pulseaudio.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  programs.bash.enableCompletion = true;

  # List services that you want to enable:
  services = {
  # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us,us(intl)";
      xkbOptions = "grp:shift_caps_toggle,nbsp:level2";

      desktopManager = {
        default = "none";
        xterm.enable = false;
      };

      #windowManager.i3.enable = true;
      windowManager.xmonad.enable = true;
      windowManager.xmonad.extraPackages = self: [ self.xmonad-contrib ];
      windowManager.xmonad.haskellPackages = pkgs.haskell.packages.ghc822;
      windowManager.default = "xmonad";

      xrandrHeads = [
        "DVI-0"
        {output="HDMI-3"; primary=true;}
        "DisplayPort-4"
      ];
    };
    sshd.enable = true;
  };

  fonts = {
    fontconfig.enable = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      fira-code
      source-han-sans-japanese
      ubuntu_font_family
    ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.ngyj = {
    createHome = true;
    home = "/home/ngyj";
    description = "namigyj";
    extraGroups = [ "wheel" "audio" "video" "networkmanager" "disk" ];
    isNormalUser = true;
    uid = 1000;
  };

  nixpkgs.config = {
    pulseaudio = true;
    allowUnfree = true;
  };

  nix = {
      package = pkgs.nixUnstable;
      trustedBinaryCaches = [ "http://cache.nixos.org" ];
      binaryCaches = [ "http://cache.nixos.org" ];
      maxJobs = pkgs.stdenv.lib.mkForce 4;
  }

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #nitrogen
    emacs
    firefox-devedition-bin
    git
    haskellPackages.xmobar
    htop
    mpv
    networkmanager
    nix-bash-completions
    p7zip
    pavucontrol
    rxvt_unicode
    scrot
    unzip
    vim
    wget
    which
    xlibs.xsetroot
    xscreensaver
    zathura
    zip
  ];

  nix.gc.automatic = false;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}

# configuration.nix
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, ... }:
{

  boot.tmpOnTmpfs = true;

  systemd.packages = [ ];

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

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
      xkbOptions = "grp:win_space_toggle,ctrl:swapcaps,grp_led:scroll";

      desktopManager.xterm.enable = false;

      windowManager.xmonad.enable = true;
      windowManager.xmonad.extraPackages = self: [ self.xmonad-contrib ];

      displayManager = {
        defaultSession = "none+xmonad";
        lightdm.background = ./assets/.greeter-image;
        sessionCommands = ''
          export PATH=$HOME/bin:$PATH

          # disable accel on mouse
          ${pkgs.xorg.xset}/bin/xset m 1/1 0
        '';
      };
    };

    sshd.enable = true;
  };

  fonts = {
    fontconfig.enable = true;
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      tewi-font
      corefonts
      dejavu_fonts
      fira-code
      source-han-sans-japanese
      ubuntu_font_family
    ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraGroups.ngyj.gid = 1000;
  users.extraGroups.aigis.gid = 1001;
  users.extraUsers = {
    ngyj = {
      createHome = true;
      home = "/home/ngyj";
      description = "namigyj";
      group = "ngyj";
      extraGroups = [ "wheel" "audio" "video" "networkmanager" "disk" "aigis" "wireshark" "sway"];
      isNormalUser = true;
      uid = 1000;
    };

    aigis = {
      createHome = true;
      home = "/home/aigis";
      description = "Aigis";
      group = "aigis";
      uid = 1001;
    };
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
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    configure = {
      customRC = builtins.readFile ./.vimrc;
      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [ fugitive vim-nix ];
      };
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    ahoviewer
    anki
    binutils
    clang-tools
    curl
    emacs
    feh
    ffmpeg
    filezilla
    firefox-devedition-bin
    git
    haskell.compiler.ghc8102
    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.stack
    haskellPackages.xmobar
    hexchat
    htop
    jpegoptim
    kdeApplications.kio-extras
    kdeFrameworks.breeze-icons
    keepassxc
    krita
    krusader
    mpv
    networkmanager
    nitrogen
    nix-bash-completions
    # nomacs
    ntfs3g
    optipng
    pavucontrol
    psmisc
    racket
    ripgrep
    rofi
    rustup
    rxvt_unicode
    scrot
    spotify
    tmsu
    unzip
    # vimus
    wget
    which
    wine
    xlibs.xsetroot
    xscreensaver
    zip
  ];

  nix.gc.automatic = true;
}

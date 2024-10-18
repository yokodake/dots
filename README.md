`DEVICE={desktop,laptop}`
`ln -s $DEVICE.nix /etc/nixos/configuration.nix`

`$DEVICE.nix` imports `hw-$DEVICE.nix` & `configuration.nix`
`hw-$DEVICE.nix` is the equivalent of `hardware-configuration.nix`
`configuration.nix` common file for both devices

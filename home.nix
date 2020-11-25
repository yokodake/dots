{ config, lib, pkgs, system, ... }:

with lib;

let fileOptions = with types; submodule {
      options = {
        src = mkOption {
          type = str;
          description = ''
            source path
          '';
        };
        dst = mkOption {
          type = str;
          description = ''
            destination path (in $HOME)
          '';
        };
      };

}; in
{
  options = {
    home-cfgs = mkOption {
      default = [];
      type = with types; listOf (submodule {
        options = {
          user = mkOption {
            description = ''
              see `config.users.extraUsers`
            '';
          };

          copy = mkOption {
            default = [];
            type = listOf (either str fileOptions);
            description = ''
              files to be copied
            '';
          };
          flan = mkOption {
            default = [];
            type = listOf (either str fileOptions);
            description = ''
              files to be passed to flan
            '';
          };
        };
      });
    };
  };

  config = let f = x: if isString x then {src=x; dst=x;} else x;
               scriptForEach = f: builtins.foldl' (acc: x: f x + acc) "";
               copies = u: map f u.copy;
               package = pkgs.stdenvNoCC.mkDerivation {
                 name = "home-cfg";
                 src  = builtins.path {
                   path = ./.;
                   filter = (path: type: !(hasSuffix "/.git" path) && !(hasSuffix ".nix" path));
                 };
                 installPhase =
                   "mkdir -p $out\n"
                   + scriptForEach
                       (usr:
                         "mkdir -p $out/${usr.user.name}\n"
                         + scriptForEach
                             (file:
                               "cp -r ${file.src} $out/${usr.user.name}/${file.dst}\n")
                             (copies usr))

                       config.home-cfgs;
               };
    in mkIf (builtins.length config.home-cfgs != 0) {
      system.userActivationScripts = {
        home-cfg-cp =
          scriptForEach (u: "cp -r ${package}/${u.user.name} -T ${u.user.home}\n") config.home-cfgs;
      };
    };
}

##################################################

{ nixpkgs  ? (import <nixpkgs> {}).pkgsMusl
, compiler ? "ghc843"

#, survey   ? ~/src/nh2/static-haskell-nix/survey/default.nix

, doStrip   ? true
, isRelease ? false
}:

##################################################
let
#------------------------------------------------#

pkgs = nixpkgs.pkgsMusl;

inherit (pkgs) lib;

haskellUtilities0 = pkgs.haskell.lib;

#------------------------------------------------#

haskellUtilities =

  let
  super = haskellUtilities0;

  haskellUtilities1 = {

    appendConfigureFlags = drv: xs:
        super.overrideCabal drv (drv:
          {
            configureFlags = (drv.configureFlags or []) ++ xs;
          });

    };

  in

  haskellUtilities0 // haskellUtilities1;

# NOTE « nixpkgs=https://github.com/NixOS/nixpkgs/archive/2c07921cff84dfb0b9e0f6c2d10ee2bfee6a85ac.tar.gz »
#      lacks some “plural” utilities, like « haskell.lib.appendConfigureFlags ».

#------------------------------------------------#

gmp6   = pkgs.gmp6.override { withStatic = true; };

zlib   = pkgs.zlib.static;

libffi = pkgs.libffi;

systemPackages = {
  inherit lib haskellUtilities;
  inherit gmp6 zlib libffi;
};

#------------------------------------------------#

haskellPackages = pkgs.haskell.packages.${compiler};

# haskellPackages_original = pkgs.haskell.packages.${compiler};
#
# haskellPackages = haskellPackages_original.override {
#     overrides = self: super: {
#       # hpc-coveralls = haskellUtilities.appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
#     };
#   };

#------------------------------------------------#

flags = { inherit doStrip isRelease; };

drv = import ./spiros.nix flags systemPackages haskellPackages;

#------------------------------------------------#
in
##################################################
rec {

  example-spiros = haskellUtilities.shellAware drv;

}
##################################################


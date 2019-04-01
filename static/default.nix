##################################################

{ survey   ? ~/src/nh2/static-haskell-nix/survey/default.nix

, nixpkgs  ? (import <nixpkgs> {}).pkgsMusl
, compiler ? "ghc843"

, doStrip   ? true
, isRelease ? false
}:

##################################################
let
#------------------------------------------------#

pkgs = nixpkgs.pkgsMusl;

inherit (pkgs) lib;

haskellUtilities = pkgs.haskell.lib;

#------------------------------------------------#

gmp6   = pkgs.gmp6.override { withStatic = true; };

zlib   = pkgs.zlib.static;

libffi = pkgs.libffi;

systemPackages = {
  inherit lib;
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

package = import ./spiros.nix flags systemPackages;

drv = haskellPackages.callPackage package {};

#------------------------------------------------#
in
##################################################
rec {

  example-spiros = haskellUtilities.shellAware drv;

}
##################################################


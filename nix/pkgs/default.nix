##################################################
{ nixpkgs

, musl     ? false

           # ^ if true, « musl » as the C Library (via « nixpkgs.pkgsMusl »).
           #   if false, « glibc » as the C Library (via « nixpkgs.pkgs »).

, ...

}:

##################################################
let
#------------------------------------------------#

pkgs =

  if   musl

  then nixpkgs.pkgsMusl

# then nixpkgs.pkgsCross.musl64

  else nixpkgs.pkgs;

#------------------------------------------------#
in
##################################################

pkgs

##################################################
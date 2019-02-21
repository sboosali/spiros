##################################################
{ overlays ? []
, config   ? {}

, nixpkgs  ? <nixpkgs>

, pkgs     ? (import nixpkgs { inherit overlays config; }).pkgs
           # ^ if « _.muslPkgs », « musl » as C Library, not « glibc ».

}:

##################################################
let

self  = pkgs;
super = pkgs;

#------------------------------------------------#

inherit (super) lib;

in
##################################################
let

environment = super.buildEnv
 {
   name                  = "skeletor-haskell-environment";

   paths                 = systemLibraries;
   pathsToLink           = [ "/" "/src" "/lib" "/include" ];
   extraOutputsToInstall = [ "out" "dev" "doc" "devdoc" "man" "info" ];

   ignoreCollisions      = true;
 };

#------------------------------------------------#

systemLibraries = with pkgs; [

 bzip2         # « bz2 »
 flac
 libjpeg
 libjpeg.dev
 libpng
 libpng.dev
 openssl
 zlib
 zlib.static

#m
#pthread

];

in
##################################################

environment

/* Notes *****************************************

« out » is necessary in « extraOutputsToInstall = [ "out" "dev" "doc" "devdoc" "man" "info" ]; ».

otherwise, for example, « flac » doesn't work. because its default « output »,
i.e. the first item of the « outputs » list, isn't « out »:

    nix-repl> pkgs.flac.outputs
    [ "bin" "dev" "out" "man" "doc" ]

*************************************************/
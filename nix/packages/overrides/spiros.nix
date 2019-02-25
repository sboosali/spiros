##################################################
{ systemPackages
, haskellPackages
, haskellUtilities

, static
, strip
}:

##################################################
let
#------------------------------------------------#

inherit (systemPackages) lib;

#------------------------------------------------#

staticLibraries = {

  glibc = systemPackages.glibc.static;

  zlib  = systemPackages.zlib.static;

  gmp   = systemPackages.gmp6.override { withStatic = true; };

};

#------------------------------------------------#

newAttributes =

    (lib.optionalAttrs (static != null)
        {
            enableStaticLibraries = static;
            
            enableSharedExecutables = (! static);
            enableSharedLibraries   = (! static);
        }
    );

#------------------------------------------------#

configureFlags = (with staticLibraries; [

    "--extra-lib-dirs=${glibc}/lib"
    "--extra-lib-dirs=${zlib}/lib"
    "--extra-lib-dirs=${gmp}/lib"

  ]) ++ (lib.optionals (!strip) [

    "--disable-executable-stripping"

  ]);

#------------------------------------------------#

buildInputs = lib.optionals static (with staticLibraries; [

  glibc
  zlib
  gmp

]);

#------------------------------------------------#
in
##################################################
oldAttributes:

  (newAttributes // {

        configureFlags = (oldAttributes.configureFlags or []) ++ configureFlags;

        librarySystemDepends    = (oldAttributes.librarySystemDepends    or []) ++ buildInputs;  #TODO#
        executableSystemDepends = (oldAttributes.executableSystemDepends or []) ++ buildInputs;  #TODO#

        isLibrary    = true;
        isExecutable = true;
  })

##################################################
##################################################
{}:

##################################################
let

self = rec {

  #----------------------------------------------#

  bool = b:

      if b then "True" else "False";

  #----------------------------------------------#

  int  = builtins.toString;

  #----------------------------------------------#

  path = builtins.toString;

  #----------------------------------------------#

  list = xs:

    (builtins.toString (builtins.map path xs));

  # ^ space-separated items.
  #----------------------------------------------#

  mkCabalProject = { file, config, libraries }:

    let

    in

    ;

  #----------------------------------------------#  

  /* 
   *
   * e.g.
   *
   *    >>> extra-lib-dirs { libraries = [ pkgsMusl.musl pkgsMusl.gmp pkgsMusl.libffi ]; suffix = "lib"; stanza = true }:
   *    "extra-lib-dirs: /nix/store/blfgah5rv7h3qzl2gv6p6d8i2sxh0vgl-musl-1.1.21/lib /nix/store/pdyjwbhb77k17n6gl78a87a70gywr8dk-gmp-6.1.2/lib /nix/store/vz8iz7ws35aww6i8521z4964xp5akalh-libffi-3.2.1/lib"
   *
   *
   *
   *
   */

  extra-lib-dirs = { libraries, suffix ? "lib", stanza ? true }:

    let

    mkExtraLibDir = library:

      ''${library}/${suffix}'';

    mkExtraLibDirs = libraries:

      let

      xs = builtins.map mkExtraLibDirs libraries;

      x  = self.list xs;

      in

      (if stanza then ''extra-lib-dirs: '' else "") + x;

    in

    mkExtraLibDirs libraries;

  #----------------------------------------------#  


  #----------------------------------------------#  
};

in
##################################################

self

##################################################
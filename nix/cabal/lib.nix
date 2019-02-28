##################################################
{ pkgs
, lib
}:

##################################################
let
#------------------------------------------------#

call = { args ? null }: x:

  if   isPath x

  then (if   isNull args
        then import x
        else import x args
       )

  else x;

#------------------------------------------------#

isPath = x:

  "path" == builtins.typeOf x;

#------------------------------------------------#

isNull = x:

  null == x;

#------------------------------------------------#

hasType = t: x:

  t == builtins.typeOf x;

#------------------------------------------------#
in
##################################################
let
#------------------------------------------------#

self = rec {

  #----------------------------------------------#

  toCabal = x:

    let

    t = builtins.typeOf x;

    f = toCabalByType t;

    y = f x;

    in

    if   null != f
    then y
    else abort ''« toCabal x » error: « x » is « ${builtins.toString x} », with type « ${t} », which isn't a value type.'';

  #----------------------------------------------#

  bool = b:

      if b then "True" else "False";

  #----------------------------------------------#

  int  = builtins.toString;

  #----------------------------------------------#

  string = builtins.toString;

  #----------------------------------------------#

  path = builtins.toString;

  #----------------------------------------------#

  paths = xs:

    (builtins.toString (builtins.map path xs));

  #----------------------------------------------#

  list = xs:

    (builtins.toString (builtins.map string xs));

  # ^ space-separated items.

  #----------------------------------------------#

  /* Make a « cabal-<name>.project » (a derivation).
   *
   * :: { file :: String, name :: Maybe String, config :: {...} } -> Derivation
   *
   * The derivation is a « writeTextFile » invocation.
   *
   *
   */

  mkCabalProjectFile = { file, name ? null, config ? {}}:

    let

    name' = asCabalProjectName { inherit name; };
    
    file' = mkCabalProjectString { inherit file config; };

    in

    pkgs.writeTextFile
      {
        name = name';
        text = file';
      };

  #----------------------------------------------#  

  /* Make a « cabal.project » file (a string).
   *
   * :: { file :: String, config :: {...} } -> String
   *
   *
   *
   *
   */

  mkCabalProjectString = { file, config ? {}}:

    let

    args = {
      inherit pkgs lib;
      cabal  = self;
      config = config';
    };

    file'   = call { inherit args; } file;
    config' = call { inherit args; } config;

    in

    file';

  #----------------------------------------------#

  /* Render an idiomatic filename for a Cabal Project File.
   *
   * :: { name :: Maybe String } -> String
   *
   * e.g.
   *
   *    >>> asCabalProjectName {}
   *    "cabal.project"
   *
   *    >>> asCabalProjectName { name = "ghcjs"; }
   *    "cabal-ghcjs.project"
   *
   *
   *
   */

  asCabalProjectName = { name ? null }:

    assert (builtins.isString name || builtins.isNull name);

    if   null != name
    then ''cabal-${name}.project''
    else ''cabal.project'';

  #----------------------------------------------#  

  /* Render an « extra-lib-dirs » stanza.
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

      x  = self.paths xs;

      in

      (if stanza then ''extra-lib-dirs: '' else "") + x;

    in

    mkExtraLibDirs libraries;

  #----------------------------------------------#  

  field = { config, cabalField, nixField ? cabalField }:

    let

    value = builtins.getAttr nixField config;

    in
   
    lib.optionalString (builtins.hasAttr nixField config && (! isNull value)) ''
      ${cabalField}: ${self.toCabal value}
    '';

  #----------------------------------------------#  
};

#------------------------------------------------#

toCabalByType = type:

  if   (builtins.hasAttr type toCabalFunctions)
  then (builtins.getAttr type toCabalFunctions)
  else null;

#------------------------------------------------#

toCabalFunctions = {

    bool   = self.bool;
    int    = self.int;
    list   = self.list;
    path   = self.path;
    string = self.string;

};

#------------------------------------------------#
in
##################################################

self

##################################################
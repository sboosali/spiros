{ cfg
, lib
, pkgs
, ...
}:

with lib;

##################################################
let

utilities     = import ./utilities.nix           { inherit lib; };

##################################################

PathOrString = types.either types.path types.str;

##################################################

configuration = import ./types/configuration.nix { inherit lib; };

repository    = import ./types/repository.nix    { inherit lib; };

 # types.submodule {
 #    options = {
 #      key = mkOption {
 #        type = types.attrs;
 #        description = ".";
 #      };

in
##################################################
{
  #------------------------------------------#

  config = mkOption {

    type    = types.submodule configuration;
    default = {};

    description = ''General configuration.

Each <code>attrName</code> is the cabal field name, and its <code>attrValue</code> is a simply-typed nix value (most are either: a string, a bool, an int, a float, or a list thereof).
'';
  };

  #------------------------------------------#

  packageConfigs = mkOption {

    type    = types.attrsOf (types.submodule configuration);
    default = {}; 

    description = ''Package-specific configuration.

Each <code>attrName</code> is the package name, and its <code>attrValue</code> is a package configuration (in the same format as the stanza-less fields of <code>programs.cabal.config</code>).

e.g. This nix attrset:

<code>
packageConfigs = {

  cryptohash.optimization = false;

  = ;

};
</code>

is rendered as these cabal stanzas:

<code>
package cryptohash
  optimization: False

package 
  :
</code>
'';
  };

  #------------------------------------------#

  repositoryConfigs = mkOption {

    type    = types.attrsOf (types.submodule repository);
    default = {};

    description = ''Additional repositories (besides <link xlink:href="https://www.hackage.haskell.org"/>).

<code>cabal</code> will search for and download packages from these.

e.g. This nix attrset:

<code>
repositoryConfigs = {

  "stackage-lts-12.20".url = http://www.stackage.org/lts-12.20;

  "head.hackage" = {
     url           = http://head.hackage.haskell.org;
     secure        = true;
     key-threshold = 3;
     root-keys:    = [ 
   "07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740"
   "2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb"
   "8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e"
     ];
  };
};
</code>

is rendered as these cabal stanzas:

<code>
repository stackage-lts-12.12
  url: http://www.stackage.org/lts-12.12

repository head.hackage
   url: http://head.hackage.haskell.org/
   secure: True
   root-keys: 07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740
          2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb
          8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e
   key-threshold: 3
</code>

'';
  };
  #------------------------------------------#

  sourceConfigs = mkOption {

    type    = types.listOf (types.submodule source-repository-package);
    default = {};

    description = ''Haskell packages from an external version control system.

e.g. This nix list:

<code>
sourceConfigs = [

    {
     type     = "git";
     location = https://github.com/hvr/HsYAML.git
     tag      = "e70cf0c171c9a586b62b3f75d72f1591e4e6aaa1";
    }

    {
     type     = "git";
     location = https://github.com/well-typed/cborg;
     tag      = "3d274c14ca3077c3a081ba7ad57c5182da65c8c1";
     subdir   = "cborg";
    }

];
</code>

is rendered as these cabal stanzas:

<code>
source-repository-package
    type:     git
    location: https://github.com/hvr/HsYAML.git
    tag:      e70cf0c171c9a586b62b3f75d72f1591e4e6aaa1

source-repository-package
    type:     git
    location: https://github.com/well-typed/cborg
    tag:      3d274c14ca3077c3a081ba7ad57c5182da65c8c1
    subdir:   cborg
</code>
'';
  };

  #------------------------------------------#

  registerNixPaths = mkOption {

    type    = types.nullOr types.bool;
    default = true;

    description = ''Whether to register <code>nix-env</code> installation locations.

If <literal>true</literal>, adds these settings to <code>programs.cabal.path</code>:

<code>
extra-lib-dirs:     ''${builtins.toString ~/.nix-profile/lib}
extra-include-dirs: ''${builtins.toString ~/.nix-profile/include}
extra-prog-path:    ''${builtins.toString ~/.nix-profile/bin}
extra-prog-path:    ''${builtins.toString ~/.nix-profile/libexec}
</code>

<code>nix-env</code> installs:

- system libraries under <filename>~/.nix-profile/lib</filename>;
- header files under <filename>~/.nix-profile/include</filename>;
- executables under <filename>~/.nix-profile/bin</filename> and <filename>~/.nix-profile/libexec</filename>;

'';
  };

  #------------------------------------------#

  extraConfig = mkOption {

    type    = types.either types.attrs types.lines;
    default = {};

    description = ''Additional configuration.

Appended after: <code>programs.cabal.config</code>, <code>programs.cabal.packageConfigs</code>, <code>programs.cabal.repositoryConfigs</code>, and <code>programs.cabal.registerNixPaths</code>.

See <link xlink:href="https://cabal.readthedocs.io/en/latest/nix-local-build.html#configuring-builds-with-cabal-project"/> for further details (about the syntax and meaning of the various fields and stanzas of a cabal file).
'';
  };

  #------------------------------------------#

  path = mkOption {             #TODO naming: target? path?

    type    = types.nullOr PathOrString;
    default = null;

    example = literalExample ''
    ''${XDG_CONFIG_HOME}/cabal/global.project
    '';
    description = ''Location (and name) of the user-wide Cabal configuration. 

    <code>null</code> (the default) is <filename>~/.cabal/config</filename>.

    If not <code>null</code>, the environment variable <envar>CABAL_CONFIG</envar> is set to this.
    '';
  };

  #------------------------------------------#

  #------------------------------------------#

  #------------------------------------------#
}
##################################################
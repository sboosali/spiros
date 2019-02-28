##################################################
{ lib
}:

##################################################
let

inherit (lib) types;

inherit (lib) mkOption;

##################################################

utilities          = import ../utilities.nix         { inherit lib; };

##################################################

profiling-detail   = import ./profiling-detail.nix   { inherit lib; };

package            = import ./package.nix            { inherit lib; };

bounded-package    = import ./bounded-package.nix    { inherit lib; };

package-constraint = import ./package-constraint.nix { inherit lib; };

##################################################

PathList = types.listOf PathOrString;

PathOrString = types.either types.path types.str;

in
##################################################

with utilities;

##################################################
let

options = {

  ############################################

  jobs = mCabalkOption {
    type = types.either types.ints.positive types.str;

    name        = "jobs";
    description = ''Run this many jobs simultaneously when building. 

If <code>"$ncpus"</code> is specified, run the number of jobs equal to the number of CPUs.'';
  };

  ############################################

  verbose = mCabalkOption {
    name = "verbose";
    type = types.ints.between 0 3;

    description = ''The verbosity of <code>cabal</code> commands.'';
  };

  ############################################

  username = mkCabalOption {
    name = "username";
    type = types.str;

    description = ''Hackage user name (used by <code>cabal new-upload</code>).'';
  };

  ############################################

  password-command = mkCabalOption {
    name = "password-command";
    type = types.str;

    example     = "passkey 'https://hackage.haskell.org/user/sboo'";
    description = ''Hackage password (for <code>username</code>).'';
  };

  ############################################

  documentation = mkCabalOption {
    type    = types.bool;
    default = false;

    description = ''Whether to build Haddock documentation by default.

NOTE <code>documentation: true</code> does not imply: <code>haddock-benchmarks, <code>haddock-executables, <code>haddock-internal, or <code>haddock-tests (they must be enabled separately, if desired).
'';
  };

  ############################################

  optional-packages = mkCabalOption {
    name    = "optional-packages";

    type    = PathList;
    default = [];

    example = [];
    description = ''Local packages to be vendored into projects (if present).'';
  };

  ############################################

  extra-packages = mkCabalOption {
    name    = "extra-packages";

    type    = types.listOf bounded-package;
    default = [];

    example = [];
    description = ''Hackage package to be vendored into projects.'';
  };

  ############################################

  extra-lib-dirs       = mkCabalOption {
    name    = "extra-lib-dirs";

    type    = PathList;
    default = [];

    example = [];
    description = '' '';
  };

  ############################################

  extra-include-dirs   = mkCabalOption {
    name    = "extra-include-dirs";

    type    = PathList;
    default = [];

    example = [];
    description = '' '';
  };

  ############################################

  extra-framework-dirs = mkCabalOption {
    name    = "extra-framework-dirs";

    type    = PathList;
    default = [];

    example = [];
    description = '' '';
  };

  ############################################

  extra-prog-path      = mkCabalOption {
    name    = "extra-prog-path";

    type    = PathList;
    default = [];

    example = [];
    description = '' '';
  };

  ############################################

  constraints = mkCabalOption {
    name    = "constraints";

    type    = types.attrsOf (types.submodule package-constraint);
    default = {};

    example = literalExample ''
              {
                foo.">=" = 2.1;
                foo."<"  = 2.2;
                foo.bar  = true;
                foo.baz  = false;
              }'';

    description = ''Constraints by package (version bounds and flag assignments).

These constraints are hard; the solver MUST satisfy them.

e.g. This nix attrset:

<code>
constraints = {

  # version bounds (for the pacakge « foo »):

    foo.">=" = 2.1;
    foo."<"  = 2.2;

  # flag assignments (for flags « bar » and « baz »):

    foo.bar  = true;
    foo.baz  = false;

};
</code>

is rendered as this cabal field:

<code>
constraints: foo >= 2.1, foo < 2.2, foo +bar -baz
</code>
'';
  };

  ############################################

  preferences = mkCabalOption {
    name    = "preferences";

    type    = types.attrsOf (types.submodule package-constraint);
    default = {};

    example = literalExample ''
              {
                foo.">=" = 2.1;
                foo."<"  = 2.2;
                foo.bar  = true;
                foo.baz  = false;
              }'';

    description = ''Preferences by package (i.e. soft constraints).

These constraints are soft; the solver MAY satisfy them. It will try, and they will be locally-optimal (but not necessarily globally-optimal).

The syntax for <code>preferences</code> is the same as for <code>constraints</code>.

TODO specify a Stackage <code>lts-X.Y</code> string.
'';
  };

  ############################################

.. cfg-field:: allow-newer: none, all or list of scoped package names (space or comma separated)
.. cfg-field:: allow-older: none, all, list of scoped package names (space or comma separated)

  ############################################

.. cfg-field:: index-state: HEAD, unix-timestamp, ISO8601 UTC timestamp.

  ############################################

  profiling-detail = mkCabalOption {
    name = "profiling-detail";

    type = profiling-detail;

    description = ''TODO'';
  };
  
  ############################################
  
  library-profiling-detail = mkCabalOption {
    name = "library-profiling-detail";

    type = profiling-detail;

    description = ''TODO'';
  };
  
  ############################################

  
  
  ############################################
  
};

in
##################################################
# Exports ########################################
##################################################
{

 inherit options;

}
##################################################
# Notes ##########################################
##################################################
/* 

== Notes

===

mkCabalLink = options@{ prefix ? "cfg-field", ... }:

===

    Package configuration options
    -----------------------------
    
    Package options affect the building of specific packages. There are three
    ways a package option can be specified:
    
    -  They can be specified at the top-level, in which case they apply only
       to **local package**, or
    
    -  They can be specified inside a ``package`` stanza, in which case they
       apply to the build of the package, whether or not it is local or
       external.
    
    -  They can be specified inside an ``package *`` stanza, in which case they
       apply to all packages, local ones from the project and also external
       dependencies.

===

    .. cfg-field:: optimization: nat
               --enable-optimization
               --disable-optimization
    :synopsis: Build with optimization.

    When optimizations are enabled, Cabal passes ``-O2`` to the C compiler.

    We also accept ``True`` (equivalent to 1) and ``False`` (equivalent
    to 0).

===

   .. cfg-field:: configure-options: args (space separated)
    :synopsis: Options to pass to configure script.


===

===

===

===

== Implementation

Get names & types of cabal-fields via this script:

    $ grep -F '.. cfg-field::' cabal/Cabal/doc/nix-local-build.rst

then link them to their stanza (if any), 
and munge them into « mkCabalOption »s (with Emacs macros).

*/
##################################################
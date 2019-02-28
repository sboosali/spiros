{ lib
}:

##################################################
let

inherit (lib) types;

inherit (lib) mkOption;

##################################################

PathList = types.listOf PathOrString;

PathOrString = types.either types.path types.str;

##################################################

source-repository-type = [

    "darcs"
    "git"
    "svn"
    "cvs"
    "mercurial"
    "hg"
    "bazaar"
    "bzr"
    "arch"
    "monotone"

];

in
##################################################

{

  ################################################
  
  type = mkOption {
    type    = types.enum source-repository-type;
    default = "git";
    example = "darcs";

    description = '''';
  };

  ################################################
  
  location = mkOption {
    type    = PathOrString;
    example = literalExample ''https://github.com/well-typed/cborg'';
    description = ''

Example <code>location</code> syntax for different <code>type</code>s:

<code>
{ type     = "git";
  location = git://github.com/foo/bar.git;
}
<code>

<code>
{ type     = "darcs";
  location = http://code.haskell.org/foo/;
}
<code>

<code>
{ type     = "cvs";
  location = "anoncvs@cvs.foo.org:/cvs";
}
<code>
'';
  };

  ################################################
  
  tag = mkOption {
    type    = types.str;
    example = "3d274c14ca3077c3a081ba7ad57c5182da65c8c1";

    description = ''A tag uniquely identifies a particular state of a source repository.

For example, a <code>tag</code> in <code>git</code> is a commit hash.
'';
  };

  ################################################
  
  branch = mkOption {
    type    = types.str;
    example = "master";

    description = ''A branch in <code>git</code>, <code>CVS</code>, or <code>SVN</code>.

Many source control systems support the notion of a branch, as a distinct concept from having repositories in separate locations. For example CVS, SVN and git use branches while for darcs uses different locations for different branches.'';
  };

  ################################################
  
  module = mkOption {
    type    = types.str;
    example = "master";

    description = ''A <code>CVS</code> named-module.

CVS requires a named module, as each CVS server can host multiple named repositories.

Required for CVS, forbidden for other <code>type</code>s.'';
  };

  ################################################
  
  subdir = mkOption {
    type    = types.nullOr types.str;
    default = null;
    example = "cborg";

    description = ''The subdirectory of the repository which contains the package.

    Some projects put the sources for multiple packages under a single source repository. This field lets you specify the relative path from the root of the repository to the top directory for the package, i.e. the directory containing the package’s .cabal file.

    This field is optional. It default to empty which corresponds to the root directory of the repository.
'';
  };

  # sha256 = mkOption {
  #   type    = types.nullOr types.str;
  #   example = "TODO";
  # };

} 
        
 # e.g.
 #
 #    source-repository-package
 #        type: git
 #        location: https://github.com/well-typed/cborg
 #        tag: 3d274c14ca3077c3a081ba7ad57c5182da65c8c1
 #        subdir: cborg
 #


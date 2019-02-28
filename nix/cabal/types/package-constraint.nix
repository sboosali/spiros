##################################################
{ lib
}:

##################################################
let

inherit (lib) types;

##################################################

mkCabalVersionOption = ;

mkCabalFlagOption = ;

in
##################################################
let

versionConstraint = {

  ">=" = mkCabalVersionOption "version-greater-than-or-equal-to" ''
  '';

  ">" = mkCabalVersionOption "version-greater-than" ''
  '';

  "<=" = mkCabalVersionOption "version-less-than-or-equal-to" ''
  '';

  "<" = mkCabalVersionOption "version-less-than" ''
  '';

  "==" = mkCabalVersionOption "version-equal-to" ''

For example:

<code>
build-depends:
  foo == 1.2.3.*,
  bar == 1.*
</code>
  '';

  "^>=" = mkCabalVersionOption "version-is-known-work-with" ''

For example:

<code>
build-depends:
  foo ^>= 1.2.3.4,
  bar ^>= 1
</code>

"This allows to assert the positive knowledge that this package is known to be semantically compatible with the releases foo-1.2.3.4 and bar-1 respectively. The information encoded via such ^>=-assertions is used by the cabal solver to infer version constraints describing semantically compatible version ranges according to the PVP contract (see below)."
  '';

};

##################################################

flagConstraint = 
;

#types.attrsOf 

##################################################

options = {

};

in
##################################################
# Exports ########################################
##################################################
rec {

 inherit options;

}
##################################################

/* NOTES:

  [1a] cabal field:

    constraints: foo >= 2.1, foo < 2.2, foo +bar -baz

  [1b] nix attrset:

    {
      foo.">=" = 2.1;
      foo."<"  = 2.2;
      foo.bar  = true;
      foo.baz  = false;
    }

*/
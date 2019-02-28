##################################################
{ lib
}:

##################################################
let

inherit (lib) types;

##################################################

mkVersionBoundOption = args:

  mkOption ({

    type = types.float;

  } // args);

in
##################################################
let

options = {

  "<=" = mkVersionBoundOption {
    name = "";
  };

  ">=" = mkVersionBoundOption {
    name = "";
  };

  "<" = mkVersionBoundOption {
    name = "";
  };

  ">" = mkVersionBoundOption {
    name = "";
  };

  "==" = mkVersionBoundOption {
    name = "";
  };

  "^=" = mkVersionBoundOption {
    name = "";
  };

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

e.g.:

   foo >= 2.1 && foo < 2.2, 

*/
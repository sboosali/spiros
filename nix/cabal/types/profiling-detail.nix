##################################################
{ lib
}:

##################################################
let

inherit (lib) types;

in
##################################################

types.enum [

    "default"
    "none"
    "exported-functions"
    "toplevel-functions"
    "all-functions"

];

##################################################
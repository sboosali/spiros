{ lib
}:

##################################################
let

inherit (lib) types;

##################################################
rec {

  compiler-flavor = [
    "ghc"
    "ghcjs"
    "jhc"
    "lhc"
    "uhc"
    "haskell-suite"
  ];

  ghc-environment-file-policy = [
    "ghc-8.4.4" "+always" "never"
  ];

  http-transport = [
    "curl" "wget" "powershell" "plain-http" 
  ];

}
##################################################
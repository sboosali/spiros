##################################################
{}:

##################################################
rec {
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


  #----------------------------------------------#
}
##################################################
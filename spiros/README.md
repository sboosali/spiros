[![Build Status](https://secure.travis-ci.org/sboosali/spiros.svg)](http://travis-ci.org/sboosali/spiros)
[![Hackage](https://img.shields.io/hackage/v/spiros.svg)](https://hackage.haskell.org/package/spiros)

# spiros

my custom prelude

[reverse dependencies](http://packdeps.haskellers.com/reverse/spiros)

# Notes

[Haskell CPP Macros](http://www.edsko.net/2014/09/13/haskell-cpp-macros/): 

# Checking for minimal package versions

    #if MIN_VERSION_process(1,2,0)
        -- ...
    #else
        -- ...
    #endif

## Checking GHC version

either indirectly via `base`:

    #if MIN_VERSION_base(4,7,0)

or directly:

    #if MIN_VERSION_GLASGOW_HASKELL(7,6,0,0)

or, with an older macro:

    #if __GLASGOW_HASKELL__ >= 706

# Checking host platform

    #if defined(mingw32_HOST_OS)
    #if defined(cygwin32_HOST_OS) 
    #if defined(darwin_HOST_OS)
    #if defined(aix_HOST_OS)

# Custom macros

In your Cabal file, [1] add a custom flag:

    Flag develop
      Description:   Turn on development settings.
      Default:       False
   
that defines a custom CPP flag:

    library
      ...
      if flag(develop)
        cpp-options: -DDEVELOP
      ...

which you can then condition on in your Haskell files (as normal):

    #if DEVELOP
    ...

Similarly, your Cabal file can set flags given the: current architecture, package versions, etc. 

For example, if you need a more fine-grained check for the GHC version (__GLASGOW_HASKELL__ gives major and minor version number but not patch level) you can add

    library
      if impl(ghc == 7.6.1)
        cpp-options: -DGHC_761

# 
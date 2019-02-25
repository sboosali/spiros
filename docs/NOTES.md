# Notes



## `error`

```haskell
error :: forall (r :: RuntimeRep). 
         forall (a :: TYPE r). 
         HasCallStack => 
         [Char] -> a

-- error stops execution and displays an error message.
```



## `GHC.Stack`




## `CPP`















## Debugging

### 

#### Problem

#### Solution


### 

#### Problem

the syntax of `lxxx` is `"lib" + <LibName> + ".so"`.

i.e. give this linker error:

    /usr/bin/ld: cannot find -lc
    /usr/bin/ld: cannot find -lltdl
    /usr/bin/ld: cannot find -lXtst
    /usr/bin/ld: cannot find -lzlib

these Dynamic Libraries are translated as:

* `-lc`    — means `libc.so`
* `-lltdl` — means `libltdl.so`
* `-lXtst` — means `libXts.so`
* `-lzlib` — means (?) `libzlib.so`

and these Static Libraries are translated as:

* TODO

#### Solution(?)

add StaticLibraries as BuildInputs.

e.g. add `glibc.static` to the `buildInputs` to get a static `-lc`.



### 

#### Problem

#### Solution


















## Static-Linking

### Statically-Linked FLTK(HS)

```nix
Executable fltkhs-buttons

  if os(windows)
    ghc-Options: -optl-mwindows
    ghc-Options: -pgml g++ "-optl-Wl,--allow-multiple-definition" "-optl-Wl,--whole-archive" "-optl-Wl,-Bstatic" "-optl-Wl,-lfltkc" "-optl-Wl,-Bdynamic" "-optl-Wl,--no-whole-archive"

  if os(darwin)
    ghc-Options: "-optl-Wl,-lfltkc"

  if !os(darwin) && !os(windows)
   ghc-Options: -pgml g++ "-optl-Wl,--allow-multiple-definition" "-optl-Wl,--whole-archive" "-optl-Wl,-Bstatic" "-optl-Wl,-lfltkc" "-optl-Wl,-Bdynamic" "-optl-Wl,--no-whole-archive"
```




















## Nix


### `nix-build` Errors

the Configure Errors include:

* `configure: error: Cannot find system libffi`

the Linker Errors include:

* `relocation R_X86_64_32 against hidden symbol ``__TMC_END__'' can not be used when making a shared object`
* `final link failed: Nonrepresentable section on output`
* `relocation R_X86_64_PC32 against undefined symbol ``__pthread_unwind'' can not be used when making a shared object; recompile with -fPIC`
* `warning: Using 'dlopen' in statically linked applications requires at runtime the shared libraries from the glibc version used for linking`



### stdout/stderr

```nix
##################################################
let

config = {

    packageOverrides = pkgs: rec {

      haskellPackages = pkgs.haskellPackages.override {

        overrides = haskellPackagesNew: haskellPackgesOld: rec {

          project3 =

            pkgs.haskell.lib.overrideCabal

              ( haskellPackagesNew.callPackage ./project3.nix {
                  tar = pkgs.libtar;
                }
              )

              ( oldDerivation: {
                  testToolDepends = [ pkgs.libarchive ];
                }
              );

        };
      };
    };
};

#------------------------------------------------#

pkgs = import <nixpkgs> { inherit config; };

in
##################################################

{

 project3 = pkgs.haskellPackages.project3;

}

##################################################
```

### 

e.g. 


```sh
$ make nix 

nix-build --show-trace  -A "packages.spiros"  "./nix"  # NixTargets

these derivations will be built:

  /nix/store/ga8rf5lkarfrlnrv3cyk85kn48k3rgmn-ghc-8.6.3.drv
  /nix/store/3v16nazzvvm0rwjdc4ybixyqpxw42m7v-hscolour-1.24.4.drv
  /nix/store/3fs43zr3c9n525imri56379ys0isbfvl-extensible-exceptions-0.1.1.4.drv
  /nix/store/iz1z35iylxf528la54420kp8hcp50pbn-random-1.1.drv
  /nix/store/d1h29h9yyfn3jxlqglkvwb7dm06m0g6q-erf-2.0.0.0.drv
  /nix/store/w3j2y4scqprnds1ni30kr16jdgvldqv7-primitive-0.6.4.0.drv
  /nix/store/m1ccr0rv890hxn9kq3dc7qxxscyk69ny-tf-random-0.5.drv
  /nix/store/kw188b41d5xilyhs3cysiar7y3kflhgp-QuickCheck-2.12.6.1.drv
  /nix/store/zrlfxy0nia6k5w8qa3c8qr88hazr8dvx-colour-2.3.4.drv
  /nix/store/jgx581q4jbx7bpjgi4x7cfslq8pam5pl-ansi-terminal-0.8.2.drv
  /nix/store/5am98cgsbqm4isa56jg9vsmglpbk09rj-ansi-wl-pprint-0.6.8.2.drv
  /nix/store/cd68m11slsmp0b7yczq39rygap972i5z-xml-1.3.14.drv
  /nix/store/d8ip3csv64mi6hx0sy9nvk4bfvnqhd22-hostname-1.0.drv
  /nix/store/vdkws9f7zkiggbg5y57i3zls0n2q4pm7-old-locale-1.0.0.7.drv
  /nix/store/vb27i8pwzxr9llbbl39plrj6fmfqkbmr-regex-base-0.93.2.drv
  /nix/store/x0c5b92bng44ymz6rrpskqr0jqqdf3qf-regex-posix-0.95.2.drv
  /nix/store/ss74x6296fijgqk4d81myj63xzmmc288-test-framework-0.8.2.0.drv
  /nix/store/05adrmaq50xnljz9jwhkhw7pmravsmmn-test-framework-quickcheck2-0.3.0.5.drv
  /nix/store/0nrkbv1vw9i1d0vv7jdaihqfwy7cmc1r-ghc-paths-0.1.0.9.drv
  /nix/store/1ca3qprzv0g5vayrlf7k8a7gpplxk83j-data-default-class-0.1.2.0.drv
  /nix/store/44sjiixijsva6j6b3437fnlsyilanqnm-nanospec-0.2.2.drv
  /nix/store/ld1hxnmjni2g7fvsn08r3i085zfckc6q-call-stack-0.1.0.drv
  /nix/store/3r3n55r7zw5rq7x9w2n3symyip8jrmmd-HUnit-1.6.0.0.drv
  /nix/store/1rbn2axnki3xf4hm16f8kkfigryypynf-hspec-expectations-0.8.2.drv
  /nix/store/2190qhjvj68qq3pjdr8f8vl573rj5750-safe-0.3.17.drv
  /nix/store/2c60r5czjwdq6isrxpq71rjsby4ia346-split-0.2.3.3.drv
  /nix/store/3f7sfjjjicdfb95bch79avsj1g49fx1l-transformers-compat-0.6.2.drv
  /nix/store/5axfrkshvppxnm0fzaac1cf5p0543gkx-th-abstraction-0.2.8.0.drv
  /nix/store/8d53g5k4fibn6kwcqr42yga2pr6qsg59-semigroups-0.18.5.drv
  /nix/store/5v2sjmjjq5wpyq8wv530hsixs0l943dz-clock-0.7.2.drv
  /nix/store/g3pbdq27wfmvh25axd17r1cqcjf1i31f-quickcheck-io-0.2.0.drv
  /nix/store/m1wid5m2p0mvnvhbald4722lnmc6fsm6-setenv-0.1.1.3.drv
  /nix/store/gzy28gkrj0dk68inda0lpd6rqqa03di7-hspec-meta-2.6.0.drv
  /nix/store/w00nlk6xq85qd9jz2zqv3pdw9ym63fgc-test-framework-hunit-0.3.0.2.drv
  /nix/store/fwgc6dagmn8qb8962kpl24r1kfh2mzv7-exceptions-0.10.0.drv
  /nix/store/bqv3ad4b98k9ic8rk680dwd7c1prlwbd-optparse-applicative-0.14.3.0.drv
  /nix/store/prqkrvmgb3c6zl8kqivggj8hilz91njd-hashable-1.2.7.0.drv
  /nix/store/d940wdwfqh4awy88dqk300q0anwpysc6-async-2.2.1.drv
  /nix/store/fq5fmps61ik7y92ygls2j23k7i61s665-unbounded-delays-0.1.1.0.drv
  /nix/store/x63ggv7iqhywidmjnya21jk5bfw5yj20-wcwidth-0.0.2.drv
  /nix/store/yl9m2h8shv6yw4zzzlr00sa6xq7prm38-tagged-0.8.6.drv
  /nix/store/sk7ah3db5fmyqmp3g6hy31mvi7s6gc0j-tasty-1.1.0.4.drv
  /nix/store/i5a0xxw5a2g3ala3j9v42ihh6mzf0444-tasty-hunit-0.10.0.1.drv
  /nix/store/lwv3mq6bh61bnh63jz6acny09s53fy6v-base-compat-0.10.5.drv
  /nix/store/lrc56wpyx71h0d3wcrvjdxxdw81qm8sl-temporary-1.3.drv
  /nix/store/jqpf9mcr5a1n6lk1f3f1c37kjfrwbz4m-silently-1.2.5.drv
  /nix/store/315bkhbq0i7a038vx0by3l9x8fprl3hh-hspec-core-2.6.0.drv
  /nix/store/jc8bd7z396aa24q0avm0ij3pl046y7z6-hspec-discover-2.6.0.drv
  /nix/store/bd1z0d39fz8ig8494wrq5i2kzqa7pzx5-hspec-2.6.0.drv
  /nix/store/n4vfdlwkbd4fc1gmmf5q3sgrlhfh9ngi-cabal-doctest-1.0.6.drv
  /nix/store/fylasrivfg6dg6barj4770wlimbjx7r7-StateVar-1.1.1.1.drv
  /nix/store/s9m8a9ymq5w4x3gdn8m1wynrcw4vy8qz-contravariant-1.5.drv
  /nix/store/3hl0zab9hdsipqfn9jkywc0y60k7rspp-syb-0.7.drv
  /nix/store/hllz4166a87r5c640hqbh2dpp8fp1smf-code-page-0.1.3.drv
  /nix/store/y0mrgdydi6zh69h3711xdwwfjydw6212-doctest-0.16.0.1.drv
  /nix/store/brkscgi4981jddbl91wqhkaqy25j72sp-generic-deriving-1.12.2.drv
  /nix/store/m7jlmwa9h4i185h9mivj5p8aqdgdkq8l-base-orphans-0.8.drv
  /nix/store/z28sklz2xmpmypbxr0fd1xazkzpsrn0c-distributive-0.5.3.drv
  /nix/store/d09nqcs0gsfx476w648008az2jkp0q1k-comonad-5.0.4.drv
  /nix/store/2fqahik8k1m2iqjlfjgwmf0n05j2gs7m-bifunctors-5.5.3.drv
  /nix/store/9i3qgrj06a0k2g8f24nfhwrak3d1jw9r-pcre-light-0.4.0.4.drv
  /nix/store/2lhgcnnblshxyb5qbqhx00qyg0hz0siw-tasty-quickcheck-0.10.drv
  /nix/store/6p6hi85g3jkqq1ymk4qiww7zcrsz71d6-transformers-base-0.4.5.2.drv
  /nix/store/m6wmph2wfx4dy72lfb12vgnsxllgd8i7-profunctors-5.2.2.drv
  /nix/store/4cnc7aalqgi5vj2nlv344lmiyfd8yx19-unordered-containers-0.2.9.0.drv
  /nix/store/wx0zk1rfjj6mn7n8g09y0rzgvg8bhb1f-semigroupoids-5.3.1.drv
  /nix/store/dp18whsr4xrd0rxgxm610aasyjrn0fzb-free-5.1.drv
  /nix/store/fmhngmmykyzqdqfl0l7aihqrx4sfgyi5-invariant-0.5.1.drv
  /nix/store/iikc8s3cc0qv5fskl24mjvq6myrb0wsm-void-0.7.2.drv
  /nix/store/imrpq7iif3vwp9xxgy6ddhqqdrzzx9lm-adjunctions-4.4.drv
  /nix/store/2shnsz53c5zvlhqfcm5lq4fk5pkah5z8-kan-extensions-5.2.drv
  /nix/store/4bq6h88m2kdh7njnwj8k1i6s94nmnpij-charset-0.3.7.1.drv
  /nix/store/4gpnxq1aha72fa6d5dhi251h3dwxj7d5-jailbreak-cabal-1.3.3.drv
  /nix/store/jj42qisfnfwq6ir9lnq88nvq61lz9f3i-th-lift-0.7.11.drv
  /nix/store/pa1qxdqwj2zmdz3dnp44w3n41q5dysvq-vector-0.12.0.1.drv
  /nix/store/4l2w8i1lmvfnizdzb7dv4jz2mlc9dx1i-th-lift-instances-0.1.11.drv
  /nix/store/b05azfwvg52a5fh5gi3389xfq65hk7z8-microlens-0.4.9.1.drv
  /nix/store/k6127wcmi7jh0hlirzv3hiv411xfw3i9-th-expand-syns-0.4.4.0.drv
  /nix/store/mprypmdm4pw7xjzs9nhq6pcrn0s93802-th-reify-many-0.1.8.drv
  /nix/store/9bm38w2h4zihqmq3i0bl6vz3vi2gmcsq-th-orphans-0.13.6.drv
  /nix/store/lb1zgmpklcikcdz3k1gnbxhknc4yjnw7-th-desugar-1.8.drv
  /nix/store/qrl552ak2m6ajmsy4vbpr69gcx615vyw-singletons-2.4.1.drv
  /nix/store/r672092f3jgl8fmn5pydb0n63rsxcjah-should-not-typecheck-2.1.0.drv
  /nix/store/6hyrlni3n646qfnpj9kkvfa971m6kpd6-parallel-3.2.2.0.drv
  /nix/store/6wi5i9ff1q2yi3w7q8nbbq86cpd0x30g-nats-1.1.2.drv
  /nix/store/fspxi33y8ywvqa3xzrrliw1im6k4fqc5-simple-reflect-0.3.3.drv
  /nix/store/kkdb63am93nv8ggl6d973a2d5nax59ms-language-haskell-extract-0.2.4.drv
  /nix/store/8ydm6ab6fyf97q2a5zajnpcd3sfmq36m-happy-1.19.9.drv
  /nix/store/zjrdmzw5419i62v7fjhgkigamy0rzvrp-haskell-src-exts-1.20.3.drv
  /nix/store/m9g0szncwcx7ima50m97sy5w32s8yq7m-test-framework-th-0.2.4.drv
  /nix/store/z50gc6klri6hxh1m41plwrjslarpimz5-reflection-2.1.4.drv
  /nix/store/smkyz2yxxlxkkmff9gd15zn2jj57qaa2-lens-4.17.drv
  /nix/store/5dp7x5p7zhc71n2acgpyjxini17kbx1c-vinyl-0.8.1.1.drv
  /nix/store/5yrvypmr6kwfmily67c2zc45kfnpcm29-string-conv-0.1.2.drv
  /nix/store/kfal2d9f2hjgb5hcp0incvc3hg57nwiq-tasty-ant-xml-1.1.4.drv
  /nix/store/lcp117f7k6d7cdz1hiz282wkx8l1601s-integer-logarithms-1.0.2.2.drv
  /nix/store/pzn7izr85m4l9jgf1ccb5xq963sm9lgp-logict-0.6.0.2.drv
  /nix/store/mk2bqbmicg1mxm6ah4kvhc2ja4dma7bj-smallcheck-1.1.5.drv
  /nix/store/y89lm9b1hn0yrkyh0dw2wq2fvmvfm38c-tasty-smallcheck-0.8.1.drv
  /nix/store/f8p9qjcmfmkq2x3bkk41xhxbq7aj0kq5-scientific-0.3.6.2.drv
  /nix/store/dn7vg95i4g26wylyjps3f0c0xabqn8gj-attoparsec-0.13.2.2.drv
  /nix/store/6imk4myd4z61myp9n9smry14pbi4i3iv-cpuinfo-0.1.0.1.drv
  /nix/store/7fa5mx5abw7cdh5w5kxxlvsa1dmvdpzb-old-time-1.1.0.3.drv
  /nix/store/9s29xbpjnilyhnzgbq5z37k4843j6acf-case-insensitive-1.2.0.11.drv
  /nix/store/as1x31g9f2wcwh15rp1c294w8g8kg761-fingertree-0.1.4.1.drv
  /nix/store/d4ks4ncczvks9plvpdmjyx308wphg2xl-utf8-string-1.0.1.1.drv
  /nix/store/g7zy10jvnk3cxwd87wn52lxjsy143mja-uuid-types-1.0.3.drv
  /nix/store/hill49lxsnf1w5kj5gwpyhrgfa0swwdj-quickcheck-instances-0.3.19.drv
  /nix/store/nmi98i0jkw4a3x0bbyfl1g6429fflyzw-parsers-0.12.9.drv
  /nix/store/q3vxdncqc3dxkhpdi9khhh4wxq19a4vg-blaze-builder-0.4.1.0.drv
  /nix/store/q8wlrninvkkjcsaccpwrklqipnwswlgm-reducers-3.12.3.drv
  /nix/store/yw9w0xkn9gq52nqvpkzvx6y2cwd9lf8v-blaze-markup-0.8.2.2.drv
  /nix/store/qgywhcdz9lym7m7lv9dwqkpj8vhycvrk-blaze-html-0.9.1.1.drv
  /nix/store/drg3hf1xsj1a026iqaw9dd7rp635mzfj-trifecta-2.drv
  /nix/store/fsfz85v4mfy5w6bwal5x9lz52ygpdyv0-prettyprinter-1.2.1.drv
  /nix/store/s245i2c5f9kvxw1sr5h54h16xrpig2dl-show-prettyprint-0.2.2.drv
  /nix/store/qp6yr0fpy1j23971vxybvr5yrs7rc60s-spiros-0.3.1.drv

```


dynamically linked: 

```sh
$ make example 

cabal new-install --project-file ./cabal.project --overwrite-policy=always "exe:example-sprios"

Wrote tarball sdist to
/home/sboo/haskell/spiros/dist-newstyle/sdist/spiros-0.3.1.tar.gz
Resolving dependencies...
Build profile: -w ghc-8.6.3 -O1
In order, the following will be built (use -v for more details):
 - spiros-0.3.1 (lib) (requires build)
 - spiros-0.3.1 (exe:example-sprios) (requires build)

Starting     spiros-0.3.1 (lib)
Building     spiros-0.3.1 (lib)
Haddock      spiros-0.3.1 (lib)
Installing   spiros-0.3.1 (lib)
Completed    spiros-0.3.1 (lib)
Starting     spiros-0.3.1 (exe:example-sprios)
Building     spiros-0.3.1 (exe:example-sprios)
Haddock      spiros-0.3.1 (exe:example-sprios)
Installing   spiros-0.3.1 (exe:example-sprios)
Completed    spiros-0.3.1 (exe:example-sprios)
Symlinking 'example-sprios'

ldd `which example-sprios`

	/nix/store/*-glibc-2.27/lib/ld-linux-x86-64.so.2 => /lib64/ld-linux-x86-64.so.2                 (0x00007f13cf025000)
	libc.so.6                                        => /nix/store/*-glibc-2.27/lib/libc.so.6       (0x00007f13ce2cd000)
	libdl.so.2                                       => /nix/store/*-glibc-2.27/lib/libdl.so.2      (0x00007f13ce681000)
	libm.so.6                                        => /nix/store/*-glibc-2.27/lib/libm.so.6       (0x00007f13cec90000)
	libpthread.so.0                                  => /nix/store/*-glibc-2.27/lib/libpthread.so.0 (0x00007f13ce0ae000)
	librt.so.1                                       => /nix/store/*-glibc-2.27/lib/librt.so.1      (0x00007f13cea88000)
	libutil.so.1                                     => /nix/store/*-glibc-2.27/lib/libutil.so.1    (0x00007f13ce885000)
	linux-vdso.so.1                                  =>                                             (0x00007fff67bbd000)
```

statically linked: 

```sh
$ make nix-static

$ ldd result-static/bin/example-sprios

	/nix/store/*-glibc-2.27/lib/ld-linux-x86-64.so.2 => /lib64/ld-linux-x86-64.so.2                 (0x00007f13cf025000)
	libc.so.6                                        => /nix/store/*-glibc-2.27/lib/libc.so.6       (0x00007f13ce2cd000)
	libdl.so.2                                       => /nix/store/*-glibc-2.27/lib/libdl.so.2      (0x00007f13ce681000)
	libm.so.6                                        => /nix/store/*-glibc-2.27/lib/libm.so.6       (0x00007f13cec90000)
	libpthread.so.0                                  => /nix/store/*-glibc-2.27/lib/libpthread.so.0 (0x00007f13ce0ae000)
	librt.so.1                                       => /nix/store/*-glibc-2.27/lib/librt.so.1      (0x00007f13cea88000)
	libutil.so.1                                     => /nix/store/*-glibc-2.27/lib/libutil.so.1    (0x00007f13ce885000)
	linux-vdso.so.1                                  =>                                             (0x00007fff67bbd000)
```

:

```sh
$ make nix-cabal-static

...
/nix/store/*-binutils-2.30/bin/ld:
    /nix/store/*-gcc-7.3.0/lib/gcc/x86_64-unknown-linux-gnu/7.3.0/crtbeginT.o:
        relocation R_X86_64_32 against hidden symbol `__TMC_END__' can not be used when making a shared object

/nix/store/*-binutils-2.30/bin/ld:
    final link failed: Nonrepresentable section on output
```

```nix

ld: .../ghc-8.6.3/rts/libHSrts_thr.a(Linker.thr_o):
    in function `internal_dlopen':
        Linker.c:(.text.internal_dlopen+0x27): 
            warning: Using 'dlopen' in statically linked applications requires at runtime the shared libraries from the glibc version used for linking

ld: .../ghc-8.6.3/rts/libHSrts_thr.a(Task.thr_o):
    undefined reference to symbol '__tls_get_addr@@GLIBC_2.3'
    ld: .../glibc-2.27/lib/ld-linux-x86-64.so.2:
        error adding symbols: DSO missing from command line
            collect2: error: ld returned 1 exit status
```

building `ghc` via `musl` (i.e. `pkgsCross.musl64.haskell.packages.ghc863`):

```nix
configure: error: Cannot find system libffi
```


```nix
```


## 




















# Cross-Compilation

## "Beginner’s guide to cross compilation in Nixpkgs"

<https://matthewbauer.us/blog/beginners-guide-to-cross.html>

> a short list of cross package sets, with their corresponding attribute names:

* x86_64 Musl  — `pkgsCross.musl64`

















# 















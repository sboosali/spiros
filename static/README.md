# Fully statically-linked `example-spiros`

`default.nix` builds a fully statically-linked executable, which should work on *any* 64-bit Linux distribution.

It uses `nix`'s cross-compilation support to build everything (including `ghc`) against the `musl` `libc`.

## Building

```sh
$(nix-build --no-out-link -A stack2nix-script) ../spiros
$(nix-build --no-out-link -A build-script)
```

(We use the `$(nix-build ...)` script approach in order to pin the version of `nix` itself for reproducibility.)

## Dependencies

Compare the dynamically-linked executable:

```sh
$ make dynamic

  cabal new-build "spiros:exe:example-spiros"
  ...

$ ldd $(cabal new-exec -- which example-spiros)

  linux-vdso.so.1                                  =>                                             (0x00007ffefad59000)

  libm.so.6                                        => /nix/store/*-glibc-2.27/lib/libm.so.6       (0x00007f3e6febd000)
  librt.so.1                                       => /nix/store/*-glibc-2.27/lib/librt.so.1      (0x00007f3e6feb3000)
  libutil.so.1                                     => /nix/store/*-glibc-2.27/lib/libutil.so.1    (0x00007f3e6fead000)
  libdl.so.2                                       => /nix/store/*-glibc-2.27/lib/libdl.so.2      (0x00007f3e6fea8000)
  libpthread.so.0                                  => /nix/store/*-glibc-2.27/lib/libpthread.so.0 (0x00007f3e6fe87000)
  libgmp.so.10                                     => /nix/store/*-gmp-6.1.2/lib/libgmp.so.10     (0x00007f3e6fd99000)
  libffi.so.6                                      => /nix/store/*-libffi-3.2.1/lib/libffi.so.6   (0x00007f3e6fe79000)
  libc.so.6                                        => /nix/store/*-glibc-2.27/lib/libc.so.6       (0x00007f3e6fbe3000)

  /nix/store/*-glibc-2.27/lib/ld-linux-x86-64.so.2 => /lib64/ld-linux-x86-64.so.2                 (0x00007f3e6fe2f000)
```

to the statically-linked executable:

```sh
$ make static

$ ldd $(cabal new-exec -- which example-spiros)
	not a dynamic executable
```

that is, these libraries (below) are all archived-into a single executable:

* `glibc-2.27/ld`
* `libc.so.6`
* `libdl.so.2`
* `libffi.so.6`
* `libgmp.so.10`
* `libm.so.6`
* `libpthread.so.0`
* `librt.so.1`
* `libutil.so.1`
* `linux-vdso.so.1` 

## Binary Caches

For faster building (optional), you can use the caches described in the [`nh2/static-haskell-nix` README](https://github.com/nh2/static-haskell-nix/blob/master/README.md#binary-caches-for-faster-building-optional).

## 


(define-module (gnu packages rav1e)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly) 
  #:use-module (gnu packages crates-io) 
;  #:use-module (gnu packages rust-apps)
  )

(define-public rust-output-vt100-0.1
  (package
    (name "rust-output-vt100")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "output_vt100" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ygqplpxz4gg3i8f3rkan2q69pqll7gv65l2mmd8r9dphnvwbkak"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/Phundrak/output-vt100-rs")
    (synopsis
      "Utility to activate escape codes in Windows' CMD and PowerShell")
    (description
      "Utility to activate escape codes in Windows' CMD and PowerShell")
    (license license:expat)))

(define-public rust-ctor-0.1
  (package
    (name "rust-ctor")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ctor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qgwkwyxishpp3wkbwq5i27zdxz539ii0sz129xj061ffnnfbia7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/mmastrac/rust-ctor")
    (synopsis
      "__attribute__((constructor)) for Rust")
    (description
      "__attribute__((constructor)) for Rust")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pretty-assertions-0.6
  (package
    (name "rust-pretty-assertions")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty_assertions" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09yl14gnmpygiqrdlsa64lcl4w6ydjl9m8jri6kgam0v9rjf309z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ctor" ,rust-ctor-0.1)
         ("rust-output-vt100" ,rust-output-vt100-0.1)
         ("rust-ansi-term" ,rust-ansi-term-0.11)
         ("rust-difference" ,rust-difference-2.0))))
    (home-page
      "https://github.com/colin-kiegel/rust-pretty-assertions")
    (synopsis
      "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
    (description
      "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-oorandom-11.1
  (package
    (name "rust-oorandom")
    (version "11.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "oorandom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01clxfnz1zwg4maynvbgj09wlkj5m3c8kjqfrp3sqp59qb4wgkpb"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://hg.sr.ht/~icefox/oorandom")
    (synopsis "A tiny, robust PRNG implementation.")
    (description
      "This package provides a tiny, robust PRNG implementation.")
    (license license:expat)))

(define-public rust-utf8-ranges-1.0
  (package
    (name "rust-utf8-ranges")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "utf8-ranges" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fpc32znar5v02nwsw7icl41jzzzzhy0si6ngqjylzrbxxpi3bml"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/BurntSushi/utf8-ranges")
    (synopsis
      "DEPRECATED. Use regex-syntax::utf8 submodule instead.")
    (description
      "DEPRECATED.  Use regex-syntax::utf8 submodule instead.")
    (license (list license:unlicense license:expat))))

(define-public rust-fst-0.4
  (package
    (name "rust-fst")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fst" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ybmdzkknhv1wx6ws86iyixfyzc04l4nm71b9va7953r1m3i6z1z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-utf8-ranges" ,rust-utf8-ranges-1.0))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).
")
    (description
      "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).
")
    (license (list license:unlicense license:expat))))

(define-public rust-regex-automata-0.1
  (package
    (name "rust-regex-automata")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-automata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r3aqa9c0s9sfrmd2w0mli16ldjzbar0rzb1x7srfjkasrqys7df"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-fst" ,rust-fst-0.4)
         ("rust-regex-syntax" ,rust-regex-syntax-0.6)
         ("rust-byteorder" ,rust-byteorder-1.3))))
    (home-page
      "https://github.com/BurntSushi/regex-automata")
    (synopsis
      "Automata construction and matching using regular expressions.")
    (description
      "Automata construction and matching using regular expressions.")
    (license (list license:unlicense license:expat))))

(define-public rust-bstr-0.2
  (package
    (name "rust-bstr")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bstr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hazfback6i2k3vhhwyj8h46id3y58zxqh22pz46hj9r1zayd298"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-regex-automata" ,rust-regex-automata-0.1)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-memchr" ,rust-memchr-2.3))))
    (home-page "https://github.com/BurntSushi/bstr")
    (synopsis
      "A string type that is not required to be valid UTF-8.")
    (description
      "This package provides a string type that is not required to be valid UTF-8.")
    (license (list license:expat license:asl2.0))))

(define-public rust-csv-core-0.1
  (package
    (name "rust-csv-core")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "csv-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "145wcc3560v1kmysqqspvddppiysr2rifqzy4nnlh3r6kxanc91b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-memchr" ,rust-memchr-2.3))))
    (home-page
      "https://github.com/BurntSushi/rust-csv")
    (synopsis
      "Bare bones CSV parsing with no_std support.")
    (description
      "Bare bones CSV parsing with no_std support.")
    (license (list license:unlicense license:expat))))

(define-public rust-csv-1.1
  (package
    (name "rust-csv")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "csv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yd2z55m2pg4al4yng4nl2y7c9dw2v7yhg5ynihxyrmmd9zzxbq0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-csv-core" ,rust-csv-core-0.1)
         ("rust-bstr" ,rust-bstr-0.2)
         ("rust-itoa" ,rust-itoa-0.4)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-ryu" ,rust-ryu-1.0))))
    (home-page
      "https://github.com/BurntSushi/rust-csv")
    (synopsis
      "Fast CSV parsing with support for serde.")
    (description
      "Fast CSV parsing with support for serde.")
    (license (list license:unlicense license:expat))))

(define-public rust-tinytemplate-1.0
  (package
    (name "rust-tinytemplate")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tinytemplate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06ipxjwl1w6synvql8b50qxbqv0w04agvmmfqcdynr9ygmkcd8sp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page
      "https://github.com/bheisler/TinyTemplate")
    (synopsis "Simple, lightweight template engine")
    (description
      "Simple, lightweight template engine")
    (license (list license:asl2.0 license:expat))))

(define-public rust-cast-0.2
  (package
    (name "rust-cast")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c5z7zryj0zwnhdgs6rw5dfvnlwc1vm19jzrlgx5055alnwk952b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-version" ,rust-rustc-version-0.2))))
    (home-page "https://github.com/japaric/cast.rs")
    (synopsis
      "Ergonomic, checked cast functions for primitive types")
    (description
      "Ergonomic, checked cast functions for primitive types")
    (license (list license:expat license:asl2.0))))

(define-public rust-phf-codegen-0.8
  (package
    (name "rust-phf-codegen")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05d8w7aqqjb6039pfm6404gk5dlwrrf97kiy1n21212vb1hyxzyb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-phf-generator" ,rust-phf-generator-0.8)
         ("rust-phf-shared" ,rust-phf-shared-0.8))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis "Codegen library for PHF types")
    (description "Codegen library for PHF types")
    (license license:expat)))

(define-public rust-phf-generator-0.8
  (package
    (name "rust-phf-generator")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09i5338d1kixq6a60fcayz6awgxjlxcfw9ic5f02abbgr067ydhp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-criterion" ,rust-criterion-0.3)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-phf-shared" ,rust-phf-shared-0.8))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis "PHF generation logic")
    (description "PHF generation logic")
    (license license:expat)))

(define-public rust-phf-macros-0.8
  (package
    (name "rust-phf-macros")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "170qm6yqn6b9mjlwb2xmm3iad9d5nzwgfawfwy7zr7s2zwcdwvvz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-hack"
          ,rust-proc-macro-hack-0.5)
         ("rust-phf-generator" ,rust-phf-generator-0.8)
         ("rust-phf-shared" ,rust-phf-shared-0.8)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis
      "Macros to generate types in the phf crate")
    (description
      "Macros to generate types in the phf crate")
    (license license:expat)))

(define-public rust-unicase-2.6
  (package
    (name "rust-unicase")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicase" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xmlbink4ycgxrkjspp0mf7pghcx4m7vxq7fpfm04ikr2zk7pwsh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-version-check" ,rust-version-check-0.9))))
    (home-page
      "https://github.com/seanmonstar/unicase")
    (synopsis
      "A case-insensitive wrapper around strings.")
    (description
      "This package provides a case-insensitive wrapper around strings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-siphasher-0.3
  (package
    (name "rust-siphasher")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "siphasher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08xvk3yi4vawppm1f81s4zrkksf95psz8gczh36y808candgi24f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0))))
    (home-page "https://docs.rs/siphasher")
    (synopsis
      "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (description
      "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-phf-shared-0.8
  (package
    (name "rust-phf-shared")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xssnqrrcn0nr9ayqrnm8xm37ac4xvwcx8pax7jxss7yxawzh360"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-siphasher" ,rust-siphasher-0.3)
         ("rust-unicase" ,rust-unicase-2.6))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis "Support code shared by PHF libraries")
    (description
      "Support code shared by PHF libraries")
    (license license:expat)))

(define-public rust-phf-0.8
  (package
    (name "rust-phf")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04pyv8bzqvw69rd5dynd5nb85py1hf7wa4ixyhrvdz1l5qin3yrx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-hack"
          ,rust-proc-macro-hack-0.5)
         ("rust-phf-shared" ,rust-phf-shared-0.8)
         ("rust-phf-macros" ,rust-phf-macros-0.8))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis
      "Runtime support for perfect hash function data structures")
    (description
      "Runtime support for perfect hash function data structures")
    (license license:expat)))

(define-public rust-palette-derive-0.5
  (package
    (name "rust-palette-derive")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "palette_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x5icddb877923rpl27bg4cjsf1x0d3layxmgwa3mpb01rh5yjqb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/Ogeon/palette")
    (synopsis
      "Automatically implement traits from the palette crate.")
    (description
      "Automatically implement traits from the palette crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-palette-0.5
  (package
    (name "rust-palette")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "palette" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nfc4ycdsx2qgf2wkcpxqxc0vmx7188jjjx3ppgs8qlf8qs06p50"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-approx" ,rust-approx-0.3)
         ("rust-palette-derive" ,rust-palette-derive-0.5)
         ("rust-phf" ,rust-phf-0.8)
         ("rust-phf-codegen" ,rust-phf-codegen-0.8)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/Ogeon/palette")
    (synopsis
      "Makes linear color calculations and conversion easy and accessible for anyone.")
    (description
      "Makes linear color calculations and conversion easy and accessible for anyone.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-utils-0.7
  (package
    (name "rust-crossbeam-utils")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a31wbrda1320gj2a6az1lin2d34xfc3xf88da4c17qy5lxcgiy3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-autocfg" ,rust-autocfg-1.0)
         ("rust-lazy-static" ,rust-lazy-static-1.4))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
      "Utilities for concurrent programming")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-argon2-0.7
  (package
    (name "rust-rust-argon2")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust-argon2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05xh5wfxgzq3b6jys8r34f3hmqqfs8ylvf934n9z87wfv95szj1b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-constant-time-eq"
          ,rust-constant-time-eq-0.1)
         ("rust-base64" ,rust-base64-0.11)
         ("rust-blake2b-simd" ,rust-blake2b-simd-0.5)
         ("rust-crossbeam-utils"
          ,rust-crossbeam-utils-0.7))))
    (home-page
      "https://github.com/sru-systems/rust-argon2")
    (synopsis
      "Rust implementation of the Argon2 password hashing function.")
    (description
      "Rust implementation of the Argon2 password hashing function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasi-0.9
  (package
    (name "rust-wasi")
    (version "0.9.0+wasi-snapshot-preview1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06g5v3vrdapfzvfq662cij7v8a1flwr2my45nnncdv2galrdzkfc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-compiler-builtins"
          ,rust-compiler-builtins-0.1)
         ("rust-rustc-std-workspace-alloc"
          ,rust-rustc-std-workspace-alloc-1.0)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0))))
    (home-page
      "https://github.com/bytecodealliance/wasi")
    (synopsis
      "Experimental WASI API bindings for Rust")
    (description
      "Experimental WASI API bindings for Rust")
    (license
      (list license:asl2.0
            ;XXX unknown-license!
            license:asl2.0
            license:expat))))

(define-public rust-getrandom-0.1
  (package
    (name "rust-getrandom")
    (version "0.1.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getrandom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sq30li71h19rhnhs1h6576ja68insajx8wvh1nn088r8pc8vg3s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-compiler-builtins"
          ,rust-compiler-builtins-0.1)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-stdweb" ,rust-stdweb-0.4)
         ("rust-wasi" ,rust-wasi-0.9)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0))))
    (home-page
      "https://github.com/rust-random/getrandom")
    (synopsis
      "A small cross-platform library for retrieving random data from system source")
    (description
      "This package provides a small cross-platform library for retrieving random data from system source")
    (license (list license:expat license:asl2.0))))

(define-public rust-redox-users-0.3
  (package
    (name "rust-redox-users")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_users" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cbl5w16l3bqm22i4vszclf6hzpljxicghmllw7j13az4s9k1ch9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-getrandom" ,rust-getrandom-0.1)
         ("rust-redox-syscall" ,rust-redox-syscall-0.1)
         ("rust-rust-argon2" ,rust-rust-argon2-0.7))))
    (home-page
      "https://gitlab.redox-os.org/redox-os/users")
    (synopsis
      "A Rust library to access Redox users and groups functionality")
    (description
      "This package provides a Rust library to access Redox users and groups functionality")
    (license license:expat)))

(define-public rust-dirs-1.0
  (package
    (name "rust-dirs")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "009rrhzj9pxyncmm2vhlj70npg0cgggv2hjbbkiwdl9vccq8kmrz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-redox-users" ,rust-redox-users-0.3)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis
      "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
      "This package provides a tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-freetype-0.4
  (package
    (name "rust-freetype")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "freetype" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a70x03n68997f08bi3n47q9wyi3pv5s9v4rjc79sihb84mnp4hi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-servo-freetype-sys"
          ,rust-servo-freetype-sys-4))))
    (home-page
      "https://github.com/servo/rust-freetype")
    (synopsis "Bindings for Freetype used by Servo")
    (description
      "Bindings for Freetype used by Servo")
    (license (list license:asl2.0 license:expat))))

(define-public rust-arrayvec-0.4
  (package
    (name "rust-arrayvec")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arrayvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fdiv5m627gh6flp4mpmi1mh647imm9x423licsr11psz97d97yd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-nodrop" ,rust-nodrop-0.1)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis
      "A vector with fixed capacity, backed by an array (it can be stored on the stack too). Implements fixed capacity ArrayVec and ArrayString.")
    (description
      "This package provides a vector with fixed capacity, backed by an array (it can be stored on the stack too).  Implements fixed capacity ArrayVec and ArrayString.")
    (license (list license:expat license:asl2.0))))

(define-public rust-euclid-0.20
  (package
    (name "rust-euclid")
    (version "0.20.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "euclid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z4b54jbgyb9zwiy7qz4x1adajpml52kxmmykzs841m8937kw1lg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-mint" ,rust-mint-0.5)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/servo/euclid")
    (synopsis "Geometry primitives")
    (description "Geometry primitives")
    (license (list license:expat license:asl2.0))))

(define-public rust-lyon-geom-0.14
  (package
    (name "rust-lyon-geom")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lyon_geom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "178z4cqqmyw0rsabbgx9phkjxjzcnq0604062lqjlq87k063216a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-euclid" ,rust-euclid-0.20)
         ("rust-arrayvec" ,rust-arrayvec-0.4)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/nical/lyon")
    (synopsis
      "2D quadratic and cubic bÃ©zier arcs and line segment math on top of euclid.")
    (description
      "2D quadratic and cubic bÃ©zier arcs and line segment math on top of euclid.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lyon-path-0.14
  (package
    (name "rust-lyon-path")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lyon_path" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qk8x46w0sf6j04l6gvhgn9kr4ymcqkmkh67w8wqahm54jn5gjqb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lyon-geom" ,rust-lyon-geom-0.14)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/nical/lyon")
    (synopsis
      "Types and utilities to store, build and iterate over 2D paths.")
    (description
      "Types and utilities to store, build and iterate over 2D paths.")
    (license (list license:expat license:asl2.0))))

(define-public rust-font-kit-0.4
  (package
    (name "rust-font-kit")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "font-kit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fmg1jmqdvsjxjbyz8chpx1mhp544mwq128ns1shhrha5a6zzdqp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lyon-path" ,rust-lyon-path-0.14)
         ("rust-core-graphics" ,rust-core-graphics-0.17)
         ("rust-float-ord" ,rust-float-ord-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-euclid" ,rust-euclid-0.20)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-servo-fontconfig"
          ,rust-servo-fontconfig-0.4)
         ("rust-freetype" ,rust-freetype-0.4)
         ("rust-log" ,rust-log-0.4)
         ("rust-core-foundation"
          ,rust-core-foundation-0.6)
         ("rust-memmap" ,rust-memmap-0.7)
         ("rust-dwrote" ,rust-dwrote-0.9)
         ("rust-dirs" ,rust-dirs-1.0)
         ("rust-byteorder" ,rust-byteorder-1.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-core-text" ,rust-core-text-13)
         ("rust-walkdir" ,rust-walkdir-2.3))))
    (home-page "https://github.com/servo/font-kit")
    (synopsis
      "A cross-platform font loading library")
    (description
      "This package provides a cross-platform font loading library")
    (license (list license:expat license:asl2.0))))

(define-public rust-web-sys-0.3
  (package
    (name "rust-web-sys")
    (version "0.3.37")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "web-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jy4q5jawzg3dxzhfwa0g3fsz7h4j0ra6y232ikc6mlcimj52vrd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
         ("rust-js-sys" ,rust-js-sys-0.3))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/web-sys/index.html")
    (synopsis
      "Bindings for all Web APIs, a procedurally generated crate from WebIDL
")
    (description
      "Bindings for all Web APIs, a procedurally generated crate from WebIDL
")
    (license (list license:expat license:asl2.0))))

(define-public rust-piston-gfx-texture-0.40
  (package
    (name "rust-piston-gfx-texture")
    (version "0.40.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-gfx_texture" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nr5awdgk3njfvfanszrv4gxz93f6skid1c8yijswccygripchqz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gfx" ,rust-gfx-0.18)
         ("rust-image" ,rust-image-0.22)
         ("rust-piston-texture" ,rust-piston-texture-0.8)
         ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page
      "https://github.com/pistondevelopers/gfx_texture")
    (synopsis
      "A Gfx texture representation that works nicely with Piston libraries")
    (description
      "This package provides a Gfx texture representation that works nicely with Piston libraries")
    (license license:expat)))

(define-public rust-piston-shaders-graphics2d-0.3
  (package
    (name "rust-piston-shaders-graphics2d")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-shaders_graphics2d" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dhh9bv4q19gdnj9d1nqq0yrvzs6gcn0c5j1p1f3xzyzq7d1gg4p"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/PistonDevelopers/shaders")
    (synopsis "Shaders for 2D graphics in Rust")
    (description "Shaders for 2D graphics in Rust")
    (license license:expat)))

(define-public rust-piston2d-gfx-graphics-0.66
  (package
    (name "rust-piston2d-gfx-graphics")
    (version "0.66.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston2d-gfx_graphics" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pmlkf5rl6pr0c1lqm0059xwj9pwlws7gaq9w6r9d916di6fzki1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gfx" ,rust-gfx-0.18)
         ("rust-piston-shaders-graphics2d"
          ,rust-piston-shaders-graphics2d-0.3)
         ("rust-piston2d-graphics"
          ,rust-piston2d-graphics-0.35)
         ("rust-piston-gfx-texture"
          ,rust-piston-gfx-texture-0.40)
         ("rust-shader-version" ,rust-shader-version-0.6)
         ("rust-draw-state" ,rust-draw-state-0.8))))
    (home-page
      "https://github.com/PistonDevelopers/gfx_graphics")
    (synopsis
      "A Gfx 2D back-end for the Piston game engine")
    (description
      "This package provides a Gfx 2D back-end for the Piston game engine")
    (license license:expat)))

(define-public rust-gl-generator-0.10
  (package
    (name "rust-gl-generator")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gl_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0146yd4i9wbgfrhnkc04w7n7civbanznc0q87skp6v7p7hbszzx0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-xml-rs" ,rust-xml-rs-0.8)
         ("rust-khronos-api" ,rust-khronos-api-3))))
    (home-page
      "https://github.com/brendanzab/gl-rs/")
    (synopsis
      "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (description
      "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (license license:asl2.0)))

(define-public rust-gl-0.11
  (package
    (name "rust-gl")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wcqpyhck0xriffkmgmldy33lwk2044hb4l02d44vm4fbvicin6p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gl-generator" ,rust-gl-generator-0.10))))
    (home-page
      "https://github.com/brendanzab/gl-rs/")
    (synopsis "OpenGL bindings")
    (description "OpenGL bindings")
    (license license:asl2.0)))

(define-public rust-pistoncore-glutin-window-0.63
  (package
    (name "rust-pistoncore-glutin-window")
    (version "0.63.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-glutin_window" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dhbyxarv5i742d400bmqdqq3f8c25kcgcg0xavrc18dc913rixc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gl" ,rust-gl-0.11)
         ("rust-glutin" ,rust-glutin-0.21)
         ("rust-pistoncore-input"
          ,rust-pistoncore-input-0.28)
         ("rust-pistoncore-window"
          ,rust-pistoncore-window-0.44)
         ("rust-shader-version" ,rust-shader-version-0.6))))
    (home-page
      "https://github.com/pistondevelopers/glutin_window")
    (synopsis
      "A Piston window back-end using the Glutin library")
    (description
      "This package provides a Piston window back-end using the Glutin library")
    (license license:expat)))

(define-public rust-shader-version-0.6
  (package
    (name "rust-shader-version")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shader_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yk651xc9irl3pl0rlplypzyzy44d0j03ji0j7hjjdjknwzpi3j7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-piston-graphics-api-version"
          ,rust-piston-graphics-api-version-0.2))))
    (home-page
      "https://github.com/pistondevelopers/shader_version")
    (synopsis
      "A helper library for detecting and picking compatible shaders")
    (description
      "This package provides a helper library for detecting and picking compatible shaders")
    (license license:expat)))

(define-public rust-pistoncore-event-loop-0.49
  (package
    (name "rust-pistoncore-event-loop")
    (version "0.49.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-event_loop" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h9ij9vx42xg39198yxdlpk842pli5jqm2kwswiv3bqqcji0fwsm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pistoncore-input"
          ,rust-pistoncore-input-0.28)
         ("rust-pistoncore-window"
          ,rust-pistoncore-window-0.44))))
    (home-page
      "https://github.com/PistonDevelopers/piston")
    (synopsis
      "A Piston event loop for games and interactive applications")
    (description
      "This package provides a Piston event loop for games and interactive applications")
    (license license:expat)))

(define-public rust-piston-graphics-api-version-0.2
  (package
    (name "rust-piston-graphics-api-version")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-graphics_api_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b5p6s45jqv057lpbxkiq3yrdjjhvcynmi2vjf8292rf0yh4hky5"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/PistonDevelopers/graphics_api_version")
    (synopsis
      "A library for storing graphics API versions")
    (description
      "This package provides a library for storing graphics API versions")
    (license license:expat)))

(define-public rust-pistoncore-window-0.44
  (package
    (name "rust-pistoncore-window")
    (version "0.44.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-window" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18qy3nnpb9jczvkiyzzznamck0pzgiyi6073jrkldnci6b3in10q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-piston-graphics-api-version"
          ,rust-piston-graphics-api-version-0.2)
         ("rust-pistoncore-input"
          ,rust-pistoncore-input-0.28))))
    (home-page
      "https://github.com/PistonDevelopers/piston")
    (synopsis "A library for window abstraction")
    (description
      "This package provides a library for window abstraction")
    (license license:expat)))

(define-public rust-pistoncore-input-0.28
  (package
    (name "rust-pistoncore-input")
    (version "0.28.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-input" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rrcz9px098m3nx98gvrvzirfdp3vg03cblfkcrp4wnvswc0hwq5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-piston-viewport"
          ,rust-piston-viewport-1.0)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-derive" ,rust-serde-derive-1.0)
         ("rust-bitflags" ,rust-bitflags-1))))
    (home-page
      "https://github.com/PistonDevelopers/piston")
    (synopsis "A structure for user input")
    (description
      "This package provides a structure for user input")
    (license license:expat)))

(define-public rust-piston-0.49
  (package
    (name "rust-piston")
    (version "0.49.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y0rbw92mzagqmwk79wv9axq0m7aid0s0d5cppyzh33wrxhdl3xj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pistoncore-input"
          ,rust-pistoncore-input-0.28)
         ("rust-pistoncore-window"
          ,rust-pistoncore-window-0.44)
         ("rust-pistoncore-event-loop"
          ,rust-pistoncore-event-loop-0.49))))
    (home-page
      "https://github.com/PistonDevelopers/piston")
    (synopsis
      "The Piston game engine core libraries")
    (description
      "The Piston game engine core libraries")
    (license license:expat)))

(define-public rust-vecmath-1.0
  (package
    (name "rust-vecmath")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vecmath" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0shmj76rj7rqv377vy365xwr5rx23kxqgkqxxrymdjjvv3hf2slm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-piston-float" ,rust-piston-float-1.0))))
    (home-page
      "https://github.com/pistondevelopers/vecmath")
    (synopsis
      "A simple and type agnostic library for vector math designed for reexporting")
    (description
      "This package provides a simple and type agnostic library for vector math designed for reexporting")
    (license license:expat)))

(define-public rust-read-color-1.0
  (package
    (name "rust-read-color")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "read_color" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1np0pk31ak7hni4hri3m75mbf8py1wdfjshmrj5krbd4p9c8hk4z"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/read_color")
    (synopsis
      "A simple library for reading hex colors")
    (description
      "This package provides a simple library for reading hex colors")
    (license (list license:expat license:asl2.0))))

(define-public rust-piston-float-1.0
  (package
    (name "rust-piston-float")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-float" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r35aasycms79hf2vf1ap40kkp8ywgl4hmfkf762dq8jwd3vw07r"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/float")
    (synopsis
      "Traits for generic floats in game development")
    (description
      "Traits for generic floats in game development")
    (license license:expat)))

(define-public rust-piston-viewport-1.0
  (package
    (name "rust-piston-viewport")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-viewport" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16378hcy41b7x3zj2z4har0wq6fl4r62kf9p106jjl8hg2dv3aq1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-piston-float" ,rust-piston-float-1.0))))
    (home-page
      "https://github.com/PistonDevelopers/viewport")
    (synopsis
      "A library for storing viewport information")
    (description
      "This package provides a library for storing viewport information")
    (license license:expat)))

(define-public rust-piston-texture-0.8
  (package
    (name "rust-piston-texture")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-texture" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pcv5my49b8xzqcb87wqh2ndgvr4s9ipys96s0h9j2plxrj3bjb2"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/texture")
    (synopsis "A generic library for textures")
    (description
      "This package provides a generic library for textures")
    (license license:expat)))

(define-public rust-interpolation-0.2
  (package
    (name "rust-interpolation")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "interpolation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00icvvgc72zdgyrwwg2p0wad4hry4d2vd6l9iqpyjpmw5dykbdyk"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/interpolation")
    (synopsis "A library for interpolation")
    (description
      "This package provides a library for interpolation")
    (license license:expat)))

(define-public rust-piston2d-graphics-0.35
  (package
    (name "rust-piston2d-graphics")
    (version "0.35.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston2d-graphics" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dx2fanxc2pj76hc5l72x0fh4qg9gchjlr8rmbhdk6jpggcmq56g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-interpolation" ,rust-interpolation-0.2)
         ("rust-rusttype" ,rust-rusttype-0.7)
         ("rust-piston-texture" ,rust-piston-texture-0.8)
         ("rust-piston-viewport"
          ,rust-piston-viewport-1.0)
         ("rust-read-color" ,rust-read-color-1.0)
         ("rust-vecmath" ,rust-vecmath-1.0)
         ("rust-fnv" ,rust-fnv-1.0))))
    (home-page
      "https://github.com/pistondevelopers/graphics")
    (synopsis
      "A library for 2D graphics that works with multiple back-ends")
    (description
      "This package provides a library for 2D graphics that works with multiple back-ends")
    (license license:expat)))

(define-public rust-gfx-0.18
  (package
    (name "rust-gfx")
    (version "0.18.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nqmxqi3x4ni0g78g77a6aldrv8cfvzhnpqhxyd2ap4aa3wldph1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-mint" ,rust-mint-0.5)
         ("rust-draw-state" ,rust-draw-state-0.8)
         ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis
      "A high-performance, bindless graphics API")
    (description
      "This package provides a high-performance, bindless graphics API")
    (license license:asl2.0)))

(define-public rust-draw-state-0.8
  (package
    (name "rust-draw-state")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "draw_state" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lfng4fz9x7bwsmzv9r20ply10w0iid6vfcrhx292s6hw8vrbkrk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0)
         ("rust-bitflags" ,rust-bitflags-1))))
    (home-page
      "https://github.com/gfx-rs/draw_state")
    (synopsis "Graphics state blocks for gfx-rs")
    (description "Graphics state blocks for gfx-rs")
    (license license:asl2.0)))

(define-public rust-gfx-core-0.9
  (package
    (name "rust-gfx-core")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0haldr99n12d90vqgvl77n59hywlklhdff85j2aljaz1yapdvyvm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-mint" ,rust-mint-0.5)
         ("rust-draw-state" ,rust-draw-state-0.8)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "Core library of Gfx-rs")
    (description "Core library of Gfx-rs")
    (license license:asl2.0)))

(define-public rust-gfx-gl-0.6
  (package
    (name "rust-gfx-gl")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx_gl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ppzj4bgjawdqz3fvnscqk8lnmgh95pwzh0v96vwy809cxj83lzj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gl-generator" ,rust-gl-generator-0.14))))
    (home-page "https://github.com/gfx-rs/gfx_gl")
    (synopsis
      "OpenGL bindings for gfx, based on gl-rs")
    (description
      "OpenGL bindings for gfx, based on gl-rs")
    (license license:asl2.0)))

(define-public rust-gfx-device-gl-0.16
  (package
    (name "rust-gfx-device-gl")
    (version "0.16.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx_device_gl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1g5yg19jvxdmviljyakhd6253bnb2qg7v8iscf48ihc0ldgki70h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-gfx-gl" ,rust-gfx-gl-0.6)
         ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "OpenGL backend for gfx-rs")
    (description "OpenGL backend for gfx-rs")
    (license license:asl2.0)))

(define-public rust-piston-window-0.105
  (package
    (name "rust-piston-window")
    (version "0.105.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston_window" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05n2905gkp5ck25kbq95ia6pj1xz63dpp247jz3xcw1d41xpvi95"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gfx-device-gl" ,rust-gfx-device-gl-0.16)
         ("rust-gfx" ,rust-gfx-0.18)
         ("rust-piston2d-graphics"
          ,rust-piston2d-graphics-0.35)
         ("rust-piston" ,rust-piston-0.49)
         ("rust-shader-version" ,rust-shader-version-0.6)
         ("rust-pistoncore-glutin-window"
          ,rust-pistoncore-glutin-window-0.63)
         ("rust-piston2d-gfx-graphics"
          ,rust-piston2d-gfx-graphics-0.66)
         ("rust-piston-texture" ,rust-piston-texture-0.8))))
    (home-page
      "https://github.com/pistondevelopers/piston_window")
    (synopsis
      "The official Piston window wrapper for the Piston game engine")
    (description
      "The official Piston window wrapper for the Piston game engine")
    (license license:expat)))

(define-public rust-plotters-0.2
  (package
    (name "rust-plotters")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "plotters" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ssycy9an23vs9hq098c7kl1dvp5ych20d994lhsw9vx4kdbhfsf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gif" ,rust-gif-0.10)
         ("rust-piston-window" ,rust-piston-window-0.105)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
         ("rust-image" ,rust-image-0.22)
         ("rust-js-sys" ,rust-js-sys-0.3)
         ("rust-web-sys" ,rust-web-sys-0.3)
         ("rust-font-kit" ,rust-font-kit-0.4)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-palette" ,rust-palette-0.5)
         ("rust-cairo-rs" ,rust-cairo-rs-0.7)
         ("rust-rusttype" ,rust-rusttype-0.8)
         ("rust-lazy-static" ,rust-lazy-static-1.4))))
    (home-page "https://github.com/38/plotters")
    (synopsis
      "A Rust drawing library focus on data plotting for both WASM and native applications")
    (description
      "This package provides a Rust drawing library focus on data plotting for both WASM and native applications")
    (license license:expat)))

(define-public rust-criterion-0.3
  (package
    (name "rust-criterion")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "criterion" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lgpr82rlmg6rm4gr3c3pla2xgxnakbf8w9sabjsig8jkikmbiqz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-plotters" ,rust-plotters-0.2)
         ("rust-atty" ,rust-atty-0.2)
         ("rust-cast" ,rust-cast-0.2)
         ("rust-criterion-plot" ,rust-criterion-plot-0.4)
         ("rust-itertools" ,rust-itertools-0.8)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-derive" ,rust-serde-derive-1.0)
         ("rust-tinytemplate" ,rust-tinytemplate-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0)
         ("rust-csv" ,rust-csv-1.1)
         ("rust-rayon" ,rust-rayon-1.3)
         ("rust-regex" ,rust-regex-1.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-oorandom" ,rust-oorandom-11.1)
         ("rust-walkdir" ,rust-walkdir-2.3)
         ("rust-clap" ,rust-clap-2))))
    (home-page
      "https://bheisler.github.io/criterion.rs/book/index.html")
    (synopsis
      "Statistics-driven micro-benchmarking library")
    (description
      "Statistics-driven micro-benchmarking library")
    (license (list license:asl2.0 license:expat))))

(define-public rust-vergen-3.1
  (package
    (name "rust-vergen")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vergen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jrr0wihm9si98qz8ghjfnalfvmfv8rqvkgj2npqa7yzjs4hvrac"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-chrono" ,rust-chrono-0.4)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-bitflags" ,rust-bitflags-1))))
    (home-page "http://github.com/rustyhorde/vergen")
    (synopsis "Generate version related functions")
    (description
      "Generate version related functions")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-hawktracer-proc-macro-0.4
  (package
    (name "rust-rust-hawktracer-proc-macro")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust_hawktracer_proc_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qfksscfv8rbbzv2zb0i9sbbqmig0dr0vrma3c1kzsfmpsynlqnb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rust-hawktracer-sys"
          ,rust-rust-hawktracer-sys-0.4))))
    (home-page
      "https://github.com/AlexEne/rust_hawktracer_proc_macro")
    (synopsis
      "helper crate for hawktracer profiling library.")
    (description
      "helper crate for hawktracer profiling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-either-1.5
  (package
    (name "rust-either")
    (version "1.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qyz1b1acad6w0k5928jw5zaq900zhsk7p8dlcp4hh61w4f6n7xv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/bluss/either")
    (synopsis
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
    (description
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-itertools-0.8
  (package
    (name "rust-itertools")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1154j48aw913v5jnyhpxialxhdn2sfpl4d7bwididyb1r05jsspm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-either" ,rust-either-1.5))))
    (home-page
      "https://github.com/bluss/rust-itertools")
    (synopsis
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-0.3
  (package
    (name "rust-proc-macro2")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ryaynnaj39l4zphcg5w8wszndd80vsrv89m5d2293gl6pry41hv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.1))))
    (home-page
      "https://github.com/alexcrichton/proc-macro2")
    (synopsis
      "A substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (description
      "This package provides a substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote-0.5
  (package
    (name "rust-quote")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s01fh0jl8qv4xggs85yahw0h507nzrxkjbf7vay3zw8d3kcyjcr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-0.3))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-0.3
  (package
    (name "rust-proc-macro2")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m0ksg6hbm46zblq0dpkwrg3n1h7n90yq1zcgwc6vpbfmr9pr6bp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.1))))
    (home-page
      "https://github.com/alexcrichton/proc-macro2")
    (synopsis
      "A substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (description
      "This package provides a substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-clang-sys-0.23
  (package
    (name "rust-clang-sys")
    (version "0.23.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hjr333izzhs6bic84qwnyzy5xzmvasib8f3zkzj4ln3a97c1xyp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-glob" ,rust-glob-0.2)
         ("rust-glob" ,rust-glob-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libloading" ,rust-libloading-0.5))))
    (home-page
      "https://github.com/KyleMayes/clang-sys")
    (synopsis "Rust bindings for libclang.")
    (description "Rust bindings for libclang.")
    (license license:asl2.0)))

(define-public rust-bindgen-0.37
  (package
    (name "rust-bindgen")
    (version "0.37.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08f2cyzr8fc027mzj2lhmn5j3w318g2ql7yfw5ngxa3yhy1an98v"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-peeking-take-while"
          ,rust-peeking-take-while-0.1)
         ("rust-cexpr" ,rust-cexpr-0.2)
         ("rust-clang-sys" ,rust-clang-sys-0.23)
         ("rust-proc-macro2" ,rust-proc-macro2-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-env-logger" ,rust-env-logger-0.5)
         ("rust-quote" ,rust-quote-0.5)
         ("rust-which" ,rust-which-1.0)
         ("rust-regex" ,rust-regex-1.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-clap" ,rust-clap-2))))
    (home-page
      "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-rust-hawktracer-sys-0.4
  (package
    (name "rust-rust-hawktracer-sys")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust_hawktracer_sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15acrj881y2g7cwsgf1nr22cixrknp8m4x08dkx1an6zf4q8bk37"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cmake" ,rust-cmake-0.1)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-bindgen" ,rust-bindgen-0.37)
         ("rust-itertools" ,rust-itertools-0.8))))
    (home-page
      "https://github.com/AlexEne/rust_hawktracer_sys")
    (synopsis
      "sys crate for the rust_hawktracer library")
    (description
      "sys crate for the rust_hawktracer library")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-hawktracer-normal-macro-0.4
  (package
    (name "rust-rust-hawktracer-normal-macro")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri
               "rust_hawktracer_normal_macro"
               version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sfjmipdbb5s498c150czr6wihjlkwwgla2jyg3cs7cyjich0mwa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rust-hawktracer-sys"
          ,rust-rust-hawktracer-sys-0.4))))
    (home-page
      "https://github.com/AlexEne/rust_hawktracer_normal_macro")
    (synopsis
      "helper crate for hawktracer profiling library.")
    (description
      "helper crate for hawktracer profiling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-hawktracer-0.7
  (package
    (name "rust-rust-hawktracer")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust_hawktracer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h9an3b73pmhhpzc2kk93nh93lplkvsffysj0rp6rxi7p4lhlj73"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rust-hawktracer-normal-macro"
          ,rust-rust-hawktracer-normal-macro-0.4)
         ("rust-rust-hawktracer-proc-macro"
          ,rust-rust-hawktracer-proc-macro-0.4))))
    (home-page
      "https://github.com/AlexEne/rust_hawktracer")
    (synopsis
      "Rust bindings for hawktracer profiling library.")
    (description
      "Rust bindings for hawktracer profiling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-error-chain-0.11
  (package
    (name "rust-error-chain")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-chain" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wykkr0naizbkwxjwia1rch8xhwvgij9khqvjzs07mrmqifislgz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-backtrace" ,rust-backtrace-0.3))))
    (home-page
      "https://github.com/rust-lang-nursery/error-chain")
    (synopsis
      "Yet another error boilerplate library.")
    (description
      "Yet another error boilerplate library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-syslog-4.0
  (package
    (name "rust-syslog")
    (version "4.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syslog" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09ykcbvwx8icvf303mqyz76ji8j6fgyyx97zpr23s788ni112r50"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-time" ,rust-time-0.1)
         ("rust-error-chain" ,rust-error-chain-0.11)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/Geal/rust-syslog")
    (synopsis "Send log messages to syslog")
    (description "Send log messages to syslog")
    (license license:expat)))

(define-public rust-unix-socket-0.5
  (package
    (name "rust-unix-socket")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unix_socket" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r0mxf3mmqvimnx4mpks1f6c4haj6jcxc0k9bs7w61f42w2718ka"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page
      "https://github.com/rust-lang-nursery/unix-socket")
    (synopsis "Unix domain socket bindings")
    (description "Unix domain socket bindings")
    (license (list license:expat license:asl2.0))))

(define-public rust-log-0.3
  (package
    (name "rust-log")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jq23hhn5h35k7pa8r7wqnsywji6x3wn1q5q7lif5q536if8v7p1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/rust-lang/log")
    (synopsis
      "A lightweight logging facade for Rust
")
    (description
      "This package provides a lightweight logging facade for Rust
")
    (license (list license:expat license:asl2.0))))

(define-public rust-syslog-3.3
  (package
    (name "rust-syslog")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syslog" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hpdnk2zm6xawpz6fv6qbn0ncfm5p0wm5c6gq7yhaz2gvsnb1jdv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-time" ,rust-time-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.3)
         ("rust-unix-socket" ,rust-unix-socket-0.5))))
    (home-page "https://github.com/Geal/rust-syslog")
    (synopsis "Send log messages to syslog")
    (description "Send log messages to syslog")
    (license license:expat)))

(define-public rust-hermit-abi-0.1
  (package
    (name "rust-hermit-abi")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hermit-abi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0blmmzik5cs79ivq70s9gal8ypgzj50wnl2hwsaam46gjjbz2p3j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-compiler-builtins"
          ,rust-compiler-builtins-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0))))
    (home-page
      "https://github.com/hermitcore/rusty-hermit")
    (synopsis
      "hermit-abi is small interface to call functions from the unikernel RustyHermit.
It is used to build the target `x86_64-unknown-hermit`.
")
    (description
      "hermit-abi is small interface to call functions from the unikernel RustyHermit.
It is used to build the target `x86_64-unknown-hermit`.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-atty-0.2
  (package
    (name "rust-atty")
    (version "0.2.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-hermit-abi" ,rust-hermit-abi-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
      "This package provides a simple interface for querying atty")
    (license license:expat)))

(define-public rust-colored-1.9
  (package
    (name "rust-colored")
    (version "1.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "colored" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nbc1czs512h1k696y7glv1kjrb2b914zpxraic6q5fgv80wizzl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-atty" ,rust-atty-0.2)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4))))
    (home-page "https://github.com/mackwic/colored")
    (synopsis
      "The most simple way to add colors in your terminal")
    (description
      "The most simple way to add colors in your terminal")
    (license license:mpl2.0)))

(define-public rust-js-sys-0.3
  (package
    (name "rust-js-sys")
    (version "0.3.37")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "js-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mbhpbw3zjjl51m24qx3ilq4y8xipm5sfa5hsavaabqs6wsx89va"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (description
      "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-bumpalo-3.2
  (package
    (name "rust-bumpalo")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bumpalo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11silgpsnfv6ir7j2nh7a69564f92vq20k9ha7zcbynpiav9vbhj"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/fitzgen/bumpalo")
    (synopsis
      "A fast bump allocation arena for Rust.")
    (description
      "This package provides a fast bump allocation arena for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ffn4152w8n629f29lwjgj3adiyixvdbff3mld49gisssbknzxys"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (description
      "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k9p8a7ng6nqan0m9555wj936lm2s1qz0fnafclwlv61yrxx6ryr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen-shared"
          ,rust-wasm-bindgen-shared-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-bumpalo" ,rust-bumpalo-3.2))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Backend code generation of the wasm-bindgen tool
")
    (description
      "Backend code generation of the wasm-bindgen tool
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-support-0.2
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro-support" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ca9bb9hnyzcmjww83x8asb76drf55ijhqv8yrl7igpixqv5p2nn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen-backend"
          ,rust-wasm-bindgen-backend-0.2)
         ("rust-wasm-bindgen-shared"
          ,rust-wasm-bindgen-shared-0.2)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (description
      "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-0.2
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1973xr0vr2aj85fkp3smk61z5ki7c4fhxlicfjxq3a0y7sv53lcb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen-macro-support"
          ,rust-wasm-bindgen-macro-support-0.2)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (description
      "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-0.2
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zx42zryw03w3maz8p65gr5bhhybr2sdzgcck5p3gy47abh7ri9c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-wasm-bindgen-macro"
          ,rust-wasm-bindgen-macro-0.2)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page "https://rustwasm.github.io/")
    (synopsis
      "Easy support for interacting between JS and Rust.
")
    (description
      "Easy support for interacting between JS and Rust.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cmmxamkzzs36zncqjjr7qm7xkb6zyrkjslnlj3axdgqki84y2c0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-time" ,rust-time-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
         ("rust-rustc-serialize"
          ,rust-rustc-serialize-0.3)
         ("rust-js-sys" ,rust-js-sys-0.3)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page
      "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-reopen-0.3
  (package
    (name "rust-reopen")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "reopen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12b3mfxkwb8akdfa701nzvqr6lsc6n84vrq088gmjy8lxlmr4an6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-signal-hook" ,rust-signal-hook-0.1)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/vorner/reopen")
    (synopsis "File reopening utility")
    (description "File reopening utility")
    (license (list license:asl2.0 license:expat))))

(define-public rust-fern-0.5
  (package
    (name "rust-fern")
    (version "0.5.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fern" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1anslk0hx9an4ypcaxqff080hgbcxm7ji7d4qf4f6qx1mkav16p6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-reopen" ,rust-reopen-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-colored" ,rust-colored-1.9)
         ("rust-syslog" ,rust-syslog-3.3)
         ("rust-syslog" ,rust-syslog-4.0))))
    (home-page "https://github.com/daboross/fern")
    (synopsis "Simple, efficient logging")
    (description "Simple, efficient logging")
    (license license:expat)))

(define-public rust-y4m-0.5
  (package
    (name "rust-y4m")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "y4m" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06g8c53qk4cla3xczywx5qlklvzsw54x77vm727mhizlsp5n93ar"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/image-rs/y4m")
    (synopsis "YUV4MPEG2 (.y4m) Encoder/Decoder.")
    (description "YUV4MPEG2 (.y4m) Encoder/Decoder.")
    (license license:expat)))

(define-public rust-bindgen-0.52
  (package
    (name "rust-bindgen")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mzy2gjiaggl602yn4a11xzrxfj18kl7pwqa5yv32njkxd257j7i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-shlex" ,rust-shlex-0.1)
         ("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-peeking-take-while"
          ,rust-peeking-take-while-0.1)
         ("rust-clang-sys" ,rust-clang-sys-0.28)
         ("rust-cexpr" ,rust-cexpr-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-env-logger" ,rust-env-logger-0.7)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-quote" ,rust-quote-1.0)
         ("rust-rustc-hash" ,rust-rustc-hash-1.1)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-lazycell" ,rust-lazycell-1.2)
         ("rust-regex" ,rust-regex-1.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-clap" ,rust-clap-2)
         ("rust-which" ,rust-which-3.1))))
    (home-page
      "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-dav1d-sys-0.3
  (package
    (name "rust-dav1d-sys")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dav1d-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1022czzp3s54r42x6rhr870w1fwzyp7b6qn0zirpz55zmqjpgnwa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-bindgen" ,rust-bindgen-0.52)
         ("rust-metadeps" ,rust-metadeps-1.1))))
    (home-page "https://github.com/rust-av/dav1d-rs")
    (synopsis "FFI bindings to dav1d")
    (description "FFI bindings to dav1d")
    (license license:expat)))

(define-public rust-num-derive-0.3
  (package
    (name "rust-num-derive")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0imprwv8cs01k46g56ajlvc97dp8kz51y2vn6cp9jkw1c6r1b2qc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/rust-num/num-derive")
    (synopsis "Numeric syntax extensions")
    (description "Numeric syntax extensions")
    (license (list license:expat license:asl2.0))))

(define-public rust-arg-enum-proc-macro-0.3
  (package
    (name "rust-arg-enum-proc-macro")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arg_enum_proc_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "021rr6j3n031ynfbm7kwb3j3bxvbsz40n0nqi78k47d3p92rihcv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/lu-zero/arg_enum_proc_macro")
    (synopsis
      "A procedural macro compatible with clap arg_enum")
    (description
      "This package provides a procedural macro compatible with clap arg_enum")
    (license license:expat)))

(define-public rust-scan-fmt-0.2
  (package
    (name "rust-scan-fmt")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scan_fmt" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gmaa07z8bkkdv5xhq2lrgml6ri7fqyyrjpiks3phmpmq3p8d0i4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-regex" ,rust-regex-1.3))))
    (home-page "https://github.com/wlentz/scan_fmt")
    (synopsis "A simple scanf()-like input for Rust")
    (description
      "This package provides a simple scanf()-like input for Rust")
    (license license:expat)))

(define-public rust-interpolate-name-0.2
  (package
    (name "rust-interpolate-name")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "interpolate_name" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05vzsiqb69d1mbpaphcg4ifjsjs6g03b8pacskfcydqhh555zcxl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/lu-zero/interpolate_name")
    (synopsis
      "Simple procedural macro attribute for repetitive tests")
    (description
      "Simple procedural macro attribute for repetitive tests")
    (license license:expat)))

(define-public rust-syn-mid-0.5
  (package
    (name "rust-syn-mid")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn-mid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12ikg5jfklixq0wsgfl7sdzjqlxgq50ygklxy4f972hjdjgm7qvv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/taiki-e/syn-mid")
    (synopsis
      "Providing the features between \"full\" and \"derive\" of syn.
")
    (description
      "Providing the features between \"full\" and \"derive\" of syn.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-proc-macro-error-attr-0.4
  (package
    (name "rust-proc-macro-error-attr")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error-attr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pk9mwcfnpf8favgc2cl4sqlmi818p96hg8pfb51wg5nzmvlnnwa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-syn-mid" ,rust-syn-mid-0.5)
         ("rust-version-check" ,rust-version-check-0.9)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://gitlab.com/CreepySkeleton/proc-macro-error")
    (synopsis
      "Attribute macro for proc-macro-error crate")
    (description
      "Attribute macro for proc-macro-error crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro-error-0.4
  (package
    (name "rust-proc-macro-error")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rvpaadwv7vmsp142qqh2axqrr9v78f1nvdsi9nhmfhy10kk1wqq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-error-attr"
          ,rust-proc-macro-error-attr-0.4)
         ("rust-version-check" ,rust-version-check-0.9)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://gitlab.com/CreepySkeleton/proc-macro-error")
    (synopsis
      "Almost drop-in replacement to panics in proc-macros")
    (description
      "Almost drop-in replacement to panics in proc-macros")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-metadata-0.6
  (package
    (name "rust-cargo-metadata")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo_metadata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1givpi2w7iwqqnl87x5yc15zcm5hs6yw490sb6abkfp1h39v9lg5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-error-chain" ,rust-error-chain-0.12)
         ("rust-semver" ,rust-semver-0.9)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-derive" ,rust-serde-derive-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page
      "https://github.com/oli-obk/cargo_metadata")
    (synopsis
      "structured access to the output of `cargo metadata`")
    (description
      "structured access to the output of `cargo metadata`")
    (license license:expat)))

(define-public rust-bytecount-0.4
  (package
    (name "rust-bytecount")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytecount" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13qpy38z5wx0rzcdvr2h0ixbfgi1dbrif068il3hwn3k2mah88mr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-packed-simd" ,rust-packed-simd-0.3))))
    (home-page "https://github.com/llogiq/bytecount")
    (synopsis
      "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
    (description
      "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pulldown-cmark-0.2
  (package
    (name "rust-pulldown-cmark")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05gfnqa0wzix5m17jrmgj0yyr9sflqm0knn79ndppsnhcan2zxgf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-getopts" ,rust-getopts-0.2)
         ("rust-bitflags" ,rust-bitflags-1))))
    (home-page
      "https://github.com/raphlinus/pulldown-cmark")
    (synopsis "A pull parser for CommonMark")
    (description
      "This package provides a pull parser for CommonMark")
    (license license:expat)))

(define-public rust-error-chain-0.12
  (package
    (name "rust-error-chain")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-chain" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ka5y0fmymxzx3gz2yrd7rpz2i555m1iw4fpmcggpzcgr1n10wfk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-backtrace" ,rust-backtrace-0.3)
         ("rust-version-check" ,rust-version-check-0.9))))
    (home-page
      "https://github.com/rust-lang-nursery/error-chain")
    (synopsis
      "Yet another error boilerplate library.")
    (description
      "Yet another error boilerplate library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-skeptic-0.13
  (package
    (name "rust-skeptic")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "skeptic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rai61hbs65nbvbhqlk1nap5hlav5qx3zmjjjzh9rhgxagc8xyyn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-error-chain" ,rust-error-chain-0.12)
         ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.2)
         ("rust-glob" ,rust-glob-0.2)
         ("rust-tempdir" ,rust-tempdir-0.3)
         ("rust-bytecount" ,rust-bytecount-0.4)
         ("rust-cargo-metadata" ,rust-cargo-metadata-0.6)
         ("rust-serde-json" ,rust-serde-json-1.0)
         ("rust-walkdir" ,rust-walkdir-2.3))))
    (home-page
      "https://github.com/budziq/rust-skeptic")
    (synopsis
      "Test your Rust markdown documentation via Cargo")
    (description
      "Test your Rust markdown documentation via Cargo")
    (license (list license:expat license:asl2.0))))

(define-public rust-err-derive-0.2
  (package
    (name "rust-err-derive")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "err-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v6wxrshfpg7mwaxzq8jwxbfiyn7zk5rlm4m8kkrwh7dpf8nrx42"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-synstructure" ,rust-synstructure-0.12)
         ("rust-skeptic" ,rust-skeptic-0.13)
         ("rust-proc-macro-error"
          ,rust-proc-macro-error-0.4)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-rustversion" ,rust-rustversion-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://gitlab.com/torkleyy/err-derive")
    (synopsis "Derive macro for `std::error::Error`")
    (description
      "Derive macro for `std::error::Error`")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-chacha-0.2
  (package
    (name "rust-rand-chacha")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00il36fkdbsmpr99p9ksmmp6dn1md7rmnwmz0rr77jbrca2yvj7l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ppv-lite86" ,rust-ppv-lite86-0.2)
         ("rust-rand-core" ,rust-rand-core-0.5))))
    (home-page
      "https://crates.io/crates/rand_chacha")
    (synopsis "ChaCha random number generator
")
    (description "ChaCha random number generator
")
    (license (list license:expat license:asl2.0))))

(define-public rust-noop-proc-macro-0.2
  (package
    (name "rust-noop-proc-macro")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "noop_proc_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0in1l0rjxzs4fylb6zad484z1c58jxyzchhc12k0cjrvm0y6zwsz"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/lu-zero/noop_proc_macro")
    (synopsis
      "No-op proc_macro, literally does nothing")
    (description
      "No-op proc_macro, literally does nothing")
    (license license:expat)))

(define-public rust-bindgen-0.50
  (package
    (name "rust-bindgen")
    (version "0.50.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fp98x0k4cawil3rqxsfrb58pq3mb5mn37rp745zxfmjfigml3nb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-shlex" ,rust-shlex-0.1)
         ("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-peeking-take-while"
          ,rust-peeking-take-while-0.1)
         ("rust-fxhash" ,rust-fxhash-0.2)
         ("rust-clang-sys" ,rust-clang-sys-0.28)
         ("rust-cexpr" ,rust-cexpr-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-proc-macro2" ,rust-proc-macro2-0.4)
         ("rust-quote" ,rust-quote-0.6)
         ("rust-env-logger" ,rust-env-logger-0.6)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-regex" ,rust-regex-1.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-which" ,rust-which-2.0)
         ("rust-clap" ,rust-clap-2))))
    (home-page
      "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-onig-sys-69.2
  (package
    (name "rust-onig-sys")
    (version "69.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "onig_sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kjijq29yx05xxg9snvqnfn53dl52hchb4sk3zhfr77mypxlx38a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-bindgen" ,rust-bindgen-0.50)
         ("rust-cc" ,rust-cc-1.0))))
    (home-page
      "http://github.com/iwillspeak/rust-onig")
    (synopsis
      "The `onig_sys` crate contains raw rust bindings to the
oniguruma library. This crate exposes a set of unsafe
functions which can then be used by other crates to
create safe wrappers around Oniguruma.

You probably don't want to link to this crate directly;
instead check out the `onig` crate.
")
    (description
      "The `onig_sys` crate contains raw rust bindings to the
oniguruma library.  This crate exposes a set of unsafe
functions which can then be used by other crates to
create safe wrappers around Oniguruma.

You probably don't want to link to this crate directly;
instead check out the `onig` crate.
")
    (license license:expat)))

(define-public rust-onig-5.0
  (package
    (name "rust-onig")
    (version "5.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "onig" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ivr0wq1zlyjhhkxpsnmpncg92sjx3rha8pnp3m1mzvgk7y27rz4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-onig-sys" ,rust-onig-sys-69.2))))
    (home-page
      "http://github.com/iwillspeak/rust-onig")
    (synopsis
      "Rust-Onig is a set of Rust bindings for the
Oniguruma regular expression library. Oniguruma
is a modern regex library with support for
multiple character encodings and regex syntaxes.
")
    (description
      "Rust-Onig is a set of Rust bindings for the
Oniguruma regular expression library.  Oniguruma
is a modern regex library with support for
multiple character encodings and regex syntaxes.
")
    (license license:expat)))

(define-public rust-winapi-util-0.1
  (package
    (name "rust-winapi-util")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vj3984cxwnf1ys3fdz6bpl7p0kdsgykpzbhmcmwi759cd8mqlgs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/BurntSushi/winapi-util")
    (synopsis
      "A dumping ground for high level safe wrappers over winapi.")
    (description
      "This package provides a dumping ground for high level safe wrappers over winapi.")
    (license (list license:unlicense license:expat))))

(define-public rust-walkdir-2.3
  (package
    (name "rust-walkdir")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "walkdir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z9g39f49cycdm9vzjf8hnfh3f1csxgd65kmlphj8r2vffy84wbp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-winapi-util" ,rust-winapi-util-0.1)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-same-file" ,rust-same-file-1.0))))
    (home-page
      "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory.")
    (description "Recursively walk a directory.")
    (license (list license:unlicense license:expat))))

(define-public rust-bincode-1.2
  (package
    (name "rust-bincode")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bincode" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gvxm3n67xv1874fwxmnlircdlphlk1hcw75ykrrnw9l2nky4lsp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0)
         ("rust-byteorder" ,rust-byteorder-1.3))))
    (home-page "https://github.com/servo/bincode")
    (synopsis
      "A binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
    (description
      "This package provides a binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
    (license license:expat)))

(define-public rust-ryu-1.0
  (package
    (name "rust-ryu")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xlx9ybzncrb7d6r9533g8ydlg6mr252pfzl4g9cqaqkpvk24mjk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-no-panic" ,rust-no-panic-0.1))))
    (home-page "https://github.com/dtolnay/ryu")
    (synopsis
      "Fast floating point to string conversion")
    (description
      "Fast floating point to string conversion")
    (license (list license:asl2.0 license:boost1.0))))

(define-public rust-itoa-0.4
  (package
    (name "rust-itoa")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13nxqrfnh83a7x5rw4wq2ilp8nxvwy74dxzysdg59dbxqk0agdxq"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis
      "Fast functions for printing integer primitives to an io::Write")
    (description
      "Fast functions for printing integer primitives to an io::Write")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1.0
  (package
    (name "rust-serde-json")
    (version "1.0.50")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rs8rsk59kgkgsrw8hyyjrlhas9k1by2jwxxqcz3c2bq2qna39vq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-itoa" ,rust-itoa-0.4)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-ryu" ,rust-ryu-1.0)
         ("rust-indexmap" ,rust-indexmap-1.3))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description
      "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-xml-rs-0.8
  (package
    (name "rust-xml-rs")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xml-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c905wsmk995xypxljpxzq6vv660r1pzgyrpsfiz13kw3hf0dzcs"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/netvl/xml-rs")
    (synopsis "An XML library in pure Rust")
    (description "An XML library in pure Rust")
    (license license:expat)))

(define-public rust-line-wrap-0.1
  (package
    (name "rust-line-wrap")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "line-wrap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ffa2whkyh9mwvdlpk6v8pjkg8p8mlzyjfymq5adll9a18sl80zk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-safemem" ,rust-safemem-0.3))))
    (home-page
      "https://bitbucket.org/marshallpierce/line-wrap-rs/src")
    (synopsis "Efficiently insert line separators")
    (description
      "Efficiently insert line separators")
    (license license:asl2.0)))

(define-public rust-plist-0.4
  (package
    (name "rust-plist")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "plist" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zqnxc5i4y6mj119vr0lzpb5j67vffpx2phhgh711533bw3ryajz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-line-wrap" ,rust-line-wrap-0.1)
         ("rust-base64" ,rust-base64-0.10)
         ("rust-xml-rs" ,rust-xml-rs-0.8)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-humantime" ,rust-humantime-1.3)
         ("rust-byteorder" ,rust-byteorder-1.3))))
    (home-page
      "https://github.com/ebarnard/rust-plist/")
    (synopsis
      "A rusty plist parser. Supports Serde serialization.")
    (description
      "This package provides a rusty plist parser.  Supports Serde serialization.")
    (license license:expat)))

(define-public rust-syntect-3.3
  (package
    (name "rust-syntect")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syntect" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f6qn1yy15b0hq9h6q1rikqnm3lh56ic6bq3ywsmdsjy8ni9splm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-plist" ,rust-plist-0.4)
         ("rust-yaml-rust" ,rust-yaml-rust-0.4)
         ("rust-regex-syntax" ,rust-regex-syntax-0.6)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-derive" ,rust-serde-derive-1.0)
         ("rust-flate2" ,rust-flate2-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0)
         ("rust-fnv" ,rust-fnv-1.0)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-lazycell" ,rust-lazycell-1.2)
         ("rust-bincode" ,rust-bincode-1.2)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-walkdir" ,rust-walkdir-2.3)
         ("rust-onig" ,rust-onig-5.0))))
    (home-page "https://github.com/trishume/syntect")
    (synopsis
      "library for high quality syntax highlighting and code intelligence using Sublime Text's grammars")
    (description
      "library for high quality syntax highlighting and code intelligence using Sublime Text's grammars")
    (license license:expat)))

(define-public rust-serde-test-1.0
  (package
    (name "rust-serde-test")
    (version "1.0.105")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_test" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vnp0wld20z1wjr8qp2hxcy6yh2zhicg1mfb0qrzxgwq2a4n6raa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0))))
    (home-page "https://serde.rs")
    (synopsis
      "Token De/Serializer for testing De/Serialize implementations")
    (description
      "Token De/Serializer for testing De/Serialize implementations")
    (license (list license:expat license:asl2.0))))

(define-public rust-ascii-1.0
  (package
    (name "rust-ascii")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ascii" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0281gc828q4j692gb66jfdr5k16gyszgqflylh0pp30rllv63xdv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0)
         ("rust-serde-test" ,rust-serde-test-1.0))))
    (home-page
      "https://github.com/tomprogrammer/rust-ascii")
    (synopsis
      "ASCII-only equivalents to `char`, `str` and `String`.")
    (description
      "ASCII-only equivalents to `char`, `str` and `String`.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-encode-unicode-0.3
  (package
    (name "rust-encode-unicode")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encode_unicode" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07w3vzrhxh9lpjgsg2y5bwzfar2aq35mdznvcp3zjl0ssj7d4mx3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-clippy" ,rust-clippy-0.0)
         ("rust-ascii" ,rust-ascii-1.0))))
    (home-page
      "https://github.com/tormol/encode_unicode")
    (synopsis
      "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.
")
    (description
      "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-console-0.9
  (package
    (name "rust-console")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "console" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h765951c9mywff534f0191slazykmif4290g2yarcwhd2cg7q25"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-width" ,rust-unicode-width-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-termios" ,rust-termios-0.3)
         ("rust-encode-unicode" ,rust-encode-unicode-0.3)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-clicolors-control"
          ,rust-clicolors-control-1.0)
         ("rust-regex" ,rust-regex-1.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4))))
    (home-page
      "https://github.com/mitsuhiko/console")
    (synopsis
      "A terminal and console abstraction for Rust")
    (description
      "This package provides a terminal and console abstraction for Rust")
    (license license:expat)))

(define-public rust-better-panic-0.2
  (package
    (name "rust-better-panic")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "better-panic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xl48v6pd9ys7wp0ni62i6q73xpd1nhf92z09sjc9n3lrj0ac4ix"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-backtrace" ,rust-backtrace-0.3)
         ("rust-console" ,rust-console-0.9)
         ("rust-syntect" ,rust-syntect-3.3))))
    (home-page
      "https://github.com/mitsuhiko/better-panic")
    (synopsis
      "Pretty panic backtraces inspired by Python's tracebacks.")
    (description
      "Pretty panic backtraces inspired by Python's tracebacks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-arbitrary-0.2
  (package
    (name "rust-arbitrary")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arbitrary" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i3fhcdyjq4isn22xx2svmpfr5hwyzi0wavbm07fs8i2dv5pdkv4"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/nagisa/rust_arbitrary/")
    (synopsis
      "The trait for generating structured data from unstructured data")
    (description
      "The trait for generating structured data from unstructured data")
    (license (list license:expat license:asl2.0))))

(define-public rust-nasm-rs-0.1
  (package
    (name "rust-nasm-rs")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nasm-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r34hiy1pc0aksrfc02zsl0zyw33i9yi7kyx8l214l7nm0mzm97y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rayon" ,rust-rayon-1.3))))
    (home-page "https://github.com/medek/nasm-rs")
    (synopsis "Run NASM during your Cargo build.")
    (description "Run NASM during your Cargo build.")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-0.2
  (package
    (name "rust-toml")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d1cz43bxrx4fd6j2p6myckf81f72bp47akg36y3flxjkhj60svk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-serialize"
          ,rust-rustc-serialize-0.3)
         ("rust-serde" ,rust-serde-0.8))))
    (home-page
      "https://github.com/alexcrichton/toml-rs")
    (synopsis
      "A native Rust encoder and decoder of TOML-formatted files and streams. Provides
implementations of the standard Serialize/Deserialize traits for TOML data to
facilitate deserializing and serializing Rust structures.
")
    (description
      "This package provides a native Rust encoder and decoder of TOML-formatted files and streams.  Provides
implementations of the standard Serialize/Deserialize traits for TOML data to
facilitate deserializing and serializing Rust structures.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-error-chain-0.10
  (package
    (name "rust-error-chain")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-chain" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y1gyj9g5c3k1nzkvxrgry8v9k86kcc585mczrm3qz019s35shyr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-backtrace" ,rust-backtrace-0.3))))
    (home-page
      "https://github.com/rust-lang-nursery/error-chain")
    (synopsis
      "Yet another error boilerplate library.")
    (description
      "Yet another error boilerplate library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-metadeps-1.1
  (package
    (name "rust-metadeps")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "metadeps" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hjla9ypycqw1snd2qf87cckcc0d5z5qvxpcijn5yrrs3f825cbk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-error-chain" ,rust-error-chain-0.10)
         ("rust-toml" ,rust-toml-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page
      "https://github.com/joshtriplett/metadeps")
    (synopsis
      "Run pkg-config from declarative dependencies in Cargo.toml")
    (description
      "Run pkg-config from declarative dependencies in Cargo.toml")
    (license (list license:expat license:asl2.0))))

(define-public rust-goblin-0.2
  (package
    (name "rust-goblin")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "goblin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1j38fkqadbsjxawr3wnj9m0qaihcwp6pmfakmhsar881509y7mfx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-scroll" ,rust-scroll-0.10)
         ("rust-plain" ,rust-plain-0.2)
         ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/m4b/goblin")
    (synopsis
      "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (description
      "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (license license:expat)))

(define-public rust-gimli-0.20
  (package
    (name "rust-gimli")
    (version "0.20.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gimli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cz6wg1niwfqf0mk28igsdnsm92cs57cai9jpzdmvw6hma863pc1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-fallible-iterator"
          ,rust-fallible-iterator-0.2)
         ("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-stable-deref-trait"
          ,rust-stable-deref-trait-1.1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-indexmap" ,rust-indexmap-1.3)
         ("rust-byteorder" ,rust-byteorder-1.3))))
    (home-page "https://github.com/gimli-rs/gimli")
    (synopsis
      "A library for reading and writing the DWARF debugging format.")
    (description
      "This package provides a library for reading and writing the DWARF debugging format.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-afl-0.5
  (package
    (name "rust-afl")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "afl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0azpi917l8nhvx25n2v670nvkxkrhcwmddfi85qnr6kchmi6y946"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-version" ,rust-rustc-version-0.2)
         ("rust-rustc-version" ,rust-rustc-version-0.2)
         ("rust-cc" ,rust-cc-1.0)
         ("rust-xdg" ,rust-xdg-2.2)
         ("rust-xdg" ,rust-xdg-2.2)
         ("rust-clap" ,rust-clap-2))))
    (home-page "https://github.com/rust-fuzz/afl.rs")
    (synopsis
      "Fuzzing Rust code with american-fuzzy-lop")
    (description
      "Fuzzing Rust code with american-fuzzy-lop")
    (license license:asl2.0)))

(define-public rust-cpp-demangle-0.2
  (package
    (name "rust-cpp-demangle")
    (version "0.2.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpp_demangle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mm064x84868q06r4m4b7byf999nrkbhx7iyc4nchyssaxpsy5a1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-glob" ,rust-glob-0.3)
         ("rust-afl" ,rust-afl-0.5))))
    (home-page
      "https://github.com/gimli-rs/cpp_demangle")
    (synopsis "A crate for demangling C++ symbols")
    (description
      "This package provides a crate for demangling C++ symbols")
    (license (list license:asl2.0 license:expat))))

(define-public rust-fallible-iterator-0.2
  (package
    (name "rust-fallible-iterator")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fallible-iterator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x31skjsynn2h7sq3qzyv4zlyk2w8jmqcs3phsg4qxhz52yj16qx"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/sfackler/rust-fallible-iterator")
    (synopsis "Fallible iterator traits")
    (description "Fallible iterator traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-indexmap-1.3
  (package
    (name "rust-indexmap")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14i2gmq9pwaafvlxmsc12j6539hjgqk4j4jz40fz763vbcn08vq7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1.0)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-rayon" ,rust-rayon-1.3))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis
      "A hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys. It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index. A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
    (description
      "This package provides a hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys.  It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index.  A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-miniz-oxide-0.3
  (package
    (name "rust-miniz-oxide")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "198n4hfpq0qcxf275l6fpzh7b9cl7ck2xs6pjgpds74bazv9yrxa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-adler32" ,rust-adler32-1.0))))
    (home-page
      "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis
      "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (description
      "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (license license:expat)))

(define-public rust-cloudflare-zlib-sys-0.2
  (package
    (name "rust-cloudflare-zlib-sys")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cloudflare-zlib-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01lwfd15ijw4d8jsqp87yv4wpmzcp84qm0qqwy3yxmm0fjr5q6by"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1.0))))
    (home-page "https://github.com/cloudflare/zlib")
    (synopsis
      "Cloudflare fork of zlib with massive performance improvements")
    (description
      "Cloudflare fork of zlib with massive performance improvements")
    (license
      (list license:expat
            license:zlib
            license:asl2.0
            license:zlib))))

(define-public rust-tokio-io-0.1
  (package
    (name "rust-tokio-io")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-io" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x06zyzinans1pn90g6i150lgixijdf1cg8y2gipjd09ms58dz2p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-log" ,rust-log-0.4)
         ("rust-bytes" ,rust-bytes-0.4))))
    (home-page "https://tokio.rs")
    (synopsis
      "Core I/O primitives for asynchronous I/O in Rust.
")
    (description
      "Core I/O primitives for asynchronous I/O in Rust.
")
    (license license:expat)))

(define-public rust-flate2-1.0
  (package
    (name "rust-flate2")
    (version "1.0.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flate2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hlb2zmn5ixrgr0i1qvrd3a7j4fpp002d0kddn2hm7hjj49z9zrc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-miniz-sys" ,rust-miniz-sys-0.1)
         ("rust-tokio-io" ,rust-tokio-io-0.1)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-cloudflare-zlib-sys"
          ,rust-cloudflare-zlib-sys-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-miniz-oxide" ,rust-miniz-oxide-0.3)
         ("rust-libz-sys" ,rust-libz-sys-1.0)
         ("rust-crc32fast" ,rust-crc32fast-1.2))))
    (home-page
      "https://github.com/alexcrichton/flate2-rs")
    (synopsis
      "Bindings to miniz.c for DEFLATE compression and decompression exposed as
Reader/Writer streams. Contains bindings for zlib, deflate, and gzip-based
streams.
")
    (description
      "Bindings to miniz.c for DEFLATE compression and decompression exposed as
Reader/Writer streams.  Contains bindings for zlib, deflate, and gzip-based
streams.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-erased-serde-0.3
  (package
    (name "rust-erased-serde")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "erased-serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lgkpkk7nx6f24gmr3psyj8d2avc9701r9jyw1i4ssp10lbnv2yq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1.0))))
    (home-page
      "https://github.com/dtolnay/erased-serde")
    (synopsis
      "Type-erased Serialize and Serializer traits")
    (description
      "Type-erased Serialize and Serializer traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-slog-2.5
  (package
    (name "rust-slog")
    (version "2.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slog" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16bv6zrdn1sm315vbnia02g31xvsmbjyz5gv3z0vrgxdli0cdj8w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-erased-serde" ,rust-erased-serde-0.3))))
    (home-page "https://github.com/slog-rs/slog")
    (synopsis
      "Structured, extensible, composable logging for Rust")
    (description
      "Structured, extensible, composable logging for Rust")
    (license
      (list license:mpl2.0
            license:expat
            license:asl2.0))))

(define-public rust-uuid-0.8
  (package
    (name "rust-uuid")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uuid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "049w16qwk3d3b9cmpgvd7fvcnwgs75l8rlsagh06w7ga9dm2zplz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-winapi" ,rust-winapi-0.3)
         ("rust-sha1" ,rust-sha1-0.6)
         ("rust-md5" ,rust-md5-0.6)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-slog" ,rust-slog-2.5))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis
      "A library to generate and parse UUIDs.")
    (description
      "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-parity-wasm-0.41
  (package
    (name "rust-parity-wasm")
    (version "0.41.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parity-wasm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rfqgjyxrxrfjq5r5n81mdklahl8g4az6yhyyvw25nh0mj6qgz6x"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/paritytech/parity-wasm")
    (synopsis "WebAssembly low-level format library")
    (description
      "WebAssembly low-level format library")
    (license (list license:expat license:asl2.0))))

(define-public rust-target-lexicon-0.10
  (package
    (name "rust-target-lexicon")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "target-lexicon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17diw9c3d1vb5rmwwk2ghsyhfs0gj5jm78hrwxxhmd67vhw743mb"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/CraneStation/target-lexicon")
    (synopsis
      "Targeting utilities for compilers and related tools")
    (description
      "Targeting utilities for compilers and related tools")
    (license (list license:asl2.0
                   ;XXX unknown-license!
                   ))))

(define-public rust-scroll-derive-0.10
  (package
    (name "rust-scroll-derive")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scroll_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a7f0xybi27p1njs4bqmxh9zyb2dqal4dbvgnhjjix4zkgm4wn7q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/m4b/scroll")
    (synopsis
      "A macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
    (description
      "This package provides a macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
    (license license:expat)))

(define-public rust-scroll-0.10
  (package
    (name "rust-scroll")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scroll" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cbcns8538sqmfnmdbphqy0fd4j8z75z802pvmz3zlwmnln37cmb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-scroll-derive" ,rust-scroll-derive-0.10))))
    (home-page "https://github.com/m4b/scroll")
    (synopsis
      "A suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
    (description
      "This package provides a suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
    (license license:expat)))

(define-public rust-goblin-0.1
  (package
    (name "rust-goblin")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "goblin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nn0aa2jf207gbyccxnrzm7n217di025z5y1ybblp7nkk11j309h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-scroll" ,rust-scroll-0.10)
         ("rust-plain" ,rust-plain-0.2)
         ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/m4b/goblin")
    (synopsis
      "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (description
      "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (license license:expat)))

(define-public rust-object-0.17
  (package
    (name "rust-object")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "object" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bmgbg4k0725lchfy9j1wnpfmywh5qhs0k4k6j2g7c0acvys8i7a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-goblin" ,rust-goblin-0.1)
         ("rust-target-lexicon" ,rust-target-lexicon-0.10)
         ("rust-scroll" ,rust-scroll-0.10)
         ("rust-parity-wasm" ,rust-parity-wasm-0.41)
         ("rust-uuid" ,rust-uuid-0.8)
         ("rust-flate2" ,rust-flate2-1.0)
         ("rust-crc32fast" ,rust-crc32fast-1.2)
         ("rust-indexmap" ,rust-indexmap-1.3))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
      "A unified interface for reading and writing object file formats.")
    (description
      "This package provides a unified interface for reading and writing object file formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-addr2line-0.11
  (package
    (name "rust-addr2line")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "addr2line" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sk5g8cb2yynlcm0wcqff9l9c9ml69rqgfrrbii0ybgdc236jkhw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
         ("rust-object" ,rust-object-0.17)
         ("rust-fallible-iterator"
          ,rust-fallible-iterator-0.2)
         ("rust-cpp-demangle" ,rust-cpp-demangle-0.2)
         ("rust-gimli" ,rust-gimli-0.20)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-lazycell" ,rust-lazycell-1.2))))
    (home-page
      "https://github.com/gimli-rs/addr2line")
    (synopsis
      "A cross-platform symbolication library written in Rust, using `gimli`")
    (description
      "This package provides a cross-platform symbolication library written in Rust, using `gimli`")
    (license (list license:asl2.0 license:expat))))

(define-public rust-backtrace-sys-0.1
  (package
    (name "rust-backtrace-sys")
    (version "0.1.35")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "066iviphi72mx9hd3njzsplk5v45jhi10mrccbbyij391ahsps3x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-compiler-builtins"
          ,rust-compiler-builtins-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0)
         ("rust-cc" ,rust-cc-1.0))))
    (home-page
      "https://github.com/alexcrichton/backtrace-rs")
    (synopsis
      "Bindings to the libbacktrace gcc library
")
    (description
      "Bindings to the libbacktrace gcc library
")
    (license (list license:expat license:asl2.0))))

(define-public rust-compiler-builtins-0.1
  (package
    (name "rust-compiler-builtins")
    (version "0.1.26")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compiler_builtins" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rhj6ccmfkh9gcxnxgjq4fg257yi4f9325nfzsphbmxwkrg06sq3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0)
         ("rust-cc" ,rust-cc-1.0))))
    (home-page
      "https://github.com/rust-lang/compiler-builtins")
    (synopsis
      "Compiler intrinsics used by the Rust compiler. Also available for other targets
if necessary!
")
    (description
      "Compiler intrinsics used by the Rust compiler.  Also available for other targets
if necessary!
")
    (license (list license:expat license:asl2.0))))

(define-public rust-backtrace-0.3
  (package
    (name "rust-backtrace")
    (version "0.3.46")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17hh1vrhfd01qpjilrdpy7q0lf2j2qv36achpg37q92rff4r5rmi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
         ("rust-compiler-builtins"
          ,rust-compiler-builtins-0.1)
         ("rust-backtrace-sys" ,rust-backtrace-sys-0.1)
         ("rust-addr2line" ,rust-addr2line-0.11)
         ("rust-goblin" ,rust-goblin-0.2)
         ("rust-cpp-demangle" ,rust-cpp-demangle-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-rustc-serialize"
          ,rust-rustc-serialize-0.3)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-findshlibs" ,rust-findshlibs-0.5)
         ("rust-memmap" ,rust-memmap-0.7)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page
      "https://github.com/rust-lang/backtrace-rs")
    (synopsis
      "A library to acquire a stack trace (backtrace) at runtime in a Rust program.
")
    (description
      "This package provides a library to acquire a stack trace (backtrace) at runtime in a Rust program.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-synstructure-0.12
  (package
    (name "rust-synstructure")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synstructure" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0igmc5fzpk6fg7kgff914j05lbpc6ai2wmji312v2h8vvjhnwrb7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.2)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/mystor/synstructure")
    (synopsis
      "Helper methods and macros for custom derives")
    (description
      "Helper methods and macros for custom derives")
    (license license:expat)))

(define-public rust-failure-derive-0.1
  (package
    (name "rust-failure-derive")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "failure_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cfjz0c9szqpxn43b2r722p6m3swzxj7aj6xhqw23ml7h8y762h3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-synstructure" ,rust-synstructure-0.12)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://rust-lang-nursery.github.io/failure/")
    (synopsis "derives for the failure crate")
    (description "derives for the failure crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-failure-0.1
  (package
    (name "rust-failure")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "failure" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0js6i6mb42q1g6q3csfbmi6q40s64k96705xbim0d8zg44j9qlmq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-failure-derive" ,rust-failure-derive-0.1)
         ("rust-backtrace" ,rust-backtrace-0.3))))
    (home-page
      "https://rust-lang-nursery.github.io/failure/")
    (synopsis
      "Experimental error handling abstraction.")
    (description
      "Experimental error handling abstraction.")
    (license (list license:expat license:asl2.0))))

(define-public rust-which-3.1
  (package
    (name "rust-which")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "094pw9pi48szshn9ln69z2kg7syq1jp80h5ps1qncbsaw4d0f4fh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-failure" ,rust-failure-0.1)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page
      "https://github.com/harryfei/which-rs.git")
    (synopsis
      "A Rust equivalent of Unix command \"which\". Locate installed executable in cross platforms.")
    (description
      "This package provides a Rust equivalent of Unix command \"which\".  Locate installed executable in cross platforms.")
    (license license:expat)))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.68")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w6z9krcqn7p200sb80dxx76iyvw3jdz949zxr1sgfr3a50c186y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis
      "Raw FFI bindings to platform libraries like libc.
")
    (description
      "Raw FFI bindings to platform libraries like libc.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-memchr-2.3
  (package
    (name "rust-memchr")
    (version "2.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0074pvsfl938ndl5js14ibc7i9q0k3zp390z843w8nlyv4bxha1p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2))))
    (home-page
      "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense license:expat))))

(define-public rust-aho-corasick-0.7
  (package
    (name "rust-aho-corasick")
    (version "0.7.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nka9509afjgal6lpymn8w2lq11dmjwxs8yjcmzys966if5l05l7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-memchr" ,rust-memchr-2.3))))
    (home-page
      "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      "Fast multiple substring searching.")
    (license (list license:unlicense license:expat))))

(define-public rust-regex-syntax-0.6
  (package
    (name "rust-regex-syntax")
    (version "0.6.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1blmlgzcg7in3kcxqabpfzzrbnamr2i671flbrmlqhfps5bvvrbz"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      "This package provides a regular expression parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-1.3
  (package
    (name "rust-regex")
    (version "1.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cx4lcgy7vdi2kij2n1dp2whl33d7974g1kxwiklhs192nclcsbz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-regex-syntax" ,rust-regex-syntax-0.6)
         ("rust-aho-corasick" ,rust-aho-corasick-0.7)
         ("rust-thread-local" ,rust-thread-local-1.0)
         ("rust-memchr" ,rust-memchr-2.3))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
      "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (description
      "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-hash-1.1
  (package
    (name "rust-rustc-hash")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-hash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/rust-lang-nursery/rustc-hash")
    (synopsis
      "speed, non-cryptographic hash used in rustc")
    (description
      "speed, non-cryptographic hash used in rustc")
    (license (list license:asl2.0 license:expat))))

(define-public rust-serde-derive-1.0
  (package
    (name "rust-serde-derive")
    (version "1.0.105")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y5gzwpy8yjv9pwh1js11vr18nfz4gg1g2kmyr6p58hvavy00pdc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://serde.rs")
    (synopsis
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1.0
  (package
    (name "rust-serde")
    (version "1.0.105")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zrj157dxvmymp5ii60anap2qqks4pkr3fwsp71wi3sv4nzzn1z7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://serde.rs")
    (synopsis
      "A generic serialization/deserialization framework")
    (description
      "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-sval-derive-0.4
  (package
    (name "rust-sval-derive")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sval_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07s7jqsdczsg0wnydfnxyrsj8zyrjmiwl4is1dfgn8dfvyi8n2bj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/sval-rs/sval")
    (synopsis "Custom derive for sval")
    (description "Custom derive for sval")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sval-0.4
  (package
    (name "rust-sval")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sval" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1aljggx64481q4wp3wx9hxsfh2bs7d64nqsrwbb2zxcpmdnbn6yk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-sval-derive" ,rust-sval-derive-0.4)
         ("rust-smallvec" ,rust-smallvec-0.6)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/sval-rs/sval")
    (synopsis
      "A no-std, object-safe serialization framework")
    (description
      "This package provides a no-std, object-safe serialization framework")
    (license (list license:asl2.0 license:expat))))

(define-public rust-log-0.4
  (package
    (name "rust-log")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "170cx0xanhlhd3a249ssqydyaismcxarqi4sdw7w9ja5m5kd96hv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-sval" ,rust-sval-0.4)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/rust-lang/log")
    (synopsis
      "A lightweight logging facade for Rust
")
    (description
      "This package provides a lightweight logging facade for Rust
")
    (license (list license:expat license:asl2.0))))

(define-public rust-cexpr-0.3
  (package
    (name "rust-cexpr")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cexpr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07fdfj4ff2974y33yixrb657riq9zl9b9h9lr0h7ridhhvxvbrgw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-nom" ,rust-nom-4.2))))
    (home-page
      "https://github.com/jethrogb/rust-cexpr")
    (synopsis "A C expression parser and evaluator")
    (description
      "This package provides a C expression parser and evaluator")
    (license (list license:asl2.0 license:expat))))

(define-public rust-bindgen-0.51
  (package
    (name "rust-bindgen")
    (version "0.51.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x9gndlnwmxsihxvsc3izyyss7g8b2djn0daafshj1gcy69i7mzb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-shlex" ,rust-shlex-0.1)
         ("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-peeking-take-while"
          ,rust-peeking-take-while-0.1)
         ("rust-clang-sys" ,rust-clang-sys-0.28)
         ("rust-cexpr" ,rust-cexpr-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-env-logger" ,rust-env-logger-0.6)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-quote" ,rust-quote-1.0)
         ("rust-rustc-hash" ,rust-rustc-hash-1.1)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-regex" ,rust-regex-1.3)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-clap" ,rust-clap-2)
         ("rust-which" ,rust-which-3.1))))
    (home-page
      "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-aom-sys-0.1
  (package
    (name "rust-aom-sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aom-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ix3djcf84kk53h6fac73n7jc614745n7kbmikxwi3s73b6vzgsr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.51)
         ("rust-metadeps" ,rust-metadeps-1.1))))
    (home-page "https://github.com/rust-av/aom-rs")
    (synopsis "FFI bindings to aom")
    (description "FFI bindings to aom")
    (license license:expat)))

(define-public rust-syn-1.0
  (package
    (name "rust-syn")
    (version "1.0.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00xvf772ys4fj9fr8kplmsqb9if215dsipi3nv54aw9q7xkfpw0d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.2)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro-hack-0.5
  (package
    (name "rust-proc-macro-hack")
    (version "0.5.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-hack" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qqbfm1byabjkph56r2rlvv4cliz4960j6hav3ljazyjqvkryr8d"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/dtolnay/proc-macro-hack")
    (synopsis
      "Procedural macros in expression position")
    (description
      "Procedural macros in expression position")
    (license (list license:expat license:asl2.0))))

(define-public rust-paste-impl-0.1
  (package
    (name "rust-paste-impl")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12jsm83dnsqnrcabfacnwcxh3h4kykl622vi7glv2wg527hqc956"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-hack"
          ,rust-proc-macro-hack-0.5)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis
      "Implementation detail of the `paste` crate")
    (description
      "Implementation detail of the `paste` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-paste-0.1
  (package
    (name "rust-paste")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yk4zbi7128dcrklsbwfa63d39x0dv8f7pdbrylvdlcj0s9v2kxb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-paste-impl" ,rust-paste-impl-0.1)
         ("rust-proc-macro-hack"
          ,rust-proc-macro-hack-0.5))))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis
      "Macros for all your token pasting needs")
    (description
      "Macros for all your token pasting needs")
    (license (list license:expat license:asl2.0))))

(define-public rust-bitstream-io-0.8
  (package
    (name "rust-bitstream-io")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitstream-io" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00a6wy54s1dmadm5xz8k2cbsd7ixvm48mlc45bk0fdy0pbra6jk1"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/tuffy/bitstream-io")
    (synopsis
      "Library for reading/writing un-aligned values from/to streams in big-endian and little-endian formats.")
    (description
      "Library for reading/writing un-aligned values from/to streams in big-endian and little-endian formats.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ivf-0.1
  (package
    (name "rust-ivf")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ivf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wfjf3rilqavrhvwagzinvng9dg28wcjk3c6c6p5qmc1xy65qfh1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitstream-io" ,rust-bitstream-io-0.8))))
    (home-page "https://github.com/xiph/rav1e")
    (synopsis "Simple ivf muxer")
    (description "Simple ivf muxer")
    (license #f
             ;XXX unknown-license!
             )))

(define-public rust-proc-macro2-1.0
  (package
    (name "rust-proc-macro2")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qxbnl8i3a5b2nxb8kdxbq6kj3pd1ckhm35wm7z3jd7n5wlns96z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page
      "https://github.com/alexcrichton/proc-macro2")
    (synopsis
      "A substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (description
      "This package provides a substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote-1.0
  (package
    (name "rust-quote")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zwd6fp74xfg4jnnnwj4v84lkzif2giwj4ch1hka9g35ghc6rp1b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-simd-helpers-0.1
  (package
    (name "rust-simd-helpers")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "simd_helpers" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/lu-zero/simd_helpers")
    (synopsis
      "Helpers to write more compact simd code")
    (description
      "Helpers to write more compact simd code")
    (license license:expat)))

(define-public rust-rav1e-0.3
  (package
    (name "rust-rav1e")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rav1e" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bsmj8kqzs5pf8dl98rsl6a67cljj1gkj3b5hmd8hn8wdy4ya173"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-simd-helpers" ,rust-simd-helpers-0.1)
         ("rust-ivf" ,rust-ivf-0.1)
         ("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-paste" ,rust-paste-0.1)
         ("rust-signal-hook" ,rust-signal-hook-0.1)
         ("rust-aom-sys" ,rust-aom-sys-0.1)
         ("rust-nasm-rs" ,rust-nasm-rs-0.1)
         ("rust-arbitrary" ,rust-arbitrary-0.2)
         ("rust-better-panic" ,rust-better-panic-0.2)
         ("rust-noop-proc-macro"
          ,rust-noop-proc-macro-0.2)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-rand-chacha" ,rust-rand-chacha-0.2)
         ("rust-err-derive" ,rust-err-derive-0.2)
         ("rust-interpolate-name"
          ,rust-interpolate-name-0.2)
         ("rust-rustc-version" ,rust-rustc-version-0.2)
         ("rust-scan-fmt" ,rust-scan-fmt-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-image" ,rust-image-0.22)
         ("rust-arg-enum-proc-macro"
          ,rust-arg-enum-proc-macro-0.3)
         ("rust-num-derive" ,rust-num-derive-0.3)
         ("rust-dav1d-sys" ,rust-dav1d-sys-0.3)
         ("rust-backtrace" ,rust-backtrace-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-y4m" ,rust-y4m-0.5)
         ("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-toml" ,rust-toml-0.5)
         ("rust-fern" ,rust-fern-0.5)
         ("rust-rust-hawktracer"
          ,rust-rust-hawktracer-0.7)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-itertools" ,rust-itertools-0.8)
         ("rust-bitstream-io" ,rust-bitstream-io-0.8)
         ("rust-console" ,rust-console-0.9)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-cc" ,rust-cc-1.0)
         ("rust-rayon" ,rust-rayon-1.3)
         ("rust-byteorder" ,rust-byteorder-1.3)
         ("rust-clap" ,rust-clap-2)
         ("rust-vergen" ,rust-vergen-3.1))
        #:cargo-development-inputs
        (("rust-rand-chacha" ,rust-rand-chacha-0.2)
         ("rust-interpolate-name"
          ,rust-interpolate-name-0.2)
         ("rust-criterion" ,rust-criterion-0.3)
         ("rust-pretty-assertions"
          ,rust-pretty-assertions-0.6)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-semver" ,rust-semver-0.9))
        #:phases
        (modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (invoke "cargo" "cinstall" "--release"
                        (string-append "--prefix=" out))))))))
    (native-inputs
     `(("cargo-c" ,rust-cargo-c-0.5)))
    (inputs
     `(("nasm" ,nasm)))
    (home-page "https://github.com/xiph/rav1e/")
    (synopsis "The fastest and safest AV1 encoder")
    (description
      "The fastest and safest AV1 encoder")
    (license license:bsd-2 ; AUDIT
             ;XXX unknown-license!
             )))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.68")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w6z9krcqn7p200sb80dxx76iyvw3jdz949zxr1sgfr3a50c186y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1.0))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis
      "Raw FFI bindings to platform libraries like libc.
")
    (description
      "Raw FFI bindings to platform libraries like libc.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-memchr-2.3
  (package
    (name "rust-memchr")
    (version "2.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0074pvsfl938ndl5js14ibc7i9q0k3zp390z843w8nlyv4bxha1p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2))))
    (home-page
      "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense license:expat))))

(define-public rust-aho-corasick-0.7
  (package
    (name "rust-aho-corasick")
    (version "0.7.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nka9509afjgal6lpymn8w2lq11dmjwxs8yjcmzys966if5l05l7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-memchr" ,rust-memchr-2.3))))
    (home-page
      "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      "Fast multiple substring searching.")
    (license (list license:unlicense license:expat))))

(define-public rust-regex-syntax-0.6
  (package
    (name "rust-regex-syntax")
    (version "0.6.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1blmlgzcg7in3kcxqabpfzzrbnamr2i671flbrmlqhfps5bvvrbz"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      "This package provides a regular expression parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-1.3
  (package
    (name "rust-regex")
    (version "1.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cx4lcgy7vdi2kij2n1dp2whl33d7974g1kxwiklhs192nclcsbz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-regex-syntax" ,rust-regex-syntax-0.6)
         ("rust-aho-corasick" ,rust-aho-corasick-0.7)
         ("rust-thread-local" ,rust-thread-local-1.0)
         ("rust-memchr" ,rust-memchr-2.3))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
      "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (description
      "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-metadata-0.9
  (package
    (name "rust-cargo-metadata")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo_metadata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00pjms89lghvizh4d55lz80hvrih9r55xv9m5wd9vcsgc163gqs6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-semver" ,rust-semver-0.9)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-derive" ,rust-serde-derive-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page
      "https://github.com/oli-obk/cargo_metadata")
    (synopsis
      "structured access to the output of `cargo metadata`")
    (description
      "structured access to the output of `cargo metadata`")
    (license license:expat)))

(define-public rust-syn-mid-0.5
  (package
    (name "rust-syn-mid")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn-mid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12ikg5jfklixq0wsgfl7sdzjqlxgq50ygklxy4f972hjdjgm7qvv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/taiki-e/syn-mid")
    (synopsis
      "Providing the features between \"full\" and \"derive\" of syn.
")
    (description
      "Providing the features between \"full\" and \"derive\" of syn.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-proc-macro-error-attr-0.4
  (package
    (name "rust-proc-macro-error-attr")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error-attr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pk9mwcfnpf8favgc2cl4sqlmi818p96hg8pfb51wg5nzmvlnnwa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-syn-mid" ,rust-syn-mid-0.5)
         ("rust-version-check" ,rust-version-check-0.9)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://gitlab.com/CreepySkeleton/proc-macro-error")
    (synopsis
      "Attribute macro for proc-macro-error crate")
    (description
      "Attribute macro for proc-macro-error crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro-error-0.4
  (package
    (name "rust-proc-macro-error")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rvpaadwv7vmsp142qqh2axqrr9v78f1nvdsi9nhmfhy10kk1wqq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-error-attr"
          ,rust-proc-macro-error-attr-0.4)
         ("rust-version-check" ,rust-version-check-0.9)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://gitlab.com/CreepySkeleton/proc-macro-error")
    (synopsis
      "Almost drop-in replacement to panics in proc-macros")
    (description
      "Almost drop-in replacement to panics in proc-macros")
    (license (list license:expat license:asl2.0))))

(define-public rust-structopt-derive-0.4
  (package
    (name "rust-structopt-derive")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "structopt-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c04bbzc5bmr2ns6qy35yz55nn3xvlq4dpwxdynnljb9ikhvi21z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-heck" ,rust-heck-0.3)
         ("rust-proc-macro-error"
          ,rust-proc-macro-error-0.4)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct, derive crate.")
    (description
      "Parse command line argument by defining a struct, derive crate.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-structopt-0.3
  (package
    (name "rust-structopt")
    (version "0.3.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "structopt" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "178m7wxnjyy9a8a961z74nazjsg79rfv3gv9g3bykfrrjmqs5yn8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-structopt-derive"
          ,rust-structopt-derive-0.4)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-clap" ,rust-clap-2))))
    (home-page
      "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct.")
    (description
      "Parse command line argument by defining a struct.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-js-sys-0.3
  (package
    (name "rust-js-sys")
    (version "0.3.37")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "js-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mbhpbw3zjjl51m24qx3ilq4y8xipm5sfa5hsavaabqs6wsx89va"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (description
      "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-bumpalo-3.2
  (package
    (name "rust-bumpalo")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bumpalo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11silgpsnfv6ir7j2nh7a69564f92vq20k9ha7zcbynpiav9vbhj"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/fitzgen/bumpalo")
    (synopsis
      "A fast bump allocation arena for Rust.")
    (description
      "This package provides a fast bump allocation arena for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ffn4152w8n629f29lwjgj3adiyixvdbff3mld49gisssbknzxys"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (description
      "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k9p8a7ng6nqan0m9555wj936lm2s1qz0fnafclwlv61yrxx6ryr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen-shared"
          ,rust-wasm-bindgen-shared-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0)
         ("rust-lazy-static" ,rust-lazy-static-1.4)
         ("rust-bumpalo" ,rust-bumpalo-3.2))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Backend code generation of the wasm-bindgen tool
")
    (description
      "Backend code generation of the wasm-bindgen tool
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-support-0.2
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro-support" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ca9bb9hnyzcmjww83x8asb76drf55ijhqv8yrl7igpixqv5p2nn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen-backend"
          ,rust-wasm-bindgen-backend-0.2)
         ("rust-wasm-bindgen-shared"
          ,rust-wasm-bindgen-shared-0.2)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (description
      "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-0.2
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1973xr0vr2aj85fkp3smk61z5ki7c4fhxlicfjxq3a0y7sv53lcb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen-macro-support"
          ,rust-wasm-bindgen-macro-support-0.2)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (description
      "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-0.2
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zx42zryw03w3maz8p65gr5bhhybr2sdzgcck5p3gy47abh7ri9c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-wasm-bindgen-macro"
          ,rust-wasm-bindgen-macro-0.2)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page "https://rustwasm.github.io/")
    (synopsis
      "Easy support for interacting between JS and Rust.
")
    (description
      "Easy support for interacting between JS and Rust.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cmmxamkzzs36zncqjjr7qm7xkb6zyrkjslnlj3axdgqki84y2c0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-time" ,rust-time-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
         ("rust-rustc-serialize"
          ,rust-rustc-serialize-0.3)
         ("rust-js-sys" ,rust-js-sys-0.3)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page
      "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-pretty-env-logger-0.3
  (package
    (name "rust-pretty-env-logger")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty_env_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x4hyjlnvvhyk9m74iypzybm22w3dl2k8img4b956239n5vf8zki"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-env-logger" ,rust-env-logger-0.6))))
    (home-page
      "https://github.com/seanmonstar/pretty-env-logger")
    (synopsis "a visually pretty env_logger")
    (description "a visually pretty env_logger")
    (license (list license:expat license:asl2.0))))

(define-public rust-indexmap-1.3
  (package
    (name "rust-indexmap")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14i2gmq9pwaafvlxmsc12j6539hjgqk4j4jz40fz763vbcn08vq7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1.0)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-rayon" ,rust-rayon-1.3))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis
      "A hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys. It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index. A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
    (description
      "This package provides a hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys.  It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index.  A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ryu-1.0
  (package
    (name "rust-ryu")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xlx9ybzncrb7d6r9533g8ydlg6mr252pfzl4g9cqaqkpvk24mjk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-no-panic" ,rust-no-panic-0.1))))
    (home-page "https://github.com/dtolnay/ryu")
    (synopsis
      "Fast floating point to string conversion")
    (description
      "Fast floating point to string conversion")
    (license (list license:asl2.0 license:boost1.0))))

(define-public rust-itoa-0.4
  (package
    (name "rust-itoa")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13nxqrfnh83a7x5rw4wq2ilp8nxvwy74dxzysdg59dbxqk0agdxq"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis
      "Fast functions for printing integer primitives to an io::Write")
    (description
      "Fast functions for printing integer primitives to an io::Write")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1.0
  (package
    (name "rust-serde-json")
    (version "1.0.50")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rs8rsk59kgkgsrw8hyyjrlhas9k1by2jwxxqcz3c2bq2qna39vq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-itoa" ,rust-itoa-0.4)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-ryu" ,rust-ryu-1.0)
         ("rust-indexmap" ,rust-indexmap-1.3))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description
      "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1.0
  (package
    (name "rust-serde-derive")
    (version "1.0.105")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y5gzwpy8yjv9pwh1js11vr18nfz4gg1g2kmyr6p58hvavy00pdc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://serde.rs")
    (synopsis
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1.0
  (package
    (name "rust-serde")
    (version "1.0.105")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zrj157dxvmymp5ii60anap2qqks4pkr3fwsp71wi3sv4nzzn1z7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://serde.rs")
    (synopsis
      "A generic serialization/deserialization framework")
    (description
      "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote-1.0
  (package
    (name "rust-quote")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zwd6fp74xfg4jnnnwj4v84lkzif2giwj4ch1hka9g35ghc6rp1b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-1.0
  (package
    (name "rust-syn")
    (version "1.0.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00xvf772ys4fj9fr8kplmsqb9if215dsipi3nv54aw9q7xkfpw0d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.2)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1.0
  (package
    (name "rust-proc-macro2")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qxbnl8i3a5b2nxb8kdxbq6kj3pd1ckhm35wm7z3jd7n5wlns96z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page
      "https://github.com/alexcrichton/proc-macro2")
    (synopsis
      "A substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (description
      "This package provides a substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-sval-derive-0.4
  (package
    (name "rust-sval-derive")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sval_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07s7jqsdczsg0wnydfnxyrsj8zyrjmiwl4is1dfgn8dfvyi8n2bj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0))))
    (home-page "https://github.com/sval-rs/sval")
    (synopsis "Custom derive for sval")
    (description "Custom derive for sval")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sval-0.4
  (package
    (name "rust-sval")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sval" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1aljggx64481q4wp3wx9hxsfh2bs7d64nqsrwbb2zxcpmdnbn6yk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-sval-derive" ,rust-sval-derive-0.4)
         ("rust-smallvec" ,rust-smallvec-0.6)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/sval-rs/sval")
    (synopsis
      "A no-std, object-safe serialization framework")
    (description
      "This package provides a no-std, object-safe serialization framework")
    (license (list license:asl2.0 license:expat))))

(define-public rust-log-0.4
  (package
    (name "rust-log")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "170cx0xanhlhd3a249ssqydyaismcxarqi4sdw7w9ja5m5kd96hv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-sval" ,rust-sval-0.4)
         ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/rust-lang/log")
    (synopsis
      "A lightweight logging facade for Rust
")
    (description
      "This package provides a lightweight logging facade for Rust
")
    (license (list license:expat license:asl2.0))))

(define-public rust-cbindgen-0.12
  (package
    (name "rust-cbindgen")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cbindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13jzbmjz1bmmfr0i80hw6ar484mgabx3hbpb2ynhk0ddqi0yr58m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-toml" ,rust-toml-0.5)
         ("rust-proc-macro2" ,rust-proc-macro2-1.0)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-syn" ,rust-syn-1.0)
         ("rust-quote" ,rust-quote-1.0)
         ("rust-serde-json" ,rust-serde-json-1.0)
         ("rust-clap" ,rust-clap-2)
         ("rust-tempfile" ,rust-tempfile-3.1))))
    (home-page "https://github.com/eqrion/cbindgen/")
    (synopsis
      "A tool for generating C bindings to Rust code.")
    (description
      "This package provides a tool for generating C bindings to Rust code.")
    (license license:mpl2.0)))

(define-public rust-cargo-c-0.5
  (package
    (name "rust-cargo-c")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo-c" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1is72jm0r73pqx2g3h1n6lvrcirwd91mmajsmb3jjg4jnayfkp0w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cbindgen" ,rust-cbindgen-0.12)
         ("rust-pretty-env-logger"
          ,rust-pretty-env-logger-0.3)
         ("rust-structopt" ,rust-structopt-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-toml" ,rust-toml-0.5)
         ("rust-cargo-metadata" ,rust-cargo-metadata-0.9)
         ("rust-serde" ,rust-serde-1.0)
         ("rust-serde-derive" ,rust-serde-derive-1.0)
         ("rust-regex" ,rust-regex-1.3))))
    (home-page "http://github.com/lu-zero/cargo-c")
    (synopsis
      "Helper program to build and install c-like libraries")
    (description
      "Helper program to build and install c-like libraries")
    (license license:expat)))


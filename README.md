![emacs-heart-perl](https://github.com/rawleyfowler/e/assets/75388349/e97e8497-1e93-4982-83ba-0cf9845e311e)

Emacs, the best Emacs Lisp interpreter also turns out to be a great Perl editor.
This collection of configurations is simplest, most ergonomic Perl editing experience I've ever had.

## Pre-requisites

You will need the following Perl tools installed via CPAN:

* Perl::Tidy
* Perl::Critic

You'll also want a Perl installed later than 5.6 (you should have that already).

Optionally, you can also install [cozette](https://github.com/slavfox/Cozette) HiDPI to your system if you want to use it as your font.

## Installing

```bash
chmod +x install
./install
```

Then, open Emacs. If you run into any errors with installing packages, run `M-x package-refresh-contents` and restart Emacs.

## Packages

In an attempt to make this configuration as distributable, re-usable, and extendable as possible,
I've cut down the package count to just the following:

* Flycheck
* Ctrlf
* Smex
* Magit
* A few common modes
* My favorite theme

## Configurations

* Flycheck uses a fairly strict Perlcritic severity level (3)
* Cperl mode is used by default
* Cperl mode has had its hideous SCALAR, ARRAY, and HASH coloring removed
* Perltidy runs on save
* Smex and Ido enabled by default
* Ctrlf enabled by default

## Why GNU Emacs?

I don't like Doom Emacs, and modal (Vim) editing doesn't really appeal to me anymore. I really like Nano,
but I can't extend Nano with useful things as easily, so instead, I use GNU Emacs.

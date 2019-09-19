ShortCut: short, reproducible phylogenomic cuts
===============================================


ShortCut is a scripting language meant to simplify a very common task in
bioinformatics: making a list of candidate genes likely to be related to a
biological process of interest. These are sometimes called phylogenomic cuts.

ShortCut downloads, installs, and runs the same command line tools you would
use manually but hides most of the details. That lets you quickly perform
searches too complex for a website without getting so far into the weeds that
you lose days programming. It should also be suitable for users with minimal to
no prior coding experience, and will be reproducable by other researchers
later.

See [the demo site][1] for a more detailed overview, tutorial, interactive
examples, and a list of available functions.

Status
------

<!-- [![Build Status](https://travis-matrix-badges.herokuapp.com/repos/jefdaj/shortcut/branches/master)](https://travis-ci.org/jefdaj/shortcut) -->
<!-- ![Build status 2](https://travis-matrix-badges.herokuapp.com/repos/jefdaj/shortcut/branches/master/1) -->

[![Status](https://travis-ci.org/jefdaj/shortcut.svg?branch=master)](https://travis-ci.org/jefdaj/shortcut)  

[![Status](http://img.shields.io/travis/jefdaj/shortcut/master.svg?style=flat)](https://travis-ci.org/jefdaj/shortcut "See test builds")

![feature-installscript](https://img.shields.io/travis/jefdaj/shortcut/feature-installscript?label=feature-installscript)

![Demo Site](https://img.shields.io/website?label=demo%20site&url=http%3A%2F%2Fshortcut.pmb.berkeley.edu)

![Env label](http://badges.herokuapp.com/travis/jefdaj/shortcut?env=BADGE=osx&label=osx&branch=feature-travisbadges)

| Mac | Linux |
|---|---|
|  |  |
|  |  |
|  |  |

![module-allvsall](https://img.shields.io/travis/jefdaj/shortcut/module-allvsall?label=module-allvsall&build=1)
![module-biomartr](https://img.shields.io/travis/jefdaj/shortcut/module-biomartr?label=module-biomartr)
![module-blast](https://img.shields.io/travis/jefdaj/shortcut/module-blast?label=module-blast)
![module-blastdb](https://img.shields.io/travis/jefdaj/shortcut/module-blastdb?label=module-blastdb)
![module-blasthits](https://img.shields.io/travis/jefdaj/shortcut/module-blasthits?label=module-blasthits)
![module-blastrbh](https://img.shields.io/travis/jefdaj/shortcut/module-blastrbh?label=module-blastrbh)
![module-busco](https://img.shields.io/travis/jefdaj/shortcut/module-busco?label=module-busco)
![module-cheat](https://img.shields.io/travis/jefdaj/shortcut/module-cheat?label=module-cheat)
![module-crbblast](https://img.shields.io/travis/jefdaj/shortcut/module-crbblast?label=module-crbblast)
![module-diamond](https://img.shields.io/travis/jefdaj/shortcut/module-diamond?label=module-diamond)
![module-greencut](https://img.shields.io/travis/jefdaj/shortcut/module-greencut?label=module-greencut)
![module-hmmer](https://img.shields.io/travis/jefdaj/shortcut/module-hmmer?label=module-hmmer)
![module-listlike](https://img.shields.io/travis/jefdaj/shortcut/module-listlike?label=module-listlike)
![module-load](https://img.shields.io/travis/jefdaj/shortcut/module-load?label=module-load)
![module-math](https://img.shields.io/travis/jefdaj/shortcut/module-math?label=module-math)
![module-mmseqs](https://img.shields.io/travis/jefdaj/shortcut/module-mmseqs?label=module-mmseqs)
![module-muscle](https://img.shields.io/travis/jefdaj/shortcut/module-muscle?label=module-muscle)
![module-orthofinder](https://img.shields.io/travis/jefdaj/shortcut/module-orthofinder?label=module-orthofinder)
![module-orthogroups](https://img.shields.io/travis/jefdaj/shortcut/module-orthogroups?label=module-orthogroups)
![module-permute](https://img.shields.io/travis/jefdaj/shortcut/module-permute?label=module-permute)
![module-plots](https://img.shields.io/travis/jefdaj/shortcut/module-plots?label=module-plots)
![module-psiblast](https://img.shields.io/travis/jefdaj/shortcut/module-psiblast?label=module-psiblast)
![module-range](https://img.shields.io/travis/jefdaj/shortcut/module-range?label=module-range)
![module-sample](https://img.shields.io/travis/jefdaj/shortcut/module-sample?label=module-sample)
![module-scores](https://img.shields.io/travis/jefdaj/shortcut/module-scores?label=module-scores)
![module-seqio](https://img.shields.io/travis/jefdaj/shortcut/module-seqio?label=module-seqio)
![module-sets](https://img.shields.io/travis/jefdaj/shortcut/module-sets?label=module-sets)
![module-setstable](https://img.shields.io/travis/jefdaj/shortcut/module-setstable?label=module-setstable)
![module-sonicparanoid](https://img.shields.io/travis/jefdaj/shortcut/module-sonicparanoid?label=module-sonicparanoid)
![module-summarize](https://img.shields.io/travis/jefdaj/shortcut/module-summarize?label=module-summarize)

![feature-cachix](https://img.shields.io/travis/jefdaj/shortcut/feature-cachix?label=feature-cachix)
![feature-darwin](https://img.shields.io/travis/jefdaj/shortcut/feature-darwin?label=feature-darwin)
![feature-installscript](https://img.shields.io/travis/jefdaj/shortcut/feature-installscript?label=feature-installscript)
![feature-nostack2nix](https://img.shields.io/travis/jefdaj/shortcut/feature-nostack2nix?label=feature-nostack2nix)
![feature-stack2nix](https://img.shields.io/travis/jefdaj/shortcut/feature-stack2nix?label=feature-stack2nix)
![feature-travisbadges](https://img.shields.io/travis/jefdaj/shortcut/feature-travisbadges?label=feature-travisbadges)

Quick Start
-----------

This should get you going on any Linux machine, and maybe MacOS:

    # 1. Install Nix
    curl https://nixos.org/nix/install | sh
    source ~/.nix-profile/etc/profile.d/nix.sh

    # 2. Build ShortCut
    git clone https://github.com/jefdaj/shortcut.git
    cd shortcut
    nix-build -j$(nproc)
    export PATH=$PWD/result/bin:$PATH

    # 3. Run self-tests (optional)
    shortcut --test

    # 4. Try it out
    shortcut

The rest of this document gives more details about each of them.


Install Nix
-----------

ShortCut is best built using [Nix][2], which ensures that all dependencies are
exactly satisfied. Not much human work is required, but it will download and/or
build a lot of packages and store them in `/nix`.

First you need the package manager itself. See [the website][2] for
instructions, or just run this:

    curl https://nixos.org/nix/install | sh
    source ~/.nix-profile/etc/profile.d/nix.sh

Installing ShortCut without this is theoretically possible, but much harder and less reliable.
Email Jeff if you want/need to try it so he can update the README with instructions!

To remove all Nix and ShortCut files later, edit the Nix line out of your `~/.bashrc` and run:

    rm -rf /nix
    rm -rf ~/.nix*
    rm -rf ~/.shortcut


Build Shortcut and run self-tests
---------------------------------

<a href="https://asciinema.org/a/MW5oHH9jMI0gFHXUnimwt3Sap" target="_blank">
  <img src="https://asciinema.org/a/MW5oHH9jMI0gFHXUnimwt3Sap.png" width="300"/>
</a>

After you have Nix, clone this repository and run `nix-build -j$(nproc)` inside
it. It will eventually create a symlink called `result` that points to the
finished package.

<a href="https://asciinema.org/a/mS8way8pStBVJ1rWQrHMAC8wN" target="_blank">
  <img src="https://asciinema.org/a/mS8way8pStBVJ1rWQrHMAC8wN.png" width="300"/>
</a>

Before using it, run the test suite to check that everything works:

    ./result/bin/shortcut --test

You might also want to add that to your `PATH` so you can call `shortcut` anywhere.
Add this line to your `~/.bashrc`.

    export PATH=$PWD/result/bin:$PATH


Build Docker image
------------------

`nix-build docker.nix` should do it.


Build Singularity image
-----------------------

`nix-build singularity.nix` should get you most of the way there, but you
should edit that file first to include any bind dirs and mount points used by
your institution's HPC environment.

The resulting `.img` file can be run with a long command like this:

```
singularity run -B /path/to/your/mount/point:/path/to/your/mount/point shortcut.img
```

That will drop you in a shell with `shortcut` + all dependencies available.
You'll only be able to use the host filesystem through the specified bind
points. Note that your institution might automatically bind some paths. You
don't need `-B` commands for those.

If you're using this, you may also want to write a custom wrapper script that
tells ShortCut how to run system calls using your HPC scheduler (SLURM or
similar).


Try it out
----------

<a href="https://asciinema.org/a/g5GErr9NQQABK6jfVHD3oX0cU" target="_blank">
  <img src="https://asciinema.org/a/g5GErr9NQQABK6jfVHD3oX0cU.png" width="300"/>
</a>

<a href="https://asciinema.org/a/euimAp0wYpVFfhZBqFaHoYc5h" target="_blank">
  <img src="https://asciinema.org/a/euimAp0wYpVFfhZBqFaHoYc5h.png" width="300"/>
</a>

These commands will run an existing script, load an existing script in the
interpreter, and start a new script in the interpreter respectively:

* `shortcut --script your-existing.cut`
* `shortcut --script your-existing.cut --interactive`
* `shortcut`

See [usage.txt][3] for other command line options, and type `:help` in the
interpreter for a list of special `:` commands (things you can only do in the live interpreter).

Now you're ready to start writing your own scripts!
See [the demo site][1] for everything related to that.


[1]: http://shortcut.pmb.berkeley.edu
[2]: https://nixos.org/nix/
[3]: usage.txt

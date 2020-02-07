OrthoLang: short, reproducible phylogenomic cuts
===============================================

OrthoLang is a scripting language meant to simplify a common task in
bioinformatics: making a list of candidate genes or genomes related to a
biological process of interest. These are sometimes called phylogenomic cuts.

It has two main design goals:

1. Be useful for biologists with limited prior coding experience
2. Codify the search in a format that can be published and reused by others

Run [scripts/install.sh](scripts/install.sh) to install it on Mac or Linux.

See [the demo site][1] for a more detailed overview, tutorial, interactive
examples, and reference of available functions.

Development Status
------------------

These are the only important tests if you want to try the current release:

[![Master branch](http://img.shields.io/travis/jefdaj/ortholang/master.svg?style=flat&label=master)](https://travis-ci.org/jefdaj/ortholang?label=master)
![Demo Site](https://img.shields.io/website?label=demo%20site&url=http%3A%2F%2Fortholang.pmb.berkeley.edu)
<!-- ![Env label](http://badges.herokuapp.com/travis/jefdaj/ortholang?env=BADGE=osx&label=osx&branch=feature-travisbadges) -->

But you may also be interested in progress on an upcoming feature.

Green checked boxes below have been done, grey ones are in progress, and blank ones are pending.
Test badges mean the [Travis](https://travis-ci.com/jefdaj/ortholang/branches) tests are passing on both Mac and Linux VMs.

These are changes to the core code or build system:

| branch           |  code | tests                                                                                     | demo |  docs |
|------------------|-------|-------------------------------------------------------------------------------------------|------|-------|
| [feature-fntags](https://github.com/jefdaj/ortholang/tree/feature-fntags) | :white_check_mark: | ![feature-fntags](https://badgen.net/travis/jefdaj/ortholang/feature-fntags?label=) |  |  |
| [feature-rerun-tests](https://github.com/jefdaj/ortholang/tree/feature-rerun-tests) | :white_check_mark: | ![feature-rerun-tests](https://badgen.net/travis/jefdaj/ortholang/feature-rerun-tests?label=) |  |  |
| [feature-singularity](https://github.com/jefdaj/ortholang/tree/feature-singularity) | :white_check_mark: | ![feature-singularity](https://badgen.net/travis/jefdaj/ortholang/feature-singularity?label=) |  |  |

And these are "modules" related to a specific language feature or bioinformatics program:

| branch           |  code | tests                                                                                     | demo |  docs |
|------------------|-------|-------------------------------------------------------------------------------------------|------|-------|
| [module-allvsall](https://github.com/jefdaj/ortholang/tree/module-allvsall) | :heavy_check_mark: | ![module-allvsall](https://badgen.net/travis/jefdaj/ortholang/module-allvsall?label=) |  |  |
| [module-biomartr](https://github.com/jefdaj/ortholang/tree/module-biomartr) | :heavy_check_mark: | ![module-biomartr](https://badgen.net/travis/jefdaj/ortholang/module-biomartr?label=) | :white_check_mark: |  |
| [module-blast](https://github.com/jefdaj/ortholang/tree/module-blast) | :heavy_check_mark: | ![module-blast](https://badgen.net/travis/jefdaj/ortholang/module-blast?label=) | :heavy_check_mark:  |  |
| [module-blastdb](https://github.com/jefdaj/ortholang/tree/module-blastdb) | :heavy_check_mark: | ![module-blastdb](https://badgen.net/travis/jefdaj/ortholang/module-blastdb?label=) | :heavy_check_mark:  |  |
| [module-blasthits](https://github.com/jefdaj/ortholang/tree/module-blasthits) | :heavy_check_mark: | ![module-blasthits](https://badgen.net/travis/jefdaj/ortholang/module-blasthits?label=) | :heavy_check_mark:  |  |
| [module-blastrbh](https://github.com/jefdaj/ortholang/tree/module-blastrbh) | :heavy_check_mark: | ![module-blastrbh](https://badgen.net/travis/jefdaj/ortholang/module-blastrbh?label=) | :white_check_mark:  |  |
| [module-busco](https://github.com/jefdaj/ortholang/tree/module-busco) | :heavy_check_mark: | ![module-busco](https://badgen.net/travis/jefdaj/ortholang/module-busco?label=) | :heavy_check_mark: |  |
| [module-cheat](https://github.com/jefdaj/ortholang/tree/module-cheat) | | ![module-cheat](https://badgen.net/travis/jefdaj/ortholang/module-cheat?label=) |  |  |
| [module-crbblast](https://github.com/jefdaj/ortholang/tree/module-crbblast) | :heavy_check_mark: | ![module-crbblast](https://badgen.net/travis/jefdaj/ortholang/module-crbblast?label=) | :heavy_check_mark:  |  |
| [module-diamond](https://github.com/jefdaj/ortholang/tree/module-diamond) | :heavy_check_mark: | ![module-diamond](https://badgen.net/travis/jefdaj/ortholang/module-diamond?label=) | :heavy_check_mark: |  |
| [module-greencut](https://github.com/jefdaj/ortholang/tree/module-greencut) | :white_check_mark: | ![module-greencut](https://badgen.net/travis/jefdaj/ortholang/module-greencut?label=) |  |  |
| [module-hmmer](https://github.com/jefdaj/ortholang/tree/module-hmmer) | :heavy_check_mark: | ![module-hmmer](https://badgen.net/travis/jefdaj/ortholang/module-hmmer?label=) | :heavy_check_mark: |  |
| [module-listlike](https://github.com/jefdaj/ortholang/tree/module-listlike) | :white_check_mark: | ![module-listlike](https://badgen.net/travis/jefdaj/ortholang/module-listlike?label=) |  |  |
| [module-load](https://github.com/jefdaj/ortholang/tree/module-load) | :heavy_check_mark: | ![module-load](https://badgen.net/travis/jefdaj/ortholang/module-load?label=) | :heavy_check_mark: |  |
| [module-math](https://github.com/jefdaj/ortholang/tree/module-math) | :heavy_check_mark: | ![module-math](https://badgen.net/travis/jefdaj/ortholang/module-math?label=) | :heavy_check_mark: |  |
| [module-mmseqs](https://github.com/jefdaj/ortholang/tree/module-mmseqs) | :heavy_check_mark: | ![module-mmseqs](https://badgen.net/travis/jefdaj/ortholang/module-mmseqs?label=) | :heavy_check_mark: |  |
| [module-muscle](https://github.com/jefdaj/ortholang/tree/module-muscle) | :heavy_check_mark: | ![module-muscle](https://badgen.net/travis/jefdaj/ortholang/module-muscle?label=) | :heavy_check_mark: |  |
| [module-orthofinder](https://github.com/jefdaj/ortholang/tree/module-orthofinder) | :white_check_mark: | ![module-orthofinder](https://badgen.net/travis/jefdaj/ortholang/module-orthofinder?label=) | :heavy_check_mark: |  |
| [module-orthogroups](https://github.com/jefdaj/ortholang/tree/module-orthogroups) | :white_check_mark: | ![module-orthogroups](https://badgen.net/travis/jefdaj/ortholang/module-orthogroups?label=) | :white_check_mark: |  |
| [module-permute](https://github.com/jefdaj/ortholang/tree/module-permute) | :white_check_mark: | ![module-permute](https://badgen.net/travis/jefdaj/ortholang/module-permute?label=) |  |  |
| [module-plots](https://github.com/jefdaj/ortholang/tree/module-plots) | :white_check_mark: | ![module-plots](https://badgen.net/travis/jefdaj/ortholang/module-plots?label=) | :white_check_mark: |  |
| [module-psiblast](https://github.com/jefdaj/ortholang/tree/module-psiblast) | :white_check_mark: | ![module-psiblast](https://badgen.net/travis/jefdaj/ortholang/module-psiblast?label=) | :heavy_check_mark: |  |
| [module-range](https://github.com/jefdaj/ortholang/tree/module-range) | :heavy_check_mark: | ![module-range](https://badgen.net/travis/jefdaj/ortholang/module-range?label=) |  |  |
| [module-sample](https://github.com/jefdaj/ortholang/tree/module-sample) | :heavy_check_mark: | ![module-sample](https://badgen.net/travis/jefdaj/ortholang/module-sample?label=) | :white_check_mark: |  |
| [module-scores](https://github.com/jefdaj/ortholang/tree/module-scores) | :heavy_check_mark: | ![module-scores](https://badgen.net/travis/jefdaj/ortholang/module-scores?label=) | :white_check_mark: |  |
| [module-seqio](https://github.com/jefdaj/ortholang/tree/module-seqio) | :heavy_check_mark: | ![module-seqio](https://badgen.net/travis/jefdaj/ortholang/module-seqio?label=) | :heavy_check_mark: |  |
| [module-sets](https://github.com/jefdaj/ortholang/tree/module-sets) | :heavy_check_mark: | ![module-sets](https://badgen.net/travis/jefdaj/ortholang/module-sets?label=) |  |  |
| [module-setstable](https://github.com/jefdaj/ortholang/tree/module-setstable) | :heavy_check_mark: | ![module-setstable](https://badgen.net/travis/jefdaj/ortholang/module-setstable?label=) |  |  |
| [module-sonicparanoid](https://github.com/jefdaj/ortholang/tree/module-sonicparanoid) | :heavy_check_mark: | ![module-sonicparanoid](https://badgen.net/travis/jefdaj/ortholang/module-sonicparanoid?label=) | :white_check_mark:  |  |
| [module-summarize](https://github.com/jefdaj/ortholang/tree/module-summarize) | :heavy_check_mark: | ![module-summarize](https://badgen.net/travis/jefdaj/ortholang/module-summarize?label=) |  |  |

Build Ortholang and run self-tests
----------------------------------

OrthoLang is best built using [Nix][2], which ensures that all dependencies are
exactly satisfied. Not much human work is required, but it will download and/or
build a lot of packages and store them in `/nix`.

First you need the package manager itself. See [the website][2] for
instructions, or just run this:

    curl https://nixos.org/nix/install | sh
    source ~/.nix-profile/etc/profile.d/nix.sh

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

    ./result/bin/ortholang --test

You might also want to add that to your `PATH` so you can call `ortholang` anywhere.
Add this line to your `~/.bashrc`.

    export PATH=$PWD/result/bin:$PATH


Docker
------

Get the latest official image from [Docker hub](https://hub.docker.com/r/jefdaj/ortholang) like so:

```
docker pull jefdaj/ortholang
```

To build a new image, edit and run [dev-scripts/build-docker-image.sh](https://github.com/jefdaj/ortholang/tree/master/dev-scripts/build-docker-image.sh).


Singularity
-----------

`nix-build singularity.nix` should get you most of the way there, but you
should edit that file first to include any bind dirs and mount points used by
your institution's HPC environment.

The resulting `.img` file can be run with a long command like this:

```
singularity run -B /path/to/your/mount/point:/path/to/your/mount/point ortholang.img
```

That will drop you in a shell with `ortholang` + all dependencies available.
You'll only be able to use the host filesystem through the specified bind
points. Note that your institution might automatically bind some paths. You
don't need `-B` commands for those.

If you're using this, you may also want to write a custom wrapper script that
tells OrthoLang how to run system calls using your HPC scheduler (SLURM or
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

* `ortholang --script your-existing-script.ol`
* `ortholang --script your-existing-script.ol --interactive`
* `ortholang`

See [usage.txt][3] for other command line options, and type `:help` in the
interpreter for a list of special `:` commands (things you can only do in the live interpreter).

Now you're ready to start writing your own scripts!
See [the demo site][1] for everything related to that.


[1]: http://ortholang.pmb.berkeley.edu
[2]: https://nixos.org/nix/
[3]: usage.txt

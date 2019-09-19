ShortCut: short, reproducible phylogenomic cuts
===============================================

[![Build Status](https://travis-matrix-badges.herokuapp.com/repos/jefdaj/shortcut/branches/master)](https://travis-ci.org/jefdaj/shortcut)

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

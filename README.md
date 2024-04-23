
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MixedUpSlimSims

<!-- badges: start -->
<!-- badges: end -->

MixedUpSlimSims is an R package to facilitate simulations of admixed
populations using SLiM, and particularly to collect the results in terms
of the ancestry tracts within the simulated individuals by way of
[tskit](https://tskit.dev/).

## Installation

You can install the development version of MixedUpSlimSims from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eriqande/MixedUpSlimSims")
```

However, in order to use it, you will also have to set up a conda
environments, and, if you are working on Mac with Apple Silicon (M1 chip
or higher) you actually have to run your R/RStudio session using an
x86_64 version of R that will run through Rosetta on you Mac.

In order to get this to work on an a mac running Apple Silicon there are
a few things that must be done.

1.  Install a conda environment within the x86_64 shell that includes
    `tskit`, `numpy`, `pandas`, and
    `bedtools`and`bcftools`.  Any version of these should work, so I am not     specifying versions here. You can name     this environment anything you want,     but it will be easiest to name it`mixed-up-parents`.

    ``` sh
    mamba  create -n mixed-up-parents -c conda-forge -c bioconda \
        anaconda::pandas \
        conda-forge::numpy \
        conda-forge::tskit  \
        bedtools \
        bcftools
    ```

    Installing this on a modern Mac requires that you have set up conda
    to work with x86_64. You can find instructions on how to do that
    [here](https://towardsdatascience.com/how-to-install-miniconda-x86-64-apple-m1-side-by-side-on-mac-book-m1-a476936bfaf0)
    or
    [here](https://taylorreiter.github.io/2022-04-05-Managing-multiple-architecture-specific-installations-of-conda-on-apple-M1/).
    Sadly, pyslim does not install with conda on arm64 because `kastore`
    is not compiled for that.

2.  To run this on Arm64 on a Mac, you could maybe do like this:
    a.  Download and build bcftools and bedtools, or get the binaries somehow (they aren't on conda for arm64 Mac)
        and then make sure they are on your path.
    b. Make a conda environment with pandas and numpy and then build kastore natively via pip:"
    ``` sh
    # first, creat a new environment with these things
    mamba  create -n mup -c conda-forge \
        anaconda::pandas \
        conda-forge::numpy

    # then activate the environment
    conda activate mup
    
    # then build kastore via pip.
    python -m pip install kastore
    
    # then add tskit to the environment
    mamba install conda-forge::tskit
    ```

4.  Update your `~/.bashrc` (or whatever file you use to initialize your
    shell) to export the path to the conda environment you just
    installed as the environment variable `MUP_CONDA`. My `~/.bashrc`
    includes the lines: this:

    ``` sh

    # For MixedUpSlimSims
    export MUP_CONDA=/Users/eriq/mambaforge-x86_64/envs/mixed-up-parents
    ```

    You should change
    `/Users/eriq/mambaforge-x86_64/envs/mixed-up-parents` to whatever
    the path is to your `mixed-up-parents` conda environment.

5.  You need to have an x86_64 version of R that can run under RStudio.
    (You probably don’t actually have to be able to run it under
    RStudio, but it is convenient to be able to do so.) I recommend that
    you install [`rig`](https://github.com/r-lib/rig) to manage your R
    installations. Once you have it installed, you can download and
    install the latest x86_64 version of R like this:

    ``` sh
    rig add -a x86_64 release
    ```

    Then you can make sure that it was installed, and see what its name
    is by using `rig list`:

    ``` sh
    rig list

    # the output should look something like this:
    * name        version    aliases
    ------------------------------------------
      4.2-arm64   (R 4.2.0)
    * 4.3-arm64   (R 4.3.3)
      4.3-x86_64  (R 4.3.3)  release
    ```

    And from that you can open this RStudio project by navigating to the
    project directory on your shell and doing:

    ``` sh
    rig rstudio 4.3-x86_64 MixedUpSlimSims.Rproj
    ```

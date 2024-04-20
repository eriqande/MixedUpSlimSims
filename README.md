
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

However, in order to use it, you will also have to set up some conda
environments, and, if you are working on Mac with Apple Silicon (M1 chip
or higher) you actually have to run your R/RStudio session using an
x86_64 version of R that will run through Rosetta on you Mac.

In order to get this to work on an a mac running Apple Silicon there are
a few things that must be done.

1.  Install a pyslim conda environment within the x86_64 shell.

    ``` sh
    mamba create -n pyslim -c conda-forge pyslim
    ```

    Doing this requires that you have set up conda to work with x86_64.
    You can find instructions on how to do that
    [here](https://towardsdatascience.com/how-to-install-miniconda-x86-64-apple-m1-side-by-side-on-mac-book-m1-a476936bfaf0)
    or
    [here](https://taylorreiter.github.io/2022-04-05-Managing-multiple-architecture-specific-installations-of-conda-on-apple-M1/).
    Sadly, pyslim does not install with conda on arm64 because `kastore`
    is not compiled for that. After that, activate the environment and
    do `pip install tspop`./

    Installing pyslim will get you `tskit` and any other conda packages
    that you need, except for a few others that we have added in there
    later. Those additional packages must be installed into your pyslim
    environment. You could do that when creating it, or be like us and
    just add them later:

    ``` sh
    conda activate pyslim
    mamba install -c conda-forge -c bioconda bcftools bedtools
    ```

2.  Then, you need to have an x86_64 version of R that can run under
    RStudio. I recommend that you install
    [`rig`](https://github.com/r-lib/rig) to manage your R
    insreetallations. Once you have it installed, you can download and
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

3.  Define the path to your pyslim conda environment, here. On my laptop
    it is:

    ``` r
    PYSLIM_CONDA <- normalizePath("~/mambaforge-x86_64/envs/pyslim")
    ```

    But it probably will be different on yours.

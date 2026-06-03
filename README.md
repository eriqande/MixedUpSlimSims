
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MixedUpSlimSims

<!-- badges: start -->

<!-- badges: end -->

MixedUpSlimSims is an R package to facilitate simulations of admixed
populations using SLiM, and particularly to collect the results in terms
of the ancestry tracts within the simulated individuals by way of
[tskit](https://tskit.dev/).

Because this depends on a lot of non-R dependencies (like python, tskit,
etc.) and because it uses reticulate under the hood you have to do some
work to set up the dependencies.

## Installation

You can install the development version of MixedUpSlimSims from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("eriqande/MixedUpSlimSims")
```

In order to use it, you will also have to set up a conda environment.

1.  Install a conda environment that includes `slim`, `tskit`, `numpy`,
    `pandas`, and `bedtools` and `bcftools`. Most version of these
    should work, but SLiM has to be version 4. You can name this
    environment anything you want, but it will be easiest to name it
    `mixed-up-parents`.

    ``` sh
    conda create -n mixed-up-parents -c conda-forge -c bioconda \
        anaconda::pandas \
        conda-forge::numpy \
        conda-forge::slim==4.2.2 \
        conda-forge::tskit  \
        bedtools \
        bcftools
    ```

2.  **On SEDNA:** If you are working on the SEDNA cluster, after you
    have installed the above conda environment, you have to do an
    additional step to keep pandas from failing when looking for a
    version of GLIBCXX that does not exist on the system:

    ``` sh
    conda activate mixed-up-parents  # activate the environment
    pip install pandas --no-cache-dir --force-reinstall  # reinstall pandas in there
    ```

    You should not have to do this unless you get errors that look like:
    `ImportError: /lib64/libstdc++.so.6: version 'GLIBCXX_3.4.29' not found`

3.  Update your `~/.bashrc` (or whatever file you use to initialize your
    shell) to export the path to the conda environment you just
    installed as the environment variable `MUP_CONDA`. My `~/.bashrc`
    includes the lines: this:

    ``` sh
    # For MixedUpSlimSims
    export MUP_CONDA=/Users/eriq/miniforge/envs/mixed-up-parents
    ```

    You should change `/Users/eriq/miniforge/envs/mixed-up-parents` to
    whatever the path is to your `mixed-up-parents` conda environment.

When using this on the cluster, you should activate the conda
environment before entering R.

In general, when working with this package, you should issue these
commands in the beginning of your script to make sure that R knows where
to find things (changing the paths as appropriate):

``` r
options(MUP_CONDA = "/Users/eriq/miniforge3/envs/mixed-up-parents")
Sys.setenv(MUP_CONDA = "/Users/eriq/miniforge3/envs/mixed-up-parents")

# set paths for python within R
reticulate::use_condaenv(condaenv = getOption("MUP_CONDA"))

# also set the path for system calls here
Sys.setenv(PATH = paste0(getOption("MUP_CONDA"), "/bin:", Sys.getenv("PATH")))
```

## Online Documentation

Be sure to head over to <https://eriqande.github.io/MixedUpSlimSims> for
the full `pkgdown` documentation.

A vignette is available there under the “Articles” tab.

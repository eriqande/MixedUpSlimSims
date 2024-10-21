
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
# install.packages("remotes")
remotes::install_github("eriqande/MixedUpSlimSims")
```

However, in order to use it, you will also have to set up a conda
environment, and, if you are working on Mac with Apple Silicon (M1 chip
or higher) you have to compile up some tskit related packages that are
not yet available on conda for arm64. (**UPDATE** it looks like kastore
is now avaiable on conda for arm64, so that simplifies things.)

1.  Install a conda environment that includes `slim`, `tskit`, `numpy`,
    `pandas`, and `bedtools` and `bcftools`. Any version of these should
    work, so I am not specifying versions here. You can name this
    environment anything you want, but it will be easiest to name it
    `mixed-up-parents`.

    ``` sh
    mamba  create -n mixed-up-parents -c conda-forge -c bioconda \
        anaconda::pandas \
        conda-forge::numpy \
        conda-forge::slim \
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
    export MUP_CONDA=/Users/eriq/mambaforge-x86_64/envs/mixed-up-parents
    ```

    You should change
    `/Users/eriq/mambaforge-x86_64/envs/mixed-up-parents` to whatever
    the path is to your `mixed-up-parents` conda environment.

When using this on the cluster, you should activate the conda
environment before entering R.

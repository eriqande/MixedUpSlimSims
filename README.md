
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MixedUpSlimSims

<!-- badges: start -->

<!-- badges: end -->

MixedUpSlimSims is an R package for simulating admixed populations with
[SLiM](https://messerlab.org/slim/) and then processing the resulting
tree sequences and genotypes in R. Its main use is to run SLiM
simulations, simplify and inspect the resulting
[tskit](https://tskit.dev/) tree sequences through Python, and return R
tibbles describing ancestry tracts, ancestry fractions, diagnostic
marker genotypes, and pairwise relationships among simulated
individuals.

This package is intended to be installed from GitHub, not CRAN. It
deliberately uses `reticulate` and command-line tools such as SLiM,
`bedtools`, and `bcftools`, so users need to set up a compatible conda
environment before using the simulation and tree-processing functions.

## Installation

Install the R package from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("eriqande/MixedUpSlimSims")
```

## External Dependencies

MixedUpSlimSims expects a conda environment that contains the Python and
command-line dependencies used by the package.

1.  Create a conda environment containing `slim`, `tskit`, `numpy`,
    `pandas`, `bedtools`, and `bcftools`. Most recent versions should
    work, but SLiM should be version 4. The environment can have any
    name; the examples below use `mixed-up-parents`.

    ``` sh
    conda create -n mixed-up-parents -c conda-forge -c bioconda \
        anaconda::pandas \
        conda-forge::numpy \
        conda-forge::slim==4.2.2 \
        conda-forge::tskit  \
        bedtools \
        bcftools
    ```

2.  Find the full path to the environment.

    ``` sh
    conda env list
    ```

3.  Export that path as `MUP_CONDA` in the shell startup file you
    normally use for R sessions, such as `~/.bashrc`, `~/.zshrc`, or a
    scheduler/cluster job script.

    ``` sh
    # For MixedUpSlimSims. Change this path to your own conda environment.
    export MUP_CONDA=/path/to/miniforge3/envs/mixed-up-parents
    ```

    Restart your shell or run `source ~/.bashrc` / `source ~/.zshrc`
    after editing the file.

4.  **On SEDNA:** If you are working on the SEDNA cluster, you may also
    need to reinstall `pandas` inside the environment to avoid a system
    `GLIBCXX` error:

    ``` sh
    conda activate mixed-up-parents  # activate the environment
    pip install pandas --no-cache-dir --force-reinstall  # reinstall pandas in there
    ```

    You should not have to do this unless you get errors that look like:
    `ImportError: /lib64/libstdc++.so.6: version 'GLIBCXX_3.4.29' not found`

## Starting an R Session

In any script or interactive session that uses the Python-backed
functions, set up `reticulate` before Python is initialized:

``` r
library(reticulate)
library(MixedUpSlimSims)

mup_conda <- Sys.getenv("MUP_CONDA")
if (mup_conda == "") {
  stop("Set MUP_CONDA to the full path of your MixedUpSlimSims conda environment.")
}

reticulate::use_condaenv(mup_conda, required = TRUE)
Sys.setenv(PATH = paste(file.path(mup_conda, "bin"), Sys.getenv("PATH"), sep = ":"))
```

On a cluster, it is also fine to activate the conda environment before
starting R, but `MUP_CONDA` should still be defined so scripts can
consistently locate the same environment.

There is intentionally no repository-level `.Rprofile` in this package.
A local `.Rprofile` with a hard-coded conda path is convenient for one
developer but fragile for everyone else, because R will silently use the
wrong path when the project is opened on another machine. If you want
local convenience settings, put them in your user-level R startup files
or create an untracked `.Rprofile` on your own machine.

## Typical Workflow

The package includes functions for:

- creating founder genotypes from allele frequencies;
- running SLiM models and reading their `.trees` output;
- using `tskit` through `reticulate` to identify nodes, individuals, and
  ancestry segments;
- summarizing individual ancestry tracts and admixture fractions;
- extracting marker genotypes from SLiM VCF output; and
- assembling simulated data sets for downstream analysis.

The package vignette shows a longer worked example using the included
example allele-frequency data and SLiM model.

## Online Documentation

Be sure to head over to <https://eriqande.github.io/MixedUpSlimSims> for
the full `pkgdown` documentation.

A vignette is available there under the “Articles” tab.

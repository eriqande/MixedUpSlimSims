# MixedUpSlimSims

## Setup

In order to get this to work on an M1 mac there are a few things that must be done.

1.  Get the package with:

    ```r
    remotes::install_github("eriqande/MixedUpSlimSims")
    ```

1.  Install a pyslim conda environment within the x86_64 shell and install bedtools into it as well.

    ```sh
    mamba create -n pyslim -c conda-forge pyslim
    conda activate pyslim
    conda install bioconda::bedtools
    ```

    Doing this requires that you have set up conda to work with x86_64. Sadly, pyslim does not install with conda on arm64 because `kastore` is not compiled for that.  After that, activate the environment and do `pip install tspop`./

2.  Then, you need to have an x86_64 version of R that can run under RStudio. I recommend that you install [`rig`](https://github.com/r-lib/rig) to manage your R insreetallations. Once you have it installed, you can download and install the latest x86_64 version of R like this:

    ```sh
    rig add -a x86_64 release
    ```

    You can make sure that it was installed, and see what its name is by using `rig list`:

    ```sh
    rig list
        
    # the output should look something like this:
    * name        version    aliases
    ------------------------------------------
      4.2-arm64   (R 4.2.0)
    * 4.3-arm64   (R 4.3.3)
      4.3-x86_64  (R 4.3.3)  release
    ```

    And from that you can open this RStudio project by navigating to the project directory on your shell and doing:

    ```sh
    rig rstudio 4.3-x86_64 MixedUpParents-slim-sim.Rproj
    ```

3.  In your scripts, you must use `reticulate::use_condaenv()` to set the conda environment, after
    you load the package with `library`.  For example, the top of your script should look something
    like:
    
    ```
    PYSLIM_CONDA <- normalizePath("~/mambaforge-x86_64/envs/pyslim")
    library(MixedUpSlimSims)
    reticulate::use_condaenv(condaenv = PYSLIM_CONDA)
    
    ```


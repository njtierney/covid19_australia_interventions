Estimating the latent social distancing effect in Australia in response to COVID-19 from multiple mobility datastreams.

# Running analyses

## Step 1: Install `capsule`

```r
install.packages("capsule", repos = "https://milesmcbain.r-universe.dev")
```

## Step 2: Reproduce the libraries used 

```r
capsule::reproduce_lib()
```

This recreates all of the R packages used in the analysis on your computer. Importantly, this will not change where your existing R packages are installed. It is just for this repository. So no need to be concerned about this impacting other analyses you run.

## Step 3: Run the target workflow

```r
capsule::run(targets::tar_make(<TARGER>))
```

This runs our targets workflow using the R packages specified.

This will check if the targets are written, and if they aren't, it will re-run the necessary ones. In this case, the targets are stored in the `_targets/objects` folder.

Note that you will for most cases need access to data
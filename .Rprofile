# source("renv/activate.R")
setHook(
  packageEvent("conflicted", "attach"),
  function(...) {
    invisible(suppressMessages(trace(conflicted:::is_superset, quote(
      if(is.primitive(get(fun))) {
        # hacking the existing exception for "lag"
        pkg <- "dplyr"
        fun <- "lag"
      }
    ), print = FALSE)))
  }
)

.First <- function() {
  options(
    repos = c(
      milesmcbain = 'https://milesmcbain.r-universe.dev',
      CRAN = 'https://cloud.r-project.org'
    ),
    browserNLdisabled = TRUE,
    deparse.max.lines = 2
  )
}


if (interactive()) {
  suppressMessages(require(devtools))
  suppressMessages(require(usethis))
}

Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)

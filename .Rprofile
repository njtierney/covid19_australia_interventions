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

library(conflicted)

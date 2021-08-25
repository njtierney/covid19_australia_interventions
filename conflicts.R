library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("pluck", "purrr")
conflict_prefer("map", "purrr")
conflict_prefer("pull", "dplyr")
conflict_prefer("complete", "tidyr")
conflict_prefer("expand", "tidyr")
weighted_mean <- weighted.mean
tfp <- reticulate::import("tensorflow_probability")
module <- greta::.internals$utils$misc$module
fl <- greta:::fl
tf_float <- greta:::tf_float
as.greta_array <- greta:::as.greta_array
# weighted mean and standard error of the weighted mean, computed with a
# bootstrap
weighted_mean <- weighted.mean
op <- greta::.internals$nodes$constructors$op
library(targets)
tar_delete(mobility_data)
tar_make(
  c(zipped_nsw)
)

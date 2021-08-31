get_nishiura_samples <- function(path) {
  
  # priors for the parameters of the lognormal distribution over the serial interval from Nishiura et
  # al., as stored in the EpiNow source code 
  read_csv(
    file = path,
    col_types = cols(param1 = col_double(),
                     param2 = col_double())
  )
  
}

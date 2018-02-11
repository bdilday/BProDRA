

run_it <- function(year=2016) {
  library(rstan)
  model_df <-  BProDRA::generate_model_df(year=year)
  mods <- BProDRA::initialize_with_lme4(model_df$ev)
  model_df <- BProDRA::update_ans(model_df, mods)
  options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
  BProDRA::do_stan_fit(model_df)

}

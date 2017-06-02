

#' @import dplyr
#' @importFrom lme4 ranef
#' @export
ranef_to_df <- function(glmer_mod, ranef_name) {
  rr = lme4::ranef(glmer_mod)[[ranef_name]]
  dplyr::data_frame(var_id=rownames(rr), value=rr[,1])
}

#' @export
load_comparison_metrics <- function() {
  readRDS(sprintf('%s/BProDRA/inst/extdata/pitching_metric_throwdown.rds', .libPaths()[[1]]))
}

#' @export
load_fitted_dra_models <- function(year) {
  ans <- list()
  fs <- Sys.glob(sprintf('%s/BProDRA/extdata/glmer*%d*rds', .libPaths()[[1]], year))
  for (f in fs) {
    metric <- stringr::str_replace(f, sprintf(".+glmer_mod_(.+)_%s.rds", year), "\\1")
    ans[[metric]] <- readRDS(f)
  }
  ans
}

#' @export
extract_pitcher_ranef <- function(mods) {
  purrr::reduce(
    lapply(names(mods),
           function(mod_key) {
             tmp<-ranef_to_df(mods[[mod_key]], "pitcher");
             tmp$model_name <- mod_key;
             return(tmp)
             }
           ),
    rbind.data.frame
    )
}


#' @export
pool_predictions <- function(.data, mods) {
  metrics <- c("HR", "3B", "2B", "1B_IF", "1B_OF", "UIBB", "IBB", "HBP", "SO",
               "Pitcher_PO", "Catcher_PO", "First_PO", "Second_PO", "Third_PO", "Short_PO",
               "LF_PO", "CF_PO", "RF_PO",
               "Pitcher_DP", "Catcher_DP",
               "First_DP", "Second_DP", "Third_DP", "Short_DP")

  vv = get_dra_model_data(.data, "HR")
  purrr::reduce(lapply(names(mods), function(mod_key) {
    print(mod_key);
    predict(mods[[mod_key]], vv, type='response', allow.new.levels=TRUE)}), cbind)

}



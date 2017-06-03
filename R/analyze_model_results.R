

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
pool_predictions <- function(event_data, mods, predict_type='response') {
  metrics <- c("HR", "3B", "2B", "1B_IF", "1B_OF", "UIBB", "IBB", "HBP", "SO",
               "Pitcher_PO", "Catcher_PO", "First_PO", "Second_PO", "Third_PO", "Short_PO",
               "LF_PO", "CF_PO", "RF_PO",
               "Pitcher_DP", "Catcher_DP",
               "First_DP", "Second_DP", "Third_DP", "Short_DP")

  vv = get_dra_model_data(event_data, "HR")
  purrr::reduce(lapply(names(mods), function(mod_key) {
    print(mod_key);
    predict(mods[[mod_key]], vv, type=predict_type, allow.new.levels=TRUE)}), cbind)

}

#' @export
get_linear_weights <- function() {

  data_frame(`1B_IF`=0.6,
       `1B_OF`=0.6,
       `2B`=0.95,
       `3B`=1.3,
       "Catcher_PO"=-0.3,
       "DP"=-0.6,
       "HBP"=0.4,
       "HR"=1.7,
       "IBB"=0.4,
       "PO"=-0.3,
       "Pitcher_PO"=-0.3,
       "SO"=-0.3,
       "UIBB"=0.4)

}

#' @export
logit_fun <- function(x) {
  exp(x)/(1+exp(x))
}

#' @export
get_delta_probs <- function(event_data, mods, pit_id, pitcher_ranef_df=NULL) {
  if(is.null(pitcher_ranef_df)){
    pitcher_ranef_df <- extract_pitcher_ranef(mods)
  }

  cc = which(event_data$PIT_ID == pit_id)
  pp <- pool_predictions(event_data[cc,], mods, predict_type = 'link')
  zz <- pitcher_ranef_df %>% subset(var_id==pit_id)

  w = pp
  w0 = t(t(pp) - zz$value)

  p = logit_fun(w)
  p0 = logit_fun(w0)
  p - p0

}

#' @export
get_dra_runs <- function(event_data, mods, pit_id, pitcher_ranef_df=NULL, delta_probs=NULL) {
  if (is.null(delta_probs)) {
    delta_probs <- get_delta_probs(event_data, mods, pit_id, pitcher_ranef_df)
  }

  lw <- get_linear_weights()
  tt = colSums(delta_probs) * c(data.matrix(lw))

  df1 = data_frame()
  for (i in seq_along(tt)) {
    df1[[names(mods)[[i]]]] = tt[[i]]
  }
  df1
}

#' @export
dra_components_boxplot <- function(.data) {

  gg <- .data %>%
    dplyr::mutate(p=exp(value)/(1+exp(value))) %>%
    ggplot(aes(x=model_name, y=p, group=model_name)) +
    geom_boxplot() + theme_minimal() + coord_flip() +
    labs(title='DRA Components', x='DRA Component', y='prediction')
  gg <- gg + theme(axis.text = element_text(size=12, face='bold'), axis.title = element_text(face='bold', size=14))

  }


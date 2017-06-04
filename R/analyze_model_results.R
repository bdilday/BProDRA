

#' @importFrom magrittr %$%
#' @import dplyr
#' @import lme4
#' @export
ranef_to_df <- function(glmer_mod, ranef_name) {
  rr = lme4::ranef(glmer_mod)[[ranef_name]]
  dplyr::data_frame(var_id=rownames(rr), value=rr[,1])
}


#' @export
average_ranef <-function(mod, pit_id, ranef_name) {
  rrb = ranef_to_df(mod, ranef_name)
  cc = as.character(mod@frame$pitcher) == as.character(pit_id)
  x = sum(cc)
  if (ranef_name %in% names(mod@frame[cc,])) {
    tmp <- mod@frame[cc,] %>% merge(rrb, by.x=ranef_name, by.y="var_id")
    mean(tmp$value)
  } else {
   0
  }

}

#' @export
summarise_ranef <- function(mod, pit_id) {
  df1 <- mod@frame
  cat(names(df1))
  cc = df1$pitcher == pit_id
  ranef_name <- names(lme4::ranef(mod))
  data_frame(ranef_name=ranef_name,
             mean_value=sapply(ranef_name, function(r) {
               average_ranef(mod, pit_id, r)}
               )
             )
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
  vv = get_dra_model_data(event_data, "HR")
  purrr::reduce(lapply(names(mods), function(mod_key) {
   # print(mod_key);
    predict(mods[[mod_key]], vv, type=predict_type, allow.new.levels=TRUE)}), cbind)

}

#' @export
get_linear_weights <- function() {

  out_val = -0.3

  data_frame(`1B_IF`=0.6,
       `1B_OF`=0.6,
       `2B`=0.95,
       `3B`=1.3,
       "HR"=1.7,
       "Catcher_PO"=out_val,
       "CF_PO"=-0.3,
       "DP"=-0.6,
       "First_PO"=out_val,
       "HBP"=0.4,
       "IBB"=0.4,
       "LF_PO"=out_val,
       "PO"=out_val,
       "Pitcher_PO"=out_val,
       "SO"=out_val,
       "UIBB"=0.4,
       "RF_PO"=out_val,
       "Second_PO"=out_val,
       "Short_PO"=out_val,
       "Third_PO"=out_val
       )

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
  tt = colSums(delta_probs) * c(data.matrix(lw[,names(mods)]))

  data_frame(event_type=names(mods), dra_runs=tt)

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


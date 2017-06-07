

#' @importFrom magrittr %$%
#' @import dplyr
#' @import lme4
#' @export
ranef_to_df <- function(glmer_mod, ranef_name) {
  rr = lme4::ranef(glmer_mod)[[ranef_name]]
  dplyr::data_frame(var_id=rownames(rr), value=rr[,1])
}

#' @export
export_dra_results <- function(year, npit=NULL) {
  print('loading events...')
  ev <- load_events_data(year)
  pit_ids <- unique(ev$PIT_ID)
  print('loading mods...')
  mods <- load_fitted_dra_models(year)
  print('getting pit ranef...')
  pit_ranef <- extract_pitcher_ranef(mods)

  if (is.null(npit)) {
    npit <- length(pit_ids)
  }

  ll <- lapply(1:npit, function(idx) {
    s <- pit_ids[[idx]]
    print(sprintf("%04d %04d %s", idx, npit, s))
    tmp <- get_dra_runs(ev, mods, s)
    tmp$pit_id <- s
    tmp
    }
  )
  dra_runs <- purrr::reduce(ll, rbind.data.frame)

  print('getting summaries...')
  model_names <- names(mods)

  ll <- lapply(1:npit, function(pit_id_idx) {
    pit_id <- pit_ids[[pit_id_idx]]
    print(sprintf("%04d %04d %s", pit_id_idx, npit, pit_id))
    ll <- lapply(1:length(model_names),
                 function(model_name_idx) {
                   model_name <- model_names[[model_name_idx]]
                   tmp <-summarise_ranef(mods[[model_name_idx]], pit_id)
                   tmp$model_name <- model_name
                   tmp
                 })
    purrr::reduce(ll, rbind.data.frame)
  })


  model_ranef <- purrr::reduce(ll, rbind.data.frame)
  tmp <- list(pit_ranef=pit_ranef, dra_runs=dra_runs, model_ranef=model_ranef)
  saveRDS(tmp, file=sprintf('./inst/extdata/dra_results_%d.rds', year))
  tmp
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
#  cat(names(df1))
  cc = df1$pitcher == pit_id
  ranef_name <- names(lme4::ranef(mod))
  data_frame(pit_id=pit_id,
             ranef_name=ranef_name,
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

  woba_weights <- list(
    HBP=0.7,
    BB=0.7,
    x1B=0.9,
    x2B=1.25,
    x3B=1.6,
    x4B=2.0
  )
  woba_scale = 1.25
  woba_scale_ = 1/woba_scale
  out_val = -0.28

  lw <- lapply(lapply(woba_weights, `+`, out_val), `*`, woba_scale_)
  data_frame(`1B_IF`=lw$x1B,
       `1B_OF`=lw$x1B,
       `2B`=lw$x2B,
       `3B`=lw$x3B,
       "HR"=lw$x4B,
       "Catcher_PO"=out_val,
       "CF_PO"=out_val,
       "DP"=out_val*2,
       "First_PO"=out_val,
       "HBP"=lw$HBP,
       "IBB"=lw$BB,
       "LF_PO"=out_val,
       "PO"=out_val,
       "Pitcher_PO"=out_val,
       "SO"=out_val,
       "UIBB"=lw$BB,
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


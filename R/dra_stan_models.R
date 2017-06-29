
library(BProDRA)
library(rstan)
library(dplyr)
library(magrittr)
library(lme4)

generate_event_data <- function(nlim = NULL, rseed=102, year=2016) {
  ev <- load_events_data(year)

  # rm pitchers as hitters
  ev %<>% filter(BAT_FLD_CD != 1)

  # anyone with less than 20 PA hitting is generic
  lowpa_batters <- ev %>% group_by(BAT_ID) %>% summarise(PA=n()) %>% filter(PA<=20) %$% BAT_ID

  # anyone with less than 20 PA hitting is generic
  lowpa_pitchers <- ev %>% group_by(PIT_ID) %>% summarise(PA=n()) %>% filter(PA<=20) %$% PIT_ID

  if (length(lowpa_batters) > 0) {
    cc <- which(ev$BAT_ID %in% lowpa_batters)
    ev[cc,]$BAT_ID <- "xxxxb001"
  }

  if (length(lowpa_pitchers) > 0) {
    cc <- which(ev$PIT_ID %in% lowpa_pitchers)
    ev[cc,]$PIT_ID <- "xxxxp001"
  }

  if (!is.null(nlim)) {
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }

  cc_hr <- which(ev$EVENT_CD == 23)
  cc_so <- which(ev$EVENT_CD == 3)
  cc_bip0 <- which(ev$EVENT_CD == 2)
  cc_bip1 <- which(ev$EVENT_CD >= 20 & ev$EVENT_CD <= 22)
  cc_bb <- which(ev$EVENT_CD >= 14 & ev$EVENT_CD <= 16)

  ev$outcome <- NA

  ev[cc_bip0,]$outcome <- 1
  ev[cc_bip1,]$outcome <- 2
  ev[cc_hr,]$outcome <- 3
  ev[cc_so,]$outcome <- 4
  ev[cc_bb,]$outcome <- 5

  assertthat::are_equal(sum(is.na(ev$outcome)), 0)
  ev %<>% mutate(bid=as.integer(as.factor(BAT_ID)))
  ev %<>% mutate(pid=as.integer(as.factor(PIT_ID)))
  ev %<>% mutate(sid=as.integer(as.factor(HOME_TEAM_ID)))
  ev %<>% mutate(pid = pid+max(bid))
  ev %<>% mutate(sid = sid+max(pid))
  ev %<>% select(GAME_ID, EVENT_ID, EVENT_CD, BAT_ID, PIT_ID, HOME_TEAM_ID, bid, pid, sid, outcome)

  ev
}

generate_model_df <- function(event_data=NULL,
                              nlim = NULL, rseed=102, year=2016) {
  if (is.null(event_data)) {
    event_data <- generate_event_data(nlim=nlim, rseed=rseed, year=year)
  }

  bat_ids <- unique(event_data$BAT_ID)
  pit_ids <- unique(event_data$PIT_ID)
  stad_ids <- unique(event_data$HOME_TEAM_ID)

  xx <- model.matrix(outcome ~ bid + pid + sid, data=event_data)[,-1]
  max_levels <- max(xx)
  LEVELS <- c(length(bat_ids), length(pit_ids), length(stad_ids))
  sum_levels = sum(LEVELS)
  ans <- list(N=dim(xx)[[1]],
              D=dim(xx)[[2]],
              K=length(unique(event_data$outcome)),
              LEVELS=LEVELS,
              x=xx,
              y=as.integer(as.character(event_data$outcome)),
              MAX_LEVEL=max_levels,
              SUM_LEVELS=sum_levels,
              ev=event_data)
}

initialize_with_lme4 <- function(model_df,
                                 frm=as.formula('outcome ~ (1|bid) + (1|pid) + (1|sid)')) {

  mods <- list()
  unique_outcomes <- unique(model_df$outcome) %>% sort()
  for (u in unique_outcomes) {
    cat(sprintf('fitting model for outcome: %d \n', u))
    cc <- which(model_df$outcome == u)
    stopifnot(length(cc) > 0)
    tmp <- model_df
    tmp[cc,]$outcome <- 1
    tmp[-cc,]$outcome <- 0
    glmer_mod <- glmer(frm, data=tmp,
                       nAGQ = 0,
                       family = binomial,
                       control=glmerControl(optimizer = "nloptwrap")
    )
    mods[[as.character(u)]] <- glmer_mod
  }
  mods

}

update_ans <- function(ans, mods) {
  nl <- length(names(mods))

  ans$RANEF_SIGMA <- matrix(rep(0, ans$D * ans$K), ncol=ans$D)
  for (i in seq_along(names(mods))) {
    ichar <- names(mods)[[i]]
    mod <- mods[[ichar]]
    thetas <- sapply(mod@theta, max, 1e-6)
    ans$RANEF_SIGMA[i,] <- thetas
  }

  rr <- matrix(rep(0, ans$K * ans$SUM_LEVELS), ncol=ans$SUM_LEVELS)
  for (idx in seq_along(names(mods))) {
    ichar <- names(mods)[[idx]]
    mod <- mods[[ichar]]
    tmp <- matrix(t(rbind(ranef(mod)$bid,ranef(mod)$pid,ranef(mod)$sid)), nrow=1)
    rr[idx,] <- tmp
  }
  cc <- rr < 1e-2
  rr[cc] <- 0.1
  ans$rr <- rr
  ans
}

get_init_fun <- function(ans, do_iden=FALSE) {
  rr <- ans$rr
  k <- ans$K

  if (do_iden) {
    function(chain_id=NULL) {
      list(ALPHAX=rr[1:(k-1),], CX=rep(-1, k-1))
    }
  } else {
    function(chain_id=NULL) {
      list(ALPHAX=rr, CX=rep(-1, k))
    }
  }

}

do_fit <- function(ans, warmup=100, iter=500, seed=10101, do_iden=FALSE) {
  init_fun <- get_init_fun(ans, do_iden=do_iden)
  if (do_iden) {
    stan(file='inst/extdata/multinom_ravel_init_identify.stan',
         model_name="multinom_iden",
         data=ans,
         iter=iter,
         warmup=warmup,
         #         init="0",
         init=init_fun,
         seed=seed,
         cores=4, chains=4)
  } else {
    stan(file='inst/extdata/multinom_ravel_init.stan',
         model_name="multinom_",
         data=ans,
         iter=iter,
         warmup=warmup,
         #init="0",
         init=init_fun,
         seed=seed,
         cores=4, chains=4)
  }
}


predict_from_stan <- function(stan_mod) {
  ee <- rstan::extract(stan_mod)

  alpha_m <- purrr::reduce(lapply(1:5, function(i) {ee$ALPHA[,i,] %>% colMeans()}), cbind)
  c_m <- colMeans(ee$C)

  #  cm[166,] + cm[1061,] + cm[1230,] + colMeans(ee$C)

  pp_mu <- t(t(alpha_m[ans$ev$bid,] + alpha_m[ans$ev$pid,] + alpha_m[ans$ev$sid,]) + c_m)

  nl <- dim(pp_mu)[[1]]
  ll <- lapply(1:nl, function(i) {exp(pp_mu[i,]) / (sum(exp(pp_mu[i,1:5])))})

}

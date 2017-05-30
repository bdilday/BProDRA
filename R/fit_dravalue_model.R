
#' @import dplyr
#' @import lme4
#' @import magrittr
#' @export
fit_dra_value_model <- function(.data, metric) {
  mod_df <- get_dra_model_data(.data, metric)
  frm <- get_dra_model_frm(metric)
  glmer_mod <- glmer(frm, data=mod_df, nAGQ = 0, family = binomial)
}

#' @export
export_dra_model <- function(.data, metric, year) {
  ofile = sprintf('./inst/extdata/glmer_mod_%s_%d.rds', metric, year)
  glmer_mod <- fit_dra_value_model(.data, metric)
  saveRDS(glmer_mod, file=ofile)
}

#' @export
get_dra_model_data <- function(.data, metric="HR") {

  .data$base_outs <- sprintf("%d%d%d%d",
                             1-as.integer(.data$BASE1_RUN_ID==''),
                             1-as.integer(.data$BASE2_RUN_ID==''),
                             1-as.integer(.data$BASE3_RUN_ID==''),
                             .data$OUTS_CT
  )

  tmp <- .data %>% group_by(GAME_ID, PIT_ID) %>%
    mutate(min_pa=min(GAME_PA_CT), PIT_PA_CT=GAME_PA_CT-min_pa, TTO=(PIT_PA_CT %/% 9) + 1 ) %>%
    ungroup()

  if (metric =='HR') {
    tmp$outcome = ifelse(tmp$EVENT_CD==23, 1, 0)
  } else if (metric == '3B') {
    tmp$outcome = ifelse(tmp$EVENT_CD==22, 1, 0)
  } else if (metric == '2B') {
    tmp$outcome = ifelse(tmp$EVENT_CD==21, 1, 0)
  } else if (metric == '1B_IF') {
    tmp$outcome = ifelse(
      (tmp$EVENT_CD==20) & (grepl('^S[1-6]{1}', tmp$EVENT_TX)) , 1, 0)
  } else if (metric == '1B_OF') {
    tmp$outcome = ifelse(
      (tmp$EVENT_CD==20) & (grepl('^S[7-9]{1}', tmp$EVENT_TX)), 1, 0)
  } else if (metric == 'UIBB') {
    tmp$outcome = ifelse(tmp$EVENT_CD==14, 1, 0)
  } else if (metric == 'IBB') {
    tmp$outcome = ifelse(tmp$EVENT_CD==15, 1, 0)
  } else if (metric == 'HBP') {
    tmp$outcome = ifelse(tmp$EVENT_CD==16, 1, 0)
  } else if (metric == 'SO') {
    tmp$outcome = ifelse(tmp$EVENT_CD==3, 1, 0)
  }  else if (metric == 'Pitcher_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 1, 1, 0)
  }  else if (metric == 'Catcher_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 2, 1, 0)
  }  else if (metric == 'First_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 3, 1, 0)
  }  else if (metric == 'Second_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 4, 1, 0)
  }  else if (metric == 'Third_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 5, 1, 0)
  }  else if (metric == 'Short_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 6, 1, 0)
  }  else if (metric == 'LF_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 7, 1, 0)
  }  else if (metric == 'CF_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 8, 1, 0)
  }  else if (metric == 'RF_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse(tmp$PO1_FLD_CD == 9, 1, 0)
  } else {
    stop(sprintf("unknown metric: %s", metric))
  }

  tmp <- tmp %>% transmute(GAME_ID=GAME_ID,
                           outcome=outcome,
                           pitcher_hitting=ifelse(BAT_FLD_CD==1, TRUE, FALSE),
                           role=PIT_START_FL,
                           bats=BAT_HAND_CD, throws=PIT_HAND_CD,
                           pitcher=PIT_ID,
                           batter=BAT_ID,
                           catcher=POS2_FLD_ID,
                           stadium=HOME_TEAM_ID,
                           Pos_2=POS2_FLD_ID,
                           Pos_3=POS3_FLD_ID,
                           Pos_4=POS4_FLD_ID,
                           Pos_5=POS5_FLD_ID,
                           Pos_6=POS6_FLD_ID,
                           Pos_7=POS7_FLD_ID,
                           Pos_8=POS8_FLD_ID,
                           Pos_9=POS9_FLD_ID,
                           inning_10=ifelse(INN_CT>9, 1, 0),
                           score_diff = ifelse(BAT_HOME_ID==1,
                                               HOME_SCORE_CT-AWAY_SCORE_CT,
                                               -HOME_SCORE_CT+AWAY_SCORE_CT),
                           open_1B_outs = as.factor(
                             ifelse(tmp$BASE1_RUN_ID=='',
                                    paste0('1', tmp$OUTS_CT),
                                    paste0('0', tmp$OUTS_CT))
                           ),
                           fld_team = ifelse(BAT_HOME_ID==1, AWAY_TEAM_ID, HOME_TEAM_ID),
                           home_team=as.factor(BAT_HOME_ID),
                           base_outs = as.factor(base_outs),
                           fld_team = ifelse(BAT_HOME_ID==1, AWAY_TEAM_ID, HOME_TEAM_ID),
                           TTO=TTO,
                           assist=as.factor(ASS1_FLD_CD)
  )
}


#' @export
get_dra_model_frm <-function(metric) {
  if (metric == 'HR') {
    #TODO: add temperature, framing
    outcome ~ role + bats + throws + (1|pitcher) + (1|stadium) + (1|pitcher_hitting) + (1|batter) + (1|catcher)
  } else if (metric == '3B') {
    #TODO: what is IF-fld?
    outcome ~ (1|batter) + (1|stadium:bats) + (1|Pos_3) + (1|Pos_4) + (1|Pos_7) + (1|Pos_8) + (1|pitcher)
  } else if (metric == '2B') {
    #TODO: add IF-fld (what is it?)
    outcome ~ (1|pitcher) + (1|batter) + (1|Pos_4) + (1|Pos_5) + (1|Pos_7) + (1|Pos_8) + (1|stadium:bats) + inning_10
  } else if (metric == '1B_IF') {
    outcome ~ bats + throws + (1|pitcher) + (1|batter) + (1|Pos_3) + (1|Pos_4) + (1|Pos_5) + (1|Pos_6)
  } else if (metric == '1B_OF') {
    outcome ~ bats + throws + (1|pitcher) + (1|batter) + (1|Pos_5) + (1|Pos_7) + (1|Pos_8) + (1|Pos_9)
  } else if (metric == 'UIBB') {
    # TODO: add framing
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|pitcher_hitting) + (1|base_outs) + bats + throws + tto + home_team
  } else if (metric == 'IBB') {
    outcome ~ bats + throws + role + inning_10 + score_diff + (1|batter) + (1|pitcher) + (1|open_1B_outs) + (1|Pos_2) + (1|fld_team)
  } else if (metric == 'HBP') {
    outcome ~ bats + throws + role + (1|batter) + (1|pitcher) + (1|base_outs) + (1|fld_team)
  }  else if (metric == 'SO') {
    # TODO: add framing
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|pitcher_hitting) + (1|base_outs) + bats + throws + tto + home_team
  }  else if (metric == 'Pitcher_PO') {
    # TODO: what is assist? is it a position cd, or a player id? I think it's probably a player, otherwise making it a random effect doesn't make sense
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_3) + (1|assist) + bats + throws
  }  else if (metric == 'Catcher_PO') {
    # TODO: what is assist? is it a position cd, or a player id? I think it's probably a player, otherwise making it a random effect doesn't make sense
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|base_outs) + bats + throws
  } else {
    stop(sprintf("unknown metric: %s", metric))
  }
}


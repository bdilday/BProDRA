
#' @import dplyr
#' @import lme4
#
#' @export
fit_dra_value_model <- function(.data, metric) {
  func_name <- sprintf("get_dra_model_data_%s", metric)
  data_pull <- get(func_name)
  mod_df <- data_pull(.data)
  glmer_mod <- glmer(outcome ~ ., data=mod_df, nAGQ = 0, family = binomial)

}

#' @export
get_dra_model_data <- function(.data, metric="HR") {

  .data$base_outs <- sprintf("%d%d%d%d",
                             1-as.integer(.data$BASE1_RUN_ID==''),
                             1-as.integer(.data$BASE2_RUN_ID==''),
                             1-as.integer(.data$BASE3_RUN_ID==''),
                             .data$OUTS_CT
  )

  if (metric =='HR') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==23, 1, 0),
                               pitcher_hitting=ifelse(BAT_FLD_CD==1, TRUE, FALSE),
                               role=PIT_START_FL, bats=BAT_HAND_CD, throws=PIT_HAND_CD,
                               pitcher=PIT_ID, batter=BAT_ID, catcher=POS2_FLD_ID, stadium=HOME_TEAM_ID)
  } else if (metric == '3B') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==22, 1, 0),
                               batter=BAT_ID,
                               stadium=HOME_TEAM_ID, bats=BAT_HAND_CD,
                               pitcher=PIT_ID,
                               Pos_3=POS3_FLD_ID,
                               Pos_4=POS4_FLD_ID,
                               Pos_7=POS7_FLD_ID,
                               Pos_8=POS8_FLD_ID)
  } else if (metric == '2B') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==21, 1, 0),
                               pitcher=PIT_ID,
                               batter=BAT_ID,
                               stadium=HOME_TEAM_ID, bats=BAT_HAND_CD,
                               Pos_4=POS4_FLD_ID,
                               Pos_5=POS5_FLD_ID,
                               Pos_7=POS7_FLD_ID,
                               Pos_8=POS8_FLD_ID,
                               inning_10=ifelse(INN_CT>9, 1, 0)
    )
  } else if (metric == '1B_IF') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==20, 1, 0),
                               bats=BAT_HAND_CD,
                               throws=PIT_HAND_CD,
                               pitcher=PIT_ID,
                               batter=BAT_ID,
                               Pos_3=POS3_FLD_ID,
                               Pos_4=POS4_FLD_ID,
                               Pos_5=POS5_FLD_ID,
                               Pos_6=POS6_FLD_ID
    )
  } else if (metric == '1B_OF') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==20, 1, 0),
                               bats=BAT_HAND_CD,
                               throws=PIT_HAND_CD,
                               pitcher=PIT_ID,
                               batter=BAT_ID,
                               Pos_5=POS5_FLD_ID,
                               Pos_7=POS7_FLD_ID,
                               Pos_8=POS8_FLD_ID,
                               Pos_9=POS9_FLD_ID
    )
  } else if (metric == 'UIBB') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==14, 1, 0),
                               bats=BAT_HAND_CD,
                               throws=PIT_HAND_CD,
                               pitcher=PIT_ID,
                               batter=BAT_ID,
                               Pos_5=POS5_FLD_ID,
                               Pos_7=POS7_FLD_ID,
                               Pos_8=POS8_FLD_ID,
                               Pos_9=POS9_FLD_ID
    )
  } else if (metric == 'IBB') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==15, 1, 0),
                               bats=BAT_HAND_CD,
                               throws=PIT_HAND_CD,
                               inning_10=ifelse(INN_CT>9, 1, 0),
                               score_diff = ifelse(BAT_HOME_ID==1,
                                                   HOME_SCORE_CT-AWAY_SCORE_CT,
                                                   -HOME_SCORE_CT+AWAY_SCORE_CT),
                               open_1B_outs = as.factor(
                                 ifelse(BASE1_RUN_ID=='', paste0('1', OUTS_CT),
                                        BASE1_RUN_ID=='', paste0('0', OUTS_CT)
                                 )),
                               pitcher=PIT_ID,
                               batter=BAT_ID,
                               Pos_2=POS2_FLD_ID,
                               fld_team = ifelse(BAT_HOME_ID==1, AWAY_TEAM_ID, HOME_TEAM_ID)
    )
  } else if (metric == 'HBP') {

    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==16, 1, 0),
                               bats=BAT_HAND_CD,
                               throws=PIT_HAND_CD,
                               pitcher=PIT_ID,
                               batter=BAT_ID,
                               base_outs = as.factor(base_outs),
                               fld_team = ifelse(BAT_HOME_ID==1, AWAY_TEAM_ID, HOME_TEAM_ID)
    )
  } else if (metric == 'SO') {
    tmp <- .data %>% transmute(outcome=ifelse(EVENT_CD==3, 1, 0),
                               bats=BAT_HAND_CD,
                               throws=PIT_HAND_CD,
                               pitcher=PIT_ID,
                               batter=BAT_ID,
                               Pos_2=POS2_FLD_ID,
                               pitcher_hitting=ifelse(BAT_FLD_CD==1, TRUE, FALSE),
                               base_outs=as.factor(base_outs),
                               tto=(GAME_PA_CT %/% 9 + 1),
                               home_team=as.factor(BAT_HOME_ID)

    )
  }


}

#' @export
get_dra_model_frm <-function(metric) {
  if (metric == 'HR') {
    outcome ~ role + bats + throws + (1|pitcher) + (1|catcher) + (1|stadium) + (1|pitcher_hitting) + (1|batter)
  } else if (metric == '3B') {
    outcome ~ (1|batter) + (1|stadium:bats) + (1|Pos_3) + (1|Pos_4) + (1|Pos_7) + (1|Pos_8) + (1|pitcher)
  } else if (metric == '2B') {
    outcome ~ (1|pitcher) + (1|batter) + (1|Pos_4) + (1|Pos_5) + (1|Pos_7) + (1|Pos_8) + (1|stadium:bats)
  } else if (metric == '1B_IF') {
    outcome ~ bats + throws + (1|pitcher) + (1|batter) + (1|Pos_3) + (1|Pos_4) + (1|Pos_5) + (1|Pos_6)
  } else if (metric == '1B_OF') {
    outcome ~ bats + throws + (1|pitcher) + (1|batter) + (1|Pos_5) + (1|Pos_7) + (1|Pos_8) + (1|Pos_9)
  } else if (metric == 'UIBB') {
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|pitcher_hitting) + (1|base_outs) + bats + throws + tto + home_team
  } else if (metric == 'SO') {
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|pitcher_hitting) + (1|base_outs) + bats + throws + tto + home_team
  } else {
    stop(sprintf("unknown metric: %s", metric))
  }
}


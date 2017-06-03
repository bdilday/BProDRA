#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

print("hey ho, let's go")
mods_list <- list()
pitcher_ranef_list <- list()

 for (year in c(2007, 2016)) {
   key <- as.character(year)
  cat(sprintf("year: %d", year))
  mods_list[[key]] <- BProDRA::load_fitted_dra_models(year)
#   pitcher_ranef_list[[key]] <- data.frame(x=1:10)
    }

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


  output$model_label <- renderText({
    year = input$year
    fit_df <- pitcher_ranef_list[[year]]
    sprintf("year: %s month; %s", year, nrow(fit_df))
    # sprintf("year: %s nrow: d", year, nrow(fit_df))
  })

  output$selectUI<-renderUI({
    pit_id_list <- unique(pitcher_ranef_list[[input$year]]$var_id)
    selectInput("nana", "select nana", pit_id_list)
  })

  output$distPlot <- renderPlot({
    year = input$year
    if (! year %in% pitcher_ranef_list) {
      pitcher_ranef_list[[year]] <- BProDRA::extract_pitcher_ranef(mods_list[[year]])
    }

    fit_df <- pitcher_ranef_list[[year]]
    fit_df %>%
      dplyr::mutate(p=exp(value)/(1+exp(value))) %>%
      ggplot(aes(x=model_name, y=p, group=model_name)) +
      geom_boxplot() + theme_minimal() + coord_flip() + labs(title=sprintf("DRA Components: %s", year))

  })



})

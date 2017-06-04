#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(BProDRA)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("BPro DRA Explorer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "year", c(2007, 2016), selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      htmlOutput("selectUI")
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       tableOutput("pitcher_components"),
       tableOutput("model_ranef")
    )
  )
))

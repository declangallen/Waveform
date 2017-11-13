library(shiny)
library(tidyverse)
library(plotly)
library(magrittr)
library(reshape2)
library(lubridate)

fluidPage(
  titlePanel("Waveform Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose Waveform File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv'))
      ),
    mainPanel(
      h4("Waveform Details"),
      verbatimTextOutput("Details"),
      h4("Click and drag to zoom below"),
      plotOutput('plot1', brush = brushOpts(
              id = "plot1_brush",
              resetOnNew = TRUE)),
      h4("Click, double click, hover or click and drag to view P/S potential details below"),
      plotOutput('plot2',
                 click = "plot2_click",
                 dblclick = "plot2_dblclick",
                 hover = "plot2_hover",
                 brush = "plot2_brush"),
      verbatimTextOutput("info")
      )
    )
)

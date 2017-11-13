library(shiny)
library(tidyverse)
library(plotly)
library(magrittr)
library(reshape2)
library(lubridate)

function(input, output) {
  
  output$Details <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    read.table(inFile$datapath,nrows = 4,header = FALSE)
    
    
  })
  
  output$plot1 <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tabledata <- read.table(inFile$datapath,skip=4,header = FALSE) %>%
      set_colnames(c("Reading","On/Off","Delay","Cycle"))%>%
      mutate(Milliseconds = seq(1,3600,1))%>%
      melt(id.vars = c("Milliseconds"), measure.vars=c("Reading","On/Off","Delay","Cycle")) %>%
      arrange(Milliseconds) 
    
    colnames(tabledata)[3] <- c("P/S Potential")
    
    change <- tabledata$Milliseconds[which(!!diff(tabledata$`P/S Potential`[tabledata$variable=="On/Off"]))*4]
    off <- change[seq(1,length(change),2)]
    on <- change[seq(2,length(change),2)]
    
    read <- tabledata$Milliseconds[(tabledata$variable=="Delay"& tabledata$`P/S Potential`==1)]
    
      
      ggplot(tabledata,aes(Milliseconds,`P/S Potential`))+
      geom_line(data=tabledata[tabledata$variable=="Reading",])+
      annotate("rect",fill="grey",alpha=0.5,
                 xmin = off, xmax = on, ymin = -Inf,ymax = Inf)+
      geom_vline(xintercept=as.numeric(read),linetype=2,colour="red",size=1,alpha=0.5)+
      scale_y_reverse()
    
 
      })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$plot2 <- renderPlot({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
   
    lat <- as.numeric(gsub(",","",read.table(inFile$datapath,
                                             skip = 3,nrows = 1,header = FALSE,stringsAsFactors = FALSE)[2]))
    
    long <- as.numeric(read.table(inFile$datapath,
                                  skip = 3,nrows = 1,header = FALSE,stringsAsFactors = FALSE)[3])
    tabledata <- read.table(inFile$datapath,skip=4,header = FALSE) %>%
      set_colnames(c("Reading","On/Off","Delay","Cycle"))%>%
      mutate(Milliseconds = seq(1,3600,1))%>%
      melt(id.vars = c("Milliseconds"), measure.vars=c("Reading","On/Off","Delay","Cycle")) %>%
      arrange(Milliseconds) 
    
    colnames(tabledata)[3] <- "P/S Potential"
    
    change <- tabledata$Milliseconds[which(!!diff(tabledata$`P/S Potential`[tabledata$variable=="On/Off"]))*4]
    off <- change[seq(1,length(change),2)]
    on <- change[seq(2,length(change),2)]
    
    read <- tabledata$Milliseconds[(tabledata$variable=="Delay"& tabledata$`P/S Potential`==1)]
    
    
    ggplot(tabledata,aes(Milliseconds,`P/S Potential`))+
      geom_line(data=tabledata[tabledata$variable=="Reading",])+
      annotate("rect",fill="grey",alpha=0.5,
               xmin = off, xmax = on, ymin = -Inf,ymax = Inf)+
      geom_vline(xintercept=as.numeric(read),linetype=2,colour="red",size=1,alpha=0.5)+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
      scale_y_reverse()
    
    
  })
  

  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0( " y=", round(e$y,4), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0(" ymin=", round(e$ymin,4), " ymax=", round(e$ymax,4))
    }
    
    paste0(
      "Click: ", xy_str(input$plot2_click),
      "Dblclick: ", xy_str(input$plot2_dblclick),
      "Hover: ", xy_str(input$plot2_hover),
      "Box extents: ", xy_range_str(input$plot2_brush)
    )
  })
}

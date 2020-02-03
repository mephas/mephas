


function(input, output) {
source("..tab/func.R")
##########----------##########----------##########

source("server_bio.R", local = TRUE)

source("server_poi.R", local = TRUE)


##########----------##########----------##########

observe({
      if (input$close > 0) stopApp()                             # stop shiny
    })
}



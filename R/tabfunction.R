## All kinds of tab functions for MEPHAS

##' @title tab functions in MEPHAS
##'
##' @export
tabhelp <- function(){
  tabPanel((a("Help Pages Online",
            target = "_blank",
            style = "margin-top:-30px; color:DodgerBlue",
            href = paste0("https://mephas.github.io/helppage/"))))
}


tabquit <- function(){
  tabPanel(
  tags$button(
    id = 'close',
    type = "button",
    class = "btn action-button",
    style = "margin-top:-8px; color:Tomato; background-color: #F8F8F8  ",
    onclick = "setTimeout(function(){window.close();},500);",  # close browser
    "Stop and Quit"))
}

##' @title tab functions in MEPHAS
##'
##' @export
tabof <- function(){
  fluidPage(

  switchInput(
         inputId = "explain_on_off",
         label = "<i class=\"fa fa-book\"></i>", # Explanation in Details
          inline = TRUE,
          onLabel = "Show",
          offLabel = "Hide",
          size = "mini"
          )
  )
}
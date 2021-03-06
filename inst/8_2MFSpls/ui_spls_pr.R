#****************************************************************************************************************************************************spls-pred

sidebarLayout(

sidebarPanel(

h4(tags$b("Prediction")),
p("Prepare model first"),
hr(),

h4(tags$b("Step 3. Test Set Preparation")),
tabsetPanel(

tabPanel("Example data", p(br()),

 h4(tags$b("Data: NKI"))

  ),
tabPanel.upload.pr(file ="newfile.spls", header="newheader.spls", col="newcol.spls", sep="newsep.spls", quote="newquote.spls")

),

hr(),

h4(tags$b("Step 4. If the model and new data are ready, click the blue button to generate prediction results.")),

#actionButton("B.pcr", (tags$b("Show Prediction >>")),class="btn btn-primary",icon=icon("bar-chart-o"))
p(br()),
actionButton("B.spls", (tags$b("Show Prediction >>")),class="btn btn-primary",icon=icon("bar-chart-o")),
p(br()),
p(br()),
hr()

),


mainPanel(

h4(tags$b("Output. Model Results")),

tabsetPanel(
tabPanel("Test Data",p(br()),
DT::DTOutput("newX.spls")
),
tabPanel("Predicted Dependent Variable",p(br()),
DT::DTOutput("pred.lp.spls")
)

)
)
)

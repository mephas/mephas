#****************************************************************************************************************************************************1.t1
sidebarLayout(

sidebarPanel(

  h4(tags$b("Step 1. Data Preparation")),

  p(tags$b("1. Give a name to your data (Required)")),

  tags$textarea(id = "cn", rows = 1, "Age"),p(br()),

  p(tags$b("2. Input data")),

  tabsetPanel(

    tabPanel("Manual Input", p(br()),
    conditionalPanel(
    condition = "input.explain_on_off",
    p(tags$i("Here was the AGE of 144 independent lymph node-positive patients"))
    ),

    p(tags$b("Please follow the example to input your data")),
    p("Data point can be separated by , ; /Enter /Tab /Space"),
    p(tags$b("Data be copied from CSV (one column) and pasted in the box")),   
    tags$textarea(
        id = "x", #p
        rows = 10,
        "50\n42\n50\n43\n47\n47\n38\n45\n31\n41\n48\n47\n38\n44\n36\n42\n42\n45\n49\n44\n32\n46\n50\n38\n43\n40\n42\n46\n41\n46\n48\n48\n36\n43\n44\n47\n40\n41\n48\n41\n45\n45\n47\n37\n43\n43\n49\n45\n41\n50"
        ),

      p("Missing values are input as NAs")
      ),
  tabPanel.upload.num(file ="file", header="header", col="col", sep="sep")
 

    ),

hr(),

  h4(tags$b("Step 2. Specify Parameter")),

  numericInput('mu', HTML("Mean (&#956&#8320) that you want to compare with your data"), 50), #p
  conditionalPanel(
    condition = "input.explain_on_off",
  p(tags$i("The specified parameter is the general age 50"))
  ),

hr(),

  h4(tags$b("Step 3. Choose Hypothesis")),

  p(tags$b("Null hypothesis")),
  HTML("&#956 = &#956&#8320: the population mean (&#956) of your data is &#956&#8320"),

  radioButtons(
    "alt",
    label = "Alternative hypothesis",
    choiceNames = list(
      HTML("&#956 &#8800 &#956&#8320: the population mean of your data is not &#956&#8320"),
      HTML("&#956 < &#956&#8320: the population mean of your data is less than &#956&#8320"),
      HTML("&#956 > &#956&#8320: the population mean of your data is greater than &#956&#8320")
      ),
    choiceValues = list("two.sided", "less", "greater")),
    conditionalPanel(
    condition = "input.explain_on_off",
    p(tags$i("We wanted to know whether the age was 50 or not, so we chose the first alternative hypothesis"))
    )

    ),


mainPanel(

  h4(tags$b("Output 1. Descriptive Results")),

  tabsetPanel(

    tabPanel("Data Preview", p(br()),

      DT::DTOutput("table")
      #shiny::dataTableOutput("table")
      ),

    tabPanel("Basic Descriptives", p(br()),

        DT::DTOutput("bas")#,
      #p(br()),
       # downloadButton("download0", "Download Results")
       ),

    tabPanel("Box-Plot", p(br()),

        plotly::plotlyOutput("bp"),#, click = "plot_click1"

        #verbatimTextOutput("info1"),
          HTML(
          "<b> Explanations </b>
          <ul>
            <li> The band inside the box is the median</li>
            <li> The box measures the difference between 75th and 25th percentiles</li>
            <li> Outliers will be in red, if existing</li>
          </ul>"

          )

      ),

    tabPanel("Mean and SD Plot", p(br()),
plotly::plotlyOutput("meanp")),


    tabPanel("Distribution Plots", p(br()),
HTML(
"<b> Explanations </b>
<ul>
<li> Normal Q–Q Plot: to compare randomly generated, independent standard normal data on the vertical axis to a standard normal population on the horizontal axis. The linearity of the points suggests that the data are normally distributed.</li>
<li> Histogram: to roughly assess the probability distribution of a given variable by depicting the frequencies of observations occurring in certain ranges of values</li>
<li> Density Plot: to estimate the probability density function of the data</li>
</ul>"
),

      p(tags$b("Normal Q–Q plot")),
      plotly::plotlyOutput("makeplot1"),
      p(tags$b("Histogram")),
      plotly::plotlyOutput("makeplot1.2"),
      sliderInput("bin","The number of bins in histogram",min = 0,max = 100,value = 0),
      p("When the number of bins is 0, plot will use the default number of bins"),
      p(tags$b("Density plot")),
      plotly::plotlyOutput("makeplot1.3")

)
),

  hr(),
  h4(tags$b("Output 2. Test Results")),p(br()),
  DT::DTOutput("t.test"),


  HTML(
    "<b> Explanations </b>
    <ul>
    <li> P Value < 0.05, then the population of the data IS significantly different from the specified mean. (Accept the alternative hypothesis)</li>
    <li> P Value >= 0.05, then the population of the data IS NOT significantly different from the specified mean. (Accept the null hypothesis)</li>
    </ul>"
  ),
  conditionalPanel(
  condition = "input.explain_on_off",
  p(tags$i("Because P < 0.05, we concluded that the age of the lymph node-positive population was significantly different from 50 years old. Thus the general age was not 50. If we reset the specified mean to 44, we could get P > 0.05"))
  )
 )

)

#****************************************************************************************************************************************************4.prop.t

 sidebarLayout(
      sidebarPanel(

    h4(tags$b("Step 1. Data Preparation")),
    p(tags$b("Please follow the example to input your data")),

      p(tags$b("1. Give names to group samples ")), 
        tags$textarea(id = "cn4",rows = 5,
        "~20\n20-24\n25-29\n30-34\n34~"
      ),

    p(tags$b("2. Give names to success/event")), 
        tags$textarea(id = "rn4",rows = 2,
        "Cancer\nNo-Cancer"
      ),
    p(br()),

    p("Data pointscan be separated by , ; /Enter /Tab"),
    p(tags$b("3. How many success/event in every group (x)")),
    tags$textarea(id = "x4", rows = 5,
    "320\n1206\n1011\n463\n220"        
    ),

    p(tags$b("4. How many trials/samples totally in every group (n > x)")),     
    tags$textarea(id = "x44", rows = 5,
    "1742\n5638\n3904\n1555\n626"
    ),

    p("Note: No Missing Value"),
    conditionalPanel(
    condition = "input.explain_on_off",

    p(tags$i("In this example, we had 5 age groups of people, as shown in n, and we recorded the number of people who had cancer in x."))
    ),

        hr(),

   h4(tags$b("Step 2. What is the order that you want to test for your samples")),

    p(tags$b("Order of the columns (same length with your sample)")),
    tags$textarea(id = "xs", rows = 5,
        "1\n2\n3\n4\n5"        
    ),
    conditionalPanel(
    condition = "input.explain_on_off",
    p(tags$i("In this case, age groups were in increasing order"))
    ),

    hr(),

    h4(tags$b("Hypothesis")),

   p(tags$b("Null hypothesis")), 
   p("There is no variation in for the sample proportion"),
    
   p(tags$b("Alternative hypothesis")), 
   p("The proportion/rate/probabilities vary with score")   

    ),

    mainPanel(

    h4(tags$b("Output 1. Contingency Table")), p(br()), 

    tabsetPanel(

    tabPanel("Table", p(br()),

        p(tags$b("Data Table")),
        DT::DTOutput("dt4"),

        p(tags$b("Cell-Column %")),
        DT::DTOutput("dt4.2")
        ),

    tabPanel("Percentage Plot", p(br()),

      plotly::plotlyOutput("makeplot4")
      )
    ),

    hr(),

    h4(tags$b("Output 2. Test Results")), p(br()), 

    DT::DTOutput("c.test4"),

     HTML(
    "<b> Explanations </b> 
    <ul> 
    <li> P Value < 0.05, then Case-Control (Row) is significantly associated with grouped Factors (Column) (Accept the alternative hypothesis)</li>
    <li> P Value >= 0.05, then Case-Control (Row) is not associated with grouped Factors (Column). (Accept the null hypothesis)</li>
    </ul>"
  ),
conditionalPanel(
    condition = "input.explain_on_off",
     p(tags$i("In this default setting, we concluded that the proportion of cancer varied among different ages. (P = 0.01)"))
     )

        )
      )
    
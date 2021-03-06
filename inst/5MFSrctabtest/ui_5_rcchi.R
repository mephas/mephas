#****************************************************************************************************************************************************5. r c chi

   sidebarLayout(
      sidebarPanel(

    h4(tags$b("Step 1. Data Preparation")),

  p(tags$b("1. Give names to each category of factor1 shown as column names")),
        tags$textarea(id = "cn5",rows = 5,
        "Smear+\nSmear-Culture+\nSmear-Culture-"
      ),
    p(tags$b("2. Give names to each category of factor2 shown as row names")), 
        tags$textarea(id = "rn5",rows = 5,
        "Penicillin\nSpectinomycin-low\nSpectinomycin-high"
      ),
        p(br()), 

    p(tags$b("3. Input R*C values in row-order")),
      p("Data point can be separated by , ; /Enter /Tab"),
      tags$textarea(id="x5", rows=10, 
      "40\n30\n130\n10\n20\n70\n15\n40\n45"),
      p("Note: No Missing Value"),
conditionalPanel(
    condition = "input.explain_on_off",
    p(tags$i("Row were different drug treatments, and columns were different responses")),
  p(tags$i("Among 200 Penicillin users, 40 got Smear+, 30 got Smear-Culture+, and others were Smear-Culture-.")),
  p(tags$i("Among 100 Spectinomycin-low users, 10 got Smear+, 20 got Smear-Culture+ and others were Smear-Culture-.")),
  p(tags$i("Among 100 Spectinomycin-high users, 15 got Smear+, 40 got Smear-Culture+ and others were Smear-Culture-."))
  ),

        hr(),

    h4(tags$b("Hypothesis")),

   p(tags$b("Null hypothesis")), 
   p("Case-Control (Row) is significantly associated with Grouped Factors (Column)"),
    
   p(tags$b("Alternative hypothesis")), 
   p("Case-Control (Row) has no significant association with Grouped Factors (Column)"),     
conditionalPanel(
    condition = "input.explain_on_off",
    p(tags$i("In this setting,  we wanted to know if there was a relationship between drug treatment and response."))
    )
   

    ),


    mainPanel(

    h4(tags$b("Output 1. Contingency Table")), p(br()), 

    tabsetPanel(

    tabPanel("Table Preview", p(br()),
    

        p(tags$b("R x C Contingency Table with Total Number")),
        DT::DTOutput("dt5"),

        p(tags$b("Expected Value")),
        DT::DTOutput("dt5.0")
        ),
    tabPanel("Percentage Table", p(br()),

        p(tags$b("Cell/Total %")),
        DT::DTOutput("dt5.3"),

        p(tags$b("Cell/Row-Total %")),
        DT::DTOutput("dt5.1"),

        p(tags$b("Cell/Column-Total %")),
        DT::DTOutput("dt5.2")
        ),

    tabPanel("Percentage Plot", p(br()),

      plotly::plotlyOutput("makeplot5")
      )
    ),

    hr(),

    h4(tags$b("Output 2. Test Results")), p(br()), 

    DT::DTOutput("c.test5"),

     HTML(
    "<b> Explanations </b> 
    <ul> 
    <li> P Value < 0.05, then Case-Control (Row) is significantly associated with Grouped Factors (Column) (Accept the alternative hypothesis)</li>
    <li> P Value >= 0.05, then Case-Control (Row) is not associated with Grouped Factors (Column). (Accept the null hypothesis)</li>
    </ul>"
  ),
conditionalPanel(
    condition = "input.explain_on_off",
     p(tags$i("In this default setting, we conclude that there was a significant relationship between drug treatment and response. (P < 0.001)"))
     )

        )
      )
    
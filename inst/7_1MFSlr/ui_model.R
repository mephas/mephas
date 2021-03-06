#****************************************************************************************************************************************************model
sidebarLayout(

sidebarPanel(

tags$head(tags$style("#formula {height: 50px; background: ghostwhite; color: blue;word-wrap: break-word;}")),
tags$head(tags$style("#str {overflow-y:scroll; max-height: 200px; background: white};")),
tags$head(tags$style("#fit {overflow-y:scroll; max-height: 400px; background: white};")),
tags$head(tags$style("#step {overflow-y:scroll;max-height: 400px; background: white};")),

h4(tags$b("Build the Model")),
p("Prepare the data in the previous tab"),
hr(),      

h4(tags$b("Step 1. Choose variables to build the model")),      

uiOutput('y'),    
uiOutput('x'),
#uiOutput('fx'),

radioButtons("intercept", "3. (Optional) Keep or remove intercept / constant term", ##> intercept or not
     choices = c("Remove intercept / constant term" = "-1",
                 "Keep intercept / constant term" = ""),
     selected = ""),

uiOutput('conf'), 
hr(),
h4(tags$b("Step 2. Check the model and generate results")),
tags$b("Valid model example: Y ~ X1 + X2"),
verbatimTextOutput("formula"),
p("'-1' in the formula indicates that the intercept/constant term has been removed"),
hr(),

h4(tags$b("Step 3. If data and model are ready, click the blue button to generate model results.")),
p(br()),
actionButton("B1", (tags$b("Show Results >>")),class="btn btn-primary",icon=icon("bar-chart-o")),
p(br()),
p(br()),
hr()


),

mainPanel(

h4(tags$b("Output 1. Data Preview")),
tabsetPanel(
  tabPanel("Variables Information", br(),
verbatimTextOutput("str")
),
tabPanel("Part of Data", br(),
 p("Please edit data in Data tab"),
DT::DTOutput("Xdata2")
)
),
hr(),

h4(tags$b("Output 2. Model Results")),
#actionButton("B1", h4(tags$b("Click 1: Output 2. Show Model Results / Refresh")),  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

tabsetPanel(

tabPanel("Model Estimation",  br(),
    
HTML(
"
<b> Explanations  </b>
<ul>
<li> For each variable, estimated coefficients (95% confidence interval), T statistic (t = ) for the significance of single variable, and P value (p = ) are given</li>
<li> T test of each variable and P < 0.05 indicates this variable is statistically significant to the model</li>
<li> Observations mean the number of samples</li>
<li> R2 (R<sup>2</sup>) is a goodness-of-fit measure for linear regression models and indicates the percentage of the variance in the dependent variable that the independent variables explain collectively.
Suppose R2 = 0.49. This result implies that 49% of the variability of the dependent variable has been accounted for, and the remaining 51% is still unaccounted for.</li>
<li> Adjusted R2 (adjusted R<sup>2</sup>) is used to compare the goodness-of-fit for regression models that contain different numbers of independent variables.</li>
<li> F statistic (F-Test for overall significance in regression) judges on multiple coefficients taken together at the same time. 
     F=(R^2/(k-1))/(1-R^2)/(n-k); n is sample size; k is number of variable + constant term</li>
</ul>
"
),
verbatimTextOutput("fit")

    ),

tabPanel("Data Fitting",  br(),

    DT::DTOutput("fitdt0")
),

tabPanel("ANOVA",  br(),

HTML(
"<b> Explanations </b>
<ul> 
<li> DF<sub>variable</sub> = 1</li>
<li> DF<sub>residual</sub> = [number of sample values] - [number of variables] -1</li>
<li> MS = SS/DF</li>
<li> F = MS<sub>variable</sub> / MS<sub>residual</sub> </li>
<li> P Value < 0.05:  the variable is significant to the model.</li>
</ul>"
    ),
    p(tags$b("ANOVA Table")),  
    DT::DTOutput("anova")),

tabPanel("AIC-based Selection",  br(),
    HTML(
    "<b> Explanations </b>
  <ul> 
    <li> The Akaike Information Criterion (AIC) is used to performs stepwise model selection. </li>
    <li> Model fits are ranked according to their AIC values, and the model with the lowest AIC value is sometime considered the 'best'. </li>
  </ul>"
    ),

    p(tags$b("Model selection suggested by AIC")),
    verbatimTextOutput("step")

    ),

tabPanel("Diagnostics Plot",   br(),
HTML(
"<b> Explanations </b>
<ul> 
<li> Q-Q normal plot of residuals checks the normality of residuals. The linearity of the points suggests that the data are normally distributed.</li>
<li> Residuals vs fitting plot finds the outliers</li>
</ul>"
),
p(tags$b("1. Q-Q normal plot of residuals")),
plotly::plotlyOutput("p.lm1"),
p(tags$b("2. Residuals vs Fitting plot")),
plotly::plotlyOutput("p.lm2")

    ),
tabPanel("3D Scatter Plot", p(br()),
HTML(
"<b> Explanations </b>
<ul> 
<li> 3D scatter plot shows the relation between dependent variable (Y), and 2 independent variable (X1, X2)
<li> Group variable split the points into groups
</ul>"
),

uiOutput("vx1"),
uiOutput("vx2"),
uiOutput("vgroup"),
plotly::plotlyOutput("p.3dl")
)

)
)
)
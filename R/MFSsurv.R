##'
##' MFSsurv includes
##' (1) K-M estimation and log-rank test for survival probability curves,
##' (2) Cox regression
##' and (3) accelerated failure time model
##'
##' @title MEPHAS: Survival Analysis (Advanced Method)
##'
##' @return shiny interface
##'
##' @import shiny
##' @import ggplot2
##' @import survival 
##' @import survminer
##'
##' @importFrom stargazer stargazer
##' @importFrom utils str write.table
##' @importFrom stats plogis resid
##' @importFrom survAUC predErr AUC.cd AUC.sh AUC.uno AUC.hc
##'
##' @examples
##' #mephas::MFSsurv()
##' ## or,
##' # library(mephas)
##' # MFSsurv()

##' @export
MFSsurv <- function(){

requireNamespace("shiny")
##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########
ui <- tagList(

navbarPage(

title = "Survival Analysis",

##########----------##########----------##########

tabPanel("Data",

headerPanel("Data Preparation"),

HTML(
"
<h4><b> 1. What you can do on this page  </b></h4>
<ul>
<li> To upload data file, preview data set, and check the correctness of data input 
<li> To pre-process some variables (when necessary) for building the model
<li> To get the basic descriptive statistics and plots of the variables
<li> To prepare the survival object as 'dependent variable' for building model
</ul>

<h4><b> 2. About your data (training set)</b></h4>

<ul>
<li> Your data need to include <b>one survival time variable and one 1/0 censoring variable</b> and <b> at least one independent variables (denoted as X)</b>
<li> Your data need to have more rows than columns
<li> Do not mix character and numbers in the same column 
<li> The data used to build model is called <b>training set</b>
</ul> 

<i><h4>Case Example 1: Diabetes data</h4>
Suppose in a study, we got some observations from a trial of laser coagulation for the treatment of diabetic retinopathy. 
Each patient had one eye randomized to laser treatment and the other eye received no treatment. 
For each eye, the event of interest was the time from initiation of treatment to the time when visual acuity dropped below 5/200 two visits in a row. 
Thus there is a built-in lag time of approximately 6 months (visits were every 3 months). 
Survival times in this dataset are therefore the actual time to blindness in months, minus the minimum possible time to event (6.5 months). 
Censor status of 0= censored; 1 = visual loss. Treatment: 0 = no treatment, 1= laser. Age is age at diagnosis.


<h4>Case Example 2: Nki70 data</h4>
Suppose we wanted to explore 100 lymph node positive breast cancer patients on metastasis-free survival. 
Data contained 5 clinical risk factors: (1) Diam: diameter of the tumor; (2) N: number of affected lymph nodes; (3) ER: estrogen receptor status; (4) Grade: grade of the tumor; and (5) Age: Patient age at diagnosis (years); 
and gene expression measurements of 70 genes found to be prognostic for metastasis-free survival in an earlier study. 
Time variable is metastasis-free follow-up time (months). Censoring indicator variable: 1 = metastasis or death; 0 = censored. 

</h4></i>

<h4> Please follow the <b>Steps</b>, and <b>Outputs</b> will give real-time analytical results. After getting data ready, please find the model in the next tabs.</h4>
"
),

hr(),
#source("0data_ui.R", local=TRUE, encoding="UTF-8")$value,
#****************************************************************************************************************************************************

sidebarLayout(

sidebarPanel(

  tags$style(type='text/css', '#surv {background-color: rgba(0,0,255,0.10); color: blue;}'),
  tags$head(tags$style("#fsum {overflow-y:scroll; max-height: 200px; background: white};")),
  tags$head(tags$style("#strnum {overflow-y:scroll; max-height: 200px; background: white};")),
  tags$head(tags$style("#strfac {overflow-y:scroll; max-height: 200px; background: white};")),
  tags$head(tags$style("#kmat1 {overflow-y:scroll; max-height: 200px; background: white};")),

selectInput("edata", h4(tags$b("Use example data (training set)")), 
        choices =  c("Diabetes","NKI70"), 
        selected = "Diabetes"),

hr(),

h4(tags$b("Use my own data (training set)")),
p("We suggested putting the survival time variable and censoring variable in the left side of all independent variables (X) "),

h4(tags$b("Step 1. Upload Data File")), 

fileInput('file', "1. Choose CSV/TXT file",
          accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),

p(tags$b("2. Show 1st row as column names?")),
checkboxInput("header", "Yes", TRUE),

p(tags$b("3. Use 1st column as row names? (No duplicates)")),
checkboxInput("col", "Yes", TRUE),

radioButtons("sep", "4. Which separator for data?",
  choiceNames = list(
    HTML("Comma (,): CSV often use this"),
    HTML("One Tab (->|): TXT often use this"),
    HTML("Semicolon (;)"),
    HTML("One Space (_)")
    ),
  choiceValues = list(",", "\t", ";", " ")
  ),

radioButtons("quote", "5. Which quote for characters?",
choices = c("None" = "",
           "Double Quote" = '"',
           "Single Quote" = "'"),
selected = '"'),

p("Correct separator and quote ensures data input successfully"),

a(tags$i("Find some example data here"),href = "https://github.com/mephas/datasets"),

hr(),

h4(tags$b("Step 2. Create a Survival Object")), 

#p(tags$b("1. Choose a Time Variable")),   

uiOutput('c'),

radioButtons("time", "2. Set Survival Time", selected="A",
  choiceNames = list(
    HTML("Choice 1. <b>Right-censored time</b>: needs time duration / follow-up"),
    HTML("Choice 2. <b>Left-truncated right-censored time</b>: needs start and end time points")
    ),
  choiceValues = list("A", "B" )
  ),

tabsetPanel(
  tabPanel("Right-censored", br(),
    uiOutput('t')
    ),
  tabPanel("Left-truncated Right-censored", br(),
    uiOutput('t1'),
    uiOutput('t2')
    )
  ),
hr(),
h4(tags$b("Step 3. Check the Survival Object")),      

verbatimTextOutput("surv", placeholder = TRUE),


hr(),


h4(tags$b("(Optional) Change the types of some variable?")),

#p(tags$b("Choice 1. Change Numeric Variables (Numbers) into Categorical Variable (Factors)")), 

uiOutput("factor1"),

#p(tags$b("Choice 2. Change Categorical Variable (Numeric Factors) into Numeric Variables (Numbers)")),

uiOutput("factor2"),

h4(tags$b("(Optional) Change the referential level for categorical variable?")), 

uiOutput("lvl"),

p(tags$b("2. Input the referential level, each line for one variable")),

tags$textarea(id='ref', column=40, "")


),


mainPanel(
h4(tags$b("Output 1. Data Information")),
p(tags$b("Data Preview")), 
p(br()),
DT::DTOutput("Xdata"),

p(tags$b("1. Numeric variable information list")),
verbatimTextOutput("strnum"),


p(tags$b("2. Categorical variable information list")),
verbatimTextOutput("strfac"),


hr(),   
h4(tags$b("Output 2. Basic Descriptives")),

tabsetPanel(

tabPanel("Basic Descriptives", p(br()),

p(tags$b("1. For numeric variable")),

DT::DTOutput("sum"),

p(tags$b("2. For categorical variable")),
verbatimTextOutput("fsum"),


#downloadButton("download1", "Download Results (Continuous variables)"),
downloadButton("download2", "Download Results (Categorical variables)")

),

tabPanel("Survival Curves",  p(br()),
  radioButtons("fun1", "Choose one plot", 
  choiceNames = list(
    HTML("1. Survival Probability"),
    HTML("2. Cumulative Events"),
    HTML("3. Cumulative Hazard")
    ),
  choiceValues = list("pct", "event","cumhaz")
  ),
plotOutput("km.a", width = "80%"),
verbatimTextOutput("kmat1")
     ),

tabPanel("Life Table",  p(br()),
  #p(tags$b("For all samples")),
DT::DTOutput("kmat")
#tags$head(tags$style("#kmat {overflow-y:scroll; max-height: 400px; background: white};"))
     ),

tabPanel("Histogram", p(br()),

p("This is to show the distribution of any numeric variable"),
uiOutput('hx'),
p(tags$b("Histogram")),
plotOutput("p2", width = "80%"),
sliderInput("bin", "The number of bins in the histogram", min = 0, max = 100, value = 0),
p("When the number of bins is 0, plot will use the default number of bins "),
p(tags$b("Density plot")),
plotOutput("p21", width = "80%"))

)

)

),
hr()


),

##########----------##########----------##########
tabPanel("Non-parametric Model",

headerPanel("Kaplan-Meier Estimator and Log-rank Test"),
HTML(
"
<p> <b>Kaplan&#8211;Meier estimator</b>, also known as the product limit estimator, is a non-parametric statistic used to estimate the survival function from lifetime data. </p>
<p> <b>Log-rank test</b> is a hypothesis test to compare the survival distributions of two samples. It compares estimates of the hazard functions of the two groups at each observed event time.

<h4><b> 1. What you can do on this page  </b></h4>
<ul>
<li> To get Kaplan-Meier survival probability estimate
<li> To get Kaplan-Meier survival curves, cumulative events distribution curves, and cumulative hazard curves by group variable
<li> To conduct log-rank test to compare the survival curves from 2 groups
<li> To conduct pairwise log-rank test to compare the survival curves from more than two groups
</ul>

<h4><b> 2. About your data </b></h4>

<ul>
<li> The independent variable is categorical
<li> Please prepare the survival object in the Data tab
</ul> 

<h4> Please follow the <b>Steps</b> to build the model, and click <b>Outputs</b> to get analytical results.</h4>
"
),

hr(),
#source("1km_ui.R", local=TRUE, encoding="UTF-8")$value
#****************************************************************************************************************************************************km

sidebarLayout(


sidebarPanel(

tags$head(tags$style("#str {overflow-y:scroll; max-height: 350px; background: white};")),
tags$head(tags$style("#kmt {overflow-y:scroll; max-height: 350px; background: white};")),
tags$head(tags$style("#kmt1 {overflow-y:scroll; max-height: 350px; background: white};")),
tags$head(tags$style("#kmlr {overflow-y:scroll; max-height: 350px; background: white};")),

h4("Example data is upload in Data tab"),      

h4(tags$b("Choose group variable to build the model")),  

p(tags$b("1. Check Surv(time, event), survival object, in the Data Tab")), 

uiOutput('g'),
tags$i("In the example of Diabetes data, we chose 'laser' as categorical group variable. 
  That is to explore if the survival curves in two laser groups were different. "),

hr(),

h4(tags$b("Log-rank Test")),      

p(tags$b("Null hypothesis")),
p("Two groups have identical hazard functions"),

radioButtons("rho", "Choose Log-rank Test Method", selected=1,
  choiceNames = list(
    HTML("1. Log-rank test"),
    HTML("2. Peto & Peto modification of the Gehan-Wilcoxon test")
    ),
  choiceValues = list(1, 2)
  ),
p("See methods explanations in the Output 2. Log-rank Test tab."),
hr(),

h4(tags$b("Pairwise Log-rank Test")),      

p(tags$b("Null hypothesis")),
p("Two groups have identical hazard functions"),

radioButtons("rho2", "1. Choose Log-rank Test Method)", selected=1,
  choiceNames = list(
    HTML("1. Log-rank test"),
    HTML("2. Peto & Peto modification of the Gehan-Wilcoxon test")
    ),
  choiceValues = list(1, 2)
  ),
radioButtons("pm", 
  "2. Choose method to adjust P value", selected="BH",
  choiceNames = list(
    HTML("Bonferroni"),
    HTML("Bonferroni-Holm: often used"),
    #HTML("Bonferroni-Hochberg"),
    #HTML("Bonferroni-Hommel"),
    HTML("False Discovery Rate-BH"),
    HTML("False Discovery Rate-BY")
    ),
  choiceValues = list("B", "BH", "FDR", "BY")
  ),
p("See methods explanations in the Output 2. Pairwise Log-rank Test tab.")

#tags$style(type='text/css', '#km {background-color: rgba(0,0,255,0.10); color: blue;}'),
#verbatimTextOutput("km", placeholder = TRUE),

),

mainPanel(

h4(tags$b("Output 1. Data Preview")),
 tabsetPanel(
 tabPanel("Browse",p(br()),
 p("This only shows the first several lines, please check full data in the 1st tab"),
 DT::DTOutput("Xdata2")
 ),
 tabPanel("Variables information",p(br()),
 verbatimTextOutput("str")
 
 )
 ),
 hr(),
 
# #h4(tags$b("Output 2. Model Results")),
#actionButton("B1", h4(tags$b("Click 1: Output 2. Show Model Results / Refresh")),  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
h4(tags$b("Output 2. Estimate and Test Results")),
p(br()),
tabsetPanel(
tabPanel("Kaplan-Meier Survival Probability",  p(br()),
  p(tags$b("Kaplan-Meier survival probability by group")),
    verbatimTextOutput("kmt")
     ),
tabPanel("Kaplan-Meier Plot by Group",  p(br()),
    radioButtons("fun2", "Which plot do you want to see?", 
  choiceNames = list(
    HTML("1. Survival Probability"),
    HTML("2. Cumulative Events"),
    HTML("3. Cumulative Hazard")
    ),
  choiceValues = list("pct", "event","cumhaz")
  ),
    plotOutput("km.p", width = "80%"),
     verbatimTextOutput("kmt1")
     ),
tabPanel("Log-Rank Test",  p(br()),
       HTML("
<b> Explanations </b>
<p>This implements the G-rho family of Harrington and Fleming (1982), with weights on each death of S(t)<sup>rho</sup>, where S is the Kaplan-Meier estimate of survival.</p>
<ul>
<li>rho = 0: log-rank or Mantel-Haenszel test
<li>rho = 1: Peto & Peto modification of the Gehan-Wilcoxon test.
<li> p < 0.05 indicates the curves are significantly different in the survival probabilities
<li> p >= 0.05 indicates the curves are NOT significantly different in the survival probabilities

</ul>"),

p(tags$b("Log-rank Test Result")),
    verbatimTextOutput("kmlr")

     ),

tabPanel("Pairwise Log-Rank Test",  p(br()),


     HTML(
  "<b> Explanations </b>
  <p>This implements the G-rho family of Harrington and Fleming (1982), with weights on each death of S(t)<sup>rho</sup>, where S is the Kaplan-Meier estimate of survival.</p>
  <ul> 
    <li><b>rho = 0:</b> log-rank or Mantel-Haenszel test
    <li><b>rho = 1:</b> Peto & Peto modification of the Gehan-Wilcoxon test.
    <li> <b>Bonferroni</b> correction is a generic but very conservative approach
    <li> <b>Bonferroni-Holm</b> is less conservative and uniformly more powerful than Bonferroni
    <li> <b>False Discovery Rate-BH</b> is more powerful than the others, developed by Benjamini and Hochberg
    <li> <b>False Discovery Rate-BY</b> is more powerful than the others, developed by Benjamini and Yekutieli
    <li> p < 0.05 indicates the curves are significantly different in the survival probabilities
    <li> p >= 0.05 indicates the curves are NOT significantly different in the survival probabilities
  </ul>"
    ),
     p(tags$b("Pairwise Log-rank Test P Value Table")),

    DT::DTOutput("PLR")
     )
)

)
),
hr()
), ## tabPanel

##########----------##########----------##########

tabPanel("Semi-Parametric Model",

headerPanel("Cox Regression"),
HTML(
"
<p><b> Cox Regression</b>, also known as Cox proportional hazard regression assumes that if the proportional hazards assumption holds (or, is assumed to hold) then it is possible to estimate the effect parameter(s) without any consideration of the hazard function.
Cox regression assumes that the effects of the predictor variables upon survival are constant over time and are additive in one scale.</p>
<h4><b> 1. What you can do on this page  </b></h4>
<ul>
<li> To build a Cox regression model
<li> To get the estimates of the model, such as (1) estimate of coefficient, (2) predictions from the training data, (3)residuals, 
(4) the adjusted survival curves, (5) proportional hazard test, and (6) diagnostic plot
</ul>

<h4><b> 2. About your data (training set) </b></h4>

<ul>
<li> Please prepare the data in the Data tab
<li> Please prepare the survival object in the Data tab
</ul> 

<h4> Please follow the <b>Steps</b> to build the model, and click <b>Outputs</b> to get analytical results.</h4>
"
),

hr(),
#source("3cox_ui.R", local=TRUE, encoding="UTF-8")$value
#****************************************************************************************************************************************************cox-pred

sidebarLayout(

sidebarPanel(

tags$head(tags$style("#cox_form {height: 100px; background: ghostwhite; color: blue;word-wrap: break-word;}")),
tags$head(tags$style("#str4 {overflow-y:scroll; height: 350px; background: white};")),
tags$head(tags$style("#fitcx {overflow-y:scroll; height: 400px; background: white};")),
tags$head(tags$style("#fitcx2 {overflow-y:scroll; height: 400px; background: white};")),
tags$head(tags$style("#zph {overflow-y:scroll; height: 150px; background: white};")),


h4("Example data is upload in Data tab"),  


h4(tags$b("Step 1. Choose some variables to build the model")),      

p(tags$b("1. Check Surv(time, event), survival object, in the Data Tab")), 

uiOutput('var.cx'),
     
radioButtons("tie", "3. (Optional) Choose Method for Ties Handling",selected="breslow",
  choiceNames = list(
    
    HTML("1. Efron method: more accurate if there are a large number of ties"),
    HTML("2. Breslow approximation: the easiest to program and the first option coded for almost all computer routines"),
    HTML("3. Exact partial likelihood method: the Cox partial likelihood is equivalent to that for matched logistic regression")
    ),
  choiceValues = list("efron","breslow","exact")
  ),

radioButtons("effect.cx", "4. (Optional) Add random effect term (the effect of heterogeneity)",
     choices = c(
      "None" = "",
      "Strata: identifies stratification variable (categorical, such as disease subtype and enrolling institutes)" = "Strata",
      "Cluster: identifies correlated groups of observations (such as multiple events per subject)" = "Cluster",
      "Gamma Frailty: allows one to add a simple gamma distributed random effects term " = "Gamma Frailty",
      "Gaussian Frailty: allows one to add a simple Gaussian distributed random effects term" = "Gaussian Frailty"
                 ),
     selected = ""),
p("Frailty: individuals have different frailties, and those who most frail will die earlier than others"),
p("Cluster model is also called marginal model. It estimates the population averaged relative risk due to the independent variable.
  Frailty model estimates the relative risk within the random effect variable"),
uiOutput('fx.cx'),
tags$i("In the example of Diabetes data: 'eye' could be used as random effect of strata;
  'id' can be used as random effect variable of cluster. " ),
p(br()),


p(tags$b("5 (Optional). Add interaction term between categorical variables")),

p('Please input: + var1:var2'), 
tags$textarea(id='conf.cx', " " ), 
hr(),

h4(tags$b("Step 2. Check Cox Model")),
verbatimTextOutput("cox_form", placeholder = TRUE),
p("'-1' in the formula indicates that intercept / constant term has been removed")
),

mainPanel(

h4(tags$b("Output 1. Data Preview")),
 tabsetPanel(
 tabPanel("Browse",p(br()),
 p("This only shows the first several lines, please check full data in the 1st tab"),
 DT::DTOutput("Xdata4")
 ),
 tabPanel("Variables information",p(br()),
 verbatimTextOutput("str4")
 )
 ),
 hr(),
 
actionButton("B2", h4(tags$b("Click 1: Output 2. Show Model Results / Refresh")),  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
p(br()),
tabsetPanel(

tabPanel("Model Estimation", br(),
HTML(
"
<b> Explanations  </b>
<ul>
<li> For each variable, estimated coefficients (coef), statistic for the significance of single variable, and P value are given.
<li> The column marked 'z' gives the Wald statistic value. It corresponds to the ratio of each regression coefficient to its standard error (z = coef/se(coef)).The Wald statistic evaluates, whether the beta coefficient of a given variable is statistically significantly different from 0.
<li> The coefficients relate to hazard; a positive coefficient indicates a worse prognosis and a negative coefficient indicates a protective effect of the variable with which it is associated.
<li> exp(coef) = hazard ratio (HR). HR = 1: No effect; HR < 1: Reduction in the hazard; HR > 1: Increase in Hazard
<li> The output also gives upper and lower 95% confidence intervals for the hazard ratio (exp(coef)), 
<li> The likelihood-ratio test, Wald test, and score log-rank statistics give global statistical significance of the model. These three methods are asymptotically equivalent. For large enough N, they will give similar results. For small N, they may differ somewhat. The Likelihood ratio test has better behavior for small sample sizes, so it is generally preferred.
</ul>
"
),
verbatimTextOutput("fitcx")

),

tabPanel("Data Fitting", p(br()),
    p(tags$b("Fitting values and residuals from the existed data")),
    DT::DTOutput("fit.cox")
),

tabPanel("Survival Curve", p(br()),
  HTML(
     "
<b> Explanations  </b>
<ul>
<li> this plot is to present expected survival curves calculated based on Cox model separately for subpopulations / strata
<li> If there is no strata() component then only a single curve will be plotted - average for the whole population
</ul>
"
),
p(tags$b("The adjusted survival curves from Cox regression")),
 plotOutput("splot", width = "80%")

),

tabPanel("Proportional Hazards Test", br(),

HTML(
"
<b> Explanations  </b>
<ul>
<li> Schoenfeld residuals are used to check the proportional hazards assumption
<li> Schoenfeld residuals are independent of time. A plot that shows a non-random pattern against time is evidence of violation of the PH assumption
<li> If the test is not statistically significant (p>0.05) for each of the independent variable, we can assume the proportional hazards
</ul>
"
),
numericInput("num", HTML("Choose N'th variable"), value = 1, min = 1, step=1),

plotOutput("zphplot", width = "80%"),

DT::DTOutput("zph")



),



tabPanel("Diagnostic Plot", p(br()),
 
HTML(
     "
<p><b> Explanations  </b></p>
<b>Martingale residuals</b> against continuous independent variable is a common approach used to detect nonlinearity. For a given continuous covariate, patterns in the plot may suggest that the variable is not properly fit.
Martingale residuals may present any value in the range (-INF, +1):
<ul>
<li>A value of martingale residuals near 1 represents individuals that 'died too soon',
<li>Large negative values correspond to individuals that 'lived too long'.
</ul>

<b>Deviance residual</b> is a normalized transform of the martingale residual. These residuals should be roughly symmetrically distributed about zero with a standard deviation of 1.
<ul>
<li>Positive values correspond to individuals that 'died too soon' compared to expected survival times.
<li>Negative values correspond to individual that 'lived too long'.
<li>Very large or small values are outliers, which are poorly predicted by the model.
</ul>

<b>Cox-Snell residuals</b> are used to check for overall goodness of fit in survival models.
<ul>
<li> Cox-Snell residuals are equal to the -log(survival probability) for each observation
<li> If the model fits the data well, Cox-Snell residuals should behave like a sample from an exponential distribution with a mean of 1
<li> If the residuals act like a sample from a unit exponential distribution, they should lie along the 45-degree diagonal line.
</ul>

<p>The residuals can be found in Data Fitting tab.<p>
<p>Red points are those who 'died soon'; black points are whose who 'lived long'<p>
"
),

p(tags$b("1. Martingale residuals plot against continuous independent variable")), 

uiOutput('var.mr'),
plotOutput("diaplot1", width = "80%"),

#p(tags$b("2. Martingale residuals plot against observation id")), 
# plotOutput("diaplot1.2", width = "80%"),

 p(tags$b("2. Deviance residuals plot by observational id")),
 plotOutput("diaplot2", width = "80%"),

 p(tags$b("3. Cox-Snell residuals plot")),
 plotOutput("csplot.cx", width = "80%")
)

)
)
),
hr()
), ## tabPanel

##########----------##########----------##########

tabPanel("Prediction1",

headerPanel("Prediction after Cox Regression"),
HTML(
"

<h4><b> 1. What you can do on this page  </b></h4>
<ul>
<li> To upload new data and get the prediction
<li> To get the evaluation if new data contains new dependent variable
<li> To get Brier Score and time-dependent AUC
</ul>

<h4><b> 2. About your data (test set)</b></h4>

<ul>
<li> New data cover all the independent variables used in the model
<li> New data not used to build the model is called <b>test set</b>
</ul> 

<h4> Please follow the <b>Steps</b> to build the model, and click <b>Outputs</b> to get analytical results.</h4>
"
),

hr(),
#source("3pr_ui.R", local=TRUE, encoding="UTF-8")$value
#****************************************************************************************************************************************************cox^pred

sidebarLayout(

sidebarPanel(

h4("Prediction is based on the accomplishment of model"),      

h4(tags$b("Step 1. Upload New Data File")),      

p("New data should include all the variables in the model"),
fileInput('newfile2', "Choose CSV/TXT file",
          accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
#helpText("The columns of X are not suggested greater than 500"),
# Input: Checkbox if file has header ----
p(tags$b("2. Show 1st row as column names?")),
checkboxInput("newheader2", "Yes", TRUE),

p(tags$b("3. Use 1st column as row names? (No duplicates)")),
checkboxInput("newcol2", "Yes", TRUE),

     # Input: Select separator ----
radioButtons("newsep2", "4. Which separator for data?",
  choiceNames = list(
    HTML("Comma (,): CSV often use this"),
    HTML("One Tab (->|): TXT often use this"),
    HTML("Semicolon (;)"),
    HTML("One Space (_)")
    ),
  choiceValues = list(",", "\t", ";", " ")
  ),

radioButtons("newquote2", "5. Which quote for characters?",
choices = c("None" = "",
           "Double Quote" = '"',
           "Single Quote" = "'"),
selected = '"'),

p("Correct separator and quote ensures data input successfully")
),


mainPanel(

actionButton("B2.1", h4(tags$b("Click 2: Output. Prediction Results / Refresh, given model and new data are ready. ")), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
p(br()),
tabsetPanel(
tabPanel("Prediction Table",p(br()),
DT::DTOutput("pred2")
),

tabPanel("Brier Score",p(br()),
HTML(
"
<p>Brier score is used to evaluate the accuracy of a predicted survival function at given time series. 
It represents the average squared distances between the observed survival status and the predicted survival probability and is always a number between 0 and 1, 
with 0 being the best possible value.<p>

<p>The Integrated Brier Score (IBS) provides an overall calculation of the model performance at all available times.<p>
"
),
numericInput("ss", HTML("Set time series: start point"), value = 1, min = 0),
numericInput("ee", HTML("Set time series:end point"), value = 10, min = 1),
numericInput("by", HTML("Set time series: sequence"), value = 1, min = 0),

p(tags$b("Brier score at given time")),
plotOutput("bsplot", width = "80%"),
DT::DTOutput("bstab")

),

tabPanel("AUC",p(br()),
HTML(
" 
<p><b> Explanations  </b></p>
AUC here is time-dependent AUC, which gives AUC at given time series.
<ul>
<li>Chambless and Diao:  assumed that lp and lpnew are the predictors of a Cox proportional hazards model. 
[Chambless, L. E. and G. Diao (2006). Estimation of time-dependent area under the ROC curve for long-term risk prediction. Statistics in Medicine 25, 3474&#8211;3486.

<li>Hung and Chiang: assumed that there is a one-to-one relationship between the predictor and the expected survival times conditional on the predictor. 
[Hung, H. and C.-T. Chiang (2010). Estimation methods for time-dependent AUC models with survival data. Canadian Journal of Statistics 38, 8&#8211;26.]

<li>Song and Zhou: in this method, the estimators remain valid even if the censoring times depend on the values of the predictors.
[Song, X. and X.-H. Zhou (2008). A semiparametric approach for the covariate specific ROC curve with survival outcome. Statistica Sinica 18, 947&#8211;965.]


<li>Uno et al.: are based on inverse-probability-of-censoring weights and do not assume a specific working model for deriving the predictor lpnew. 
It is assumed that there is a one-to-one relationship between the predictor and the expected survival times conditional on the predictor. 
[Uno, H., T. Cai, L. Tian, and L. J. Wei (2007). Evaluating prediction rules for t-year survivors with censored regression models. Journal of the American Statistical Association 102, 527&#8211;537.]
</ul>

"
),

numericInput("ss1", HTML("Set time series: start point"), value = 1, min = 0),
numericInput("ee1", HTML("Set time series: end point"), value = 10, min = 1),
numericInput("by1", HTML("Set time series sequence"), value = 1, min = 0),

tags$i("The example time series: 1, 2, 3, ...,10"),

radioButtons("auc", "Choose one AUC estimator",
  choiceNames = list(
    HTML("Chambless and Diao"),
    HTML("Hung and Chiang"),
    HTML("Song and Zhou"),
    HTML("Uno et al.")
    ),
  choiceValues = list("a", "b", "c", "d")
  ),
p(tags$b("Time dependent AUC at given time")),
plotOutput("aucplot", width = "80%"),
DT::DTOutput("auctab")

)
)
) 
),
hr()
),

##########----------##########----------##########

tabPanel("Parametric Model",

headerPanel("Accelerated Failure Time (AFT) Model"),
HTML(
"
<p><b>Accelerated failure time (AFT) model</b> is a parametric model assumes that the effect of a covariate is to accelerate or decelerate the life course of a disease by some constant.</p>

<h4><b> 1. What you can do on this page  </b></h4>
<ul>
<li> To build AFT model
<li> To get the estimates of the model, such as coefficients of parameters, residuals, and diagnostic plot
<li> To get fitted values which are predicted from the training data
</ul>

<h4><b> 2. About your data </b></h4>

<ul>
<li> Please prepare the data in the Data tab
</ul> 

<h4> Please follow the <b>Steps</b> to build the model, and click <b>Outputs</b> to get analytical results.</h4>
"
),

hr(),
#source("2aft_ui.R", local=TRUE, encoding="UTF-8")$value
#****************************************************************************************************************************************************aft


sidebarLayout(

sidebarPanel(

tags$head(tags$style("#aft_form {height: 100px; background: ghostwhite; color: blue;word-wrap: break-word;}")),
tags$head(tags$style("#str3 {overflow-y:scroll; height: 350px; background: white};")),
tags$head(tags$style("#fit {overflow-y:scroll; height: 400px; background: white};")),
#tags$head(tags$style("#fit2 {overflow-y:scroll; height: 400px; background: white};")),
#tags$head(tags$style("#step {overflow-y:scroll;height: 400px; background: white};")),


h4("Example data is upload in Data tab"),      


h4(tags$b("Step 1. Choose independent variables to build the model")),    

p(tags$b("1. Check Surv(time, event), survival object, in the Data Tab")), 

uiOutput('var'),

radioButtons("dist", "3. Choose AFT Model",
  choiceNames = list(
    HTML("1. Log-normal regression model"),
   # HTML("2. Extreme regression model"),
    HTML("2. Weibull regression model"),
    HTML("3. Exponential regression model"),  
    HTML("4. Log-logistic regression model")
    ),
  choiceValues = list("lognormal","weibull", "exponential","loglogistic")
  ),

radioButtons("intercept", "4. (Optional) Keep or remove intercept / constant term", ##> intercept or not
     choices = c("Remove intercept / constant" = "-1",
                 "Keep intercept / constant term" = ""),
     selected = ""),

radioButtons("effect", "5. (Optional) Add random effect term (the effect of heterogeneity)",
     choices = c(
      "None" = "",
      "Strata: identifies stratification variable (categorical, such as disease subtype and enrolling institutes)" = "Strata",
      "Cluster: identifies correlated groups of observations (such as multiple events per subject)" = "Cluster"
      #"Gamma Frailty: allows one to add a simple gamma distributed random effects term" = "Gamma Frailty",
      #"Gaussian Frailty: allows one to add a simple Gaussian distributed random effects term" = "Gaussian Frailty"
                 ),
     selected = ""),

uiOutput('fx.c'),
tags$i("In the example of Diabetes data: 'eye' could be used as random effect of strata;
  'id' can be used as random effect variable of cluster. " ),
  p(br()),

p(tags$b("6. (Optional) Add interaction term between categorical variables")),

p('Please input: + var1:var2'), 
tags$textarea(id='conf', " " ), 
hr(),

h4(tags$b("Step 2. Check AFT Model")),
verbatimTextOutput("aft_form", placeholder = TRUE),
p("'-1' in the formula indicates that intercept / constant term has been removed")
),

mainPanel(

h4(tags$b("Output 1. Data Preview")),
 tabsetPanel(
 tabPanel("Browse",p(br()),
 p("This only shows the first several lines, please check full data in the 1st tab"),
 DT::DTOutput("Xdata3")
 ),
 tabPanel("Variables information",p(br()),
 verbatimTextOutput("str3")
 )
 ),
 hr(),
 
actionButton("B1", h4(tags$b("Click 1: Output 2. Show Model Results / Refresh")),  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
p(br()),
tabsetPanel(

tabPanel("Model Estimation", br(),
HTML(
"
<b> Explanations  </b>
<ul>
<li> For each variable, estimated coefficients (Value), statistic for the significance of single variable, and p value are given.
<li> The column marked 'z' gives the Wald statistic value. It corresponds to the ratio of each regression coefficient to its standard error (z = coef/se(coef)).The Wald statistic evaluates, whether the beta coefficient of a given variable is statistically significantly different from 0.
<li> The coefficients relate to hazard; a positive coefficient indicates a worse prognosis and a negative coefficient indicates a protective effect of the variable with which it is associated.
<li> exp(Value) = hazard ratio (HR). HR = 1: No effect; HR < 1: Reduction in the hazard; HR > 1: Increase in Hazard
<li> Scale and Log(scale) are the estimated parameters in the error term of AFT model
<li> The log-likelihood is given in the model. When maximum likelihood estimation is used to generate the log-likelihoods, then the closer that the log-likelihood(LL) is to zero, the better is the model fit.
</ul>
"
),
verbatimTextOutput("fit")

),

tabPanel("Data Fitting", p(br()),

    p(tags$b("Fitting values and residuals from the existed data")),
    DT::DTOutput("fit.aft")
),

tabPanel("Diagnostics Plot", p(br()),

HTML(
     "
<p><b> Explanations  </b></p>
<b>Martingale residuals</b> against continuous independent variable is a common approach used to detect nonlinearity. For a given continuous covariate, patterns in the plot may suggest that the variable is not properly fit.
Martingale residuals may present any value in the range (-INF, +1):
<ul>
<li>A value of martingale residuals near 1 represents individuals that 'died too soon',
<li>Large negative values correspond to individuals that 'lived too long'.
</ul>

<b>Deviance residual</b> is a normalized transform of the martingale residual. These residuals should be roughly symmetrically distributed about zero with a standard deviation of 1.
<ul>
<li>Positive values correspond to individuals that 'died too soon' compared to expected survival times.
<li>Negative values correspond to individual that 'lived too long'.
<li>Very large or small values are outliers, which are poorly predicted by the model.
</ul>

<b>Cox-Snell residuals</b> are used to check for overall goodness of fit in survival models.
<ul>
<li> Cox-Snell residuals are equal to the -log(survival probability) for each observation
<li> If the model fits the data well, Cox-Snell residuals should behave like a sample from an exponential distribution with a mean of 1
<li> If the residuals act like a sample from a unit exponential distribution, they should lie along the 45-degree diagonal line.
</ul>

<p>The residuals can be found in Data Fitting tab.<p>
<p>Red points are those who 'died soon'; black points are whose who 'lived long'<p>

"
),

p(tags$b("1. Martingale residuals plot against continuous independent variable")), 
uiOutput('var.mr2'),
plotOutput("mrplot", width = "80%"),

p(tags$b("2. Deviance residuals plot by observational id")),
plotOutput("deplot", width = "80%"),

p(tags$b("3. Cox-Snell residuals plot")),
plotOutput("csplot", width = "80%")

)

)
)
),
hr()
), ## tabPanel

##########----------##########----------##########

tabPanel("Prediction2",

headerPanel("Prediction after Accelerated Failure Time (AFT) model"),
HTML(
"

<h4><b> 1. What you can do on this page  </b></h4>
<ul>
<li> To upload new data and get the prediction
<li> To get the evaluation if new data contains new dependent variable
</ul>

<h4><b> 2. About your data (test set)</b></h4>

<ul>
<li> New data cover all the independent variables used in the model.
<li> New data not used to build the model is called <b>test set</b>
</ul> 

<h4> Please follow the <b>Steps</b> to build the model, and click <b>Outputs</b> to get analytical results.</h4>
"
),

hr(),
#source("2pr_ui.R", local=TRUE, encoding="UTF-8")$value
#****************************************************************************************************************************************************pred-aft


sidebarLayout(

sidebarPanel(

h4("Prediction is based on the accomplishment of model"),      

h4(tags$b("Step 1. Upload New Data File")),      

p("New data should include all the variables in the model"),
fileInput('newfile', "Choose CSV/TXT file",
          accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
#helpText("The columns of X are not suggested greater than 500"),
# Input: Checkbox if file has header ----
p(tags$b("2. Show 1st row as column names?")),
checkboxInput("newheader", "Yes", TRUE),

p(tags$b("3. Use 1st column as row names? (No duplicates)")),
checkboxInput("newcol", "Yes", TRUE),

     # Input: Select separator ----
radioButtons("newsep", "4. Which separator for data?",
  choiceNames = list(
    HTML("Comma (,): CSV often use this"),
    HTML("One Tab (->|): TXT often use this"),
    HTML("Semicolon (;)"),
    HTML("One Space (_)")
    ),
  choiceValues = list(",", "\t", ";", " ")
  ),

radioButtons("newquote", "5. Which quote for characters?",
choices = c("None" = "",
           "Double Quote" = '"',
           "Single Quote" = "'"),
selected = '"'),

p("Correct separator and quote ensures data input successfully")
),


mainPanel(

actionButton("B1.1", h4(tags$b("Click 2: Output. Prediction Results / Refresh, given model and new data are ready. ")), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
p(br()),
tabsetPanel(
tabPanel("Prediction Table",p(br()),
DT::DTOutput("pred")
),

tabPanel("Predicted Survival Plot",p(br()),
p("The predicted survival probability of N'th observation"),

numericInput("line", HTML("Choose N'th observation (N'th row of new data)"), value = 1, min = 1),

plotOutput("p.s", width = "80%"),
DT::DTOutput("pred.n")
)
)
) 
),
hr()
),


##########----------##########----------##########
##########----------##########----------##########
tabPanel((a("Help Pages Online",
            target = "_blank",
            style = "margin-top:-30px; color:DodgerBlue",
            href = paste0("https://mephas.github.io/helppage/")))),
tabPanel(
  tags$button(
    id = 'close',
    type = "button",
    class = "btn action-button",
    style = "margin-top:-8px; color:Tomato; background-color: #F8F8F8  ",
    onclick = "setTimeout(function(){window.close();},500);",  # close browser
    "Stop and Quit"))

))

##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########
##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########

server <- function(input, output) {


#source("0data_server.R", local=TRUE)$value
#****************************************************************************************************************************************************

#load("Surv.RData")

data <- reactive({
                switch(input$edata,
               "Diabetes" = dia.train,
               "NKI70" = nki.train
               )  
                })


DF0 = reactive({
  inFile = input$file
  if (is.null(inFile)){
    x<-data()
    }
  else{
if(!input$col){
    csv <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote=input$quote)
    }
    else{
    csv <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote=input$quote, row.names=1)
    }
    validate( need(ncol(csv)>1, "Please check your data (nrow>1, ncol>1), valid row names, column names, and spectators") )
    validate( need(nrow(csv)>1, "Please check your data (nrow>1, ncol>1), valid row names, column names, and spectators") )

  x <- as.data.frame(csv)
}
return(as.data.frame(x))
})

## variable type
type.num0 <- reactive({
colnames(DF0()[unlist(lapply(DF0(), is.numeric))])
})

output$factor1 = renderUI({
selectInput(
  'factor1',
  HTML('1. Convert real-valued numeric variable into categorical variable'),
  selected = NULL,
  #choices = names(DF()),
  choices = type.num0(),
  multiple = TRUE
)
})

DF1 <- reactive({
df <-DF0() 
df[input$factor1] <- as.data.frame(lapply(df[input$factor1], factor))
return(df)
  })

type.fac1 <- reactive({
colnames(DF1()[unlist(lapply(DF1(), is.factor))])
})

output$factor2 = renderUI({
selectInput(
  'factor2',
  HTML('2. Convert categorical variable into real-valued numeric variable'),
  selected = NULL,
  #choices = names(DF()),
  choices = type.fac1(),
  multiple = TRUE
)
})



DF2 <- reactive({
  df <-DF1() 
df[input$factor2] <- as.data.frame(lapply(df[input$factor2], as.numeric))
return(df)
  })

type.fac2 <- reactive({
colnames(DF2()[unlist(lapply(DF2(), is.factor))])
})

output$lvl = renderUI({
selectInput(
'lvl',
HTML('1. Choose categorical variable'),
selected = NULL,
choices = type.fac2(),
multiple = TRUE
)
})

DF3 <- reactive({
   
  if (length(input$lvl)==0 || length(unlist(strsplit(input$ref, "[\n]")))==0 ||length(input$lvl)!=length(unlist(strsplit(input$ref, "[\n]")))){
  df <- DF2()
}

else{
  df <- DF2()
  x <- input$lvl
  y <- unlist(strsplit(input$ref, "[\n]"))
  for (i in 1:length(x)){
    #df[,x[i]] <- as.factor(as.numeric(df[,x[i]]))
    df[,x[i]] <- relevel(df[,x[i]], ref= y[i])
  }

}
return(df)
  
  })

type.bi3 <- reactive({
  df <- DF3()
  names <- apply(df,2,function(x) { length(levels(as.factor(x)))==2 &&  max(x)==1 && min(x)==0 })
  #df <- DF3()[,names]
  #names <- apply(df,2,function(x) {  })
  x <- colnames(df)[names]
  return(x)
  })

type.time3 <- reactive({
  df <- DF3()
  names <- apply(df,2,function(x) { length(levels(as.factor(x)))>2 })
  df <- DF3()[,names]
  x <- colnames(df[unlist(lapply(df, is.numeric))])
  return(x)
  })

type.num3 <- reactive({
colnames(DF3()[unlist(lapply(DF3(), is.numeric))])
})

type.fac3 <- reactive({
colnames(DF3()[unlist(lapply(DF3(), is.factor))])
})

output$t = renderUI({
selectInput(
't',
tags$b('Choose time-duration variable, numeric'),
selected = type.time3()[1],
choices = c("NULL",type.time3()))
})

output$t1 = renderUI({
selectInput(
't1',
('Start-time variable, numeric'),
selected = "NULL",
choices = c("NULL",type.time3()))
})

output$t2 = renderUI({
selectInput(
't2',
('End-time variable, numeric'),
selected = "NULL",
choices = c("NULL",type.time3()))
})

output$c = renderUI({
selectInput(
'c',
('1. Choose 1/0 censoring information variable (1= event, 0=censor)'),
selected = type.bi3()[1],
choices = type.bi3())
})

##3. Survival Object
surv = reactive({
if (input$time == "A"){
y <- paste0("Surv(", input$t, ",", input$c, ")")
}
if (input$time == "B"){
y <- paste0("Surv(", input$t1, ",", input$t2, ",", input$c, ")")
}
return(y)
})

output$surv = renderPrint({
validate(need(length(levels(as.factor(DF3()[, input$c])))==2, "Please choose a binary variable as censoring information")) 
surv()
})


output$Xdata <- DT::renderDT(
DF3(), 
    extensions = list(
      'Buttons'=NULL,
      'Scroller'=NULL),
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      deferRender = TRUE,
      scrollY = 300,
      scroller = TRUE))



output$strnum <- renderPrint({str(DF3()[,type.num3()])})
#output$str.fac <- renderPrint({str(DF2()[,type.fac()])})
output$strfac <- renderPrint({Filter(Negate(is.null), lapply(DF3(),levels))})


sum <- reactive({
  x <- DF3()[,type.num3()]
  res <- as.data.frame(psych::describe(x))[,-c(1,6,7)]
  rownames(res) = names(x)
  colnames(res) <- c("Total Number of Valid Values", "Mean" ,"SD", "Median", "Minimum", "Maximum", "Range","Skew","Kurtosis","SE")
  return(res)
  })

output$sum <- DT::renderDT({sum()},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))

fsum = reactive({
  x <- DF3()[,type.fac3()]
  summary(x)
  })

output$fsum = renderPrint({fsum()})
 
 output$download2 <- downloadHandler(
     filename = function() {
       "lr.des2.txt"
     },
     content = function(file) {
       write.table(fsum(), file, row.names = TRUE)
     }
   )
 
 km.a = reactive({
  y <- paste0(surv(), "~1")
  fit <- survfit(as.formula(y), data = DF3())
  fit$call <- NULL
  return(fit)
})

output$km.a= renderPlot({

y <- paste0(surv(), "~1")
fit <- surv_fit(as.formula(y), data = DF3())

ggsurvplot(fit, DF3(),
          fun=paste0(input$fun1), 
           conf.int = TRUE,
           pval = FALSE,
           risk.table = "abs_pct",
           #surv.median.line = "hv", 
           palette = "Set1",
           ggtheme = theme_minimal(),
           legend="bottom",
           risk.table.y.text.col = TRUE, # colour risk table text annotations.
           risk.table.y.text = FALSE) 
  })

output$kmat1= renderPrint({
(km.a())})

output$kmat= DT::renderDT({
res<- data.frame(
  time=km.a()[["time"]],
  n.risk=km.a()[["n.risk"]],
  n.event=km.a()[["n.event"]],
  n.censor=km.a()[["n.censor"]],
  surv=km.a()[["surv"]],
  lower=km.a()[["lower"]],
  upper=km.a()[["upper"]],
  std.err=km.a()[["std.err"]],
  cumhaz=km.a()[["cumhaz"]]
  #std.chaz=km.a()[["std.chaz"]],
  )
colnames(res) <- c("Time", "Number of at Risk", "Number of Event", "Number of Censor", 
  "Survival Probability", "95% CI lower limit", "95% CI upper limit",
  "SE of Surv. Prob.", "Cumulative Hazard Probability")
return(res)
},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE)
)

 
## histogram
 output$hx = renderUI({
   selectInput(
     'hx',
     tags$b('Choose a numeric variable'),
     selected = type.num3()[1], 
     choices = type.num3())
 })
 
output$p2 = renderPlot({
   plot_hist1(DF3(), input$hx, input$bin)
   
   })

output$p21 = renderPlot({
    plot_density1(DF3(), input$hx)
     
   })
 


#source("1km_server.R", local=TRUE)$value
#****************************************************************************************************************************************************km


DF4 <- reactive({
  if (input$time=="B") {df <-DF3()[ ,-which(names(DF3()) %in% c(input$c,input$t1,input$t2))]}
  else {df <-DF3()[ ,-which(colnames(DF3()) %in% c(input$c,input$t))]}
return(df)
  })

type.fac4 <- reactive({
colnames(DF4()[unlist(lapply(DF4(), is.factor))])
})

output$g = renderUI({
selectInput(
'g',
tags$b('2. Choose categorical variable'),
selected = type.fac4()[1],
choices = type.fac4(),
multiple=TRUE)
})


output$Xdata2 <- DT::renderDT(
head(DF3()),
options = list(scrollX = TRUE,dom = 't'))
### for summary
output$str <- renderPrint({str(DF3())})



## 4. output results
### 4.2. model

kmfit = reactive({
  validate(need(input$g, "Please choose categorical variable"))
  y <- paste0(surv(), "~", paste0(as.factor(input$g), collapse = "+"))
  fit <- surv_fit(as.formula(y), data = DF3())
  fit$call <- NULL
  return(fit)
})

# 
# # residual plot
# output$km.p= renderPlot({
#autoplot(kmfit(), conf.int = FALSE)+ theme_minimal() + ggtitle("") 
#+annotate("text", x = .75, y = .25, label = paste("P value ="))
# })

output$km.p= renderPlot({
  validate(need(input$g, "Please choose categorical variable"))

  y <- paste0(surv(), "~", paste0(as.factor(input$g), collapse = "+"))
  fit <- surv_fit(as.formula(y), data = DF3())

ggsurvplot(fit, DF3(),
          fun=paste0(input$fun2), 
           conf.int = FALSE,
           pval = TRUE,
           risk.table = "abs_pct",
           #surv.median.line = "hv", 
           palette = "Set1",
           ggtheme = theme_minimal(),
           legend="bottom",
           risk.table.y.text.col = TRUE, # colour risk table text annotations.
           risk.table.y.text = FALSE,
           surv.plot.height =0.7,        
           risk.table.height =0.3) 
  })

output$kmt1= renderPrint({
(kmfit())
  })

output$kmt= renderPrint({
summary(kmfit())
  })
# 
 LR = reactive({
  validate(need(input$g, "Please choose categorical variable"))

   y <- paste0(surv(), "~", paste0(as.factor(input$g), collapse = "+"))
  fit <- survdiff(as.formula(y), rho=input$rho, data = DF3())
  fit$call <- NULL
  return(fit)
})

output$kmlr = renderPrint({
LR()})

 PLR = reactive({
  validate(need(input$g, "Please choose categorical variable"))

   y <- paste0(surv(), "~", paste0(as.factor(input$g), collapse = "+"))

  if (input$pm == "B"){
  res <- pairwise_survdiff(as.formula(y), rho=input$rho2, p.adjust.method = "bonf", data = DF3())$p.value
}
  if (input$pm == "BH"){
  res <- pairwise_survdiff(as.formula(y), rho=input$rho2, p.adjust.method = "holm", data = DF3())$p.value
  }
  #if (input$method == "BHG"){
  #  res <- pairwise.t.test(x[,namesm()[1]], x[,namesm()[2]], 
  #  p.adjust.method = "hochberg")$p.value
  #}
  #  if (input$method == "BHL"){
  #  res <- pairwise.t.test(x[,namesm()[1]], x[,namesm()[2]], 
  #  p.adjust.method = "hommel")$p.value
  #}
    if (input$pm == "FDR"){
  res <- pairwise_survdiff(as.formula(y), rho=input$rho2, p.adjust.method = "BH", data = DF3())$p.value
  }
    if (input$pm == "BY"){
  res <- pairwise_survdiff(as.formula(y), rho=input$rho2, p.adjust.method = "BY", data = DF3())$p.value

  }
  res <- as.data.frame(res)
  return(res)
})

output$PLR = DT::renderDT({PLR()}, 
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))
# 


#source("2aft_server.R", local=TRUE)$value
#****************************************************************************************************************************************************aft

output$var = renderUI({
selectInput(
'var',
tags$b('2. Choose some independent variables (X)'),
selected = names(DF4())[1],
choices = names(DF4()),
multiple=TRUE)
})


output$fx.c = renderUI({
selectInput(
  'fx.c',
  tags$b('Choose one random effect variable'),
selected = names(DF4())[1],
choices = names(DF4()),
)
})

output$Xdata3 <- DT::renderDT(
head(DF3()), options = list(scrollX = TRUE,dom = 't'))
### for summary
output$str3 <- renderPrint({str(DF3())})


aft = reactive({
validate(need(input$var, "Please choose some independent variable"))

if (input$effect=="") {f = paste0(surv(), '~', paste0(input$var, collapse = "+"), input$conf,input$intercept)}
if (input$effect=="Strata") {f = paste0(surv(), '~', paste0(input$var, collapse = "+"), "+strata(", input$fx.c, ")",input$conf,input$intercept)}
if (input$effect=="Cluster") {f = paste0(surv(), '~', paste0(input$var, collapse = "+"), "+cluster(", input$fx.c, ")", input$conf,input$intercept)}
#if (input$effect=="Gamma Frailty") {f = paste0(surv(), '~', paste0(input$var, collapse = "+"), "+frailty(", input$fx.c, ")", input$conf,input$intercept)}
#if (input$effect=="Gaussian Frailty") {f = paste0(surv(), '~', paste0(input$var, collapse = "+"), "+frailty.gaussian(", input$fx.c, ")", input$conf,input$intercept)}

  #fit <- survreg(as.formula(f), data = DF3(), dist=input$dist)
 
  return(f)
})

output$aft_form = renderPrint({cat(aft()) })

aftfit = eventReactive(input$B1, {
  survreg(as.formula(aft()), data = DF3(), dist=input$dist)
  })


#gfit = eventReactive(input$B1, {
#  glm(formula(), data = DF3())
#})
# 
output$fit = renderPrint({ 
res <- summary(aftfit())
res$call <- "AFT Model Result"
return(res)
})

fit.aft <- eventReactive(input$B1, {

if (input$time=="B") {y = DF3()[,input$t2]-DF3()[,input$t1]}
else {y=DF3()[,input$t]}

 res <- data.frame(
  Y = y,
  c= DF3()[,input$c],
  lp = aftfit()$linear.predictors,
  fit = predict(aftfit(), type="response"),
  Residuals = resid(aftfit(),  type="response"),
  devres = -resid(aftfit(),  type="deviance")
 )
 res$std <- (log(res[,1])-res[,3])/aftfit()$scale
  
  if (input$dist=="weibull") {res$csr <- -log(exp(-exp(res$std)))}
  if (input$dist=="exponential") {res$csr <- -log(-exp(-exp(res$std)))}
  #if (input$dist=="extreme") {res$csr <- -log(exp(-exp(res$std)))}
  if (input$dist=="lognormal") {res$csr <- -log(1-pnorm(res$std))}
  if (input$dist=="loglogistic") {res$csr <- -log(1-plogis(res$std))}

res$mar <- res$c- res$csr

 colnames(res) <- c("Times", "Censor","Linear Part = bX", "Predicted Time","Residuals = Time - Predicted Time", "Deviance residuals",
  "Standardized residuals = (log(Times)-bX)/scale", "Cox-snell Residuals", "Martingale residuals = censor - Cox-snell")
 return(res)
  })
# 
 output$fit.aft = DT::renderDT(fit.aft(),
    extensions = 'Buttons', 
    options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    scrollX = TRUE))

output$csplot = renderPlot({

fit = survfit(Surv(fit.aft()[,8], fit.aft()[,2]) ~ 1)
Htilde=cumsum(fit$n.event/fit$n.risk)

d = data.frame(time = fit$time, H = Htilde)
plot_coxstep(d)

#ggplot() + geom_step(data = d, mapping = aes(x = d[,"time"], y = d[,"H"])) + 
#  geom_abline(intercept =0,slope = 1, color = "red") +
#  theme_minimal() + xlab("Cox-Snell residuals") + ylab("Estimated Cumulative Hazard Function")
  })

output$deplot = renderPlot({

df <- data.frame(id=seq_len(nrow(fit.aft())), dev=fit.aft()[,6])

plot_res(df, "id", "dev")+
xlab("Observation Id") + ylab("Deviance residuals")+
geom_point(shape = 19, size=1, color=(fit.aft()[,2]+1))

#ggplot(df, aes(x=id, y=dev)) + geom_point(shape = 20) + geom_hline(yintercept = 0, color="red", linetype=2)+
#geom_smooth(method = "loess", linetype=2) + xlab("Observation Id") + ylab("Deviance residuals") + theme_minimal()
  })

output$var.mr2 = renderUI({
selectInput(
'var.mr2',
tags$b('Choose one continuous independent variable'),
selected = type.num4()[1],
choices = type.num4())
})

output$mrplot = renderPlot({

df <- data.frame(id=DF3()[,input$var.mr2], dev=fit.aft()[,9])
#df <- data.frame(id=seq_len(nrow(fit.aft())), dev=fit.aft()[,9])

validate(need(length(levels(as.factor(DF3()[,input$var.mr2])))>2, "Please choose a continuous variable"))

plot_res(df, "id", "dev")+xlab(input$var.mr2) + ylab("Martingale residuals")+geom_point(shape = 19, size=1, color=(fit.aft()[,2]+1))

#ggplot(df, aes(x=id, y=dev)) + geom_point(shape = 20, color="red")+ ylim(-2,2)+
#geom_smooth(method = "loess", se = FALSE, linetype=1, color="black", size = 0.5) + xlab(input$var.mr) + ylab("Martingale residuals") + theme_minimal()
  })




#source("2pr_server.R", local=TRUE)$value
#****************************************************************************************************************************************************aft-pred

newX = reactive({
  inFile = input$newfile
  if (is.null(inFile)){
    if (input$edata=="Diabetes") {x <- dia.test}
    else {x<- nki.test}
    }
  else{
if(!input$newcol){
    csv <- read.csv(inFile$datapath, header = input$newheader, sep = input$newsep, quote=input$newquote)
    }
    else{
    csv <- read.csv(inFile$datapath, header = input$newheader, sep = input$newsep, quote=input$newquote, row.names=1)
    }
    validate( need(ncol(csv)>1, "Please check your data (nrow>1, ncol>1), valid row names, column names, and spectators") )
    validate( need(nrow(csv)>1, "Please check your data (nrow>1, ncol>1), valid row names, column names, and spectators") )

  x <- as.data.frame(csv)
}
return(as.data.frame(x))
})

#prediction plot
# prediction
pred = eventReactive(input$B1.1,
{
  res <- data.frame(
  lp = predict(aftfit(), newdata = newX(), type="link"),
  predict= predict(aftfit(), newdata = newX(), type="response"))
  colnames(res) <- c("Linear Predictors", "Predictors")
  res <- cbind.data.frame(res, newX())
  return(res)
})

output$pred = DT::renderDT({
pred()
},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))


pred.n <- reactive({
  ptime <- predict(aftfit(), newdata=pred()[input$line,], type='quantile', p=c(1:98/100), se=TRUE)
  df <- data.frame(estimate =ptime$fit, 
                   up.band =ptime$fit + 2*ptime$se.fit,
                   low.band=ptime$fit - 2*ptime$se.fit,
                   ybreak=1-c(1:98/100))
  colnames(df)=c("Estimated Times", "95% CI up band", "95% CI lower band", "Survival Probability")
  return(df)
})

output$pred.n = DT::renderDT({
pred.n()
},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))
 output$p.s = renderPlot({
  

 #matplot(cbind(ptime$fit, ptime$fit + 1.96*ptime$se.fit,
 #                          ptime$fit - 1.96*ptime$se.fit), 1-c(1:98/100),
 #       xlab="Time", ylab="Survival", type='l', lty=c(1,2,2), col=1)
plot_mat(pred.n(), "Survival Probability")+xlab("Estimated Times") +ylab("Survival Probability")
  
  })





#source("3cox_server.R", local=TRUE)$value
#****************************************************************************************************************************************************cox


output$var.cx = renderUI({
selectInput(
'var.cx',
tags$b('2. Choose some independent variables (X)'),
selected = names(DF4())[1],
choices = names(DF4()),
multiple=TRUE)
})


output$fx.cx = renderUI({
selectInput(
  'fx.cx',
  tags$b('Choose one random effect variable'),
selected = names(DF4())[2],
choices = names(DF4())
)
})

output$Xdata4 <- DT::renderDT(
head(DF3()), options = list(scrollX = TRUE,dom = 't'))
### for summary
output$str4 <- renderPrint({str(DF3())})


cox = reactive({

validate(need(input$var.cx, "Please choose some independent variable"))

if (input$effect.cx=="") {f = paste0(surv(), '~', paste0(input$var.cx, collapse = "+"), input$conf.cx)}
if (input$effect.cx=="Strata") {f = paste0(surv(), '~', paste0(input$var.cx, collapse = "+"), "+strata(", input$fx.cx, ")",input$conf.cx)}
if (input$effect.cx=="Cluster") {f = paste0(surv(), '~', paste0(input$var.cx, collapse = "+"), "+cluster(", input$fx.cx, ")", input$conf.cx)}
if (input$effect.cx=="Gamma Frailty") {f = paste0(surv(), '~', paste0(input$var.cx, collapse = "+"), "+frailty(", input$fx.cx, ")", input$conf.cx)}
if (input$effect.cx=="Gaussian Frailty") {f = paste0(surv(), '~', paste0(input$var.cx, collapse = "+"), "+frailty.gaussian(", input$fx.cx, ")", input$conf.cx)}

  #fit <- survreg(as.formula(f), data = DF3(), dist=input$dist)
 
  return(f)
})

output$cox_form = renderPrint({cat(cox()) })

coxfit = eventReactive(input$B2, {
  coxph(as.formula(cox()), data = DF3(), ties=input$tie)
  })


#gfit = eventReactive(input$B1, {
#  glm(formula(), data = DF3())
#})
# 
output$fitcx = renderPrint({ 
res <- summary(coxfit())
res$call <- "Cox Regression Result"
return(res)
})

output$zphplot = renderPlot({
  validate(need((input$effect.cx =="" ), "Models with random effect terms can not be used in this plot."))

f<-cox.zph(coxfit())
p <- ggcoxzph(f,
  point.col = "red", point.size = 1, point.shape = 19,
  point.alpha = 0.7, caption = NULL, 
  ggtheme = theme_minimal(), palette = "Set1",
  font.x = 12,font.y = 12,font.main = 12)
p[input$num]
  })

output$zph = DT::renderDT({ 
res <- cox.zph(coxfit())
res.tab<- as.data.frame(res[["table"]])
colnames(res.tab) <- c("Chi-squared", "Degree of Freedom", "P Value")
return(res.tab)
},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))


fit.cox <- reactive({
if (input$time=="B") {y = DF3()[,input$t2]-DF3()[,input$t1]}
else {y=DF3()[,input$t]}
 res <- data.frame(
  Y = y,
  E = DF3()[,input$c],
  lp = coxfit()$linear.predictors,
  risk=exp(coxfit()$linear.predictors),
  #expected=predict(coxfit(),type="expected"),
  #survival=predict(coxfit(),type="survival"),
  Residuals = resid(coxfit(), type="martingale"),
  Residuals2 = resid(coxfit(), type="deviance"),
  Residuals3 = DF3()[,input$c]-resid(coxfit(), type="martingale")
 )

 colnames(res) <- c("Time", "Censor", "Linear Part = bX", "Risk Score = exp(bX)", 
  #"Expected number of events", "survival Prob. = exp(-Expected number of events)",
  "Martingale Residuals", "Deviance Residuals", "Cox-Snell Residuals")
 return(res)
  })
# 
 output$fit.cox = DT::renderDT(fit.cox(),
    extensions = 'Buttons', 
    options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    scrollX = TRUE))

output$splot = renderPlot({
validate(need((input$effect.cx =="" || input$effect.cx=="Strata" ||input$effect.cx=="Cluster"), "Frailty models can not be used in this plot."))
survminer::ggadjustedcurves(coxfit(), data=DF3(),
  ggtheme = theme_minimal(), palette = "Set1",
  font.x = 12,font.y = 12,font.main = 12)
  })

output$csplot.cx = renderPlot({

fit=survfit(Surv(fit.cox()[,7],fit.cox()[,2])~1)
Htilde=cumsum(fit$n.event/fit$n.risk)

d = data.frame(time = fit$time, H = Htilde)
plot_coxstep(d)

#ggplot() + geom_step(data = d, mapping = aes(x = d[,"time"], y = d[,"H"])) + 
#  geom_abline(intercept =0,slope = 1, color = "red") +
#  theme_minimal() + xlab("Cox-Snell residuals") + ylab("Estimated Cumulative Hazard Function")
  })

type.num4 <- reactive({
colnames(DF4()[unlist(lapply(DF4(), is.numeric))])
})

output$var.mr = renderUI({
selectInput(
'var.mr',
tags$b('Choose one continuous independent variable'),
selected = type.num4()[1],
choices = type.num4())
})

output$diaplot1 = renderPlot({

df <- data.frame(id=DF3()[,input$var.mr], dev=fit.cox()[,5])

validate(need(length(levels(as.factor(DF3()[,input$var.mr])))>2, "Please choose a continuous variable"))

plot_res(df, "id", "dev")+xlab(input$var.mr) + ylab("Martingale residuals")+
geom_point(shape = 19, size=1, color=(fit.cox()[,2]+1))


#ggcoxdiagnostics(coxfit(), ox.scale ="observation.id",
#  type = "martingale", hline.size = 0.5,point.size = 0.5, point.shape = 10,
#  ggtheme = theme_minimal(),font.x = 12,font.y = 12,font.main = 12)
#validate(need(length(levels(as.factor(DF3()[,input$var.mr])))>2, "Please choose a continuous variable"))

#f = paste0(surv(), '~', paste0(input$var.mr))

#fit<-coxph(as.formula(f), data = DF3(), ties=input$tie)
#ggcoxfunctional(fit, data = DF3(), ylim=c(-2,2),
#  point.size = 1, point.shape = 19,
#  ggtheme = theme_minimal(), palette = "Set1",
#  font.x = 12,font.y = 12,font.main = 12)

  })

#output$diaplot1.2 = renderPlot({
#ggcoxdiagnostics(coxfit(), ox.scale ="observation.id",
#  type = "martingale", hline.size = 0.5,point.size = 0.5, point.shape = 10,
#  ggtheme = theme_minimal(),font.x = 12,font.y = 12,font.main = 12)
#  })

output$diaplot2 = renderPlot({

df <- data.frame(id=seq_len(nrow(fit.cox())), dev=fit.cox()[,6])

plot_res(df, "id", "dev")+
xlab("Observation Id") + ylab("Deviance residuals")+
geom_point(shape = 19, size=1, color=(fit.cox()[,2]+1))

#ggcoxdiagnostics(coxfit(), ox.scale ="observation.id",
#  type = "deviance", hline.size = 0.5,point.size = 1, point.shape = 19,
#  ggtheme = theme_minimal(), palette = "Set1",
#  font.x = 12,font.y = 12,font.main = 12) 
  })



#source("3pr_server.R", local=TRUE)$value
#****************************************************************************************************************************************************cox-pred

newX2 = reactive({
  inFile = input$newfile2
  if (is.null(inFile)){
    if (input$edata=="Diabetes") {x <- dia.test}
    else {x<- nki.test}
    }
  else{
if(!input$newcol2){
    csv <- read.csv(inFile$datapath, header = input$newheader2, sep = input$newsep2, quote=input$newquote2)
    }
    else{
    csv <- read.csv(inFile$datapath, header = input$newheader2, sep = input$newsep2, quote=input$newquote2, row.names=1)
    }
    validate( need(ncol(csv)>1, "Please check your data (nrow>1, ncol>1), valid row names, column names, and spectators") )
    validate( need(nrow(csv)>1, "Please check your data (nrow>1, ncol>1), valid row names, column names, and spectators") )

  x <- as.data.frame(csv)
}
return(as.data.frame(x))
})
#prediction plot
# prediction
pred2 = eventReactive(input$B2.1,
{
  res <- data.frame(
  lp = predict(coxfit(), newdata = newX2(), type="lp"),
  risk= predict(coxfit(), newdata = newX2(), type="risk"))
  colnames(res) <- c("Linear Predictors = bX", "Risk score = exp(bX)")
  res <- cbind.data.frame(res, newX2())
  return(res)
})

output$pred2 = DT::renderDT({
pred2()
},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))

BStab <- reactive(
{
if (input$time=="B") {
Surv.rsp <- Surv(DF3()[,input$t1], DF3()[,input$t2], DF3()[,input$c])
Surv.rsp.new <- Surv(pred2()[,input$t1], pred2()[,input$t2], pred2()[,input$c])
}
else {
Surv.rsp <- Surv(DF3()[,input$t], DF3()[,input$c])
Surv.rsp.new <- Surv(pred2()[,input$t], pred2()[,input$c])  
}

lp <- predict(coxfit())
lpnew <- predict(coxfit(), newdata=pred2())

times <- seq(input$ss,input$ee, input$by)                  

BrierScore <- survAUC::predErr(Surv.rsp, Surv.rsp.new, lp, lpnew, times, 
                      type = "brier", int.type = "weighted")

df <- data.frame(Times=BrierScore$times, BrierScore=BrierScore$error, IBS=rep(BrierScore$ierror, length(times)))
colnames(df) <- c("Times", "BrierScore", "Integrated Brier Score")
return(df)
})

output$bstab = DT::renderDT({
BStab()},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))

output$bsplot = renderPlot({
plot_line1(BStab(), "Times", "BrierScore")

#gplot(BStab(), aes(x=Times, y=BrierScore)) + geom_line() +ylim(0,1) + theme_minimal()

  })

#AUCtab <- eventReactive(input$B2.1,
AUCtab <- reactive(
{
if (input$time=="B") {
Surv.rsp <- Surv(DF3()[,input$t1], DF3()[,input$t2], DF3()[,input$c])
Surv.rsp.new <- Surv(pred2()[,input$t1], pred2()[,input$t2], pred2()[,input$c])
}
else {
Surv.rsp <- Surv(DF3()[,input$t], DF3()[,input$c])
Surv.rsp.new <- Surv(pred2()[,input$t], pred2()[,input$c])  
}

lp <- predict(coxfit())
lpnew <- predict(coxfit(), newdata=pred2())

times <- seq(input$ss1,input$ee1, input$by1)                  

if (input$auc=="a") {AUC <- survAUC::AUC.cd(Surv.rsp, Surv.rsp.new, lp, lpnew, times)}
if (input$auc=="b") {AUC <- survAUC::AUC.hc(Surv.rsp, Surv.rsp.new, lpnew, times)}
if (input$auc=="c") {AUC <- survAUC::AUC.sh(Surv.rsp, Surv.rsp.new, lp, lpnew, times)}
if (input$auc=="d") {AUC <- survAUC::AUC.uno(Surv.rsp, Surv.rsp.new, lpnew, times)}


df <- data.frame(Times=AUC$times, AUC=AUC$auc, IAUC=rep(AUC$iauc, length(times)))
colnames(df) <- c("Times", "AUC", "Integrated AUC")
return(df)
})

output$auctab = DT::renderDT({
AUCtab()},
extensions = 'Buttons', 
options = list(
dom = 'Bfrtip',
buttons = c('copy', 'csv', 'excel'),
scrollX = TRUE))

output$aucplot = renderPlot({
plot_line1(AUCtab(), "Times", "AUC")

  })



observe({
      if (input$close > 0) stopApp()                             # stop shiny
    })

}

##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########
##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########----------##########

app <- shinyApp(ui = ui, server = server)

runApp(app, quiet = TRUE)

}
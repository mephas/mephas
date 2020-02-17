# mephas 
<!-- MarkdownTOC -->

- [**Web server MEPHAS**](#web-server-mephas)
- [**R package mephas, installations**](#r-package-mephas-installations)
    - [Installation from GitHub in RStudio console](#installation-from-github-in-rstudio-console)
    - [Installation from ".tar.gz" file](#installation-from-targz-file)
    - [Find a statistical methods using the flowchart](#find-a-statistical-methods-using-the-flowchart)
    - [Commands to open mephas interfaces](#commands-to-open-mephas-interfaces)

<!-- /MarkdownTOC -->

MEPHAS is a shiny-based statistical software that was developed to support statistical data analyses for medical and pharmaceutical students, researchers, and doctors.

Two platforms are available: (1) web server and (2) R package.

<a id="web-server-mephas"></a>
## **Web server MEPHAS**

<a id="homepage-httpsalain003phsosaka-uacjpmephas"></a>
#### [Homepage](https://alain003.phs.osaka-u.ac.jp/mephas/) and [source code at GitHub](https://github.com/mephas/mephas_web)

------

<a id="r-package-mephas-installations"></a>
## **R package mephas, installations**

**We suggested to use the latest version of R and RStudio. The old version may not support some visualization functions**

#### R package needs to be installed in R console. If you have not installed R, download R first:

* R (latest version, >=3.5.0), [download here](https://www.r-project.org/)

##### You can use RStudio console, R console, and even command line interface to open the R environment

* RStudio is an integrated development environment (IDE) for R. [Download RStudio here](https://rstudio.com/products/rstudio/)


#### The followings show how to install in RStudio, which is the same in R console and R in command line interface

------

<a id="installation-from-github-in-rstudio-console"></a>
### Installation from GitHub in RStudio console

To install package from GitHub, we need R package "remotes". Package "mephas" need to use some functions from "mephas.tools", so we need to install 2 packages.

#### First, check the existence of R package "[remotes](https://CRAN.R-project.org/package=remotes)"

    > packageVersion("remotes")

#### If error message said "there is no package called 'remotes', then install "remotes" package first.

    > install.packages("remotes")

#### If R package "remotes" has been installed, then install "mephas.tools" and "mephas" package from GitHub

    > remotes::install_github(c("mephas/mephas.tools", "mephas/mephas"), upgrade="never")
    

Note: `upgrade="never"`: suppress the updates of the R packages, [see details here](https://www.rdocumentation.org/packages/remotes/versions/2.1.0/topics/install_github); package "remotes" and "devtools" shared the same `install_github` function, thus package "remotes" can be replaced by package "[devtools](https://CRAN.R-project.org/package=devtools )"
      

[Videos of the installation process on windows](https://alain003.phs.osaka-u.ac.jp/mephas/installation/installation.html)

------

<a id="installation-from-targz-file"></a>
### Installation from ".tar.gz" file 

Installing from ".tar.gz" file is not recommended. Users need to install two packages: "mephas.tools" and "mephas". Some packages are also required.

#### Download files: [mephas.tools_1.0.0.tar.gz](https://github.com/mephas/mephas_tar.gz/raw/master/mephas.tools_1.0.0.tar.gz) and [mephas_1.1.tar.gz](https://github.com/mephas/mephas_tar.gz/raw/master/mephas_1.1.tar.gz) (updated 2020-2)

##### Check whether you have installed the following packages in R

    > mephas.need.packages <- c("DescTools",
                            "DT",
                            "exactRankTests",
                            "dunn.test",
                            "ROCR",
                            "ggplot2",
                            "magrittr",
                            "psych",
                            "pls",
                            "plotly",
                            "reshape",
                            "shiny",
                            "shinythemes",
                            "shinyWidgets",
                            "survival",
                            "survminer",
                            "survAUC",
                            "spls")
    > not.installed.packages <- mephas.need.packages[!(mephas.need.packages %in% installed.packages()[,"Package"])]
    > not.installed.packages


##### If some packages have not been installed, install them first

    > if(length(not.installed.packages)) install.packages(not.installed.packages)

[Videos of the installation process on windows](https://alain003.phs.osaka-u.ac.jp/mephas/installation/installation.html)


------

<a id="find-a-statistical-methods-using-the-flowchart"></a>
### Find a statistical methods using the flowchart

[Flowchart.pdf](https://alain003.phs.osaka-u.ac.jp/mephas/MEPHAS_flow.pdf)

<a id="commands-to-open-mephas-interfaces"></a>
### Commands to open mephas interfaces

#### Load the package

Before open the applications, we need to load the package "mephas" together with "mephas.tools"

    > library(mephas)

Then, you can open the applications as follows. 

#### All-in-one function, [see details](https://mephas.github.io/reference/mephasOpen.html)

    > mephasOpen()

[Videos of the opening process on windows](https://alain003.phs.osaka-u.ac.jp/mephas/installation/installation.html)

#### Alternatively, open each interfaces using the method names

##### 1. Probability distributions

Continuous probability distribution, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/1_1MFScondist/)

    > MFScondist()

Discrete probability distribution, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/1_2MFSdisdist/)

    > MFSdisdist()
    
##### 2. Parametric T test for means, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/2MFSttest/)

    > MFSttest()

##### 3. Non-parametric tests for median, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/3MFSnptest/)

    > MFSnptest()

##### 4. Test for binomial proportions, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/4MFSproptest/)

    > MFSproptest()

##### 5. Test for contingency tables, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/5MFSrctabtest/)

    > MFSrctabtest()

##### 6. Analysis of variance, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/6MFSanova/)

    > MFSanova()

##### 7. Statistical model, 

Linear regression, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/7_1MFSlr/)
    
    > MFSlr()

Logistic regression, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/7_2MFSlogit/)

    > MFSlogit()

Survival analysis, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/7_3MFSsurv/)
    
    > MFSsurv()

##### 8. Dimensional analysis

Dimensional analysis 1, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/8_1MFSpca/)

    > MFSpca()

Dimensional analysis 2, [web server](https://alain003.phs.osaka-u.ac.jp/mephas_web/8_2MFSpls/)

    > MFSpls()


More functions are under construction...


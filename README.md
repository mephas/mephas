# mephas 
<!-- MarkdownTOC -->

- [**Web server MEPHAS**](#web-server-mephas)
- [**R package mephas, installations**](#r-package-mephas-installations)
  - [Installation from GitHub in RStudio console](#installation-from-github-in-rstudio-console)
  - [Installation from ".tar.gz" file](#installation-from-targz-file)
  - [Find a statistical methods using the flowchart](#find-a-statistical-methods-using-the-flowchart)
  - [Commands to open mephas windows](#commands-to-open-mephas-windows)

<!-- /MarkdownTOC -->

MEPHAS is a shiny-based statistical software that was developed to support statistical data analyses for medical and pharmaceutical students, researchers, and doctors.

Two platforms are available: (1) web server and (2) R package.

<a id="web-server-mephas"></a>
## **Web server MEPHAS**

<a id="homepage-httpsalain003phsosaka-uacjpmephas"></a>
#### [Homepage](https://alain003.phs.osaka-u.ac.jp/mephas/) and [source code at GitHub](https://mephas.github.io/mephas_web/)

------

<a id="r-package-mephas-installations"></a>
## **R package mephas, installations**

#### R package needs to be installed in R console, if you have not installed R, download R firstly:

* R (latest version, >=3.5.0), [download here](https://www.r-project.org/)

##### You can use RStudio console, R console, and even command line interface to open the R environment

* RStudio is an integrated development environment (IDE) for R. [Download RStudio here](https://rstudio.com/products/rstudio/)


##### The followings show how to install in RStudio, which is the same in R console and R in command line interface

------

<a id="installation-from-github-in-rstudio-console"></a>
### Installation from GitHub in RStudio console

#### Before installing, check the existence of R package "devtools", [see details about "devtools"](https://cran.r-project.org/web/packages/devtools/readme/README.html)

    > packageVersion("devtools")

#### If R package "devtools" has been installed, then install "mephas" package from GitHub

    > devtools::install_github(“mephas/mephas”,upgrade="never")
    
#### If error message said "there is no package called ‘devtools'", then install "devtools" package firstly

    > install.packages("devtools")
    > devtools::install_github(“mephas/mephas”,upgrade="never")


Note: `upgrade="never"`: suppress the updates of the R packages, [see details here](https://www.rdocumentation.org/packages/remotes/versions/2.1.0/topics/install_github); package "remotes" and "devtools" shared the same `install_github` function, thus package "devtools" can be replaced by package "remotes", [see details about "remotes"](https://remotes.r-lib.org/)
      

[Videos of the installation process on windows](https://alain003.phs.osaka-u.ac.jp/mephas/installation/installation.html)

------

<a id="installation-from-targz-file"></a>
### Installation from ".tar.gz" file 

#### File: [mephas_1.1.tar.gz](https://github.com/mephas/mephas.tar.gz) (updated 2020-1)

##### Check whether you have installed the following packages

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

<a id="commands-to-open-mephas-windows"></a>
### Commands to open mephas windows

#### Load the package

Before open the applications, please load the package mephas

    > library(mephas)

Then, you can open the applications as follows. 

#### All-in-one function, [see details](https://mephas.github.io/mephas/reference/mephasOpen.html)

    > mephasOpen()

[Videos of the opening process on windows](https://alain003.phs.osaka-u.ac.jp/mephas/installation/installation.html)

#### Alternatively, open each interfaces using the method names

##### 1. Probability distributions

Continuous probability distribution, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/1_1MFScondist/)

    > MFScondist()

Discrete probability distribution, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/1_2MFSdisdist/)

    > MFSdisdist()
    
##### 2. Parametric T test for means, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/2MFSttest.html)

    > MFSttest()

##### 3. Non-parametric tests for median, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/3MFSnptest.html)

    > MFSnptest()

##### 4. Test for binomial proportions, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/4MFSproptest.html)

    > MFSproptest()

##### 5. Test for contingency tables, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/5MFSrctabtest.html)

    > MFSrctabtest()

##### 6. Analysis of variance, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/6MFSanova.html)

    > MFSanova()

##### 7. Statistical model, 

Linear regression, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/7_1MFSlr.html)
    
    > MFSlr()

Logistic regression, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/7_2MFSlogit.html)

    > MFSlogit()

Survival analysis, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/7_3MFSsurv.html)
    
    > MFSsurv()

##### 8. Dimensional analysis

Dimensional analysis 1, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/8_1MFSpca.html)

    > MFSpca()

Dimensional analysis 2, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/8_2MFSpls.html)

    > MFSpls()


More functions are under construction...


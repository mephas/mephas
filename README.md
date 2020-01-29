# mephas 
<!-- MarkdownTOC -->

- [**Web server MEPHAS**](#web-server-mephas)
- [**R package mephas, installation**](#r-package-mephas-installation)
  - [Required Software](#required-software)
  - [Installation from GitHub at RStudio console](#installation-from-github-at-rstudio-console)
  - [Installation from ".tar.gz" file](#installation-from-targz-file)
  - [Commands to open mephas windows](#commands-to-open-mephas-windows)
- [Some differences between web server MEPHAS and R package mephas](#some-differences-between-web-server-mephas-and-r-package-mephas)

<!-- /MarkdownTOC -->

MEPHAS is a shiny-based statistical software that was developed to support statistical data analyses for medical and pharmaceutical students, researchers, and doctors.

Two platforms are available: (1) web server and (2) R package.

<a id="web-server-mephas"></a>
## **Web server MEPHAS**

<a id="homepage-httpsalain003phsosaka-uacjpmephas"></a>
#### Homepage: [https://alain003.phs.osaka-u.ac.jp/mephas/](https://alain003.phs.osaka-u.ac.jp/mephas/) 

<a id="source-code-at-github-mephas_web"></a>
##### Source code at GitHub: [check here](https://mephas.github.io/mephas_web/)

------

<a id="r-package-mephas-installation"></a>
## **R package mephas, installation**

<a id="required-software"></a>
### Required Software

#### 1. R (latest version, >=3.5.0), [download here](https://www.r-project.org/)

#### 2. RStudio (Recommend), [download here](https://rstudio.com/products/rstudio/)

------

<a id="installation-from-github-at-rstudio-console"></a>
### Installation from GitHub at RStudio console

#### Before installing, check the existence of R package "devtools", [see details about "devtools"](https://cran.r-project.org/web/packages/devtools/readme/README.html)

    > packageVersion("devtools")

#### If R package "devtools" has been installed, then install "mephas" package from GitHub
      
    > devtools::install_github(“mephas/mephas”,upgrade="never")
    
#### If error message said "there is no package called ‘devtools'", then install "devtools" package firstly

    > install.packages("devtools")
    > devtools::install_github(“mephas/mephas”,upgrade="never")

Note: `upgrade="never"`: suppress the updates of the R packages, [see details here](https://www.rdocumentation.org/packages/remotes/versions/2.1.0/topics/install_github); package "remotes" and "devtools" shared the same `install_github` function, thus package "devtools" can be replaced by package "remotes", [see details about "remotes"](https://remotes.r-lib.org/)
      
------

<a id="installation-from-targz-file"></a>
### Installation from ".tar.gz" file 

#### File: [mephas_1.1.tar.gz](https://github.com/mephas/mephas.tar.gz) (updated 2020-1)

#### You may need to install the following packages Before or During the Installation (if there were warnings)

    > install.packages(c("DescTools",
                          "DT",
                          "exactRankTests",
                          "dunn.test",
                          "ROCR",
                          "ggplot2",
                          "psych",
                          "pls",
                          "reshape",
                          "shiny",
                          "survival",
                          "survminer",
                          "survAUC",
                          "stargazer",
                          "spls"))


------

<a id="commands-to-open-mephas-windows"></a>
### Commands to open mephas windows

#### Load the whole package

Before open the applications, please load the package mephas

    > library(mephas)

Then, you can open the applications as follows. 

#### All-in-one function, [see details](https://mephas.github.io/mephas/reference/mephasOpen.html)

    > mephasOpen()

#### Equivalently,

##### 1. Probability distributions

Continuous probability distribution, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/1_1MFScondist/)

    > MFScondist()

Discrete probability distribution, [web server](https://alain003.phs.osaka-u.ac.jp/mephas/1_2MFSdisdist/)

    > MFScondist()
    
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

<a id="some-differences-between-web-server-mephas-and-r-package-mephas"></a>
## Some differences between web server MEPHAS and R package mephas

##### Web server MEPHAS employs plotly for 2D and 3D plot; R package mephas employs ggplot2 for 2D plots only

##### Web server MEPHAS has comparatively more explanation and interpretation than R package mephas

##### Web server MEPHAS updates earlier than R package mephas

##### R package mephas uses only local R and Rstudio. After installed, it can be used with no need for Internet

qlcRecode
=========

**Functions for data recoding in Quantitative Language Comparison (QLC)**

When using external data, there are often various tweaks that one would like to perform before using the data for further research. This package offers assistance for some common recoding problems occurring in Quantitative Language Comparison (QLC).

Currently only the recoding of nominal data is implemented. Please see the vignette for a detailed explanation of the intended usage.

This is an early alpha version, not yet available on CRAN. However, it is pretty easy to install this package directly from github into R by using:

    install.packages("devtools")
    devtools::install_github("cysouw/qlcRecode", build_vignettes = TRUE)

When the option "build_vignettes" gives errors, either update the devtools package, or leave out the vignettes. It is an interesting vignette though, but you can also directly get the vignette as a PDF from the source here.

Michael Cysouw
cysouw@mac.com
setwd("~/Dropbox/gates/gasl/")
library(devtools)
library(formatR)
rebuild <- function() {
    tidy_dir(recursive = T, arrow = T)
    document()
    build_vignettes()
    test()
    build()
    install()
    
    install()
    load_all()
}

rebuild()


# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("betaselectr_lav.Rmd.original", output = "betaselectr_lav.Rmd")

setwd(base_dir)

# For articles

# base_dir <- getwd()

# setwd("vignettes/articles")
# knitr::knit("", output = "med_mg.Rmd")

# setwd(base_dir)

# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("betaselectr_lav.Rmd.original", output = "betaselectr_lav.Rmd", envir = new.env())
knitr::knit("betaselectr_lm.Rmd.original", output = "betaselectr_lm.Rmd", envir = new.env())
knitr::knit("betaselectr_glm.Rmd.original", output = "betaselectr_glm.Rmd", envir = new.env())
setwd(base_dir)

# For articles

# base_dir <- getwd()

# setwd("vignettes/articles")
# knitr::knit("", output = "med_mg.Rmd")

# setwd(base_dir)

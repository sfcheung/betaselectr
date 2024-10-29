skip("To be tested in an interactive section")
# A long test with parallel
# Check the speed of parallel processing

# Adapted from stdmod

library(testthat)
library(betaselectr)

system.time(lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 800, iseed = 5678, ncpus = 2,
            progress = TRUE, parallel = TRUE, load_balancing = TRUE))
system.time(lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 800, iseed = 5678, ncpus = 2,
            progress = FALSE, parallel = TRUE, load_balancing = FALSE))
system.time(lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 800, iseed = 5678, ncpus = 2,
            progress = TRUE, parallel = TRUE))
system.time(lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 800, iseed = 5678, ncpus = 2,
            progress = FALSE, parallel = TRUE))
system.time(lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 800, iseed = 5678,
            progress = TRUE, parallel = FALSE))
system.time(lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1, data_test_mod_cat, to_standardize = "iv", do_boot = TRUE, bootstrap = 800, iseed = 5678,
            progress = FALSE, parallel = FALSE))

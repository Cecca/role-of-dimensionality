source("R/packages.R")
source("R/functions.R")
source("R/plan.R")

drake::make(plan, lock_envir = FALSE)

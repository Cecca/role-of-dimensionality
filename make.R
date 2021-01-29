source("R/packages.R")
source("R/functions.R")
source("R/plan.R")
source("R/scores_distributions.R")

drake::make(plan, lock_envir = FALSE)
drake::make(scores_plan, lock_envir = FALSE)

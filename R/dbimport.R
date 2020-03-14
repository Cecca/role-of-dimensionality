library(tidyverse)
library(dbplyr)
library(arrow)
library(here)

data <- read_parquet(here("detail.parquet"))

con <- DBI::dbConnect(RPostgres::Postgres())

DBI::dbWriteTable(
  con,
  "detail",
  head(data),
  overwrite = TRUE
)

copy_to(con, 
        data,
        name="detail", 
        overwrite=TRUE, 
        temporary=FALSE)


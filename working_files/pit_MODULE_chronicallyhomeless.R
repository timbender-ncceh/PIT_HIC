library(openxlsx)


# get filename----
ch.input.name <- list.files(pattern = "^A003 -")

# get sheets----
ch.sheet.names <- loadWorkbook(file = ch.input.name) %>% names()

# load data---
ch.summary.df <- readWorkbook(ch.input.name, sheet = "Summary") %>%
  as_tibble()
ch.clientdetail.df <- readWorkbook(ch.input.name, sheet = "Client Detail") %>%
  as_tibble()
ch.disabilityDQerrors.df <- readWorkbook(ch.input.name,sheet = "Disability DQ Errors") %>%
  as_tibble()

# remove unneeded vars
rm(ch.sheet.names, ch.input.name)

# tidy----

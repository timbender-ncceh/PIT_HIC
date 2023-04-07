library(devtools)

# creates an dataframe output that shows clients who are living in a household
# with at least 1 person who has a CH condition

# Run pit_MODULE_chronicallyhomeless_ind.R ----
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev/working_files/pit_MODULE_chronicallyhomeless_ind.R?raw=TRUE")

# returns: 
CH.status.ind

# get needed data from output2A----
colnames(output2A)

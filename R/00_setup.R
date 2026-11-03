# Step 0: install the packages this project uses, at the versions in renv.lock.
# Run once from the project folder:  Rscript R/00_setup.R

options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# renv gives the project its own library (renv/library/), so these versions do not touch
# anything else on the machine. .Rprofile activates it whenever R starts in this folder.
renv::restore(prompt = FALSE)

for (p in c("sf", "dplyr", "readr", "ggplot2", "jsonlite", "testthat"))
  cat(sprintf("  %-9s %s\n", p, as.character(packageVersion(p))))

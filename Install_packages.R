required <- c(
  # tidyverse
  "tidyverse",
  # Rstudio
  "rmarkdown", 
  # W3 packages
  "tinytex", "pander", "kableExtra", "remotes",
  # biometryassist
  "biometryassist"
)
installed <- rownames(installed.packages())
notInstalled <- setdiff(required, installed)

if (length(notInstalled) > 0){
  
  message("Installing required packages...\n")
  install.packages(notInstalled, quiet = TRUE, type = ifelse(Sys.info()[['sysname']]=="Linux", "source", "binary"), 
                  Ncpus = getOption("Ncpus", max(parallel::detectCores() - 2, 1)))
  
  installed <- rownames(installed.packages()) # Update to see what's there now
  missingPackages <- setdiff(required, installed)
  
  
  if (length(missingPackages) > 0){
    invisible(sapply(missingPackages, 
                     function(x){
                       warning("\nThe package ", x, " has not installed sucessfully.\n",
                               "Please copy and paste the code:\n\n\t install.packages('", x,
                               "')\n\n and try running it again on the console. \n\n
                               If that fails again, please email the output to biometrytraining@adelaide.edu.au\n")
                     }))
    stop("Installation unsuccessful.")
                     }
  else{
    message("\nAll required packages have been installed.\n\n\n")
  }
}

#Clean up variables
# rm(list = ls())

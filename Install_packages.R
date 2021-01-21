required <- c(
  # tidyverse
  "tidyverse",
  # plotting
  "ggthemes",
  # Rstudio
  "knitr", "rmarkdown", 
  # Experimental design
  "agricolae",
  #W2 extra packages
  "asremlPlus", "data.table",
  #W3 packages
  "tinytex", "pander", "kableExtra", "xtable", "devtools"
)
installed <- rownames(installed.packages())
notInstalled <- setdiff(required, installed)

if (length(notInstalled) > 0){
  
  message("Installing required packages...\n")
  install.packages(notInstalled, quiet = TRUE, type = ifelse(Sys.info()[['sysname']]=="Linux", "source", "binary"))
  
  installed <- rownames(installed.packages()) # Update to see what's there now
  missingPackages <- setdiff(required, installed)
  
  
  if (length(missingPackages) > 0){
    invisible(sapply(missingPackages, 
                     function(x){
                       warning("\nThe package ", x, " has not installed sucessfully.\n",
                               "Please copy and paste the code:\n\n\t install.packages('", x,
                               "')\n\n and try running it again on the console. \n\n
                               If that fails again, please email the output to s.rogers@adelaide.edu.au\n")
                     }))
    stop("Installation unsuccessful.")
                     }
  else{
    message("\nAll required packages have been installed.\n\n\n")
  }
}

drat::addRepo("biometryhub")
install.packages("BiometryTraining")
#devtools::install_github("biometryhub/BiometryTraining", upgrade = "never", quiet = TRUE)

if(!"BiometryTraining" %in% rownames(installed.packages())) {
  warning("\nThe BiometryTraining package has not installed sucessfully.\n",
          "Please copy and paste the code:\n\n\t devtools::install_github('biometryhub/BiometryTraining', upgrade = 'never')\n\n 
          and try running it again on the console. \n\n
          If that fails again, please email the output to s.rogers@adelaide.edu.au\n")
}

#Clean up variables
# rm(list = ls())

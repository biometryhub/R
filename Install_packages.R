# Connect to BioC & install using the correct packages

# reqVers <- "3.4.1"
# curVers <- paste(R.Version()[c("major", "minor")], collapse = ".")
# if (curVers != reqVers){
# 	msg <- paste("Your R version is", curVers, ".\nPlease ensure you are running R", reqVers,
# 							 "\nThis can be downloaded from https://cran.r-project.org/")
# 	stop(msg)
# }
# message("You have the correct version of R installed.")

# update.packages(ask = FALSE, quiet = TRUE)

required <- c(
	# tidyverse
	"tidyverse",
	# plotting
	"ggthemes",
	# Rstudio
	"knitr", "rmarkdown",
	# Experimental design
	"agricolae"
)
installed <- rownames(installed.packages())
notInstalled <- setdiff(required, installed)

if (length(notInstalled) > 0){
    
    message("Installing/updating other required packages...\n")
    install.packages(notInstalled, quiet = TRUE, type = "binary")
    
    installed <- rownames(installed.packages()) # Update to see what's there now
    missingPackages <- setdiff(required, installed)
    
    
    if (length(missingPackages) > 0){
        invisible(sapply(missingPackages, 
                         function(x){
                             warning("\nThe package ", x, " has not installed sucessfully.\n",
                                     "Please copy and paste the code:\n\n\t install.packages(", x,
                                     ")\n\nand email the output to s.rogers@adelaide.edu.au\n")
                         }))
        stop("Installation unsuccessful.")
    }
    else{
        message("All required packages have been installed.")
    }
}
rm(list = ls())

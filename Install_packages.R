# Connect to BioC & install using the correct packages

# reqVers <- "3.2.0"
# curVers <- paste(R.Version()[c("major", "minor")], collapse = ".")
# if(substr(curVers, 1, 3) < substr(reqVers, 1, 3)){
#     
# 	msg <- paste("Your R version is", curVers, ".\nPlease ensure you are running R", reqVers,
# 							 "\nThis can be downloaded from https://cran.r-project.org/")
# 	stop(msg)
# }

# update.packages(ask = FALSE, quiet = TRUE)

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
	"emmeans", "asremlPlus", "gridExtra", "ggpubr"
)
installed <- rownames(installed.packages())
notInstalled <- setdiff(required, installed)

if (length(notInstalled) > 0){
    
    message("Installing required packages...\n")
    install.packages(notInstalled, quiet = TRUE, type = "binary")
    
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

#Clean up variables
# rm(list = ls())

#Source functions for Sharon's workshop
source("https://raw.githubusercontent.com/therog1/R/master/graphing.R")
#OR source("http://bit.do/graphing-R")
source("https://raw.githubusercontent.com/therog1/R/master/satab.R")
#OR source("http://bit.do/satab")
source("https://raw.githubusercontent.com/therog1/R/master/W2_functions.r")
#OR source("http://bit.do/W2_functions")
save.image()




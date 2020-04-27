##############################################
#### Skeletal ANOVA Table ####################
##############################################
# Created by Sharon Nielsen, September 2017

satab <- function(design.obj, design, sp.facW = NULL, sp.facS = NULL){
    .Deprecated(new = "des.info", package = "BiometryTraining", 
                msg = paste0("These functions are no longer maintained and will be removed at a future date.\nPlease use the package we have created instead: https://github.com/biometryhub/BiometryTraining\nThis function has been replaced by des.info."))
    
    if(design != "sp"){
        cat(format("Source of Variation",width = 40), "df", "\n")
        cat("=============================================\n")
    }
    
    if(design == "crd"){
        trt <- names(design.obj)[3]
        totdf <- nrow(design.obj)-1
        trtdf <- length(unique(design.obj[[trt]]))-1
        errdf <- totdf - trtdf
        
        cat(format(trt, width = 40), trtdf, "\n")
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width = 40), totdf, "\n")
    }
    
    if(design == "rcbd"){
        trt <- names(design.obj)[3]
        blkdf <- length(unique(design.obj$block))-1
        totdf <- nrow(design.obj)-1
        trtdf <- length(unique(design.obj[[trt]]))-1
        errdf <- totdf - trtdf - blkdf
        
        cat(format("Block stratum", width = 40), blkdf, "\n")
        cat("---------------------------------------------\n")
        cat(format(trt ,width = 40), trtdf, "\n")
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
    
    if(design == "faccrd"){
        trt <- names(design.obj)[3:length(names(design.obj))]
        totdf <- nrow(design.obj)-1
        trtdf <- c()
        for(i in 1:length(trt)){
            dd <- length(unique(design.obj[[trt[i]]]))-1
            trtdf <- c(trtdf,dd)
        }
        errdf <- totdf - sum(trtdf)
        for(i in 1:length(trt)){
            cat(format(trt[i], width = 40), trtdf[i], "\n")
        }
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
    
    
    if(design == "facrcbd"){
        trt <- names(design.obj)[3:length(names(design.obj))]
        totdf <- nrow(design.obj)-1
        trtdf <- c()
        for(i in 1:length(trt)){
            dd <- length(unique(design.obj[[trt[i]]]))-1
            trtdf <- c(trtdf,dd)
        }
        blkdf <- length(unique(design.obj$block))-1
        cat(format("Block stratum", width = 40), blkdf, "\n")
        cat("---------------------------------------------\n")
        errdf <- totdf - sum(trtdf) - blkdf
        for(i in 1:length(trt)){
            cat(format(trt[i], width = 40), trtdf[i], "\n")
        }
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
    
    if(design == "ls"){
        trt <- names(design.obj)[4]
        rowdf <- length(unique(design.obj$row))-1
        coldf <- length(unique(design.obj$col))-1
        totdf <- nrow(design.obj)-1
        trtdf <- length(unique(design.obj[[trt]]))-1
        errdf <- totdf - trtdf - coldf - rowdf
        
        cat(format("Row", width = 40), rowdf, "\n")
        cat(format("Column", width = 40), coldf, "\n")
        cat(format(trt, width = 40), trtdf, "\n")
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
    
    if(design == "sp"){
        blkdf <- length(unique(design.obj$block))-1
        totdf <- nrow(design.obj)-1
        numwplots <- nrow(design.obj)/length(unique(des.out$splots))
        sp.facWdf <- length(unique(des.out[[sp.facW]]))-1
        wpresdf <- (numwplots - 1) - blkdf - sp.facWdf
        
        trtAdf <- length(unique(design.obj[[sp.facW]]))-1
        trtBdf <- length(unique(design.obj[[sp.facS]]))-1
        trtABdf <- trtAdf * trtBdf
        errdf <- totdf - trtAdf - trtBdf - trtABdf - blkdf - wpresdf
        
        cat(format("Source of Variation",width = 45), "df", "\n")
        cat("==================================================\n")
        cat(format("Block stratum", width = 45), blkdf, "\n")
        cat("--------------------------------------------------\n")
        cat("Whole plot stratum", "\n")
        cat(format(" ", width = 9), format(sp.facW, width = 35), trtAdf, "\n")
        cat(format("Whole plot Residual", width = 45), wpresdf, "\n")
        cat("==================================================\n")
        cat("Subplot stratum", "\n")
        cat(format(" ", width = 9), format(sp.facS, width = 35), trtBdf, "\n")
        cat(format(" ", width = 9), format(paste(sp.facW,sp.facS,sep = ":"), width = 35), trtABdf, "\n")
        cat(format(" ", width = 9), format("Subplot Residual", width = 35), errdf, "\n")
        cat("==================================================\n")
        cat(format("Total",width = 45), totdf, "\n")
    }
}

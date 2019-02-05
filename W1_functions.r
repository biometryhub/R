##############################################
#### Graphing function #######################
##############################################
plot.des <- function(design.obj, design, nrows, ncols, plot.fac = "trt", sp.facW = NULL, sp.facS = NULL, sp.rep = NULL){
    
    if (!require("RColorBrewer")) {
        install.packages("RColorBrewer")
        library(RColorBrewer)
    }
    if (!require("ggplot2")) {
        install.packages("ggplot2")
        library(ggplot2)
    }
    
    if(design == "lsd"){
        des <- design.obj
        des[[plot.fac]] <- factor(des[[plot.fac]])
        
        # Number of treatments
        ntrt <- length(levels(factor(des[[plot.fac]])))
        
        # create the colours for the graph
        color_palette <- colorRampPalette(brewer.pal(11, "Spectral"))(ntrt)
        # create the graph
        plt <- ggplot(des, aes(x = col, y = row, fill = des[[plot.fac]])) + geom_tile(colour = "black") +
            geom_text(aes(label = des[[plot.fac]])) +
            theme_bw() + scale_fill_manual(values = color_palette, name = plot.fac)
    }
    
    if(design == "crd" | design == "rcbd" | design == "fac"){
        
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        
        if(design == "rcbd") {
            # If the number of blocks evenly divides the number of rows (but not columns), 
            # then re-sort plan by rows first and then columns to get it displaying blocks in the correct orientation
            if(length(unique(plan$row)) %% length(unique(design.obj$block)) == 0 & 
               length(unique(plan$col)) %% length(unique(design.obj$block)) != 0) {
                plan <- plan[order(plan$row, plan$col),]
            }
        }
        
        des <- cbind(plan, design.obj)
        
        if(design == "fac"){
            des$trt <- paste("A", des$A, "B", des$B, sep = " ")
        }
        
        
        
        des[[plot.fac]] <- factor(des[[plot.fac]])
        
        # Number of treatments
        ntrt <- length(levels(factor(des[[plot.fac]])))
        
        # create the colours for the graph
        color_palette <- colorRampPalette(brewer.pal(11, "Spectral"))(ntrt)
        # create the graph
        plt <- ggplot(des, aes(x = col, y = row, fill = des[[plot.fac]])) + geom_tile(colour = "black") +
            geom_text(aes(label = des[[plot.fac]])) +
            theme_bw() + scale_fill_manual(values = color_palette, name = plot.fac)
        
    }
    
    if(design == "sp"){
        
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj)
        
        des$Wplt <- rep(rep(1:length(unique(des[[sp.facW]])), each = length(unique(des[[sp.facS]]))), sp.rep)
        des$trt <- factor(paste(des[[sp.facW]], des[[sp.facS]], sep = " "))
        
        # Number of treatments
        ntrt <- length(levels(factor(des[[plot.fac]])))
        
        # create the colours for the graph
        color_palette <- colorRampPalette(brewer.pal(11, "Spectral"))(ntrt)
        # create the graph
        plt <- ggplot(des, aes(x = col, y = row, fill = des[[plot.fac]])) + geom_tile(colour = "black") +
            geom_text(aes(label = des[[plot.fac]])) +
            theme_bw() + scale_fill_manual(values = color_palette, name = plot.fac)
    }
    plt + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
}





##############################################
##############################################
##############################################

##############################################
#### Skeletal ANOVA Table ####################
##############################################

satab <- function(design.obj, design, sp.facW = NULL, sp.facS = NULL){
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
        trtABdf <- trtdf[1]*trtdf[2]
        errdf <- totdf - sum(trtdf) - trtABdf
        for(i in 1:length(trt)){
            cat(format(trt[i], width = 40), trtdf[i], "\n")
        }
        cat(format("AB", width = 40), trtABdf, "\n")
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
        trtABdf <- trtdf[1]*trtdf[2]
        blkdf <- length(unique(design.obj$block))-1
        cat(format("Block stratum", width = 40), blkdf, "\n")
        cat("---------------------------------------------\n")
        errdf <- totdf - sum(trtdf) - trtABdf - blkdf
        for(i in 1:length(trt)){
            cat(format(trt[i], width = 40), trtdf[i], "\n")
        }
        cat(format("AB", width = 40), trtABdf, "\n")
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
    
    if(design == "lsd"){
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
    
    if(design == "faclsd"){
        rowdf <- length(unique(design.obj$row))-1
        coldf <- length(unique(design.obj$col))-1
        totdf <- nrow(design.obj)-1
        trtAdf <- length(unique(design.obj$A))-1
        trtBdf <- length(unique(design.obj$B))-1
        trtABdf <- trtAdf * trtBdf
        errdf <- totdf - trtAdf - trtBdf - trtABdf - rowdf - coldf
        
        cat(format("Row", width = 40), rowdf, "\n")
        cat(format("Column", width = 40), coldf, "\n")
        cat(format("A", width = 40), trtAdf, "\n")
        cat(format("B", width = 40), trtBdf, "\n")
        cat(format("AB", width = 40), trtABdf, "\n")
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
}

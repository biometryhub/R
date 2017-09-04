##############################################
#### Graphing function #######################
##############################################
#Created by Sharon Nielsen, August 2017


plot.des <- function(design.obj, design, nrows, ncols, plot.fac = "trt", sp.facW = NULL, sp.facS = NULL, sp.rep = NULL){
    
    if (!require("RColorBrewer")) {
        install.packages("RColorBrewer")
        library(RColorBrewer)
    }
    
    if(design == "ls"){
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
    
    plt
    
}

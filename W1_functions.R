##############################################
#### Graphing function #######################
#################################################
#### Graphing function #######################
##############################################
plot.des <- function(design.obj, nrows, ncols, brows, bcols){
    .Deprecated(new = "des.info", package = "BiometryTraining", 
                msg = paste0("These functions are no longer maintained.\nPlease use the package we have created instead: https://github.com/biometryhub/BiometryTraining or email us for more information: biometrytraining@adelaide.edu.au.\nThis function has been replaced by des.info."))
    
    nth_element <- function(vector, starting_position, n) {
        vector[seq(starting_position, length(vector), n)]
    }
    
    if (!require("RColorBrewer")) {
        install.packages("RColorBrewer")
        library(RColorBrewer)
    }
    
    des <- design.obj$parameters$design
    
    ifelse(des == "factorial",
           design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
           design <- des)
    
    if(design == "crd"){
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)
        
        names(des)[5] <- "trt"
        ntrt <- nlevels(as.factor(des$trt))
    }
    
    if(design == "rcbd"){
        
        names(design.obj$book)[3] <- "trt"
        ntrt <- nlevels(as.factor(design.obj$book$trt))
        
        xx <- c()
        rr <- nrows/brows
        cc <- ncols/bcols
        if(cc < ncols){
            aa <- dim(design.obj$book)[1]/cc
            for(zz in 1:cc){
                for(i in 1:ntrt){
                    vec1 <-((zz-1)*aa+1)
                    vec2 <- zz*aa
                    bb <- nth_element((vec1:vec2), i, ntrt)
                    xx <- c(xx, bb)
                }}
            des <- design.obj$book[xx,]
        } else {
            des <- design.obj$book
        }
        
        
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, des)
    }
    
    if(design == "lsd"){
        des <- design.obj$book
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)
        
        names(des)[4] <- "trt"
        ntrt <- nlevels(as.factor(des$trt))
    }
    
    
    
    if(design == "factorial_crd"){
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)
        
        des$trt <- factor(paste("A", des$A, "B", des$B, sep = ""))
        ntrt <- nlevels(as.factor(des$trt))
    }
    
    
    if(design == "factorial_rcbd"){
        
        design.obj$book$trt <- factor(paste("A", design.obj$book$A, "B", design.obj$book$B, sep = ""))
        ntrt <- nlevels(as.factor(design.obj$book$trt))
        
        
        xx <- c()
        rr <- nrows/brows
        cc <- ncols/bcols
        if(cc < ncols){
            aa <- dim(design.obj$book)[1]/cc
            for(zz in 1:cc){
                for(i in 1:ntrt){
                    vec1 <-((zz-1)*aa+1)
                    vec2 <- zz*aa
                    bb <- nth_element((vec1:vec2), i, ntrt)
                    xx <- c(xx, bb)
                }}
            des <- design.obj$book[xx,]
        } else {
            des <- design.obj$book
        }
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, des)
    }
    
    
    
    
    if(design == "factorial_lsd"){
        des$trt <- factor(paste("A", des$A, "B", des$B, sep = ""))
        ntrt <- nlevels(as.factor(des$trt))
        
        des <- design.obj$book
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)
        
    }
    
    
    
    
    
    if(design == "split"){
        
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)
        
        des$trt <- factor(paste("wp", des[,6], "sp", des[,7], sep = ""))
        
        # Number of treatments
        ntrt <- nlevels(as.factor(des$trt))
    }
    
    
    
    # create the colours for the graph
    color_palette <- colorRampPalette(brewer.pal(11, "Spectral"))(ntrt)
    # create the graph
    plt <- ggplot(des, aes(x = col, y = row, fill = trt)) + geom_tile(colour = "black") +
        geom_text(aes(label = trt)) +
        theme_bw() + scale_fill_manual(values = color_palette, name = "Treatment")
    
    print(plt)
    
    return(des)
    
}



##############################################
##############################################
##############################################

##############################################
#### Skeletal ANOVA Table ####################
##############################################

satab <- function(design.obj){
    
    des <- design.obj$parameters$design
    
    ifelse(des == "factorial",
           design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
           design <- des)
    
    design.obj <- design.obj$book
    
    if(design != "split"){
        cat(format("Source of Variation",width = 40), "df", "\n")
        cat("=============================================\n")
    }
    
    if(design == "crd"){
        trt <- names(design.obj)[3]
        totdf <- nrow(design.obj)-1
        trtdf <- length(unique(design.obj[,3]))-1
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
        trtdf <- length(unique(design.obj[,3]))-1
        errdf <- totdf - trtdf - blkdf
        
        cat(format("Block stratum", width = 40), blkdf, "\n")
        cat("---------------------------------------------\n")
        cat(format(trt ,width = 40), trtdf, "\n")
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
    
    if(design == "factorial_crd"){
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
    
    
    if(design == "factorial_rcbd"){
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
        trtdf <- length(unique(design.obj[,4]))-1
        errdf <- totdf - trtdf - coldf - rowdf
        
        cat(format("Row", width = 40), rowdf, "\n")
        cat(format("Column", width = 40), coldf, "\n")
        cat(format(trt, width = 40), trtdf, "\n")
        cat(format("Residual", width = 40), errdf, "\n")
        cat("=============================================\n")
        cat(format("Total", width= 40), totdf, "\n")
    }
    
    
    
    if(design == "split"){
        blkdf <- length(unique(design.obj$block))-1
        totdf <- nrow(design.obj)-1
        numwplots <- nrow(design.obj)/length(unique(design.obj$splots))
        sp.facWdf <- length(unique(design.obj[,4]))-1
        wpresdf <- (numwplots - 1) - blkdf - sp.facWdf
        
        trtAdf <- length(unique(design.obj[,4]))-1
        trtBdf <- length(unique(design.obj[,5]))-1
        trtABdf <- trtAdf * trtBdf
        errdf <- totdf - trtAdf - trtBdf - trtABdf - blkdf - wpresdf
        
        cat(format("Source of Variation",width = 45), "df", "\n")
        cat("==================================================\n")
        cat(format("Block stratum", width = 45), blkdf, "\n")
        cat("--------------------------------------------------\n")
        cat("Whole plot stratum", "\n")
        cat(format(" ", width = 9), format(names(design.obj)[4], width = 35), trtAdf, "\n")
        cat(format("Whole plot Residual", width = 45), wpresdf, "\n")
        cat("==================================================\n")
        cat("Subplot stratum", "\n")
        cat(format(" ", width = 9), format(names(design.obj)[5], width = 35), trtBdf, "\n")
        cat(format(" ", width = 9), format(paste(names(design.obj)[4],names(design.obj)[5],sep = ":"), width = 35), trtABdf, "\n")
        cat(format(" ", width = 9), format("Subplot Residual", width = 35), errdf, "\n")
        cat("==================================================\n")
        cat(format("Total",width = 45), totdf, "\n")
    }
    
    if(design == "factorial_lsd"){
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



Summary <- function(design.obj, nrows, ncols, brows = NA, bcols = NA){
    plt <- plot.des(design.obj, nrows, ncols, brows, bcols)
    satab(design.obj)
    
    return(plt)
}
###########################################
plot.des <- function(design.obj, nrows, ncols, brows, bcols){
.Deprecated("des.info", )
  
nth_element <- function(vector, starting_position, n) {
  vector[seq(starting_position, length(vector), n)]
  }

if (!require("RColorBrewer")) {
install.packages("RColorBrewer")
library(RColorBrewer)
}

des <- design.obj$parameters$design

ifelse(des == "factorial",
design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
design <- des)

if(design == "crd"){
plan <- expand.grid(row = 1:nrows, col = 1:ncols)
des <- cbind(plan, design.obj$book)

names(des)[5] <- "trt"
ntrt <- nlevels(des$trt)
}

if(design == "rcbd"){

names(design.obj$book)[3] <- "trt"
ntrt <- nlevels(design.obj$book$trt)

xx <- c()
rr <- nrows/brows
cc <- ncols/bcols
if(cc < ncols){
aa <- dim(design.obj$book)[1]/cc
for(zz in 1:cc){
for(i in 1:ntrt){
vec1 <-((zz-1)*aa+1)
vec2 <- zz*aa
bb <- nth_element((vec1:vec2), i, ntrt)
xx <- c(xx, bb)
}}
des <- design.obj$book[xx,]
} else {
des <- design.obj$book
}


plan <- expand.grid(row = 1:nrows, col = 1:ncols)
des <- cbind(plan, des)
}

if(design == "lsd"){
des <- design.obj$book
des$row <- as.numeric(des$row)
des$col <- as.numeric(des$col)

names(des)[4] <- "trt"
ntrt <- nlevels(des$trt)
}



if(design == "factorial_crd"){
plan <- expand.grid(row = 1:nrows, col = 1:ncols)
des <- cbind(plan, design.obj$book)

des$trt <- factor(paste("A", des$A, "B", des$B, sep = ""))
ntrt <- nlevels(des$trt)
}


if(design == "factorial_rcbd"){

design.obj$book$trt <- factor(paste("A", design.obj$book$A, "B", design.obj$book$B, sep = ""))
ntrt <- nlevels(design.obj$book$trt)


xx <- c()
rr <- nrows/brows
cc <- ncols/bcols
if(cc < ncols){
aa <- dim(design.obj$book)[1]/cc
for(zz in 1:cc){
for(i in 1:ntrt){
vec1 <-((zz-1)*aa+1)
vec2 <- zz*aa
bb <- nth_element((vec1:vec2), i, ntrt)
xx <- c(xx, bb)
}}
des <- design.obj$book[xx,]
} else {
des <- design.obj$book
}
plan <- expand.grid(row = 1:nrows, col = 1:ncols)
des <- cbind(plan, des)
}




if(design == "factorial_lsd"){
des$trt <- factor(paste("A", des$A, "B", des$B, sep = ""))
ntrt <- nlevels(des$trt)

des <- design.obj$book
des$row <- as.numeric(des$row)
des$col <- as.numeric(des$col)

}





if(design == "split"){

plan <- expand.grid(row = 1:nrows, col = 1:ncols)
des <- cbind(plan, design.obj$book)

des$trt <- factor(paste("wp", des[,6], "sp", des[,7], sep = ""))

# Number of treatments
ntrt <- nlevels(des$trt)
}



# create the colours for the graph
color_palette <- colorRampPalette(brewer.pal(11, "Spectral"))(ntrt)
# create the graph
plt <- ggplot(des, aes(x = col, y = row, fill = trt)) + geom_tile(colour = "black") +
geom_text(aes(label = trt)) +
theme_bw() + scale_fill_manual(values = color_palette, name = "Treatment")

print(plt)

return(des)

}



##############################################
##############################################
##############################################

##############################################
#### Skeletal ANOVA Table ####################
##############################################

satab <- function(design.obj){

des <- design.obj$parameters$design

ifelse(des == "factorial",
design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
design <- des)

design.obj <- design.obj$book

    if(design != "split"){
    cat(format("Source of Variation",width = 40), "df", "\n")
    cat("=============================================\n")
    }

    if(design == "crd"){
    trt <- names(design.obj)[3]
    totdf <- nrow(design.obj)-1
    trtdf <- length(unique(design.obj[,3]))-1
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
    trtdf <- length(unique(design.obj[,3]))-1
    errdf <- totdf - trtdf - blkdf

    cat(format("Block stratum", width = 40), blkdf, "\n")
    cat("---------------------------------------------\n")
    cat(format(trt ,width = 40), trtdf, "\n")
    cat(format("Residual", width = 40), errdf, "\n")
    cat("=============================================\n")
    cat(format("Total", width= 40), totdf, "\n")
    }

    if(design == "factorial_crd"){
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


    if(design == "factorial_rcbd"){
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
    trtdf <- length(unique(design.obj[,4]))-1
    errdf <- totdf - trtdf - coldf - rowdf

    cat(format("Row", width = 40), rowdf, "\n")
    cat(format("Column", width = 40), coldf, "\n")
    cat(format(trt, width = 40), trtdf, "\n")
    cat(format("Residual", width = 40), errdf, "\n")
    cat("=============================================\n")
    cat(format("Total", width= 40), totdf, "\n")
    }



    if(design == "split"){
    blkdf <- length(unique(design.obj$block))-1
    totdf <- nrow(design.obj)-1
    numwplots <- nrow(design.obj)/length(unique(design.obj$splots))
    sp.facWdf <- length(unique(design.obj[,4]))-1
    wpresdf <- (numwplots - 1) - blkdf - sp.facWdf

    trtAdf <- length(unique(design.obj[,4]))-1
    trtBdf <- length(unique(design.obj[,5]))-1
    trtABdf <- trtAdf * trtBdf
    errdf <- totdf - trtAdf - trtBdf - trtABdf - blkdf - wpresdf

    cat(format("Source of Variation",width = 45), "df", "\n")
    cat("==================================================\n")
    cat(format("Block stratum", width = 45), blkdf, "\n")
    cat("--------------------------------------------------\n")
    cat("Whole plot stratum", "\n")
    cat(format(" ", width = 9), format(names(design.obj)[4], width = 35), trtAdf, "\n")
    cat(format("Whole plot Residual", width = 45), wpresdf, "\n")
    cat("==================================================\n")
    cat("Subplot stratum", "\n")
    cat(format(" ", width = 9), format(names(design.obj)[5], width = 35), trtBdf, "\n")
    cat(format(" ", width = 9), format(paste(names(design.obj)[4],names(design.obj)[5],sep = ":"), width = 35), trtABdf, "\n")
    cat(format(" ", width = 9), format("Subplot Residual", width = 35), errdf, "\n")
    cat("==================================================\n")
    cat(format("Total",width = 45), totdf, "\n")
    }

if(design == "factorial_lsd"){
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



Summary <- function(design.obj, nrows, ncols, brows = NA, bcols = NA){
plt <- plot.des(design.obj, nrows, ncols, brows, bcols)
satab(design.obj)

return(plt)
}

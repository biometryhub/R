
source("http://bit.do/W2_functions")

library(agricolae)
library(ggplot2)
library(emmeans)
library(asreml)
library(asremlPlus)
######################################################################
# Example 1
######################################################################

dat <- read.csv("example1.csv")

str(dat)

dat$trt <- factor(dat$trt)


ggplot(data = dat, aes(x = trt, y = RL)) + 
geom_boxplot() +
theme_bw()

######################################################################
#CRD analysis

dat.aov <- aov(RL ~ trt, data = dat)            # fitting the model

shapiro.test(dat.aov$residuals)
resplt(dat.aov)

summary(dat.aov)
anova(dat.aov)

library(emmeans)


pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tk.out <- HSD.test(dat.aov, trt = "trt", 
                    console = TRUE)


tk.out$groups$trt <- factor(row.names(tk.out$groups),
                 levels = c("1", "5", "10", "20"))

library(dplyr)

pred.out <- left_join(pred.out, tk.out$groups, 
                      by = "trt")



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci


# graph the predicted values 
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 0.1) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Calcium Concentration", y = "Predicted Root Length (cm)")



######################################################################
# Example 2
######################################################################

dat <- read.csv("example2.csv")
str(dat)

#dat$trt <- factor(dat$trt)


ggplot(data = dat, aes(x = trt, y = TuberLengthGrowth)) + 
geom_boxplot() +
theme_bw() + 
labs(y = "Growth in Tuber Length (mm)", x = NULL)


######################################################################
#CRD analysis

dat.aov <- aov(TuberLengthGrowth ~ trt, data = dat)            # fitting the model

resplt(dat.aov)
shapiro.test(dat.aov$residuals)

anova(dat.aov)

pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)


tk.out <- HSD.test(dat.aov, trt = "trt", console = TRUE)
tk.out$groups$trt <- factor(row.names(tk.out$groups))


pred.out <- left_join(pred.out, tk.out$groups, by = "trt")


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

 
# order the treatment by growth
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))
 


# graph the predicted values 
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 0.1) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Treatment", y = "Predicted Growth in Tuber Length (mm)")


######################################################################
# Example 3
######################################################################

dat <- read.csv("example3.csv")
str(dat)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Variety, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

dat.aov <- aov(Yield ~ Block + Variety, data = dat)            # fitting the model

resplt(dat.aov)

anova(dat.aov)
shapiro.test(dat.aov$residuals)

pred.out <- emmeans(dat.aov, "Variety")
pred.out
pred.out <- data.frame(pred.out)

tk.out <- HSD.test(dat.aov, trt = "Variety", console = TRUE)
tk.out$groups$Variety <- factor(row.names(tk.out$groups))

pred.out <- left_join(pred.out, tk.out$groups, by = "Variety")

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by yield size
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$Variety <- factor(as.character(pred.out$Variety),
                levels = as.character(pred.out$Variety))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Variety)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Variety", y = "Predicted Yield (t/ha)")

#######################################################################
# Example 4
#######################################################################

dat <- read.csv("example4.csv")

str(dat)

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)

ggplot(data = dat, aes(x = trt, y = DM)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = row, y = DM)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = col, y = DM)) + geom_boxplot() +
theme_bw()

# fitting the model
dat.aov <- aov(DM ~ row + col + trt, data = dat)

resplt(dat.aov)

anova(dat.aov)
shapiro.test(dat.aov$residuals)

pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)


tk.out <- HSD.test(dat.aov, trt = "trt", console = TRUE)
tk.out$groups$trt <- factor(row.names(tk.out$groups))

pred.out <- left_join(pred.out, tk.out$groups, by = "trt")
pred.out



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by DM
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 5) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Soil Type", y = "Predicted Dry Matter (kg/ha)")

######################################################################
# LMM Example 3
######################################################################

library(asreml)

dat <- read.csv("example3.csv")
dat$Block <- factor(dat$Block)
dat$Plot <- factor(dat$Plot)

library(asreml)
dat.asr <- asreml(Yield ~ Variety, random = ~ Block, 
rcov = ~ id(Plot), data = dat)

anova(dat.asr)

dat.ww <- wald(dat.asr, denDF = "default")$Wald


resplt(dat.asr)

#The ANOVA table
round(dat.ww,3)


shapiro.test(dat.asr$residuals)

dat.pred <- predict(dat.asr, classify = "Variety",
sed = TRUE)

dat.pred
pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Variety", sig = 0.95)
pred.out


pred.out$ci <- qt(p = 0.975, dat.asr$nedf) * pred.out$standard.error
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Treatments by Yield size
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Variety <- factor(as.character(pred.out$Variety),
                levels = as.character(pred.out$Variety))

# graph the predicted values 
ggplot(data = pred.out, aes(x = Variety)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")


library(asremlPlus)
dat.current <- asreml(Yield ~ Variety, random = ~ Block, data = dat)
dat.reduced <- asreml(Yield ~ Variety, data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, 
                reduced.asreml.obj = dat.reduced)



#######################################################################
# LMM Example 4
#######################################################################

dat <- read.csv("example4.csv")

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)
dat$plots <- factor(dat$plots)


# fitting the model
dat.asr <- asreml(DM ~ trt, random = ~ row + col, 
rcov = ~ id(plots), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)


#The ANOVA table
round(dat.ww,3)
shapiro.test(dat.asr$residuals)

dat.pred <- predict(dat.asr, classify = "trt",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, 
                    pred = "trt", sig = 0.95)
pred.out



pred.out$ci <- qt(p = 0.975, dat.asr$nedf) * pred.out$standard.error
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Treatments by DM
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))


# graph the predicted values 
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 5) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Soil Type", y = "Predicted Dry Matter (kg/ha)")


summary(dat.asr)$varcomp

library(asremlPlus)
dat.current <- asreml(DM ~ trt, random = ~ row + col, 
rcov = ~ id(plots), data = dat)
dat.reduced <- asreml(DM ~ trt, random = ~ row, 
rcov = ~ id(plots), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


######################################################################
# Example 5
######################################################################

dat <- read.csv("example5.csv")
str(dat)

dat$WholePlot <- factor(dat$WholePlot)

ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 6))

ggplot(data = dat, aes(x = Fungicide, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

######################################################################
# Split-plot analysis
######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
random = ~ Block + Block:WholePlot, rcov = ~ units, data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.asr <- asreml(Yield ~ Fungicide + Genotype,
random = ~ Block + Block:WholePlot, 
rcov = ~ units, data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.pred <- predict(dat.asr, classify = "Genotype",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Genotype", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Genotype)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Genotype, y = up, label = groups), vjust = 0, 
          hjust = 0, nudge_y = 0.05, angle = 90, size = 2) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))

dat.pred <- predict(dat.asr, classify = "Fungicide",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Fungicide", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Fungicide)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Fungicide, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")




summary(dat.asr)$varcomp

# Likelihood Ratio test for Block:WholePlot
dat.current <- asreml(Yield ~ Genotype + Fungicide,
random = ~ Block + Block:WholePlot, rcov = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Fungicide,
random = ~ Block, rcov = ~ units, data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Genotype + Fungicide,
random = ~ Block, rcov = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Fungicide,
rcov = ~ units, data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

######################################################################
# Example 6
######################################################################

dat <- read.csv("example6.csv")
str(dat)
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Treatment, y = Yield)) + geom_boxplot() +
theme_bw()

######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Treatment, random = ~ Block,
rcov = ~ ar1(Column):ar1(Row), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

# Predict
dat.pred <- predict(dat.asr, classify = "Treatment",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Treatment", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, 
          angle = 90, nudge_y = 0.3) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")  + 
theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 8))

summary(dat.asr)$varcomp

# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Treatment,
random = ~ Block, rcov = ~ ar1(Column):ar1(Row), data = dat)

dat.reduced <- asreml(Yield ~ Treatment, rcov = ~ ar1(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Row)

dat.reduced <- asreml(Yield ~ Treatment,
random = ~ Block, rcov = ~ ar1(Column):id(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Column)

dat.reduced <- asreml(Yield ~ Treatment,
random = ~ Block, rcov = ~ id(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

######################################################################
# Example 7
######################################################################

dat <- read.csv("example7.csv")
str(dat)

dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Herbicide, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Rate, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

######################################################################
# fitting the model

dat.asr <- asreml(Yield ~ lin(Row) + Treatment,
random = ~ Block + spl(Row) + Row,  rcov = ~ id(Column):ar1(Row), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald
round(dat.ww,3)

dat.asr <- asreml(Yield ~ lin(Row) + Control + Herbicide + Herbicide:Rate,
random = ~ Block + spl(Row) + Row,  rcov = ~ id(Column):ar1(Row), data = dat)

dat.ww <- wald(dat.asr, denDF = "default")$Wald
round(dat.ww,3)



plot(variogram(dat.asr))

dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


summary(dat.asr)$varcomp


# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
random = ~ Block, rcov = ~ ar1(Column):ar1(Row), data = dat)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate, rcov = ~ ar1(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Row)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
random = ~ Block, rcov = ~ ar1(Column):id(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Column)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
random = ~ Block, rcov = ~ id(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


dat.pred <- predict(dat.asr, classify = "Control",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Control", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Control)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Control, y = up, label = groups), vjust = 0, nudge_y = 0.01) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")


dat.pred <- predict(dat.asr, classify = "Herbicide",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Herbicide", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Herbicide)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Herbicide, y = up, label = groups), vjust = 0, nudge_y = 0.01) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")

dat.pred <- predict(dat.asr, classify = "Herbicide:Rate",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Herbicide:Rate", sig = 0.95)
pred.out
pred.out$Treatment <- paste(pred.out$Herbicide, pred.out$Rate, sep = "_")

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, angle = 90, nudge_y = 0.1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")  + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 8))



#########################################################################################################################
#ANCOVA
#########################################################################################################################

dat <-read.csv("ancova.csv")

dat$A <- factor(dat$A)

ggplot(data = dat, aes(x = A, y = Y)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = X, y = Y, goup = A, col = A)) + 
geom_point() + theme_bw()


ggplot(data = dat, aes(x = X, y = Y, goup = A, col = A)) + 
geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
theme_bw()

dat.asr <- asreml(Y ~ A + X + A:X, data = dat)

dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)





library(agridat)
dat <- cochran.crd

dat$Control <- rep("No", nrow(dat))
dat$Control[dat$trt == "O"] <- "Yes"

dat$Season <- substring(dat$trt, 1, 1)

dat$Rate <- substring(dat$trt, 2)
dat <- dat[order(dat$row, dat$col),]
















######################################################################
# Exercise 16
######################################################################

dat <- read.csv("exercise16.csv")
str(dat)

dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
  theme_bw()


ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
  theme_bw()

######################################################################
# fitting the model
summary(dat$Yield)


dat.asr <- asreml(Yield ~ Genotype,
                  random = ~ Block,  
                  rcov = ~ id(Column):ar1(Row), data = dat)



dat.asr <- asreml(Yield ~ Genotype + lin(Row),
                  random = ~ Block + spl(Row) + Row,  
                  rcov = ~ id(Column):ar1(Row), data = dat)


plot(variogram(dat.asr))

dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

ggplot(dat, aes(x = as.numeric(Row), y = Yield)) + geom_point()


summary(dat.asr)$varcomp


# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
                      random = ~ Block, rcov = ~ ar1(Column):ar1(Row), data = dat)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate, rcov = ~ ar1(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

#reml.lrt.asreml(full.asreml.obj = dat.asr, reduced.asreml.obj = dat.asr2)

# Likelihood Ratio test for ar1(Row)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
                      random = ~ Block, rcov = ~ ar1(Column):id(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Column)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
                      random = ~ Block, rcov = ~ id(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


dat.pred <- predict(dat.asr, classify = "Control",
                    present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Control", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# graph the predicted values 
ggplot(data = pred.out, aes(x = Control)) +
  geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
  geom_text(aes(x = Control, y = up, label = groups), vjust = 0, nudge_y = 0.01) +
  geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
  labs(x = "", y = "Predicted Yield (t/ha)")







library(agridat)

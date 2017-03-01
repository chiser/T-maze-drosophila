##### Import the csv file into R. If I dont have the data ordered in the way I wanted I could always reshape it with stack/unstack functions as well as with reshape function

Tmaze <- read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = "." )

nExp <- length (Tmaze[[1]])
nGroups <- length(levels(Tmaze[[1]]))
N<- table(Tmaze$Fly.line)

# I get errors every time with apply functions...I forget it for the momment

#Tmaze$PI <- apply(Tmaze,1,function(Tmaze=Tmaze) (Tmaze[2]-Tmaze[4]))

#Tmaze$PI <- (Tmaze$Red-Tmaze$Dark)/(Tmaze$Red+Tmaze$Dark) 

###### A less efficient way of calculating PIs
Tmaze[,2]<- as.integer(Tmaze[,2])
Tmaze[,4]<- as.integer(Tmaze[,4])
Tmaze$PI <- vector("numeric", length = nExp)
          for(i in 1:nExp){
                                    Tmaze$PI[i] <- (((Tmaze[[i,2]]-Tmaze[[i,4]])/(Tmaze[[i,2]]+Tmaze[[i,4]])) + ((Tmaze[[i,5]]-Tmaze[[i,7]])/(Tmaze[[i,5]]+Tmaze[[i,7]])))/2                
                                    if(Tmaze[[i,5]]==0 && Tmaze[[i,7]]==0 ) Tmaze$PI[i] <- ((Tmaze[[i,2]]-Tmaze[[i,4]])/(Tmaze[[i,2]]+Tmaze[[i,4]])) 
          }
Tmaze$PI1 <- vector("numeric", length = nExp)
for(i in 1:nExp){
  Tmaze$PI1[i] <- (Tmaze[[i,2]]-Tmaze[[i,4]])/(Tmaze[[i,2]]+Tmaze[[i,4]])               
  }
Tmaze$PI2 <- vector("numeric", length = nExp)
for(i in 1:nExp){
  Tmaze$PI2[i] <-  (Tmaze[[i,5]]-Tmaze[[i,7]])/(Tmaze[[i,5]]+Tmaze[[i,7]])               
 }

Tmaze$PI3 <- Tmaze$PI1-Tmaze$PI2
Rlikelihood<- sqrt(sum(Tmaze$PI3)^2)
# Weights of PI1 and weights of PI2 according to the number of flies. I still need to validate it
ExpFliesMean <- mean(Tmaze[,2]+Tmaze[,4]+Tmaze[,5]+Tmaze[,7])
ExpFliesMean1 <- mean(Tmaze[,2]+Tmaze[,4])
ExpFliesMean2 <- mean(Tmaze[,5]+Tmaze[,7])

Tmaze$Weight<- (Tmaze[,2]+Tmaze[,4]+Tmaze[,5]+Tmaze[,7])/ExpFliesMean
Tmaze$Weight1<- (Tmaze[,2]+Tmaze[,4])/ExpFliesMean1
Tmaze$Weight2<- (Tmaze[,5]+Tmaze[,7])/ExpFliesMean2

Tmaze$weightedPI <- Tmaze$Weight*Tmaze$PI
Tmaze$weightedPI1 <- Tmaze$Weight1*Tmaze$PI1
Tmaze$weightedPI2 <- Tmaze$Weight2*Tmaze$PI2
###### This is in order to make groups according to their names in the case of fly food. So that they can be assigned a different colour in the plot for instance. pmatch should do the same
#### A factor level to sort the ones with ATR and without in the experimental group and the genetic controls

Tmaze$Treatment2 <- ifelse(grepl("ATR", Tmaze[[1]], ignore.case = T), "ATR", ifelse (grepl("Co", Tmaze[[1]], ignore.case = T), "Co", ""))
Tmaze$Background <- ifelse(grepl("white", Tmaze[[1]], ignore.case = T), "white", ifelse (grepl("yellow", Tmaze[[1]], ignore.case = T), "yellow", ""))
Tmaze$comparison <- ifelse(grepl(">", Tmaze[[1]], ignore.case = T), "Experimental", "Genetic Control")

#############Another way of doing it
#dataATR <- grep("ATR",data[[1]])
#dataCo <- grep ("Co", data[[1]])
#dataGenetic <- grep ("AUS", data[[1]])

idGroup <- data.frame("Group"=levels(Tmaze[[1]]),"Treatment"= ifelse(grepl("ATR", levels(Tmaze[[1]]), ignore.case = T), "Experimental ATR", ifelse (grepl("Co", levels(Tmaze[[1]]), ignore.case = T), "Experimental Co", "Genetic Control")),
                      "Colour"=ifelse(grepl("ATR", levels(Tmaze[[1]]), ignore.case = T), "darkgoldenrod", ifelse (grepl("Co", levels(Tmaze[[1]]), ignore.case = T), "darkgoldenrod1", "darkgreen")))
#try with grep(invert=TRUE)


### Another way of making the treatments... specifying the treatment name by the last letters
#idGroup$Treatment2 <- sub('.*(?=.{4}$)', '', idGroup$Group, perl=T)

#### To create two columns just for differentiating treatment and geneticline

idGroup$LINE <- gsub("(ATR)", "", idGroup$Group, fixed = TRUE)
idGroup$LINE <- gsub("(Co)", "", idGroup$LINE, fixed = TRUE)

### make medians and means for the groups in the idGroup table. With lapply I creat lists, so I have to be careful, with the for loops below I create numeric vectors

idGroup$mean <- NULL
mean <- NULL

idGroup$mean <- sapply(seq_len(nrow(idGroup)), function(i) { 
  mean(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
})

idGroup$median <- NULL
median <- NULL
 
idGroup$median <- sapply(seq_len(nrow(idGroup)), function(i) { 
  median(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
})

#### less effficient way of calculating median and mean

#idGroup$median <- NULL
#median <- NULL  
#for(i in 1:length(idGroup$Group)){
  
#  idGroup$median[i] <- median(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
#}

#idGroup$mean <- NULL
#mean <- NULL  
#for(i in 1:length(idGroup$Group)){
  
#  idGroup$mean[i] <- mean(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
#}

##### To order the groups for plotting. This will only work nicely if I put a "1" in front of my control line so that it put it the first

#idGroup <- idGroup[with(idGroup, order(Group)), ]
#Tmaze <- Tmaze[with(Tmaze, order(Fly.line)), ]
###### Ordering data by putting first the Genetic controls and the the lines with their ATR controls in a descending order by mean

library(dplyr)

idGroup$Treatment <- ordered(idGroup$Treatment, levels = c("Genetic Control", "Experimental ATR", "Experimental Co")) 
idGroup <- idGroup[order(idGroup$Treatment), ]


idGroup$rank <- (idGroup$Treatment!="Genetic Control")*1
idGroup <- idGroup %>% group_by(LINE) %>% mutate(temp=mean(mean)) %>% 
  ungroup %>% arrange(rank, -temp) %>% select(-rank, -temp)


###### The merge statement in base R can perform the equivalent of inner and left joins, as well as right and full outer joins, which are unavailable in sqldf.
######
#library(sqldf)
#
###### Firstly, you can get the mean of column Mean for each group with this statement (similar to aggregate in R)
#sqldf("
#                                      SELECT 
#                                      `Group` AS `Group`, 
#                                      AVG(`Mean`) AS `GroupMean` 
#                                      FROM idGroup 
#                                      GROUP BY `Group`;")
######Then it is a case of using the JOIN statement (like merge in R) to join this table to the original one, putting 'Gen' at the top and then sorting by GroupMean. I call these these tables t1 and t2, join them together, and then select from them the columns I want, and sorting the table.                                     
#sqldf("
#SELECT 
#    t1.`Group` AS `Group`, 
#    t1.`Treatment` AS `Treatment`, 
#    t1.`Mean` AS `Mean`, 
#    t2.`GroupMean` AS `GroupMean` 
#FROM
#    (SELECT * FROM idGroup) t1
#    JOIN
#    (SELECT 
#        `Group` AS `Group`, 
#        AVG(`Mean`) AS `GroupMean` 
#    FROM idGroup 
#    GROUP BY `Group`) t2
#    ON t1.`Group` = t2.`Group`
#ORDER BY CASE `Treatment` WHEN 'Genenetic Control' THEN 1 ELSE 2 END, 
#    `GroupMean` DESC, 
#    `Mean` DESC;
#")  

###### Order the Tmaze data in the way the idGroup table is ordered. It looks fine in the Global environment and in the plots. However opening the table the order isn´t there

levels <- as.character(idGroup$Group)
Tmaze$Fly.line <- factor(Tmaze$Fly.line, levels = levels)


#Packages necessary for the statistics
#require("car")
#require("plyr")
#require("multcomp")
#require("pwr")

# making a power test. For one tail anova or for linear models
#signif <- scan(n=1)
#power <- scan(n=1)
# k is the number of groups and n the common sample size of groups. f values of 0.1, 0.25, and 0.4 represent small, medium, and large effect sizes respectively. 
#pwr.anova.test(k = , n = , f = , sig.level = signif , power = power)
# u and v are the numerator and denominator degrees of freedom, and f2 the effect size
#pwr.f2.test(u =, v = , f2 = , sig.level = signif , power = power)

#Checking for normality
#par(mfrow=c(3, 4))
#qplots <- sapply(seq_len(nrow(idGroup)), function(i) { 
#  qqPlot(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
#})

# pnorm() I think is for finding out where are the datapoint located in the normal distribution

#something to export or to wait for an answer. An alternative would be to do it with shapiro test and continue automatically

#if(all(N > 1))
#Shapiro<-sapply(seq_len(nrow(idGroup)), function(i) { 
#  shapiro.test(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
#})
#replications(Tmaze.lm)

#Tmaze.lm <- lm(Tmaze$PI ~ Tmaze$Fly.line) 
#Tmaze.res <- resid(Tmaze.lm)
#Tmaze$res <- Tmaze.lm$residuals
#Tmaze.stdres <- rstandard(Tmaze.lm)
#Tmaze$abs <- abs(Tmaze$res)
#Tmaze.stdabs <- abs(Tmaze.stdres)
#Shapiro<-shapiro.test(Tmaze$res)
#if(Shapiro$p.value <= 0.05) message ("Data is not normally distributed")
#qqnorm(Tmaze.res)
#qqline(Tmaze.res)

# If it isnt normal, continue it by transforming with the t1way and t1waybt functions

#Checking for homocedasticity. Check to what should be the levene applied upon
#Levene <-leveneTest(Tmaze$PI,Tmaze$Fly.line) It doesn´t give me the proper results I don´t know why. It would be better because I can take the p-value out of it
#Levene <-aov(Tmaze$abs~Tmaze$Fly.line)
#Levene2<- summary(Levene)
#if(Levene$Pr[1] <= 0.05) message ("Data is not homocedastic")
#Tmaze <-transform(Tmaze, VarTransformation = abs(log(abs(Tmaze$PI))))
#If it isn´t homocedastic, continue it by oneway.test and Welchs tests

#If it is homocedastic, make an ANOVA

#ANOVA <- aov(Tmaze.lm)
#summary(ANOVA)

#DependentVar <- Tmaze$VarTransformation
#Supplementation <- Tmaze$Treatment2
#Background <- Tmaze$Background
#Genetics <- Tmaze$comparison

#ifelse(length(Supplementation) == length(Background),print(length(Supplementation)), message("length of the two first factors aren´t equal"))

#anova <- aov(DependentVar ~ Supplementation*Background*Genetics )
#summary(anova)
message("Enter the name of the output file")
output<- scan(n=1,what= character())
#boxplot(data$PI ~ data$Fly.line, ylab = "PI", las=2, at =c(1,2, 4,5, 7,8, 10, 12,13, 15,16, 18,19, 21,22, 24),par(mar = c(12, 5, 4, 2) + 0.1), 
#col = c(c(rep(c("darkgoldenrod","darkgoldenrod1"),3),"darkgoldenrod",rep(c("darkgoldenrod","darkgoldenrod1"),4),"darkgoldenrod")))
pdf(paste(output,".pdf",sep=""), paper = "a4")

boxplot(Tmaze$PI ~ Tmaze$Fly.line, ylab = "PI", las =2, ylim = c(-1,1),col= as.character(idGroup$Colour), main="PI as a whole",cex.lab = 1.4, cex.axis= 1, par(mar = c(17, 5, 4, 2) + 0.1) ) 
segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "1", lwd = 1)
points(Tmaze$PI ~ Tmaze$Fly.line)

boxplot(Tmaze$PI1 ~ Tmaze$Fly.line, ylab = "PI", las =2 , ylim = c(-1,1),col= as.character(idGroup$Colour),main="PI first repetition", cex.lab = 1.4, cex.axis= 1, par(mar = c(17, 5, 4, 2) + 0.1)) 
segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "blue", lwd = 1)
points(Tmaze$PI1 ~ Tmaze$Fly.line)

boxplot(Tmaze$PI2 ~ Tmaze$Fly.line, ylab = "PI", las =2 , ylim = c(-1,1),col= as.character(idGroup$Colour),main="PI second repetition", cex.lab = 1.4, cex.axis= 1, par(mar = c(17, 5, 4, 2) + 0.1))
segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "blue", lwd = 1)
points(Tmaze$PI2 ~ Tmaze$Fly.line)

boxplot(Tmaze$PI3 ~ Tmaze$Fly.line, ylab = "PI", las =2 , ylim = c(-2,2),col= as.character(idGroup$Colour),main="PI as a difference from two repetitions", cex.lab = 1.4, cex.axis= 1, par(mar = c(17, 5, 4, 2) + 0.1))
segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "blue", lwd = 1)
points(Tmaze$PI3 ~ Tmaze$Fly.line)

boxplot(Tmaze$weightedPI ~ Tmaze$Fly.line, ylab = "PI", las =2 , ylim = c(-1,1),col= as.character(idGroup$Colour), main="number of flies-weighted PI as a whole",cex.lab = 1.4, cex.axis= 1, par(mar = c(17, 5, 4, 2) + 0.1))
segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "blue", lwd = 1)
points(Tmaze$weightedPI ~ Tmaze$Fly.line)

boxplot(Tmaze$weightedPI1 ~ Tmaze$Fly.line, ylab = "PI", las =2 , ylim = c(-1,1),col= as.character(idGroup$Colour), main="number of flies-weighted PI first rep",cex.lab = 1.4, cex.axis= 1, par(mar = c(17, 5, 4, 2) + 0.1))
segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "blue", lwd = 1)
points(Tmaze$weightedPI1 ~ Tmaze$Fly.line)

boxplot(Tmaze$weightedPI2 ~ Tmaze$Fly.line, ylab = "PI", las =2 , ylim = c(-1,1),col= as.character(idGroup$Colour), main="number of flies-weighted PI secon rep",cex.lab = 1.4, cex.axis= 1, par(mar = c(17, 5, 4, 2) + 0.1))
segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "blue", lwd = 1)
points(Tmaze$weightedPI2 ~ Tmaze$Fly.line)

dev.off()

#boxplot(data$PI ~ data$Fly.line, ylab = "PI", mtext(n, side = 2,line = 8), las=2, at =c(1,2, 4,5, 7,8, 10, 12,13, 15,16, 18,19, 21,22, 24),par(mar = c(12, 5, 4, 2) + 0.1), 
#       col = c(c(rep(c("darkgoldenrod","darkgoldenrod1"),3),"darkgoldenrod",rep(c("darkgoldenrod","darkgoldenrod1"),4),"darkgoldenrod")))


#export <- write.csv(Tmaze,file="Tmaze.csv")


##### Import the csv file into R. If I dont have the data ordered in the way I wanted I could always reshape it with stack/unstack functions as well as with reshape function

Tmaze <- read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = "." )

nExp <- length (Tmaze[[1]])
nGroups <- length(levels(Tmaze[[1]]))
N<- table(Tmaze$Fly.line)

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
# Weights of PI1 and weights of PI2 according to the number of flies.
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


idGroup <- data.frame("Group"=levels(Tmaze[[1]]),"Treatment"= ifelse(grepl("ATR", levels(Tmaze[[1]]), ignore.case = T), "Experimental ATR", ifelse (grepl("Co", levels(Tmaze[[1]]), ignore.case = T), "Experimental Co", "Genetic Control")),
                      "Colour"=ifelse(grepl("ATR", levels(Tmaze[[1]]), ignore.case = T), "darkgoldenrod", ifelse (grepl("Co", levels(Tmaze[[1]]), ignore.case = T), "darkgoldenrod1", "darkgreen")))

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

idGroup$std <- sapply(seq_len(nrow(idGroup)), function(i) { 
  sd(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
})

idGroup$experiments_n <- sapply(seq_len(nrow(idGroup)), function(i) { 
  length(Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line])
})

idGroup$se <- idGroup$std / sqrt(idGroup$experiments_n)


##### To order the groups for plotting. This will only work nicely if I put a "1" in front of my control line so that it put it the first

library(dplyr)

idGroup$Treatment <- ordered(idGroup$Treatment, levels = c("Genetic Control", "Experimental ATR", "Experimental Co")) 
idGroup <- idGroup[order(idGroup$Treatment), ]


idGroup$rank <- (idGroup$Treatment!="Genetic Control")*1
idGroup <- idGroup %>% group_by(LINE) %>% mutate(temp=mean(mean)) %>% 
  ungroup %>% arrange(rank, -temp) %>% select(-rank, -temp)


###### Order the Tmaze data in the way the idGroup table is ordered. It looks fine in the Global environment and in the plots. However opening the table the order isn´t there

levels <- as.character(idGroup$Group)
Tmaze$Fly.line <- factor(Tmaze$Fly.line, levels = levels)

message("Enter the name of the output file")
output<- scan(n=1,what= character())
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

setEPS()
postscript("redTmaze_barplot.eps")
par(mar=c(9,4,1,1))
barCenters <- barplot(height = idGroup$mean,
                      names.arg = as.character(idGroup$LINE),
                      beside = true, las = 2,ylim=c(-1,1),
                      cex.names = 0.75,
                      main = "Screen T-maze with red light",
                      ylab = "PI",
                      border = "black", axes = TRUE)

segments(barCenters, idGroup$mean - idGroup$se, barCenters,
         idGroup$mean + idGroup$se, lwd = 1.5)
dev.off()
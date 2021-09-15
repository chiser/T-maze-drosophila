##### Import the csv file into R

Tmaze <- read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = "," )

nExp <- length (Tmaze[[1]])
nGroups <- length(levels(Tmaze[[1]]))


# I get errors every time with apply functions...I forget it for the momment
#Tmaze$PI <- apply(Tmaze,1,function(Tmaze=Tmaze) (Tmaze[2]-Tmaze[4]))

#Tmaze$PI <- (Tmaze$Red-Tmaze$Dark)/(Tmaze$Red+Tmaze$Dark) 

###### A less efficient way of calculating PIs

Tmaze$PI <- vector("numeric", length = nExp)
          for(i in 1:nExp){
                                    Tmaze$PI[i] <- (Tmaze[[i,3]]-Tmaze[[i,5]])/(Tmaze[[i,3]]+Tmaze[[i,5]])                 
          }

###### This is in order to make groups according to their names in the case of fly food. So that they can be assigned a different colour in the plot for instance. pmatch should do the same
#### A factor level to sort the ones with ATR and without in the experimental group and the genetic controls


Tmaze$Treatment2 <- ifelse(grepl("ATR", Tmaze[[1]], ignore.case = T), "Experimental ATR", ifelse (grepl("Co", Tmaze[[1]], ignore.case = T), "Experimental Co", "Genetic Control"))

#############Another way of doing it
#dataATR <- grep("ATR",data[[1]])
#dataCo <- grep ("Co", data[[1]])
#dataGenetic <- grep ("AUS", data[[1]])

idGroup <- data.frame ("Group"=levels(Tmaze[[1]]),"Treatment"= ifelse(grepl("TH>", levels(Tmaze[[1]]), ignore.case = T), "Positive Control",ifelse(grepl("Tdc2", levels(Tmaze[[1]]), ignore.case = T), "Positive Control", ifelse(grepl("ATR", levels(Tmaze[[1]]), ignore.case = T), "Experimental ATR", ifelse (grepl("Co", levels(Tmaze[[1]]), ignore.case = T), "Experimental Co", "Genetic Control")))),
                      "Colour"=ifelse(grepl("ATR", levels(Tmaze[[1]]), ignore.case = T), "darkgoldenrod", ifelse (grepl("Co", levels(Tmaze[[1]]), ignore.case = T), "darkgoldenrod1", "darkgreen")))

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

idGroup$Treatment <- ordered(idGroup$Treatment, levels = c("Genetic Control", "Positive Control", "Experimental ATR", "Experimental Co")) 
idGroup <- idGroup[order(idGroup$Treatment), ]


idGroup$rank <- ifelse (idGroup$Treatment =="Positive Control", 1, ifelse (idGroup$Treatment =="Genetic Control", 0, 2))

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

#boxplot(data$PI ~ data$Fly.line, ylab = "PI", las=2, at =c(1,2, 4,5, 7,8, 10, 12,13, 15,16, 18,19, 21,22, 24),par(mar = c(12, 5, 4, 2) + 0.1), 
#col = c(c(rep(c("darkgoldenrod","darkgoldenrod1"),3),"darkgoldenrod",rep(c("darkgoldenrod","darkgoldenrod1"),4),"darkgoldenrod")))

boxplot(Tmaze$PI ~ Tmaze$Fly.line, ylab = "PI", las =2 , ylim = c(-1,1),col= as.character(idGroup$Colour), cex.axis =1.2, cex.lab = 1.2 , par(mar = c(17, 8, 1, 5) + 0.1)) + segments(x0 = 0, y0 = 0, x1 = 30, y1 = 0, col = "blue", lwd = 1)


#boxplot(data$PI ~ data$Fly.line, ylab = "PI", mtext(n, side = 2,line = 8), las=2, at =c(1,2, 4,5, 7,8, 10, 12,13, 15,16, 18,19, 21,22, 24),par(mar = c(12, 5, 4, 2) + 0.1), 
#       col = c(c(rep(c("darkgoldenrod","darkgoldenrod1"),3),"darkgoldenrod",rep(c("darkgoldenrod","darkgoldenrod1"),4),"darkgoldenrod")))


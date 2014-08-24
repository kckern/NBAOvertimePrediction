library(randomForest)
library(rpart.plot)
library(rpart)
library(caTools)
library(randomForest)
library(AUC)
library(stringi)




#setwd("C:\\Users\\KC\\Dropbox\\School\\CurrentSemester\\15.071 The Analytics Edge\\Project")
setwd("/Users/kckern/Dropbox/School/CurrentSemester/15.071 The Analytics Edge/Project")
NBA =  read.csv("NBA.csv", stringsAsFactors=FALSE)


NBA$playoffs = ifelse(NBA$doy >= 115 & NBA$doy <= 225 , 1, 0)


#Calculate Quarter Differences (Positive=Home up, Negative = Away Up)
NBA$d1=abs(NBA$h1-NBA$a1)
NBA$d2=abs((NBA$h1+NBA$h2)-(NBA$a1+NBA$a2))
NBA$d3=abs((NBA$h1+NBA$h2+NBA$h3)-(NBA$a1+NBA$a2+NBA$a3))

#Calculate Difference in Points Left to Score
NBA$pl1=abs((NBA$hPSG-NBA$h1)-(NBA$aPSG-NBA$a1))
NBA$pl2=abs((NBA$hPSG-NBA$h2)-(NBA$aPSG-NBA$a2))
NBA$pl3=abs((NBA$hPSG-NBA$h3)-(NBA$aPSG-NBA$a3))

#Calculate Difference in Stats and Standings
NBA$dWins = abs(NBA$hW-NBA$aW)   	#Wins
NBA$dLosses = abs(NBA$hL-NBA$aL) 	#Losses
NBA$dPerc = abs(NBA$hP-NBA$aP) 		#Percent
NBA$dGB = abs(NBA$hGB-NBA$aGB) 		#Games Behind
NBA$dPSG = abs(NBA$hPSG-NBA$aPSG) 	#Points Scored per Game
NBA$dPAG = abs(NBA$hPAG-NBA$aPAG) 	#Points Allowed per Game

if(FALSE){
#Calculate Difference in Stats and Standings
NBA$dWins = (NBA$hW-NBA$aW)   	#Wins
NBA$dLosses = (NBA$hL-NBA$aL) 	#Losses
NBA$dPerc = (NBA$hP-NBA$aP) 		#Percent
NBA$dGB = (NBA$hGB-NBA$aGB) 		#Games Behind
NBA$dPSG = (NBA$hPSG-NBA$aPSG) 	#Points Scored per Game
NBA$dPAG = (NBA$hPAG-NBA$aPAG) 	#Points Allowed per Game


NBA$zd1 = round(scale(NBA$d1),1)
NBA$zd2 = round(scale(NBA$d2),1)
NBA$zd3 = round(scale(NBA$d3),1)

NBA$zpl1 = round(scale(NBA$pl1),1)
NBA$zpl2 = round(scale(NBA$pl2),1)
NBA$zpl3 = round(scale(NBA$pl3),1)

NBA$zdWins = round(scale(NBA$dWins),1)
NBA$zdLosses = round(scale(NBA$dLosses),1)
NBA$zdPerc = round(scale(NBA$dPerc),1)
NBA$zdGB = round(scale(NBA$dGB),1)
NBA$zdPSG = round(scale(NBA$dPSG),1)
NBA$zdPAG = round(scale(NBA$dPAG),1)

}



#Train/Test Splitting
set.seed(1000)
split = sample.split(NBA$OT, SplitRatio = 0.5)
train = subset(NBA, split==TRUE)
test = subset(NBA, split==FALSE)


funcGLM <- function(formula){
  
  
  trainModel   = glm(formula,data=train)
  testModel   = glm(formula,data=test)
  
  actualP   	= predict(testModel, newdata = test)
  predctP 		= predict(trainModel, newdata = test)
  
  actualP 		= round(100*actualP,1)
  predctP 		= round(100*predctP,1)
  diff 			= abs(actualP-predctP)
  
  test$pred = predctP
  test$actu = actualP
  
  assign("test", test, envir = .GlobalEnv)
  
  avgdiff 		= round(mean(diff),3)
  sd 			= round(sd(predctP),3)
  mean 			= round(mean(predctP),3)
  
  
  formula=Reduce(paste, deparse(formula))
  time = "Pre";
  if(stri_detect_fixed(formula, "d1")) time = "Q1"
  if(stri_detect_fixed(formula, "pl1")) time = "Q1"
  if(stri_detect_fixed(formula, "d2")) time = "Q2"
  if(stri_detect_fixed(formula, "pl2")) time = "Q2"
  if(stri_detect_fixed(formula, "d3")) time = "Q3"
  if(stri_detect_fixed(formula, "pl3")) time = "Q3"
  assign("results", rbind(results,data.frame(method="LogReg", time ,  formula, mean=mean, stdv=sd, error=avgdiff)), envir = .GlobalEnv)
  
}
funcCART <- function(formula){
  
  
  trainModel   = rpart(formula,data=train,cp=0.0001,minbucket=100)
  testModel   = rpart(formula,data=test,cp=0.0001,minbucket=100)
  
  actualP     = predict(testModel, newdata = test)
  predctP 		= predict(trainModel, newdata = test)
  
  actualP 		= round(100*actualP,1)
  predctP 		= round(100*predctP,1)
  diff 			= abs(actualP-predctP)
  
  avgdiff 		= round(mean(diff),3)
  sd 			= round(sd(predctP),3)
  mean 			= round(mean(predctP),3)
  
  
  formula=Reduce(paste, deparse(formula))
  time = "Pre";
  if(stri_detect_fixed(formula, "d1")) time = "Q1"
  if(stri_detect_fixed(formula, "pl1")) time = "Q1"
  if(stri_detect_fixed(formula, "d2")) time = "Q2"
  if(stri_detect_fixed(formula, "pl2")) time = "Q2"
  if(stri_detect_fixed(formula, "d3")) time = "Q3"
  if(stri_detect_fixed(formula, "pl3")) time = "Q3"
  assign("results", rbind(results,data.frame(method="CART", time ,  formula, mean=mean, stdv=sd, error=avgdiff)), envir = .GlobalEnv)
  
}

funcRF <- function(formula){
  
  
  trainModel   = randomForest(formula,data=train)
  testModel   = randomForest(formula,data=test)
  
  actualP     = predict(testModel, newdata = test)
  predctP 		= predict(trainModel, newdata = test)
  
  actualP 		= round(100*actualP,1)
  predctP 		= round(100*predctP,1)
  diff 			= abs(actualP-predctP)
  
  avgdiff 		= round(mean(diff),3)
  sd 			= round(sd(predctP),3)
  mean 			= round(mean(predctP),3)
  
  
  formula=Reduce(paste, deparse(formula))
  time = "Pre";
  if(stri_detect_fixed(formula, "d1")) time = "Q1"
  if(stri_detect_fixed(formula, "pl1")) time = "Q1"
  if(stri_detect_fixed(formula, "d2")) time = "Q2"
  if(stri_detect_fixed(formula, "pl2")) time = "Q2"
  if(stri_detect_fixed(formula, "d3")) time = "Q3"
  if(stri_detect_fixed(formula, "pl3")) time = "Q3"
  assign("results", rbind(results,data.frame(method="RF", time ,  formula, mean=mean, stdv=sd, error=avgdiff)), envir = .GlobalEnv)
  
}
    

funcPlot <- function(data){
  
  
  hist(data, breaks=50, freq=FALSE,  main="Probability of Overtime", col="cadetblue2", xlim=c(0,15),ylim=c(0,.7))
  curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col="darkblue", lwd=2) 
  
  d <- density(data)
  plot(d, main="",xlim=c(0,25),ylim=c(0,.8))
  polygon(d, col="cadetblue2", border="cadetblue4")
  curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col="darkblue", lwd=2) 
  
  axis(1, tck=1, col.ticks="light gray")
  axis(1, tck=-0.015, col.ticks="black")
  axis(2, tck=1, col.ticks="light gray", lwd.ticks="1")
  axis(2, tck=-0.015)
  
  box()
  
  
}


funcBoxPlot <- function(pre,q1,q2,q3,baseline){
  
  
  
  boxplot(q3,q2,q1,pre,baseline,horizontal=TRUE,col=c("darkseagreen4","darkseagreen3","darkseagreen2","darkseagreen1","darkseagreen1"),names=c("3rd Quarter","2nd Quarter","1st Quarter","Pre Game","Baseline"), xlab="Probability",ylab="Time of Assesment",ylim=c(0,25))
  grid(nx = 0, ny = 1, col = "black", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  
  
  
  
}



results = data.frame();

funcGLM(OT~dPerc+playoffs+dGB)
funcGLM(OT~dPerc+playoffs+dGB+pl1+d1)
funcGLM(OT~dPerc+playoffs+dGB+pl1+pl1+d2+pl2)
funcGLM(OT~dPerc+playoffs+dGB+pl1+pl1+d2+pl2+pl3+d3)

funcRF(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG)
funcRF(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+d1)
funcRF(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2)
funcRF(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2+pl3+d3)

funcCART(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG)
funcCART(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+d1)
funcCART(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2)
funcCART(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2+pl3+d3)


rpart(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2+pl3+pl1+d3,data=NBA,minsplit=1)


results$zStdv = round(scale(results$stdv),2)
results$zError = -round(scale(results$error),2)
results$score = round(results$stdv*(1/results$error),2)
results$zScore = round((results$zStdv+results$zError)/2,2)

write.table(results, file = "results.csv",sep = ",")









#LOGISTIC REGRESSION
  NBA$LGpre = predict(glm(OT~dPerc+playoffs+dGB,data=NBA),data=NBA)
  NBA$LGq1 = predict(glm(OT~dPerc+playoffs+dGB+pl1+d1,data=NBA),data=NBA)
  NBA$LGq2 = predict(glm(OT~dPerc+playoffs+dGB+pl1+pl1+d2+pl2,data=NBA),data=NBA)
  NBA$LGq3 = predict(glm(OT~dPerc+playoffs+dGB+pl1+pl1+d2+pl2+pl3+d3,data=NBA),data=NBA)

  NBA$LGpre =round(100*NBA$LGpre,1)
  NBA$LGq1 =round(100*NBA$LGq1,1)
  NBA$LGq2 =round(100*NBA$LGq2,1)
  NBA$LGq3 =round(100*NBA$LGq3,1)
  
  summary(NBA$LGpre);
  sd(NBA$LGpre);
  summary(NBA$LGq1);
  sd(NBA$LGq1);
  summary(NBA$LGq2);
  sd(NBA$LGq2);
  summary(NBA$LGq3);
  sd(NBA$LGq3);
  
  funcBoxPlot(NBA$LGpre,NBA$LGq1,NBA$LGq2,NBA$LGq3,round(100*nrow(subset(subset(NBA, 1==1), OT==1))/nrow(subset(NBA, 1==1)),3))
  funcPlot(NBA$LGpre)
  funcPlot(NBA$LGq1)
  funcPlot(NBA$LGq2)
  funcPlot(NBA$LGq3)

#RANDOM FOREST
NBA$RFpre = predict(randomForest(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG,data=NBA),data=NBA)
NBA$RFq1 = predict(randomForest(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1,data=NBA),data=NBA)
NBA$RFq2 = predict(randomForest(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2,data=NBA),data=NBA)
NBA$RFq3 = predict(randomForest(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2+pl3+d3,data=NBA),data=NBA)

NBA$RFpre =round(100*NBA$RFpre,1)
NBA$RFq1 =round(100*NBA$RFq1,1)
NBA$RFq2 =round(100*NBA$RFq2,1)
NBA$RFq3 =round(100*NBA$RFq3,1)

summary(NBA$RFpre);
sd(NBA$RFpre);
summary(NBA$RFq1);
sd(NBA$RFq1);
summary(NBA$RFq2);
sd(NBA$RFq2);
summary(NBA$RFq3);
sd(NBA$RFq3);

funcBoxPlot(NBA$RFpre,NBA$RFq1,NBA$RFq2,NBA$RFq3,round(100*nrow(subset(subset(NBA, 1==1), OT==1))/nrow(subset(NBA, 1==1)),3))
funcPlot(NBA$RFpre)
funcPlot(NBA$RFq1)
funcPlot(NBA$RFq2)
funcPlot(NBA$RFq3)



#CART
NBA$CRpre = predict(rpart(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG,data=NBA,cp=0.0001,minbucket=100),data=NBA)
NBA$CRq1 = predict(rpart(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1,data=NBA,cp=0.0001,minbucket=100),data=NBA)
NBA$CRq2 = predict(rpart(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2,data=NBA,cp=0.0001,minbucket=100),data=NBA)
NBA$CRq3 = predict(rpart(OT~dPerc+playoffs+dGB+dWins+dLosses+dPSG+dPAG+pl1+pl1+d2+pl2+pl3+d3,data=NBA,cp=0.0001,minbucket=100),data=NBA)


NBA$CRpre =round(100*NBA$CRpre,1)
NBA$CRq1 =round(100*NBA$CRq1,1)
NBA$CRq2 =round(100*NBA$CRq2,1)
NBA$CRq3 =round(100*NBA$CRq3,1)

summary(NBA$CRpre);
sd(NBA$CRpre);
summary(NBA$CRq1);
sd(NBA$CRq1);
summary(NBA$CRq2);
sd(NBA$CRq2);
summary(NBA$CRq3);
sd(NBA$CRq3);

funcBoxPlot(NBA$CRpre,NBA$CRq1,NBA$CRq2,NBA$CRq3,round(100*nrow(subset(subset(NBA, 1==1), OT==1))/nrow(subset(NBA, 1==1)),3))
funcPlot(NBA$CRpre)
funcPlot(NBA$CRq1)
funcPlot(NBA$CRq2)
funcPlot(NBA$CRq3)




write.table(NBA, file = "nba_pred.csv",sep = ",")



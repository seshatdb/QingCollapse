###Qing SDT Study PSI###
library(PerformanceAnalytics)
library(ggplot2)
# make sure your working directory is a folder that contains
# 1) this R-script
# 2) QingDatasets folder that contains the data files
#
# To check the current working directory:
# getwd()
#############################################################################
{###  MMP
  agri_dat<-read.csv('./QingDatasets/CrisisDB_agricultural_data.csv', sep=",", header=TRUE,quote = "",row.names = NULL,stringsAsFactors = F) #read Qing agri data from Georg dataset
  agri_dat$Agricultural.population..1000./agri_dat$Total.Population..1000. ### Agri pop at constant 85%
  agri_dat <- agri_dat[,c(1,2,4)]
  colnames(agri_dat) <- c("Year", "Population", "Arable")
  pop_dat<-read.csv('./QingDatasets/clio_pop_data.csv') #read Qing Pop data from ClioInfra
  pop_dat <- pop_dat[complete.cases(pop_dat),4:5]
  colnames(pop_dat) <- c("Year", "Population")
  # pop_dat <- rbind(pop_dat, agri_dat[,1:2])
  agri_dat <- rbind(agri_dat[,c(1,3)], c(1910,1130000)) ### Extrapolate to 1910
  
  PSI <- data.frame(Year=seq(1640, 1910, by=10))
  { ### Population
    reslt <- loess(Population ~ Year, data = pop_dat, span = 0.25)
    PSI$Population <- predict(reslt, PSI$Year)
    plot(pop_dat, pch=16)
    lines(PSI, lwd=2)
  }
  
  { ### Arable land in mu
    plot(agri_dat, pch=16)
    reslt <- loess(Arable ~ Year, data = agri_dat, span = 1)
    PSI$Arable <- predict(reslt, PSI$Year)
    lines(subset(PSI, select = c("Year", "Arable")), lwd=2)
  }
  { ### MMP = 1/(land/peasant)
    PSI$MMP <- 1/(PSI$Arable/PSI$Population)
    plot(subset(PSI, select = c("Year", "MMP")), ty="l", lwd=2)
  }
}

#############################################################################
{###  EMP
  {### Number of jinshi degrees
    seshat_dat<-read.csv('./QingDatasets/CrisisDB_data.csv', sep=",", header=TRUE,quote = "",row.names = NULL,stringsAsFactors = F) #read Qing dataset from CrisisDB SQL
    jinshi_dat <- seshat_dat[seshat_dat$variable_sub_name == "jinshi_degrees_awarded",5:4]
    colnames(jinshi_dat) <- c("Year", "Degrees")
    plot(jinshi_dat, pch=16)
    reslt <- loess(Degrees ~ Year, data = jinshi_dat, span = 0.5)
    PSI$Degrees <- predict(reslt, PSI$Year)
    PSI$Degrees[1] <- PSI$Degrees[2]
    PSI$Degrees[nrow(PSI)] <- PSI$Degrees[nrow(PSI) - 1]
    lines(subset(PSI, select = c("Year", "Degrees")), lwd=2)
  }
  {### EMP = 1/(degrees per capita)
    PSI$EMP <- 1/(PSI$Degrees/PSI$Population)
    plot(subset(PSI, select = c("Year", "EMP")), ty="l", lwd=2)
  }
}

#############################################################################
{### SFD
  fisc_dat<-read.csv('./QingDatasets/qing_fiscal_balance.csv', sep=",", header=TRUE,quote = "",row.names = NULL,stringsAsFactors = F) #read Qing agri data from Georg dataset
  plot(fisc_dat$Year, fisc_dat$Revenue, pch=16)
  reslt <- loess(Revenue ~ Year, data = fisc_dat, span = 0.5)
  PSI$Revenue <- predict(reslt, PSI$Year)
  lines(subset(PSI, select = c("Year", "Revenue")), lwd=2)
  
  plot(fisc_dat$Year, fisc_dat$Expenses, pch=16)
  reslt <- loess(Expenses ~ Year, data = fisc_dat, span = 0.5)
  PSI$Expenses <- predict(reslt, PSI$Year)
  lines(subset(PSI, select = c("Year", "Expenses")), lwd=2)
  
  PSI$Balance <- PSI$Revenue - PSI$Expenses
  PSI$Balance[1:2] <- PSI$Balance[3]
  plot(PSI$Year, PSI$Balance/1000, ty="l", lwd=2, xlab="", ylab="Fiscal Balance, x1000 taels")
  abline(v=seq(1650,1900, by=50),  h=seq(-60,0,by=20), col="grey")
  
   PSI$SFD <- -PSI$Balance/PSI$Revenue
   # PSI$SFD <- PSI$Expenses/PSI$Revenue  ### Aternative (gives the same end result)
   PSI$SFD[1:2] <- PSI$SFD[3]
  plot(PSI$Year, PSI$SFD)
  plot(PSI$Year, PSI$SFD, ty="l", lwd=2, xlab="", ylab="SFD")
}

#############################################################################
{### Wars
  war_dat<-read.csv('./QingDatasets/qing_conflict_data.csv', sep=",", header=TRUE,quote = "",row.names = NULL,stringsAsFactors = F) 
  dat <- war_dat[war_dat$ConflictType == "Internal",5:6]
  Internal <- vector()
  for(i in 1:nrow(dat)){   Internal <- c(Internal,dat[i,1]:dat[i,2]) }
  PSI$InternWar <- NA
  for(i in 1:nrow(PSI)){
    dt <- Internal[Internal >= PSI$Year[i]-5 & Internal < PSI$Year[i]+5 ]
    PSI$InternWar[i] <- length(dt)
  }
  
  plot(PSI$Year, PSI$InternWar, ty="l", lwd=2)
  
  dat <- war_dat[war_dat$ConflictType == "External",5:6]
  External <- vector()
  for(i in 1:nrow(dat)){   External <- c(External,dat[i,1]:dat[i,2]) }
  PSI$ExternWar <- NA
  for(i in 1:nrow(PSI)){
    dt <- Internal[External >= PSI$Year[i]-5 & External < PSI$Year[i]+5 ]
    PSI$ExternWar[i] <- length(dt)
  }
  lines(PSI$Year, PSI$ExternWar, lwd=2, col="brown")
  
  #### External Wars for the manuscript figure
  plot(PSI$Year, PSI$ExternWar, ty="l", lwd=2, xlab="", ylab="External Wars")
}


#############################################################################
### Disasters
{### Famines
  dat<-read.csv('./QingDatasets/qing_famine_data.csv', sep=",", header=TRUE,quote = "",row.names = NULL,stringsAsFactors = F) 
  PSI$Famine <- NA
  for(i in 1:nrow(PSI)){
    dt <- dat[dat$Year >= PSI$Year[i] - 5 & dat$Year < PSI$Year[i] + 5,]
    PSI$Famine[i] <- sum(dt[,2])
  }
  dat<-read.csv('./QingDatasets/qing_drought_data.csv', sep=",", header=TRUE,quote = "",row.names = NULL,stringsAsFactors = F) 
  PSI$Drought <- NA
  for(i in 1:nrow(PSI)){
    dt <- dat[dat$Year >= PSI$Year[i] - 5 & dat$Year < PSI$Year[i] + 5,]
    PSI$Drought[i] <- sum(dt[,2])
  }
  dat<-read.csv('./QingDatasets/qing_crop_data.csv', sep=",", header=TRUE,quote = "",row.names = NULL,stringsAsFactors = F) 
  PSI$CropFailure <- NA
  for(i in 1:nrow(PSI)){
    dt <- dat[dat$Year >= PSI$Year[i] - 5 & dat$Year < PSI$Year[i] + 5,]
    PSI$CropFailure[i] <- sum(dt[,2])
  }
  
  plot(x=c(1640,1920), y=c(0, 800), ty="n", xaxt="n",yaxt="n", xlab="", ylab="Disasters") 
  axis(1, at=seq(1640, 1920, by=20))
  axis(2, at=seq(0,800, by=100))
  gdat <- subset(PSI, select = c(Year, Drought, Famine, CropFailure))
  gdat <- gdat[2:(nrow(gdat)-1),]  ### Drop the first and last decades as incomplete
  lines(gdat$Year, gdat$Drought, ty="l", lwd=2, col="brown")
  lines(gdat$Year, gdat$Famine, ty="l", lwd=2, col="red")
  lines(gdat$Year, gdat$CropFailure, ty="l", lwd=2, col="darkgreen")
  legend("topleft", c("Drought", "Famine", "Crop Failure"), bty="n", lwd=2, col=c("brown","red", "darkgreen"))
}

chart.Correlation(gdat, pch=16)

{### For the manuscript figure
  plot(x=c(1640,1910), y=c(0, 800), ty="n", xaxt="n",yaxt="n", xlab="", ylab="Disasters") 
  axis(1, at=seq(1640, 1920, by=20))
  axis(2, at=seq(0,800, by=100))
  gdat <- subset(PSI, select = c(Year, Drought, Famine, CropFailure))
  gdat <- gdat[2:(nrow(gdat)-1),]  ### Drop the first and last decades as incomplete
  lines(gdat$Year, gdat$Drought, ty="l", lwd=2, col="brown")
  lines(gdat$Year, gdat$Famine, ty="l", lwd=2, col="red")
  legend("topleft", c("Drought", "Famine"), bty="n", lwd=2, col=c("brown","red"))
}

{### Plot PSI components
  plot(x=c(1640,1920), y=c(0,1), ty="n", xaxt="n",yaxt="n", xlab="", ylab="Scaled Variables") 
  axis(1, at=seq(1640, 1920, by=20))
  axis(2, at=seq(0,1, by=0.1))
  gdat <- subset(PSI, select = c(Year, MMP, EMP, SFD, InternWar))
  for(i in 2:ncol(gdat)){gdat[,i] <- (gdat[,i]-min(gdat[,i]))/(max(gdat[,i])-min(gdat[,i])) }
  gdat$PSI <- gdat$MMP*gdat$EMP*gdat$SFD
  gdat$PSI <- (gdat$PSI-min(gdat$PSI))/(max(gdat$PSI)-min(gdat$PSI))
  PSI$PSI <- gdat$PSI
  
  lines(gdat$Year, gdat$MMP, ty="l", lwd=2, col="brown")
  lines(gdat$Year, gdat$EMP, ty="l", lwd=2, col="darkgreen")
  lines(gdat$Year, gdat$SFD, ty="l", lwd=2, col="blue")
  lines(gdat$Year, gdat$PSI, ty="l", lwd=3, col="darkgrey")
  legend("topleft", c("MMP", "EMP", "SFD", "PSI"), bty="n", lwd=2, 
         col=c("brown", "darkgreen", "blue", "darkgrey"))
}

{### Plot PSI and Internal War
  plot(x=c(1640,1920), y=c(0,1), ty="n", xaxt="n",yaxt="n", xlab="", ylab="Scaled Variables") 
  axis(1, at=seq(1640, 1920, by=20))
  axis(2, at=seq(0,1, by=0.1))
  abline(v=seq(1640,1920, by=20),  h=seq(0,1,by=0.2), col="grey")
  lines(gdat$Year, gdat$PSI, ty="l", lwd=3, col="darkgrey")
  lines(gdat$Year, gdat$InternWar, ty="l", lwd=2, col="red")
  legend("topleft", c("PSI", "InternWar"), bty="n", lwd=2, col=c("darkgrey","red"))
}

#################################################################################
#### Analysis
### Correlations
chart.Correlation(subset(PSI, select = c(MMP, EMP, SFD, PSI, Drought, ExternWar, InternWar)))

### Regressions: best (non-lagged) model
RD <- subset(PSI, select = c(InternWar, PSI))
print( res <- summary(fit <- lm(RD)) )

RD <- subset(PSI, select = c(InternWar,EMP,SFD))
print( res <- summary(fit <- lm(RD)) )

RD <- subset(PSI, select = c(InternWar, PSI, CropFailure))
print( res <- summary(lm(RD)))

### Best combination of PSI components as predictors
RD <- subset(PSI, select = c(InternWar, EMP, SFD))
print( res <- summary(fit <- lm(RD)) )  

### Lagged regression: lagged PSI is the best predictor of InternWar
RD <- data.frame(Inst1 = PSI$InternWar[2:nrow(PSI)])
RD$Inst <- PSI$InternWar[1:(nrow(PSI)-1)]
RD$PSI <- PSI$PSI[1:(nrow(PSI)-1)]
RD$Drought <- PSI$Drought[1:(nrow(PSI)-1)]
RD$Famine <- PSI$Famine[1:(nrow(PSI)-1)]
print( res <- summary(fit <- lm(RD)) )
print( res <- summary(fit <- lm(RD[,c(1,3)])) )

summary(lm(RD[,c("Inst","PSI","Drought","Famine")]))

### PSI as sum
RD <- subset(PSI, select = c(InternWar, PSI,Year))
print( res <- summary(fit <- lm(RD)) )
RD$PSI <- gdat$MMP + gdat$EMP + gdat$SFD
print( res <- summary(fit <- lm(RD)) )

###
test<-subset(PSI,select=c(InternWar,PSI,Year))
test$PSI.add<-gdat$MMP + gdat$EMP + gdat$SFD
ggplot()+
  geom_line(data=test,aes(x=Year,PSI),colour="blue")+
  geom_line(data=test,aes(x=Year,PSI.add),colour="red")

  
summary(lm(subset(PSI, select = c(InternWar, Drought, Famine, PSI))))

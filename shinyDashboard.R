library(shiny)
library(lubridate)
library(BH)
require(markdown)
require(data.table)
library(dplyr)
library(DT)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyAce)
library(sendmailR)
library(shiny)
library(stringr)
library(webshot)
library(leaflet)
library(htmlwidgets)
library(leaflet.extras)
library(openxlsx)
library(tidyverse)
library(lubridate)
options(scipen=999)
library(aws.s3)
options(warn = -1)

setwd('/home/dillon.ashburn2/RLOW/')
storeLatLong <- read.csv("StoreLatLon.csv")
load("finalRLOWImage.RData")
CalculationCodes <- read.xlsx("CalculationCodes.xlsx")
#type is grouping variable (vpo), master is dataset (vpoMaster),start1 and end1 are dates
calcAppOf1 <- function(type,master,start1,end1){
  
  name1 <- as.Date(start1)
  
  name3 <- as.Date(end1) + 6
  
  start1 = as.numeric(as.Date(start1))
  
  end1 = as.numeric(as.Date(end1))
  
  master = master %>% filter(Start_Num>=start1, Start_Num<=end1)
  
  numWeeks1 <<- ((end1 - start1) / 7) + 1
  
  final = unique(master[1])
  
  calcTemp = unique(master[1])
  
  names = colnames(master)
  
  for(j in names[2:length(names)]){
    
    testString = j
    
    temp = unlist(strsplit(testString, split=""))
    
    calcName <<- unlist(strsplit(testString, split='ZZ', fixed=TRUE))[1]
    
    for(i in 1:length(temp)){
      
      if(paste0(temp[i],temp[i+1]) == "ZZ"){
        
        calcIndicator = paste0(temp[i+2],temp[i+3],temp[i+4],temp[i+5])
        
      }
      
    }
    
    colnames(master)[colnames(master)==testString] <- "current"
    dependent = CalculationCodes$Dependent[CalculationCodes$Metric == calcName]
    dependentIndicator = CalculationCodes$Type[CalculationCodes$Metric == dependent]
    colnames(master)[colnames(master)==paste0(dependent,"ZZ",dependentIndicator)] <- "dependent"
    
    if(calcIndicator == "sAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = sAVG.1(current,Num_Stores))}
    if(calcIndicator == "tAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = tAVG.1(current,dependent))}
    if(calcIndicator == "rAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))}
    if(calcIndicator == "SUMS"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = SUMS.1(current))}
    if(calcIndicator == "MEAN"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = MEAN(current))}
    if(calcIndicator == "DATE"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = DATE(current))}
    if(calcIndicator == "mAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = mAVG.1(current,dependent))}
    if(calcIndicator == "smAV"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = smAV.1(current,Num_Stores))}
    if(calcIndicator == "PERC"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = PERC(current))}
    if(calcIndicator == "INFO"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = INFO(current))}
    if(calcIndicator == "mSUM"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = mSUM.1(current))}
    
    colnames(master)[colnames(master)=="current"] <- testString
    colnames(master)[colnames(master)=="dependent"] <- paste0(dependent,"ZZ",dependentIndicator)
    colnames(calcTemp)[2] <- calcName
    
    final = merge(final,calcTemp,by = type)
    
  }
  
  final = final %>% select(-Num_Stores) %>% select(-Start_Num)
  
  moneyCols4 = c(3,7,11,15,18,19,20)
  percCols4 = c(21)
  roundCols4 = c(2,4,5,6,8,9,10,12,13,14,16,17,22,23,27,31)
  
  
  return(final)
}
#type is grouping variable (vpo), master is dataset (vpoMaster),start1,start2, end1 and end2 are dates
calcAppOf2 <- function(type,master,start1,end1,start2,end2){
  
  name1 <<- as.Date(start1)
  name2 <<- as.Date(start2)
  name3 <<- as.Date(end1) + 6
  name4 <<- as.Date(end2) + 6
  
  start1 = as.numeric(as.Date(start1))
  start2 = as.numeric(as.Date(start2))
  end1 = as.numeric(as.Date(end1))
  end2 = as.numeric(as.Date(end2))
  
  master1 = master %>% filter(Start_Num>=start1, Start_Num<=end1)
  
  master2 = master %>% filter(Start_Num>=start2, Start_Num<=end2)
  
  numWeeks1 <<- ((end1 - start1) / 7) + 1
  
  numWeeks2 <<- ((end2 - start2) / 7) + 1
  
  final1 = unique(master[1])
  final2 = unique(master[1])
  
  calcTemp1 = unique(master[1])
  calcTemp2 = unique(master[1])
  
  names = colnames(master)
  
  for(j in names[2:length(names)]){
    
    testString = j
    
    temp = unlist(strsplit(testString, split=""))
    
    calcName <<- unlist(strsplit(testString, split='ZZ', fixed=TRUE))[1]
    
    for(i in 1:length(temp)){
      
      if(paste0(temp[i],temp[i+1]) == "ZZ"){
        
        calcIndicator = paste0(temp[i+2],temp[i+3],temp[i+4],temp[i+5])
        
      }
      
    }
    
    colnames(master1)[colnames(master1)==testString] <- "current"
    dependent = CalculationCodes$Dependent[CalculationCodes$Metric == calcName]
    dependentIndicator = CalculationCodes$Type[CalculationCodes$Metric == dependent]
    colnames(master1)[colnames(master1)==paste0(dependent,"ZZ",dependentIndicator)] <- "dependent"
    
    colnames(master2)[colnames(master2)==testString] <- "current"
    dependent = CalculationCodes$Dependent[CalculationCodes$Metric == calcName]
    dependentIndicator = CalculationCodes$Type[CalculationCodes$Metric == dependent]
    colnames(master2)[colnames(master2)==paste0(dependent,"ZZ",dependentIndicator)] <- "dependent"
    
    if(calcIndicator == "sAVG"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = sAVG.1(current,Num_Stores))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = sAVG.2(current,Num_Stores))
    }
    if(calcIndicator == "tAVG"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = tAVG.1(current,dependent))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = tAVG.2(current,dependent))
    }
    if(calcIndicator == "rAVG"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = rAVG.2(current,dependent))
    }
    if(calcIndicator == "SUMS"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = SUMS.1(current))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = SUMS.2(current))
    }
    if(calcIndicator == "MEAN"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = MEAN(current))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = MEAN(current))
    }
    if(calcIndicator == "DATE"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = DATE(current))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = DATE(current))
    }
    if(calcIndicator == "mAVG"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = mAVG.1(current,dependent))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = mAVG.2(current,dependent))
    }
    if(calcIndicator == "smAV"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = smAV.1(current,Num_Stores))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = smAV.2(current,Num_Stores))
    }
    if(calcIndicator == "PERC"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = PERC(current))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = PERC(current))
    }
    if(calcIndicator == "INFO"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = INFO(current))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = INFO(current))
    }
    if(calcIndicator == "mSUM"){
      calcTemp1 = master1 %>% group_by_(type) %>% summarise(tempName = mSUM.1(current))
      calcTemp2 = master2 %>% group_by_(type) %>% summarise(tempName = mSUM.2(current))
      
    }
    
    colnames(master1)[colnames(master1)=="current"] <- testString
    colnames(master1)[colnames(master1)=="dependent"] <- paste0(dependent,"ZZ",dependentIndicator)
    colnames(calcTemp1)[2] <- calcName
    
    colnames(master2)[colnames(master2)=="current"] <- testString
    colnames(master2)[colnames(master2)=="dependent"] <- paste0(dependent,"ZZ",dependentIndicator)
    colnames(calcTemp2)[2] <- calcName
    
    final1 = merge(final1,calcTemp1,by = type)
    
    final2 = merge(final2,calcTemp2,by = type)
    
  }
  
  final1 = final1[(!duplicated(final1[c(type)])),]
  final2 = final2[(!duplicated(final2[c(type)])),]
  
  final1[is.na(final1)] = 0
  final2[is.na(final2)] = 0
  
  for(i in 2:length(names(final1))) {
    for(j in 1:length(final1$Start_Date)){
      temp = paste0(final1[j,i]," | ",final2[j,i])
      final1[j,i] = temp
    }
    
  }
  
  final1 = final1 %>% select(-Start_Num,-Num_Stores,-Start_Date,-End_Date)
  
  return(final1)
}
calcAppLWs <- function(type,master){
  
  final = unique(master[1])
  
  calcTemp = unique(master[1])
  
  names = colnames(master)
  
  for(j in names[2:length(names)]){
    
    testString = j
    
    temp = unlist(strsplit(testString, split=""))
    
    calcName <<- unlist(strsplit(testString, split='ZZ', fixed=TRUE))[1]
    
    for(i in 1:length(temp)){
      
      if(paste0(temp[i],temp[i+1]) == "ZZ"){
        
        calcIndicator = paste0(temp[i+2],temp[i+3],temp[i+4],temp[i+5])
        
      }
      
    }
    
    colnames(master)[colnames(master)==testString] <- "current"
    dependent = CalculationCodes$Dependent[CalculationCodes$Metric == calcName]
    dependentIndicator = CalculationCodes$Type[CalculationCodes$Metric == dependent]
    colnames(master)[colnames(master)==paste0(dependent,"ZZ",dependentIndicator)] <- "dependent"
    
    if(calcIndicator == "sAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = sAVG.1(current,Num_Stores))}
    if(calcIndicator == "tAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))}
    if(calcIndicator == "rAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))}
    if(calcIndicator == "SUMS"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = SUMS.1(current))}
    if(calcIndicator == "MEAN"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = MEAN(current))}
    if(calcIndicator == "DATE"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = DATE(current))}
    if(calcIndicator == "mAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))}
    if(calcIndicator == "smAV"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = sAVG.1(current,Num_Stores))}
    if(calcIndicator == "PERC"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = MEAN(current))}
    if(calcIndicator == "INFO"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = INFO(current))}
    if(calcIndicator == "mSUM"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = SUMS.1(current))}
    
    colnames(master)[colnames(master)=="current"] <- testString
    colnames(master)[colnames(master)=="dependent"] <- paste0(dependent,"ZZ",dependentIndicator)
    colnames(calcTemp)[2] <- calcName
    
    final = merge(final,calcTemp,by = type)
    
  }
  
  final = final %>% select(-Num_Stores) %>% select(-Start_Num)
  
  return(final)
} 
#reference word doc for what these are doing
#some functions, like SUMS, are not exactly unique but provide rounding and formatting in them
#function.1 is divided by numWeeks1 (1st date range), function.2 is divided by numWeeks2 (2nd date range)
sAVG.1 <- function(var,Num_Stores){
  
  temp = as.numeric((sum(var*Num_Stores)/mean(Num_Stores))/numWeeks1)
  
  temp = round(temp,digits = 2)
  
  return(temp)
}
tAVG.1 <- function(var,dependent){
  
  return(time_converter(as.numeric(sum(var*dependent)/mean(dependent))/numWeeks1))
}
rAVG.1 <- function(var,dependent){
  
  temp = as.numeric((sum(var*dependent)/mean(dependent))/numWeeks1)
  
  temp = round(temp,digits = 2)
  
  return(temp)
  
}
SUMS.1 <- function(var){
  
  temp = sum(var,na.rm = TRUE)/numWeeks1
  
  temp = round(temp,digits = 2)
  
  return(temp)
}
sAVG.2 <- function(var,Num_Stores){
  
  temp = as.numeric((sum(var*Num_Stores)/mean(Num_Stores))/numWeeks2)
  
  temp = round(temp,digits = 2)
  
  return(temp)
}
tAVG.2 <- function(var,dependent){
  
  return(time_converter(as.numeric(sum(var*dependent)/mean(dependent))/numWeeks2))
}
rAVG.2 <- function(var,dependent){
  
  temp = as.numeric((sum(var*dependent)/mean(dependent))/numWeeks2)
  
  temp = round(temp,digits = 2)
  
  return(temp)
}
SUMS.2 <- function(var){
  
  temp = sum(var,na.rm = TRUE)/numWeeks2
  
  temp = round(temp,digits = 2)
  
  return(temp)
}
MEAN <- function(var){
  temp = mean(var)
  
  temp = round(temp, digits = 2)
  
  return(temp)
}
DATE <- function(var){
  
  if(calcName == "End_Date"){
    
    return(name3)
    
  }
  
  else{
    
    return(name1)
    
  }
}
mAVG.1 <- function(var,dependent){
  
  temp = as.numeric((sum(var*dependent)/mean(dependent))/numWeeks1)
  temp = round(temp,digits = 2)
  temp = paste0("$",temp)  
  return(temp)
}
smAV.1 <- function(var,Num_Stores){
  temp = as.numeric((sum(var*Num_Stores)/mean(Num_Stores))/numWeeks1)
  temp = round(temp,digits = 2)
  temp = paste0("$",temp)
  return(temp)
}
PERC <- function(var){
  temp = mean(var)
  temp = round(temp,digits = 4)
  temp = paste0(temp,"%")
  return(temp)
}
INFO <- function(var){
  return(var[1])
}
mAVG.2 <- function(var,dependent){
  temp = as.numeric((sum(var*dependent)/mean(dependent))/numWeeks2)
  temp = round(temp,digits = 2)
  temp = paste0("$",temp)  
  return(temp)
}
smAV.2 <- function(var,Num_Stores){
  temp = as.numeric((sum(var*Num_Stores)/mean(Num_Stores))/numWeeks2)
  temp = round(temp,digits = 2)
  temp = paste0("$",temp)
  return(temp)
}
mSUM.1 <- function(var){
  
  temp = sum(var,na.rm = TRUE)/numWeeks1
  
  temp = round(temp,digits = 2)
  
  temp = paste0("$",temp)
  
  return(temp)
  
}
mSUM.2 <- function(var){
  
  temp = sum(var,na.rm = TRUE)/numWeeks2
  
  temp = round(temp,digits = 2)
  
  temp = paste0("$",temp)
  
  return(temp)
  
}

#dynamicFuncPeriod and dynamicPeriods aren't actually used in app, but could be used for user input of periods and specific fiscal years
dynamicFuncPeriod <- function(data){
  result <- data %>%
    group_by(FISCAL_PERIOD_IN_YEAR,FISCAL_YEAR) %>%
    mutate(oosTransMeasured = (X..Transactions.Measured-FourWall...Transactions.Measured)) %>%
    summarise(Total_Cost_To_Store = sum(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Total_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE),
              Total_Num_Of_Deliveries = sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
              Weekday_Total_Cost_To_Store = sum(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Weekday_Total_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE),
              Weekday_Total_Num_Of_Deliveries = sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
              Saturday_Total_Cost_To_Store = sum(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Saturday_Total_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE),
              Saturday_Total_Num_Of_Deliveries = sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
              Sunday_Total_Cost_To_Store = sum(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Sunday_Total_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE),
              Sunday_Total_Num_Of_Deliveries = sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
              Total_Pro_Sales=sum(Professional_Sales,na.rm = TRUE),
              Total_Delivery_Sales=sum(Delivery_Sales,na.rm = TRUE),
              Total_Walk_In_Sales=sum(Walk_In_Sales,na.rm = TRUE),
              Total_Delivered_Trans=sum(Delivered_Trans,na.rm = TRUE),
              Weekly_Average_Cost_To_Store = mean(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Weekly_Average_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
              Weekly_Average_Num_Of_Deliveries = mean(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
              Weekday_Average_Cost_To_Store = mean(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Weekday_Average_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
              Weekday_Average_Num_Of_Deliveries = mean(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
              Saturday_Average_Cost_To_Store = mean(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Saturday_Average_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
              Saturday_Average_Num_Of_Deliveries = mean(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
              Sunday_Average_Cost_To_Store = mean(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
              Sunday_Average_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
              Sunday_Average_Num_Of_Deliveries = mean(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
              Weekly_Pro_Sales=mean(Professional_Sales,na.rm = TRUE),
              Weekly_Delivery_Sales=mean(Delivery_Sales,na.rm = TRUE),
              Weekly_Walk_In_Sales=mean(Walk_In_Sales,na.rm = TRUE),
              Avg_Delivered_Trans_Perc=mean(Delivered_Trans_Perc,na.rm = TRUE),
              Weekly_Delivered_Trans=mean(Delivered_Trans,na.rm = TRUE),
              Num_Of_Transactions = sum(X..Transactions.Measured,na.rm = TRUE),
              AvgTTS = time_converter(sum((X..Transactions.Measured * Avg..TTS),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
              AvgTTDrive = time_converter(sum((X..Transactions.Measured * Avg..Start.To.Delv.),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
              AvgTTFullfill = time_converter(sum((X..Transactions.Measured * Avg..Fulfill),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
              Num_Of_Transactions_4Wall = sum(FourWall...Transactions.Measured,na.rm = TRUE),
              AvgTTS_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..TTS),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
              AvgTTDrive_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Start.To.Delv.),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
              AvgTTFullfill_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Fulfill),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
              Num_Of_Transactions_OOS = sum(X..Transactions.Measured - FourWall...Transactions.Measured,na.rm = TRUE),
              AvgTTS_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..TTS),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
              AvgTTDrive_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Start.To.Delv.),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
              AvgTTFullfill_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Fulfill),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)))
  
  
  return(result)
}
dynamicPeriods <- function(data,period,year){
  temp <- data %>%
    filter(FISCAL_PERIOD_IN_YEAR==period,FISCAL_YEAR==year)
  
  result <- dynamicFuncPeriod(temp)
  
  moneyCols = c(2,5,8,11,14:16,18,21,24,27,30:32)+1
  percCols = c(33)+1
  roundCols = c(3,4,6,7,9,10,12,13,17,19,20,22,23,25,26,28,29,34)+1
  otherCols = c(1,35:46)+1
  
  result[is.na(result)] = 0
  
  for(j in moneyCols){
    
    for(i in 1:length(result$Total_Cost_To_Store)){
      
      temp = paste0("$",round(as.numeric(result[i,j]),digits = 2))
      
      result[i,j] = temp
      
    }
    
  }
  
  for(j in percCols){
    
    for(i in 1:length(result$Total_Cost_To_Store)){
      
      temp = paste0(100*round(as.numeric(result[i,j]),digits = 4),"%")
      
      result[i,j] = temp
      
    }
    
  }
  
  for(j in roundCols){
    
    for(i in 1:length(result$Total_Cost_To_Store)){
      
      temp = paste0(round(as.numeric(result[i,j]),digits = 2))
      
      result[i,j] = temp
      
    }
    
  }
  
  for(j in otherCols){
    
    for(i in 1:length(result$Total_Cost_To_Store)){
      
      temp = paste0(result[i,j])
      
      result[i,j] = temp
      
    }
    
  }
  
  dfTranspose <- data.frame(t(result[-1]))
  colnames(dfTranspose) <- result[, 1]
  
  return(dfTranspose)
}

#creates LXW output dataset
masterMarketProTTSSummaryLW.v2 <- function(data){
  
  today = Sys.Date()
  data <- data %>%
    mutate(numericLastSunday = as.numeric(as.Date(Date_Start)))
  
  LW <- as.numeric(as.Date(lastSunday(today))) - 7
  L4Wstart <- lastSunday(today) - 35
  L4Wend <- lastSunday(today) - 8
  L12Wstart <- lastSunday(today) - 91
  L12Wend <- lastSunday(today) - 8
  
  LW = data %>%
    filter(data$numericLastSunday==LW)
  
  L4W = data %>%
    filter(data$numericLastSunday>=L4Wstart, data$numericLastSunday<=L4Wend)
  
  L12W = data %>%
    filter(data$numericLastSunday>=L12Wstart, data$numericLastSunday<=L12Wend)
  
  YTD <- subset(data, FISCAL_YEAR==year(today))
  yearWeek = max(YTD$FISCAL_WEEK_OF_YEAR,na.rm = TRUE)
  
  temp1 = LW  %>%
    summarise( #Time = 'LW',
      Total_Cost_To_Store = sum(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Total_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE),
      Total_Num_Of_Deliveries = sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Total_Cost_To_Store = sum(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekday_Total_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE),
      Weekday_Total_Num_Of_Deliveries = sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Total_Cost_To_Store = sum(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Saturday_Total_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE),
      Saturday_Total_Num_Of_Deliveries = sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Total_Cost_To_Store = sum(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Sunday_Total_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE),
      Sunday_Total_Num_Of_Deliveries = sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Total_Pro_Sales=sum(Professional_Sales,na.rm = TRUE),
      Total_Delivery_Sales=sum(Delivery_Sales,na.rm = TRUE),
      Total_Walk_In_Sales=sum(Walk_In_Sales,na.rm = TRUE),
      Total_Delivered_Trans=sum(Delivered_Trans,na.rm = TRUE),
      Weekly_Average_Cost_To_Store = mean(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekly_Average_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Average_Num_Of_Deliveries = mean(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Cost_To_Store = mean(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekday_Average_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Num_Of_Deliveries = mean(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Cost_To_Store = mean(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Saturday_Average_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Num_Of_Deliveries = mean(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Cost_To_Store = mean(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Sunday_Average_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Num_Of_Deliveries = mean(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Pro_Sales=mean(Professional_Sales,na.rm = TRUE),
      Weekly_Delivery_Sales=mean(Delivery_Sales,na.rm = TRUE),
      Weekly_Walk_In_Sales=mean(Walk_In_Sales,na.rm = TRUE),
      Avg_Delivered_Trans_Perc=mean(Delivered_Trans_Perc,na.rm = TRUE),
      Weekly_Delivered_Trans=mean(Delivered_Trans,na.rm = TRUE),
      Num_Of_Transactions = sum(X..Transactions.Measured,na.rm = TRUE),
      AvgTTS = time_converter(sum((X..Transactions.Measured * Avg..TTS),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive = time_converter(sum((X..Transactions.Measured * Avg..Start.To.Delv.),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill = time_converter(sum((X..Transactions.Measured * Avg..Fulfill),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_4Wall = sum(FourWall...Transactions.Measured,na.rm = TRUE),
      AvgTTS_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..TTS),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Start.To.Delv.),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Fulfill),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_OOS = sum(X..Transactions.Measured - FourWall...Transactions.Measured,na.rm = TRUE),
      AvgTTS_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..TTS),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTDrive_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Start.To.Delv.),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTFullfill_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Fulfill),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)))
  
  temp2 = L4W  %>%
    summarise(#Time = 'L4W',
      Total_Cost_To_Store = sum(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE)/4,
      Total_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/4,
      Total_Num_Of_Deliveries = sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE)/4,
      Weekday_Total_Cost_To_Store = sum(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE)/4,
      Weekday_Total_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/4,
      Weekday_Total_Num_Of_Deliveries = sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE)/4,
      Saturday_Total_Cost_To_Store = sum(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE)/4,
      Saturday_Total_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/4,
      Saturday_Total_Num_Of_Deliveries = sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE)/4,
      Sunday_Total_Cost_To_Store = sum(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE)/4,
      Sunday_Total_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/4,
      Sunday_Total_Num_Of_Deliveries = sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE)/4,
      Total_Pro_Sales=sum(Professional_Sales,na.rm = TRUE)/4,
      Total_Delivery_Sales=sum(Delivery_Sales,na.rm = TRUE)/4,
      Total_Walk_In_Sales=sum(Walk_In_Sales,na.rm = TRUE)/4,
      Total_Delivered_Trans=sum(Delivered_Trans,na.rm = TRUE)/4,
      Weekly_Average_Cost_To_Store = mean(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekly_Average_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Average_Num_Of_Deliveries = mean(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Cost_To_Store = mean(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekday_Average_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Num_Of_Deliveries = mean(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Cost_To_Store = mean(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Saturday_Average_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Num_Of_Deliveries = mean(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Cost_To_Store = mean(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Sunday_Average_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Num_Of_Deliveries = mean(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Pro_Sales=mean(Professional_Sales,na.rm = TRUE),
      Weekly_Delivery_Sales=mean(Delivery_Sales,na.rm = TRUE),
      Weekly_Walk_In_Sales=mean(Walk_In_Sales,na.rm = TRUE),
      Avg_Delivered_Trans_Perc=mean(Delivered_Trans_Perc,na.rm = TRUE),
      Weekly_Delivered_Trans=mean(Delivered_Trans,na.rm = TRUE),
      Num_Of_Transactions = sum(X..Transactions.Measured,na.rm = TRUE)/4,
      AvgTTS = time_converter(sum((X..Transactions.Measured * Avg..TTS),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive = time_converter(sum((X..Transactions.Measured * Avg..Start.To.Delv.),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill = time_converter(sum((X..Transactions.Measured * Avg..Fulfill),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_4Wall = sum(FourWall...Transactions.Measured,na.rm = TRUE)/4,
      AvgTTS_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..TTS),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Start.To.Delv.),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Fulfill),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_OOS = sum(X..Transactions.Measured - FourWall...Transactions.Measured,na.rm = TRUE)/4,
      AvgTTS_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..TTS),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTDrive_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Start.To.Delv.),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTFullfill_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Fulfill),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)))
  
  temp3 = L12W  %>%
    summarise(#Time = 'L4W',
      Total_Cost_To_Store = sum(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE)/12,
      Total_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/12,
      Total_Num_Of_Deliveries = sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE)/12,
      Weekday_Total_Cost_To_Store = sum(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE)/12,
      Weekday_Total_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/12,
      Weekday_Total_Num_Of_Deliveries = sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE)/12,
      Saturday_Total_Cost_To_Store = sum(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE)/12,
      Saturday_Total_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/12,
      Saturday_Total_Num_Of_Deliveries = sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE)/12,
      Sunday_Total_Cost_To_Store = sum(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE)/12,
      Sunday_Total_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/12,
      Sunday_Total_Num_Of_Deliveries = sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE)/12,
      Total_Pro_Sales=sum(Professional_Sales,na.rm = TRUE)/12,
      Total_Delivery_Sales=sum(Delivery_Sales,na.rm = TRUE)/12,
      Total_Walk_In_Sales=sum(Walk_In_Sales,na.rm = TRUE)/12,
      Total_Delivered_Trans=sum(Delivered_Trans,na.rm = TRUE)/12,
      Weekly_Average_Cost_To_Store = mean(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekly_Average_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Average_Num_Of_Deliveries = mean(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Cost_To_Store = mean(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekday_Average_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Num_Of_Deliveries = mean(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Cost_To_Store = mean(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Saturday_Average_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Num_Of_Deliveries = mean(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Cost_To_Store = mean(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Sunday_Average_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Num_Of_Deliveries = mean(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Pro_Sales=mean(Professional_Sales,na.rm = TRUE),
      Weekly_Delivery_Sales=mean(Delivery_Sales,na.rm = TRUE),
      Weekly_Walk_In_Sales=mean(Walk_In_Sales,na.rm = TRUE),
      Avg_Delivered_Trans_Perc=mean(Delivered_Trans_Perc,na.rm = TRUE),
      Weekly_Delivered_Trans=mean(Delivered_Trans,na.rm = TRUE),
      Num_Of_Transactions = sum(X..Transactions.Measured,na.rm = TRUE)/12,
      AvgTTS = time_converter(sum((X..Transactions.Measured * Avg..TTS),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive = time_converter(sum((X..Transactions.Measured * Avg..Start.To.Delv.),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill = time_converter(sum((X..Transactions.Measured * Avg..Fulfill),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_4Wall = sum(FourWall...Transactions.Measured,na.rm = TRUE)/12,
      AvgTTS_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..TTS),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Start.To.Delv.),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Fulfill),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_OOS = sum(X..Transactions.Measured - FourWall...Transactions.Measured,na.rm = TRUE)/12,
      AvgTTS_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..TTS),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTDrive_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Start.To.Delv.),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTFullfill_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Fulfill),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)))
  
  temp4 = YTD  %>%
    summarise(#Time = 'L4W',
      Total_Cost_To_Store = sum(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE)/yearWeek,
      Total_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/yearWeek,
      Total_Num_Of_Deliveries = sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE)/yearWeek,
      Weekday_Total_Cost_To_Store = sum(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE)/yearWeek,
      Weekday_Total_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/yearWeek,
      Weekday_Total_Num_Of_Deliveries = sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE)/yearWeek,
      Saturday_Total_Cost_To_Store = sum(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE)/yearWeek,
      Saturday_Total_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/yearWeek,
      Saturday_Total_Num_Of_Deliveries = sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE)/yearWeek,
      Sunday_Total_Cost_To_Store = sum(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE)/yearWeek,
      Sunday_Total_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/yearWeek,
      Sunday_Total_Num_Of_Deliveries = sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE)/yearWeek,
      Total_Pro_Sales=sum(Professional_Sales,na.rm = TRUE)/yearWeek,
      Total_Delivery_Sales=sum(Delivery_Sales,na.rm = TRUE)/yearWeek,
      Total_Walk_In_Sales=sum(Walk_In_Sales,na.rm = TRUE)/yearWeek,
      Total_Delivered_Trans=sum(Delivered_Trans,na.rm = TRUE)/yearWeek,
      Weekly_Average_Cost_To_Store = mean(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekly_Average_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Average_Num_Of_Deliveries = mean(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Cost_To_Store = mean(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekday_Average_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Average_Num_Of_Deliveries = mean(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Cost_To_Store = mean(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Saturday_Average_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Average_Num_Of_Deliveries = mean(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Cost_To_Store = mean(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Sunday_Average_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Average_Num_Of_Deliveries = mean(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Pro_Sales=mean(Professional_Sales,na.rm = TRUE),
      Weekly_Delivery_Sales=mean(Delivery_Sales,na.rm = TRUE),
      Weekly_Walk_In_Sales=mean(Walk_In_Sales,na.rm = TRUE),
      Avg_Delivered_Trans_Perc=mean(Delivered_Trans_Perc,na.rm = TRUE),
      Weekly_Delivered_Trans=mean(Delivered_Trans,na.rm = TRUE),
      Num_Of_Transactions = sum(X..Transactions.Measured,na.rm = TRUE)/yearWeek,
      AvgTTS = time_converter(sum((X..Transactions.Measured * Avg..TTS),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive = time_converter(sum((X..Transactions.Measured * Avg..Start.To.Delv.),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill = time_converter(sum((X..Transactions.Measured * Avg..Fulfill),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_4Wall = sum(FourWall...Transactions.Measured,na.rm = TRUE)/yearWeek,
      AvgTTS_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..TTS),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Start.To.Delv.),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Fulfill),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_OOS = sum(X..Transactions.Measured - FourWall...Transactions.Measured,na.rm = TRUE)/yearWeek,
      AvgTTS_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..TTS),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTDrive_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Start.To.Delv.),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTFullfill_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Fulfill),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)))
  
  
  #finalSummary = rbind(temp1,temp2,temp3,temp4)
  
  #df2 <- data.frame(t(finalSummary[-1]))
  #colnames(df2) <- finalSummary[, 1]
  
  moneyCols = c(2,5:7,9,12,15,18,21:23)-1
  percCols = c(24)-1
  roundCols = c(3,4,10,13,16,19,25)-1
  otherCols = c(8,11,14,17,20,26:37)-1
  
  temp1[is.na(temp1)] = 0
  temp2[is.na(temp2)] = 0
  temp3[is.na(temp3)] = 0
  temp4[is.na(temp4)] = 0
  
  for(j in moneyCols){
    
    for(i in 1:length(temp4$Total_Cost_To_Store)){
      
      temp = paste0("$",round(as.numeric(temp1[i,j]),digits = 2),
                    " | $",round(as.numeric(temp2[i,j]),digits = 2),
                    " | $",round(as.numeric(temp3[i,j]),digits = 2),
                    " | $",round(as.numeric(temp4[i,j]),digits = 2))
      
      temp4[i,j] = temp
      
    }
    
  }
  
  for(j in percCols){
    
    for(i in 1:length(temp4$Total_Cost_To_Store)){
      
      temp = paste0(100*round(as.numeric(temp1[i,j]),digits = 4),"% | ",
                    100*round(as.numeric(temp2[i,j]),digits = 4),"% | ",
                    100*round(as.numeric(temp3[i,j]),digits = 4),"% | ",
                    100*round(as.numeric(temp4[i,j]),digits = 4),"%")
      
      temp4[i,j] = temp
      
    }
    
  }
  
  for(j in roundCols){
    
    for(i in 1:length(temp4$Total_Cost_To_Store)){
      
      temp = paste0(round(as.numeric(temp1[i,j]),digits = 2)," | ",
                    round(as.numeric(temp2[i,j]),digits = 2)," | ",
                    round(as.numeric(temp3[i,j]),digits = 2)," | ",
                    round(as.numeric(temp4[i,j]),digits = 2))
      
      temp4[i,j] = temp
      
    }
    
  }
  
  for(j in otherCols){
    
    for(i in 1:length(temp4$Total_Cost_To_Store)){
      
      temp = paste0(temp1[i,j]," | ",
                    temp2[i,j]," | ",
                    temp3[i,j]," | ",
                    temp4[i,j])
      
      temp4[i,j] = temp
      
    }
    
  }
  
  df2 <- data.frame(t(temp4))
  colnames(df2) <- c("LW | L4W | L12W | YTD")
  
  
  # dfFinal = df2 %>%
  #   unite(LW,L4W,L12W,YTD, col='LW | L4W | L12W | YTD',sep = ' | ')
  
  return(df2)
  
}  
dynamicMarketProTTSSummaryLW.v2 <- function(data,type){
  
  today = Sys.Date()
  data <- data %>%
    mutate(numericLastSunday = as.numeric(as.Date(Date_Start))) 
  
  data$division = trimws(data$division)
  
  data <- data %>%
    filter(division == "North"|division == "South")
  
  
  temp1 = sumFunc(data,"LW",type)
  temp2 = sumFunc(data,"L4W",type)
  temp3 = sumFunc(data,"L12W",type)
  temp4 = sumFunc(data,"YTD",type)
  
  temp1[is.na(temp1)] = 0
  temp2[is.na(temp2)] = 0
  temp3[is.na(temp3)] = 0
  temp4[is.na(temp4)] = 0
  
  
  moneyCols = c(3,7,11,15,18,19,20)
  percCols = c(21)
  roundCols = c(2,4,5,6,8,9,10,12,13,14,16,17,22,23,27,31)
  otherCols = c(24,25,26,28,29,30,32,33,34)
  
  for(j in moneyCols){
    
    for(i in 1:length(temp4$Weekly_Average_Num_Of_Deliveries)){
      
      temp = paste0("$",round(as.numeric(temp1[i,j]),digits = 2),
                    " | $",round(as.numeric(temp2[i,j]),digits = 2),
                    " | $",round(as.numeric(temp3[i,j]),digits = 2),
                    " | $",round(as.numeric(temp4[i,j]),digits = 2))
      
      temp4[i,j] = temp
      
    }
    
  }
  for(j in percCols){
    
    for(i in 1:length(temp4$Weekly_Average_Num_Of_Deliveries)){
      
      temp = paste0(100*round(as.numeric(temp1[i,j]),digits = 4),"% | ",
                    100*round(as.numeric(temp2[i,j]),digits = 4),"% | ",
                    100*round(as.numeric(temp3[i,j]),digits = 4),"% | ",
                    100*round(as.numeric(temp4[i,j]),digits = 4),"%")
      
      temp4[i,j] = temp
      
    }
    
  }
  for(j in roundCols){
    
    for(i in 1:length(temp4$Weekly_Average_Num_Of_Deliveries)){
      
      temp = paste0(round(as.numeric(temp1[i,j]),digits = 2)," | ",
                    round(as.numeric(temp2[i,j]),digits = 2)," | ",
                    round(as.numeric(temp3[i,j]),digits = 2)," | ",
                    round(as.numeric(temp4[i,j]),digits = 2))
      
      temp4[i,j] = temp
      
    }
    
  }
  for(j in otherCols){
    
    for(i in 1:length(temp4$Weekly_Average_Num_Of_Deliveries)){
      
      temp = paste0(temp1[i,j]," | ",
                    temp2[i,j]," | ",
                    temp3[i,j]," | ",
                    temp4[i,j])
      
      temp4[i,j] = temp
      
    }
    
  }
  
  
  
  
  
  return(temp4)
  
}

#pulls and sets up LXW data. data var is master dataset var called "data", LWs = (LW,L4W,L12W,YTD), type is grouping variable
sumFunc <- function(data,LWs,type){
  if(LWs=="LW"){
    LW <- as.numeric(as.Date(lastSunday(today))) - 7
    dataLWs = data %>%
      filter(data$numericLastSunday==LW)
    wks = 1
  }
  if(LWs=="L4W"){
    L4Wstart <- lastSunday(today) - 35
    L4Wend <- lastSunday(today) - 8
    dataLWs = data %>%
      filter(data$numericLastSunday>=L4Wstart, data$numericLastSunday<=L4Wend)
    wks = 4
  }
  if(LWs == "L12W"){
    L12Wstart <- lastSunday(today) - 91
    L12Wend <- lastSunday(today) - 8
    dataLWs = data %>%
      filter(data$numericLastSunday>=L12Wstart, data$numericLastSunday<=L12Wend)
    wks = 12
  }
  if(LWs=="YTD"){
    YTD <- subset(data, FISCAL_YEAR==year(today))
    wks = max(YTD$FISCAL_WEEK_OF_YEAR,na.rm = TRUE)
    dataLWs <- subset(data, FISCAL_YEAR==year(today))
  }
  
  temp1 = dataLWs  %>%
    group_by_(type)%>%
    summarise(
      Weekly_Average_Num_Of_Deliveries = mean(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Total_Cost_To_Store = mean(WEEKLY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekly_Average_To_Miles = sum(WEEKLY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekly_Total_Num_Of_Deliveries = sum(WEEKLY_NUM_OF_DELIVERIES,na.rm = TRUE)/wks,
      Weekday_Average_Num_Of_Deliveries = mean(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Total_Cost_To_Store = mean(WEEKDAY_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Weekday_Average_To_Miles = sum(WEEKDAY_TOTAL_MILES,na.rm = TRUE)/sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE),
      Weekday_Total_Num_Of_Deliveries = sum(WEEKDAY_NUM_OF_DELIVERIES,na.rm = TRUE)/wks,
      Saturday_Average_Num_Of_Deliveries = mean(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Total_Cost_To_Store = mean(SAT_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Saturday_Average_To_Miles = sum(SAT_TOTAL_MILES,na.rm = TRUE)/sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE),
      Saturday_Total_Num_Of_Deliveries = sum(SAT_NUM_OF_DELIVERIES,na.rm = TRUE)/wks,
      Sunday_Average_Num_Of_Deliveries = mean(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Total_Cost_To_Store = mean(SUN_TOTAL_COST_TO_STORE,na.rm = TRUE),
      Sunday_Average_To_Miles = sum(SUN_TOTAL_MILES,na.rm = TRUE)/sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE),
      Sunday_Total_Num_Of_Deliveries = sum(SUN_NUM_OF_DELIVERIES,na.rm = TRUE)/wks,
      Weekly_Pro_Sales=mean(Professional_Sales,na.rm = TRUE),
      Weekly_Delivery_Sales=mean(Delivery_Sales,na.rm = TRUE),
      Weekly_Walk_In_Sales=mean(Walk_In_Sales,na.rm = TRUE),
      Avg_Delivered_Trans_Perc=mean(Delivered_Trans_Perc,na.rm = TRUE),
      Weekly_Delivered_Trans=mean(Delivered_Trans,na.rm = TRUE),
      Num_Of_Transactions = sum(X..Transactions.Measured,na.rm = TRUE)/wks,
      AvgTTS = time_converter(sum((X..Transactions.Measured * Avg..TTS),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive = time_converter(sum((X..Transactions.Measured * Avg..Start.To.Delv.),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill = time_converter(sum((X..Transactions.Measured * Avg..Fulfill),na.rm = TRUE)/sum(X..Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_4Wall = sum(FourWall...Transactions.Measured,na.rm = TRUE)/wks,
      AvgTTS_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..TTS),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTDrive_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Start.To.Delv.),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      AvgTTFullfill_4Wall = time_converter(sum((FourWall...Transactions.Measured * FourWall.Avg..Fulfill),na.rm = TRUE)/sum(FourWall...Transactions.Measured,na.rm = TRUE)),
      Num_Of_Transactions_OOS = sum(X..Transactions.Measured - FourWall...Transactions.Measured,na.rm = TRUE)/wks,
      AvgTTS_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..TTS),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTDrive_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Start.To.Delv.),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE)),
      AvgTTFullfill_OOS = time_converter(sum(((X..Transactions.Measured-FourWall...Transactions.Measured) * OOS.Avg..Fulfill),na.rm = TRUE)/sum((X..Transactions.Measured-FourWall...Transactions.Measured),na.rm = TRUE))
    )
  return(temp1)
}

#creates data input for LXW heatmap
dynamicHeatMap <- function(data,LWs){
  today = Sys.Date()
  data <- data %>%
    mutate(numericLastSunday = as.numeric(as.Date(Start_DateZZDATE))) %>%
    rename(LatitudeZZINFO = Latitude, LongitudeZZINFO = Longitude)
  
  
  if(LWs=="LW"){
    LW <- as.numeric(as.Date(lastSunday(today))) - 7
    dataLWs = data %>%
      filter(data$numericLastSunday==LW)
    numWeeks1 <<- 1
  }
  
  if(LWs=="L4W"){
    L4Wstart <- lastSunday(today) - 35
    L4Wend <- lastSunday(today) - 8
    dataLWs = data %>%
      filter(data$numericLastSunday>=L4Wstart, data$numericLastSunday<=L4Wend)
    numWeeks1 <<- 4
  }
  
  if(LWs == "L12W"){
    L12Wstart <- lastSunday(today) - 91
    L12Wend <- lastSunday(today) - 8
    dataLWs = data %>%
      filter(data$numericLastSunday>=L12Wstart, data$numericLastSunday<=L12Wend)
    numWeeks1 <<- 12
  }
  
  if(LWs=="YTD"){
    YTD <- subset(data, FISCAL_YEAR==year(today))
    numWeeks1 <<- max(YTD$FISCAL_WEEK_OF_YEAR,na.rm = TRUE)
    dataLWs <- subset(data, FISCAL_YEAR==year(today))
  }
  
  temp = calcAppLWs("Store",dataLWs)
  
  temp = temp %>%
    select(-c(numericLastSunday)) %>%
    mutate(Percent_OOS = (Num_Of_Transactions_OOS/Num_Of_Transactions)*100)
  
  temp[is.na(temp)] = 0
  
  return(temp)
  
}

#same as sumFunc but for custom date ranges
#type = grouping variable, master = vpoMaster, etc, start and end are dates
calcAppCustom <- function(type,master,start1,end1){
  
  name1 <<- as.Date(start1)
  
  name3 <<- as.Date(end1) + 6
  
  start1 = as.numeric(as.Date(start1))
  
  end1 = as.numeric(as.Date(end1))
  
  master = master %>% filter(Start_Num>=start1, Start_Num<=end1)
  
  numWeeks1 <<- ((end1 - start1) / 7) + 1
  
  final = unique(master[1])
  
  calcTemp = unique(master[1])
  
  names = colnames(master)
  
  for(j in names[2:length(names)]){
    
    testString = j
    
    temp = unlist(strsplit(testString, split=""))
    
    calcName <<- unlist(strsplit(testString, split='ZZ', fixed=TRUE))[1]
    
    for(i in 1:length(temp)){
      
      if(paste0(temp[i],temp[i+1]) == "ZZ"){
        
        calcIndicator = paste0(temp[i+2],temp[i+3],temp[i+4],temp[i+5])
        
      }
      
    }
    
    colnames(master)[colnames(master)==testString] <- "current"
    dependent = CalculationCodes$Dependent[CalculationCodes$Metric == calcName]
    dependentIndicator = CalculationCodes$Type[CalculationCodes$Metric == dependent]
    colnames(master)[colnames(master)==paste0(dependent,"ZZ",dependentIndicator)] <- "dependent"
    
    if(calcIndicator == "sAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = sAVG.1(current,Num_Stores))}
    if(calcIndicator == "tAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))}
    if(calcIndicator == "rAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))}
    if(calcIndicator == "SUMS"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = SUMS.1(current))}
    if(calcIndicator == "MEAN"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = MEAN(current))}
    if(calcIndicator == "DATE"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = DATE(current))}
    if(calcIndicator == "mAVG"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = rAVG.1(current,dependent))}
    if(calcIndicator == "smAV"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = sAVG.1(current,Num_Stores))}
    if(calcIndicator == "PERC"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = MEAN(current))}
    if(calcIndicator == "INFO"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = INFO(current))}
    if(calcIndicator == "mSUM"){calcTemp = master %>% group_by_(type) %>% summarise(tempName = SUMS.1(current))}
    
    colnames(master)[colnames(master)=="current"] <- testString
    colnames(master)[colnames(master)=="dependent"] <- paste0(dependent,"ZZ",dependentIndicator)
    colnames(calcTemp)[2] <- calcName
    
    final = merge(final,calcTemp,by = type)
    
  }
  
  final = final %>% select(-Num_Stores) %>% select(-Start_Num)
  
  
  return(final)
} 

#dynamicHeatMap but with custom range instead of LW, L4W, L12W, YTD
dynamicHeatMapCustom <- function(data,start1,end1){
  
  data <- data %>%
    mutate(numericLastSunday = as.numeric(as.Date(Start_DateZZDATE))) %>%
    rename(LatitudeZZINFO = Latitude, LongitudeZZINFO = Longitude)
  
  temp = calcAppCustom("Store",data,start1,start2)
  
  temp = temp %>%
    mutate(Percent_OOS = (Num_Of_Transactions_OOS/Num_Of_Transactions)*100)
  
  temp[is.na(temp)] = 0
  
  return(temp)
  
}

#creates heatmap leaflet
#data = dynamicHeatmap[Custom] , heat = color variable , radius = size of circle variable
#region is region # or Company for all, reverse flips color scale (Y,N)
#Region lat/long is hard coded to each region
heatMap <- function(data, heat, radius, region, reverse){
  name1 = heat
  name2 = radius
  
  data = data %>%
    select(Store,Latitude,Longitude,heat,radius,region) %>%
    rename(Heat = heat,Radius = radius)
  
  
  
  data$Heat <- round(data$Heat,digits = 2)
  data$Radius <- round(data$Radius, digits = 2)
  data$Latitude <- as.numeric(data$Latitude)
  data$Longitude <- as.numeric(data$Longitude)
  
  
  tf = ifelse(reverse == "Yes", TRUE, FALSE)
  
  if(region=="Company"){
    radius = data$Radius/max(data$Radius)*50
    
    quant = quantile(data$Heat,type = 8)
    
    pal <- colorQuantile(palette = c("RdYlGn"), domain = quant, n=5, reverse = tf)
    
    leaflet(data = data) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addCircleMarkers(radius=radius,color = ~pal(data$Heat), opacity = 1,
                       popup = ~paste0("Store ", Store, "<br/>",name1," " , data$Heat, "<br/>", name2," ", data$Radius)) %>%
      addLegend(title = name1, pal = pal, values = quant, position = "bottomright",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0(round(cuts[-n],digits = 2), " &ndash; ", round(cuts[-1],digits = 2))
                }) %>%
      addSearchOSM() %>%
      addResetMapButton()
  } else{
    if(region=='21'){
      ln1 = -75.154278 #min(data$Longitude[data$region==Region])
      ln2 = -66.602518 #max(data$Longitude[data$region==Region])
      lt1 = 47.522653 #max(data$Latitude[data$region==Region])
      lt2 = 39.325246 #min(data$Latitude[data$region==Region])
    }
    if(region=='28'){
      ln1 = -83.593982 #min(data$Longitude[data$region==Region])
      ln2 = -73.565658 #max(data$Longitude[data$region==Region])
      lt1 = 45.096286 #max(data$Latitude[data$region==Region])
      lt2 = 39.172104  #min(data$Latitude[data$region==Region])
    }
    if(region=='22'){
      ln1 = -79.722402 #min(data$Longitude[data$region==Region])
      ln2 = -73.926015 #max(data$Longitude[data$region==Region])
      lt1 = 41.070529 #max(data$Latitude[data$region==Region])
      lt2 = 36.588494  #min(data$Latitude[data$region==Region])
    }
    if(region=='43'){
      ln1 = -83.194086 #min(data$Longitude[data$region==Region])
      ln2 = -75.323479 #max(data$Longitude[data$region==Region])
      lt1 = 38.916125 #max(data$Latitude[data$region==Region])
      lt2 = 32.814377 #min(data$Latitude[data$region==Region])
    }
    if(region=='27'){
      ln1 = -89.574948 #min(data$Longitude[data$region==Region])
      ln2 = -78.474361 #max(data$Longitude[data$region==Region])
      lt1 = 39.8333 #max(data$Latitude[data$region==Region])
      lt2 = 33.735938  #min(data$Latitude[data$region==Region])
    }
    if(region=='29'){
      ln1 = -89.265077 #min(data$Longitude[data$region==Region])
      ln2 = -80.801208 #max(data$Longitude[data$region==Region])
      lt1 = 46.964255  #max(data$Latitude[data$region==Region])
      lt2 = 36.842218 #min(data$Latitude[data$region==Region])
    }
    if(region=='44'){
      ln1 = -85.784613 #min(data$Longitude[data$region==Region])
      ln2 = -79.074161 #max(data$Longitude[data$region==Region])
      lt1 = 35.079849 #max(data$Latitude[data$region==Region])
      lt2 = 29.090192 #min(data$Latitude[data$region==Region])
    }
    if(region=='45'){
      ln1 = -83.241748 #min(data$Longitude[data$region==Region])
      ln2 = -79.690966 #max(data$Longitude[data$region==Region])
      lt1 = 29.530863 #max(data$Latitude[data$region==Region])
      lt2 = 24.368125 #min(data$Latitude[data$region==Region])
    }
    if(region=='46'){
      ln1 = -95.255826  #min(data$Longitude[data$region==Region])
      ln2 = -84.111293 #max(data$Longitude[data$region==Region])
      lt1 = 36.540035 #max(data$Latitude[data$region==Region])
      lt2 = 28.627378 #min(data$Latitude[data$region==Region])
    }
    if(region=='41'){
      ln1 = -109.489408 #min(data$Longitude[data$region==Region])
      ln2 = -93.242438 #max(data$Longitude[data$region==Region])
      lt1 = 37.539344  #max(data$Latitude[data$region==Region])
      lt2 = 25.407323 #min(data$Latitude[data$region==Region])
    }
    if(region=='20'){
      ln1 = -102.808183 #min(data$Longitude[data$region==Region])
      ln2 = -86.016677 #max(data$Longitude[data$region==Region])
      lt1 = 49.471425 #max(data$Latitude[data$region==Region])
      lt2 = 35.189716  #min(data$Latitude[data$region==Region])
    }
    if(region=='42'){
      ln1 = -125.443951 #min(data$Longitude[data$region==Region])
      ln2 = -101.344335 #max(data$Longitude[data$region==Region])
      lt1 = 49.344879 #max(data$Latitude[data$region==Region])
      lt2 = 30.626187 #min(data$Latitude[data$region==Region])
    }
    # Region = as.numeric(region)
    
    # ln1 = min(data$Longitude[data$region==Region])
    ## ln2 = max(data$Longitude[data$region==Region])
    #lt1 = max(data$Latitude[data$region==Region])
    #lt2 = min(data$Latitude[data$region==Region])
    
    radius = data$Radius/max(data$Radius)*50
    
    quant = quantile(data$Heat,type = 8)
    
    pal <- colorQuantile(palette = c("RdYlGn"), domain = quant, n=5, reverse = tf)
    
    leaflet(data = data) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      fitBounds(lng1 = ln1, lat1 = lt1,
                lng2 = ln2, lat2 = lt2) %>%
      addCircleMarkers(radius=radius,color = ~pal(data$Heat), opacity = 1,
                       popup = ~paste0("Store ", Store, "<br/>",name1," " , data$Heat, "<br/>", name2," ", data$Radius)) %>%
      addLegend(title = name1, pal = pal, values = quant, position = "bottomright",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0(round(cuts[-n],digits = 2), " &ndash; ", round(cuts[-1],digits = 2))
                }) %>%
      addSearchOSM() %>%
      addResetMapButton()
  }
  
}

#finds last sunday
lastSunday <- function(today){
  
  return(today - as.numeric(today+4)%%7)
  
} 

#load up on start HTML
appCSS <- "
#loading-content {
position: absolute; 
background: #A52238;
opacity: 1;
z-index: 100;
left: 0;
right: 0;
height: 1000%;
text-align: center;
color: #FFFFFF;
margin-top: 100px;
}

"

#java code for retreiving screen height for map size
jscode <- '
$(document).on("shiny:connected", function(e) {
var jsHeight = window.innerHeight;
Shiny.onInputChange("GetScreenHeight",jsHeight);
var jsWidth = window.innerWidth;
Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'
#If TRUE, chat will be cleared on startup (not desirable)
#If FALSE, chats will be maintained
clear = FALSE

#instatiating the chat and users fields
vars <- reactiveValues(chat=NULL, users=NULL)

if(clear == TRUE){
  vars$chat <- "Welcome to AAP RLOW Message Board!"
}
if(clear == FALSE){
  if (file.exists("chat.Rds")){
    vars$chat <- readRDS("chat.Rds")
  } else {
    vars$chat <- "Welcome to AAP RLOW Message Board!"
  }
}
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

#populate original options for remove column
removeOptions <- names(regionMaster)

#in Data Analysis Tab, these are options which appear based on selected dataset to select which variables you want to include
vpoOptions = names(vpoMaster %>% select(-Start_DateZZDATE) %>% select(-End_DateZZDATE) %>% select(-Num_Stores) %>% select(-Start_Num) %>% select(-vpo))
for(i in 1:length(vpoOptions)){
  
  vpoOptions[i] = unlist(strsplit(vpoOptions[i], split='ZZ', fixed=TRUE))[1]
  
}
storeOptions = names(storeMaster %>% select(-Start_DateZZDATE) %>% select(-End_DateZZDATE) %>% select(-Num_Stores) %>% select(-Start_Num) %>% select(-Store))
for(i in 1:length(storeOptions)){
  
  storeOptions[i] = unlist(strsplit(storeOptions[i], split='ZZ', fixed=TRUE))[1]
  
}
divisionOptions = names(divisionMaster %>% select(-Start_DateZZDATE) %>% select(-End_DateZZDATE) %>% select(-Num_Stores) %>% select(-Start_Num) %>% select(-division))
for(i in 1:length(divisionOptions)){
  
  divisionOptions[i] = unlist(strsplit(divisionOptions[i], split='ZZ', fixed=TRUE))[1]
  
}
districtOptions = names(districtMaster %>% select(-Start_DateZZDATE) %>% select(-End_DateZZDATE) %>% select(-Num_Stores) %>% select(-Start_Num) %>% select(-District))
for(i in 1:length(districtOptions)){
  
  districtOptions[i] = unlist(strsplit(districtOptions[i], split='ZZ', fixed=TRUE))[1]
  
}
regionOptions = names(regionMaster %>% select(-Start_DateZZDATE) %>% select(-End_DateZZDATE) %>% select(-Num_Stores) %>% select(-Start_Num) %>% select(-Region))
for(i in 1:length(regionOptions)){
  
  regionOptions[i] = unlist(strsplit(regionOptions[i], split='ZZ', fixed=TRUE))[1]
  
}

#populates landing page options (arbitrary default is division)
options = divisionOptions

#populates options for heat map radius and heat (options come from store level)
heatMapOptions = storeOptions

#counters for update buttons
counter = -1
counterRemove = -1
counterRemoveVar = -1

#populate region options for heat map
regionChoices = c("Company","21","22","28","43","27","29","44","45","46","20","41","42")

#user interface
ui1 <- 
  
  fluidPage(
    #load up screen on start
    setBackgroundColor("#CCCCCC"),
    useShinyjs(),
    inlineCSS(appCSS),
    tags$script(jscode),
    div(
      id = "loading-content",
      h2(paste0("Loading Complete When AAP Logo is Visible"))
    ),
    
    #whole app is in a navbarPage with tabs
    navbarPage("RLOWDED", 
               
               #Page 1, Data Analysis
               tabPanel("Data Analysis",
                        
                        #rendering AAP logo
                        imageOutput("myImage",width = 125,height = 125),
                        
                        #panel select metric options
                        sidebarPanel(width = 4,
                                     actionButton(inputId = "clearAllTop",
                                                  label = "Clear selection",
                                                  icon = icon("square-o")),
                                     actionButton(inputId = "selectAllTop",
                                                  label = "Select all",
                                                  icon = icon("check-square-o")),
                                     uiOutput("optionsControl")
                        ),
                        
                        mainPanel(
                          
                          #date range input 1
                          column(6,offset = 0, dateRangeInput(inputId = "dates", label = "Date Range 1",start = "2019-03-03",end="2019-03-09",startview = "year")),
                          
                          #dataset select
                          column(6,selectInput(inputId = "datasetType", label = "Choose a Data Type:", choices = c("Division","Region","District","Store","VPO"))),
                          
                          #date range 2 for compare
                          column(6,offset = 0, dateRangeInput(inputId = "dates2", label = "Date Range 2",start = "2019-03-03",end = "2019-03-16",startview = "year")),
                          
                          #file name for download
                          column(6,textInput(inputId = "datasetName",label = "Download File Name:")),
                          verbatimTextOutput("datasetName"),
                          
                          #select 1 range, 2 range or LXW
                          column(6,selectInput(inputId = "datasetType2", label = "Choose a Date Range Option:", choices = c("1 Range","2 Ranges (Compare)","LXW"))),
                          
                          #update table button
                          column(2,actionButton("updater", "Update Table")),
                          
                          #download data button which initiates download
                          column(2,downloadButton("downloadData", "Download"))
                          
                        ),
                        
                        mainPanel(
                          # Data 
                          tabPanel(p(icon("table"), "Dataset"),
                                   dataTableOutput(outputId="dTable",height="auto",width = "auto")
                          ) # end of "Dataset" tab panel
                        )     
               ),
               
               #Page 2 
               tabPanel("Data Visualization - Map",
                        
                        #AAP logo
                        imageOutput("myImage1",width = 125,height = 125),
                        
                        mainPanel(
                          
                          #select variable for color visualization
                          column(3,selectInput(inputId = "heatType", label = "Choose a Variable to be displayed by heat:", choices = heatMapOptions)),
                          
                          #select variable for circle size
                          column(3,selectInput(inputId = "radiusType", label = "Choose a Varible to be displayed by radius:", choices = heatMapOptions)),
                          
                          #select time range, LW, l4W, L12W, YTD
                          column(3,selectInput(inputId = "timeType", label = "Choose a date range to view for the heat map:", choices = c("LW","L4W","L12W","YTD","Date Range Select"))),
                          
                          #select region to zoom to
                          column(3,selectInput(inputId = "regionZoomSelect", label = "Choose a Region to view or view Company-wide:", choices = regionChoices)),
                          
                          #date range input for if timeType is set to date range
                          column(4,offset = 0, dateRangeInput(inputId = "dates3", label = "Date Range",start = "2019-03-03",end="2019-03-09",startview = "year")),
                          
                          #flips the color scale from red to green or green to red
                          column(4,offset = 0,selectInput("flipImage", "Flip Red - Green Scale",choices = c("Red High, Green Low", "Red Low, Green High"))),
                          
                          #saves image to R console (not working yet)
                          column(4,offset = 0,actionButton("downloadImage", "Save Image")),
                          
                          #renders leaflet (map)
                          uiOutput("leaf1")
                          
                          
                          
                        )
                        
               ),
               
               #Page 3
               tabPanel("Email Sender",
                        
                        #AAP logo
                        imageOutput("myImage2",width = 125,height = 125),
                        
                        sidebarPanel(
                          #various text inputs
                          textInput("from", "From:", value="from@advance-auto.com"),
                          textInput("to", "To:", value="to@advance-auto.com"),
                          textInput("subject", "Subject:", value=""),
                          actionButton("send", "Send mail")
                        ),
                        
                        mainPanel(    
                          #essentially another text input
                          aceEditor("message", value="write message here")
                        )
                        
               ),
               
               #page 4
               tabPanel("Message Board",
                        
                        #AAP logo
                        imageOutput("myImage3",width = 125,height = 125),
                        
                        bootstrapPage(
                          
                          #HTML chat setup
                          includeCSS("shinychat.css"),
                          
                          #calls sendonEnter Java script 
                          includeScript("sendOnEnter.js"),
                          
                          #design of text size, color, background of chat window 
                          div(
                            
                            class = "container-fluid", 
                            div(class = "row-fluid",
                                
                                tags$head(tags$title("AAP RLOW Message Board")),
                                
                                div(class="span6", style="padding: 10px 0px;",
                                    h1("AAP RLOW Message Board")
                                ), div(class="span6", id="play-nice",
                                       "For Data-Related Comments Only"
                                )
                                
                            ),
                            
                            div(
                              class = "row-fluid", 
                              mainPanel(
                                
                                uiOutput("chat"),
                                
                                
                                fluidRow(
                                  div(class="span10",
                                      textInput("entry", "")
                                  )
                                )
                              ),
                              
                              sidebarPanel(
                                
                                textInput("user", "Your User ID:", value=paste0("User",sample(1:100, 3, replace=TRUE),sample(1:100, 3, replace=TRUE),sample(1:100, 3, replace=TRUE))),
                                tags$hr(),
                                h5("Connected Users"),
                                
                                uiOutput("userList"),
                                tags$hr()
                              )
                            )
                          )
                        )
               ),
               
               #Page 5
               tabPanel("Upload Data",
                        
                        #AAP logo
                        imageOutput("myImage4",width = 125,height = 125),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                            #select dataset to add data to
                            selectInput(inputId = "datasetAddType", label = "Choose a Dataset to Append to (DO THIS BEFORE UPLOADING):", choices = c("Region","District","Store","VPO","Division")),
                            
                            #opens file explorer on user computer, must upload a CSV
                            fileInput("file1", "Choose CSV File",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                            tags$hr()
                          ),
                          
                          mainPanel(
                            
                            #Show user upload in datatable
                            #adding vars to selected dataset is done behind the scenes
                            dataTableOutput("contents",height = "auto",width = "auto")
                            
                          )
                        )
               ),
               #page 6
               tabPanel("Remove Data",
                        
                        #AAP logo
                        imageOutput("myImage5",width = 125,height = 125),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                            #select dataset to remove from
                            selectInput(inputId = "datasetRemoveType", label = "Choose a Dataset to Remove a Column From:", choices = c("Region","District","Store","VPO","Division")),
                            
                            #variables in selected dataset which can be removed
                            selectInput(inputId = "datasetRemoveChoice", label = "Choose a Dataset to Remove a Column From:", choices = removeOptions),
                            
                            #update table to new dataset selection
                            actionButton("updaterRemove", "Update Table"),
                            
                            #remove var button, removes selected variable
                            actionButton("removeVar", "Remove Variable")
                            
                          ),
                          
                          mainPanel(
                            
                            #outputs dataset selected, reupdates every time update table button is pressed
                            tabPanel(p(icon("table"), "Dataset"),
                                     dataTableOutput(outputId="rTable",height="auto",width = "auto")
                            )
                            
                            
                            
                            
                          )
                          
                        )
                        
               )
               
    ),
    
    hidden(
      #p displays at bottom of each page
      div(
        id = "app-content",
        p("Advance Auto Parts - Supply Chain - 2019")
      )
      
    )
  )


server1 <-
  function(input, output, session) {
    
    
    #render the AAP logo on each page
    output$myImage <- renderImage({
      filename <- normalizePath(file.path('logo.png'))
      list(src = filename)}, deleteFile = FALSE)
    output$myImage1 <- renderImage({
      filename <- normalizePath(file.path('logo.png'))
      list(src = filename)}, deleteFile = FALSE)
    output$myImage2 <- renderImage({
      filename <- normalizePath(file.path('logo.png'))
      list(src = filename)}, deleteFile = FALSE)
    output$myImage3 <- renderImage({
      filename <- normalizePath(file.path('logo.png'))
      list(src = filename)}, deleteFile = FALSE)
    output$myImage4 <- renderImage({
      filename <- normalizePath(file.path('logo.png'))
      list(src = filename)}, deleteFile = FALSE)
    output$myImage5 <- renderImage({
      filename <- normalizePath(file.path('logo.png'))
      list(src = filename)}, deleteFile = FALSE)
    
    #values which change dynamically
    values <- reactiveValues()
    
    #options to select metrics to view
    values$options <- options
    
    #select all
    observe({
      if(input$selectAllTop > 0) {
        updateCheckboxGroupInput(session=session, inputId="options",
                                 choices=options, selected=options)
        values$options <- options
      }
    })
    
    #clear all
    observe({
      if(input$clearAllTop > 0) {
        updateCheckboxGroupInput(session=session, inputId="options",
                                 choices=options, selected=NULL)
        values$options <- c()
      }
    })
    
    #render set of checkboxes
    output$optionsControl <- renderUI({
      checkboxGroupInput('options', 'Select Columns to Display:',
                         options, selected = values$options)
    })
    
    #update values in date range select 1 on page 1
    observe({
      updateDateRangeInput(session,'dates')
      start1 <<- input$dates[1]
      end1 <<- lastSunday(input$dates[2])
      
    })
    
    #update values in date range select 2 on page 1
    observe({
      updateDateRangeInput(session,'dates2')
      start1 <<- input$dates[1]
      end1 <<- lastSunday(input$dates[2])
      start2 <<- input$dates2[1]
      end2 <<- lastSunday(input$dates2[2])
    })
    
    #update values in date range select on heat map page
    observe({
      updateDateRangeInput(session,'dates3')
      start3 <<- input$dates3[1]
      end3 <<- lastSunday(input$dates3[2])
    })
    
    #update the variable associated with the color of the circles on the map
    observe({
      
      updateSelectInput(session = session, 'heatType')
      
    })
    
    #update the variable associated with the size of the circles on the map
    observe({
      
      updateSelectInput(session = session, 'radiusType')
      
    })
    
    #update time range of map, update region to view, and flip the image 
    observe({
      
      updateSelectInput(session = session,'timeType')
      updateSelectInput(session = session,'regionZoomSelect')
      updateSelectInput(session = session, "flipImage")
      
      
      
    })
    
    #options selected to display by the table on page 1
    observe({
      
      selectedOptions <<- input$options
      
    })
    
    #dataset type to look at on page 1
    datasetInputType <- reactive({
      switch(input$datasetType,
             "Region" = "Region",
             "District" = "District",
             "Store" = "Store",
             "VPO" = "vpo",
             "Division" = "division")
    })
    
    #view (1 side, 2 side, lxw) to display on page 1
    datasetInputType2 <- reactive({
      switch(input$datasetType2,
             "1 Range" = "1side",
             "2 Ranges (Compare)" = "2side",
             "LXW" = "lxw")
    })
    
    #this whole reactive thing is what builds the data in the datatable depending on above inputs
    datasetInput <- reactive({
      #initial load up
      if(input$updater == 0){
        typeTime <<- 15
        counter <<- counter + 2
        options <<- divisionOptions
        selectedOptions = divisionOptions
        return(calcAppOf1("division",divisionMaster,start1,end1) %>% select("division",selectedOptions))
      }
      
      #will update every time the update table button is pressed
      else if(input$updater == counter){
        #increment counter to keep the update stable
        counter <<- counter + 1
        
        #looks at input from above
        if(datasetInputType() == "Region"){
          #length of loading bar
          typeTime  <<-  15
          #options which can be selected to display
          options <<- regionOptions
          values$options <<- regionOptions
          
          if(datasetInputType2() == "2side"){
            #does calculations over 2 provided ranges, selects the grouping variable name and all selected options to display in table
            return(calcAppOf2(datasetInputType(),regionMaster,start1,end1,start2,end2) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "1side"){
            #does calculations over 1 provided range, selects the grouping variable name and all selected options to display in table
            return(calcAppOf1(datasetInputType(),regionMaster,start1,end1) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "lxw"){
            #does calculations over LXW ranges, selects the grouping variable name and all selected options to display in table
            return(dynamicMarketProTTSSummaryLW.v2(data,"region")%>% select("region",selectedOptions))
          }
        }
        #commented each part of the code for Region, view above
        if(datasetInputType() == "District"){
          typeTime  <<-  20
          options <<- districtOptions
          values$options <<- districtOptions
          
          if(datasetInputType2() == "2side"){
            
            return(calcAppOf2(datasetInputType(),districtMaster,start1,end1,start2,end2) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "1side"){
            
            return(calcAppOf1(datasetInputType(),districtMaster,start1,end1) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "lxw"){
            
            return(dynamicMarketProTTSSummaryLW.v2(data,"district")%>% select("district",selectedOptions))
          }
        }
        #commented each part of the code for Region, view above
        if(datasetInputType() == "Store"){
          typeTime  <<-  30
          options <<- storeOptions
          values$options <<- storeOptions
          if(datasetInputType2() == "2side"){
            
            return(calcAppOf2(datasetInputType(),storeMaster,start1,end1,start2,end2) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "1side"){
            
            return(calcAppOf1(datasetInputType(),storeMaster,start1,end1) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "lxw"){
            
            return(dynamicMarketProTTSSummaryLW.v2(data,"Store")%>% select(datasetInputType(),selectedOptions))
          }
        }
        #commented each part of the code for Region, view above
        if(datasetInputType() == "vpo"){
          typeTime <<- 15
          options <<- vpoOptions
          values$options <<- vpoOptions
          
          if(datasetInputType2() == "2side"){
            
            return(calcAppOf2(datasetInputType(),vpoMaster,start1,end1,start2,end2) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "1side"){
            
            return(calcAppOf1(datasetInputType(),vpoMaster,start1,end1) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "lxw"){
            
            return(dynamicMarketProTTSSummaryLW.v2(data,"vpo")%>% select(datasetInputType(),selectedOptions))
          }
        }
        #commented each part of the code for Region, view above
        if(datasetInputType() == "division"){
          typeTime  <<-  15
          options <<- divisionOptions
          values$options <<- divisionOptions
          if(datasetInputType2() == "2side"){
            
            return(calcAppOf2(datasetInputType(),divisionMaster,start1,end1,start2,end2) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "1side"){
            
            return(calcAppOf1(datasetInputType(),divisionMaster,start1,end1) %>% select(datasetInputType(),selectedOptions))
          }
          if(datasetInputType2() == "lxw"){
            
            return(dynamicMarketProTTSSummaryLW.v2(data,"division")%>% select(datasetInputType(),selectedOptions))
          }
        }
        
      }
      
    })
    
    #creating datatable on page 1
    output$dTable <- renderDataTable({
      
      #updates the checkboxes and datatable at the same time
      output$optionsControl <- renderUI({
        checkboxGroupInput('options', 'Select Columns to Display:',
                           options, selected = values$options)
      })
      
      #takes data input from function above, adds a progress bar whose length depends on type of data above (store is longest)
      datatable(datasetInput(),rownames = FALSE,options = list(scrollX = TRUE,lengthMenu = list(c(10, 25, 50,100,-1), c('10', '25','50','100', 'All'))),
                withProgress(message = 'Updating Data Table',style = 'notification', {
                  for (i in 1:typeTime) {
                    incProgress(1/typeTime)
                    Sys.sleep(0.1)
                  }
                })
      )})
    
    #downloads a CSV of the datatable
    output$downloadData <- downloadHandler(
      #takes user input of name
      filename = function() {
        paste(input$datasetName, ".csv", sep = "")
      },
      #creates csv
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    
    #create the backend code of the map
    output$mymap <- renderLeaflet({
      
      #if we are doing an LXW range
      if(toString(input$timeType != "Date Range Select")){
        
        heatMapData <<- dynamicHeatMap(storeMasterMap,toString(input$timeType))
        
      }
      
      #if we have a date range
      if(toString(input$timeType == "Date Range Select")){
        
        heatMapData <<- dynamicHeatMapCustom(storeMasterMap,start3,end3)
        
      }
      
      #sets color scale Red: High, Green: Low of selected color metric
      if(input$flipImage == "Red High, Green Low"){
        
        flip <<- "Yes"
        
      }
      
      #sets color scale Red: Low, Green: High of selected color metric
      if(input$flipImage == "Red Low, Green High"){
        
        flip <<- "No"
        
      }
      
      #actually make the heat map
      heatMap(heatMapData,toString(input$heatType),toString(input$radiusType),input$regionZoomSelect,flip)
      
    })
    
    #create the actual map widget
    output$leaf1 <- renderUI({
      leafletOutput("mymap",height = input$GetScreenHeight-250, width = input$GetScreenWidth)
    })
    
    #email sender, basically wait on all inputs and for user to hit send
    observe({
      if(is.null(input$send) || input$send==0) return(NULL)
      from <- isolate(input$from)
      to <- isolate(input$to)
      subject <- isolate(input$subject)
      msg <- isolate(input$message)
      sendmail(from, to, subject, msg)
    })
    
    #download map image, which currently just downloads to R console
    observe({
      if(is.null(input$downloadImage) || input$downloadImage==0) return(NULL)
      saveWidget(m, file = "m.html")
      webshot("m.html", file = "m.png")
    })
    
    #download map image, which currently just downloads to R console
    #trying multiple ways that dont quite work
    output$downloadImage <- downloadHandler(
      filename = "map.png",
      content = function(file){png("m.html",file = file)
      }  
    )
    
    #setting up ability to change username
    sessionVars <- reactiveValues(username = "")
    
    #initial load var, false if first load up
    init <- FALSE
    
    #when a person leaves the room they are removed from active users and chat says they left
    session$onSessionEnded(function(){
      isolate({
        vars$users <- vars$users[vars$users != sessionVars$username]
        vars$chat <- c(vars$chat, paste0(linePrefix(),
                                         tags$span(class="user-exit",
                                                   sessionVars$username,
                                                   "left the room.")))
      })
    })
    
    #chat setup
    observe({
      
      #reactive dependency
      input$user
      
      if (!init){
        #initial username
        sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
        isolate({
          vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                            tags$span(class="user-enter",
                                                      sessionVars$username,
                                                      "entered the room.")))
        })
        init <<- TRUE
      } else{
        #use already has a name
        isolate({
          if (input$user == sessionVars$username || input$user == ""){
            #do nothing
            return()
          }
          
          #update username, remove the old one
          vars$users <- vars$users[vars$users != sessionVars$username]
          
          #notify chat log of the change
          vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                            tags$span(class="user-change",
                                                      paste0("\"", sessionVars$username, "\""),
                                                      " -> ",
                                                      paste0("\"", input$user, "\""))))
          
          #now store user's username 
          sessionVars$username <- input$user
        })
      }
      #add new name to list of users
      isolate(vars$users <- c(vars$users, sessionVars$username))
    })
    
    #allow users to set name
    observe({
      updateTextInput(session, "user", 
                      value=sessionVars$username)    
    })
    
    #display list of connected users
    output$userList <- renderUI({
      tagList(tags$ul( lapply(vars$users, function(user){
        return(tags$li(user))
      })))
    })
    
    #basically, send the chat entry to the chat log on enter button press
    observe({
      if(input$send < 1){
        return()
      }
      isolate({
        vars$chat <<- c(vars$chat, 
                        paste0(linePrefix(),
                               tags$span(class="username",
                                         tags$abbr(title=Sys.time(), sessionVars$username)
                               ),
                               ": ",
                               tagList(input$entry)))
      })
      updateTextInput(session, "entry", value="")
    })
    
    output$chat <- renderUI({
      if (length(vars$chat) > 500){
        #display only most recent 500 lines for speed and brevity (easily changed)
        vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
      }
      #save chat in folder (stores all I believe)
      saveRDS(vars$chat, "chat.Rds")
      
      #pass chat in as html
      HTML(vars$chat)
    })
    
    #select dataset to add to
    #select dataset to remove from
    observe({
      
      updateSelectInput(session = session, 'datasetAddType')
      updateSelectInput(session = session, 'datasetRemoveType')
      
    })
    
    #which dataset to add to
    datasetAddType <- reactive({
      
      #update options on data analysis page, user will likely go view that dataset next after uploading to it
      values$options <- options
      
      switch(input$datasetAddType,
             "Region" = "region",
             "District" = "district",
             "Store" = "store",
             "VPO" = "vpo",
             "Division" = "division")
      
    })
    
    output$contents <- renderDataTable({
      #wait for file upload
      req(input$file1)
      #dataset being added
      addTemp <- read.csv(input$file1$datapath)
      #make sure the dates are dates, sometimes "2019-03-03" can be interpreted as 2013 (2019 - 3 - 3)
      addTemp$Start_DateZZDATE = as.Date(addTemp$Start_DateZZDATE)
      addTemp$End_DateZZDATE = as.Date(addTemp$End_DateZZDATE)
      
      #if adding to region...
      if(datasetAddType()=="region"){
        
        #pull from master to know all of the options we currently have
        tempOptions = names(regionMaster)
        
        #for each name in uploaded data
        for(i in names(addTemp)){
          current = i
          #essentially check to make sure nobody is uploading a column name that is already there
          #we will merge/join by start_date, end_date, and grouping var, so those should stay
          for(j in tempOptions){
            if(current == j) {
              if((current != "Region")&(current!="Start_DateZZDATE")&(current != "End_DateZZDATE")){
                addTemp = addTemp[names(addTemp) != current]
              }
            }
          }
        }
        #add regionMaster to new data, combining the two
        regionMaster <<- right_join(regionMaster,addTemp,by = c("Region","Start_DateZZDATE","End_DateZZDATE"))
        #add column names of temp to region options
        regionOptions = c(regionOptions,names(addTemp))
        
        #seperate out indicator ZZ
        for(i in 1:length(regionOptions)){
          regionOptions[i] = unlist(strsplit(regionOptions[i], split='ZZ', fixed=TRUE))[1]
        }
        
        #remove these duplicates which we know will be there
        regionOptions = regionOptions[regionOptions!="Region"]
        regionOptions = regionOptions[regionOptions!="End_Date"]
        regionOptions = regionOptions[regionOptions!="Start_Date"]
        regionOptions <<- unique(regionOptions)
        
      }
      #commented every bit of code for region, see above
      if(datasetAddType()=="district"){
        
        tempOptions = names(districtMaster)
        for(i in names(addTemp)){
          current = i
          for(j in tempOptions){
            if(current == j) {
              if((current != "District")&(current!="Start_DateZZDATE")&(current != "End_DateZZDATE")){
                addTemp = addTemp[names(addTemp) != current]
              }
            }
          }
        }
        districtMaster <<- right_join(districtMaster,addTemp,by = c("District","Start_DateZZDATE","End_DateZZDATE"))
        districtOptions = c(districtOptions,names(addTemp))
        for(i in 1:length(districtOptions)){
          districtOptions[i] = unlist(strsplit(districtOptions[i], split='ZZ', fixed=TRUE))[1]
        }
        districtOptions = districtOptions[districtOptions!="District"]
        districtOptions = districtOptions[districtOptions!="End_Date"]
        districtOptions = districtOptions[districtOptions!="Start_Date"]
        districtOptions <<- unique(districtOptions)
        
      }
      #commented every bit of code for region, see above
      if(datasetAddType()=="store"){
        tempOptions = names(storeMaster)
        for(i in names(addTemp)){
          current = i
          for(j in tempOptions){
            if(current == j) {
              if((current != "Store")&(current!="Start_DateZZDATE")&(current != "End_DateZZDATE")){
                addTemp = addTemp[names(addTemp) != current]
              }
            }
          }
        }
        storeMaster <<- right_join(storeMaster,addTemp,by = c("Store","Start_DateZZDATE","End_DateZZDATE"))
        storeOptions = c(storeOptions,names(addTemp))
        for(i in 1:length(storeOptions)){
          storeOptions[i] = unlist(strsplit(storeOptions[i], split='ZZ', fixed=TRUE))[1]
        }
        storeOptions = storeOptions[storeOptions!="Store"]
        storeOptions = storeOptions[storeOptions!="End_Date"]
        storeOptions = storeOptions[storeOptions!="Start_Date"]
        storeOptions <<- unique(storeOptions)
      }
      #commented every bit of code for region, see above
      if(datasetAddType()=="vpo"){ 
        tempOptions = names(vpoMaster)
        for(i in names(addTemp)){
          current = i
          for(j in tempOptions){
            if(current == j) {
              if((current != "vpo")&(current!="Start_DateZZDATE")&(current != "End_DateZZDATE")){
                addTemp = addTemp[names(addTemp) != current]
              }
            }
          }
        }
        vpoMaster <<- right_join(vpoMaster,addTemp,by = c("vpo","Start_DateZZDATE","End_DateZZDATE"))
        vpoOptions = c(vpoOptions,names(addTemp))
        for(i in 1:length(vpoOptions)){
          vpoOptions[i] = unlist(strsplit(vpoOptions[i], split='ZZ', fixed=TRUE))[1]
        }
        vpoOptions = vpoOptions[vpoOptions!="vpo"]
        vpoOptions = vpoOptions[vpoOptions!="End_Date"]
        vpoOptions = vpoOptions[vpoOptions!="Start_Date"]
        vpoOptions <<- unique(vpoOptions)
      }
      #commented every bit of code for region, see above
      if(datasetAddType()=="division"){
        
        tempOptions = names(divisionMaster)
        for(i in names(addTemp)){
          current = i
          for(j in tempOptions){
            if(current == j) {
              if((current != "division")&(current!="Start_DateZZDATE")&(current != "End_DateZZDATE")){
                addTemp = addTemp[names(addTemp) != current]
              }
            }
          }
        }
        divisionMaster <<- right_join(divisionMaster,addTemp,by = c("division","Start_DateZZDATE","End_DateZZDATE"))
        divisionOptions = c(divisionOptions,names(addTemp))
        for(i in 1:length(divisionOptions)){
          divisionOptions[i] = unlist(strsplit(divisionOptions[i], split='ZZ', fixed=TRUE))[1]
        }
        divisionOptions = divisionOptions[divisionOptions!="division"]
        divisionOptions = divisionOptions[divisionOptions!="End_Date"]
        divisionOptions = divisionOptions[divisionOptions!="Start_Date"]
        divisionOptions <<- unique(divisionOptions)
      }
      
      #finally, render the datatable the user uploaded so they know the data is right/it worked
      return(datatable(addTemp,rownames = FALSE,options = list(scrollX = TRUE,lengthMenu = list(c(10, 25, 50,100,-1), c('10', '25','50','100', 'All'))),
                       withProgress(message = 'Updating Data Table',style = 'notification', {
                         for (i in 1:typeTime) {
                           incProgress(1/typeTime)
                           Sys.sleep(0.1)
                         }
                       })
      ))
    })
    
    #select master set to remove from
    datasetRemoveType <- reactive({
      
      switch(input$datasetRemoveType,
             "Region" = "region",
             "District" = "district",
             "Store" = "store",
             "VPO" = "vpo",
             "Division" = "division")
      
      
    })
    
    #depending on removeType selected, update the options to remove from to be accurate to that dataset
    observe({
      
      if(datasetRemoveType()=="vpo"){updateSelectInput(session,"datasetRemoveChoice",choices = colnames(vpoMaster))}
      if(datasetRemoveType()=="division"){updateSelectInput(session,"datasetRemoveChoice",choices = colnames(divisionMaster))}
      if(datasetRemoveType()=="store"){updateSelectInput(session,"datasetRemoveChoice",choices = colnames(storeMaster))}
      if(datasetRemoveType()=="district"){updateSelectInput(session,"datasetRemoveChoice",choices = colnames(districtMaster))}
      if(datasetRemoveType()=="region"){updateSelectInput(session,"datasetRemoveChoice",choices = colnames(regionMaster))}
      
    })
    
    #watch the var selected to be removed
    observe({
      
      updateSelectInput(session,'datasetRemoveChoice')
      
    })
    
    datasetRemove <- reactive({
      #initial dataset displayed is region
      if(input$updaterRemove == 0){
        typeTime <<- 15
        counterRemove <<- counterRemove + 2
        return(head(regionMaster))
      }
      
      #counter for table updates that user is doing 100% intentionally
      else if(input$updaterRemove == counterRemove){
        counterRemove <<- counterRemove + 1
        
        #returning head(master) so it is quick
        if(datasetRemoveType() == "region"){
          typeTime  <<-  15
          return(head(regionMaster))
        }
        if(datasetRemoveType() == "district"){
          typeTime  <<-  20
          return(head(districtMaster))
        }
        if(datasetRemoveType() == "store"){
          typeTime  <<-  30
          return(head(storeMaster))
        }
        if(datasetRemoveType() == "vpo"){
          typeTime <<- 15
          return(head(vpoMaster))
        }
        if(datasetRemoveType() == "division"){
          typeTime  <<-  15
          return(head(divisionMaster))
        }
        
      }
      
    })
    
    observe({
      #remove button watcher
      if(input$removeVar == 0){
        
        counterRemoveVar <<- counterRemoveVar + 2
      }
      
      #remove var if it isnt in this list of unremovable things
      else if(input$removeVar == counterRemoveVar){
        
        removeOption = unlist(strsplit(input$datasetRemoveChoice, split='ZZ', fixed=TRUE))[1]
        
        unremovableOptions = c("Region","Store","District","division","vpo","Start_Num","Num_Stores","Start_DateZZDATE","End_DateZZDATE",
                               "Weekly_Total_Num_Of_DeliveriesZZSUMS","Weekday_Total_Num_Of_DeliveriesZZSUMS","Saturday_Total_Num_Of_DeliveriesZZSUMS","Sunday_Total_Num_Of_DeliveriesZZSUMS",
                               "Num_Of_TransactionsZZSUMS","Num_Of_Transactions_4WallZZSUMS","Num_Of_Transactions_OOSZZSUMS")
        
        #remove var if it isnt in this list of unremovable things
        if(removeOption %in% unremovableOptions){return()}
        
        counterRemoveVar <<- counterRemoveVar + 1
        
        #if it is not, and region is selected
        if(datasetRemoveType() == "region"){
          
          for(i in 1:length(colnames(regionMaster))){
            
            if(colnames(regionMaster)[i] == input$datasetRemoveChoice){
              
              #set selected to "current"
              colnames(regionMaster)[i] = "current"
              
            }
            
          }
          #remove current
          regionMaster <<- regionMaster %>% select(-current)
          
          #and remove it from options
          removeOption = unlist(strsplit(input$datasetRemoveChoice, split='ZZ', fixed=TRUE))[1]
          for(i in 1:length(regionOptions)){
            
            if(regionOptions[i] == removeOption){
              
              regionOptions[i] = "current"
              
            }
            
          }
          regionOptions <<- regionOptions[regionOptions!="current"]
        }
        #look at region for specific commenting
        if(datasetRemoveType() == "district"){
          for(i in 1:length(colnames(districtMaster))){
            
            if(colnames(districtMaster)[i] == input$datasetRemoveChoice){
              
              colnames(districtMaster)[i] = "current"
              
              districtOptions[i] = "current"
              
            }
            
          }
          districtMaster <<- districtMaster %>% select(-current)
          districtOptions <<- districtOptions %>% select(-current)
          
          removeOption = unlist(strsplit(input$datasetRemoveChoice, split='ZZ', fixed=TRUE))[1]
          for(i in 1:length(districtOptions)){
            
            if(districtOptions[i] == removeOption){
              
              districtOptions[i] = "current"
              
            }
            
          }
          districtOptions <<- districtOptions[districtOptions!="current"]
        }
        #look at region for specific commenting
        if(datasetRemoveType() == "store"){
          for(i in 1:length(colnames(storeMaster))){
            
            if(colnames(storeMaster)[i] == input$datasetRemoveChoice){
              
              colnames(storeMaster)[i] = "current"
              
              regionOptions[i] = "current"
              
            }
            
          }
          storeMaster <<- storeMaster %>% select(-current)
          
          removeOption = unlist(strsplit(input$datasetRemoveChoice, split='ZZ', fixed=TRUE))[1]
          for(i in 1:length(storeOptions)){
            
            if(storeOptions[i] == removeOption){
              
              storeOptions[i] = "current"
              
            }
            
          }
          storeOptions <<- storeOptions[storeOptions!="current"]
        }
        #look at region for specific commenting
        if(datasetRemoveType() == "vpo"){
          for(i in 1:length(colnames(vpoMaster))){
            
            if(colnames(vpoMaster)[i] == input$datasetRemoveChoice){
              
              colnames(vpoMaster)[i] = "current"
              
              regionOptions[i] = "current"
              
            }
            
          }
          vpoMaster <<- vpoMaster %>% select(-current)
          
          removeOption = unlist(strsplit(input$datasetRemoveChoice, split='ZZ', fixed=TRUE))[1]
          for(i in 1:length(vpoOptions)){
            
            if(vpoOptions[i] == removeOption){
              
              vpoOptions[i] = "current"
              
            }
            
          }
          vpoOptions <<- vpoOptions[vpoOptions!="current"]
        }
        #look at region for specific commenting
        if(datasetRemoveType() == "division"){
          for(i in 1:length(colnames(divisionMaster))){
            
            if(colnames(divisionMaster)[i] == input$datasetRemoveChoice){
              
              colnames(divisionMaster)[i] = "current"
              
              regionOptions[i] = "current"
              
            }
            
          }
          divisionMaster <<- divisionMaster %>% select(-current)
          
          removeOption = unlist(strsplit(input$datasetRemoveChoice, split='ZZ', fixed=TRUE))[1]
          for(i in 1:length(divisionOptions)){
            
            if(divisionOptions[i] == removeOption){
              
              divisionOptions[i] = "current"
              
            }
            
          }
          divisionOptions <<- divisionOptions[divisionOptions!="current"]
        }
        
        #finally, if the removed var was selected previosly, deselect it        
        for(i in 1:length(selectedOptions)){
          
          
          
          if(removeOption == selectedOptions[i]){
            
            selectedOptions[i] = "current"
            
            
          }
        }
        
        selectedOptions <<- selectedOptions[selectedOptions!="current"]
        
      }
      
    })
    
    #render table in remove tab so users can see results of remove in there
    output$rTable <- renderDataTable({
      
      datatable(datasetRemove(),rownames = FALSE,options = list(scrollX = TRUE,lengthMenu = list(c(10, 25, 50,100,-1), c('10', '25','50','100', 'All'))),
                withProgress(message = 'Updating Data Table',style = 'notification', {
                  for (i in 1:typeTime) {
                    incProgress(1/typeTime)
                    Sys.sleep(0.1)
                  }
                })
      )})
    
    #part of load up screen, all code is read and then it knows to display the screen for 3 seconds and then hide
    Sys.sleep(3)
    hide(id = "loading-content", anim = TRUE, animType = "fade") 
    show("app-content")
  }

#runApp()
shinyApp(ui1,server1)

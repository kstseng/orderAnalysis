load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\dataForUse_2010.rdata")
####################
####################
####################
head(dataForUse)
if(length(which(dataForUse$Qty == 0)) > 0){dataForUse <- dataForUse[-which(dataForUse$Qty == 0), ]}

summary(dataForUse$TimeDiff)
summary(dataForUse$US.Amt)

timeToMonth <- sapply(1:nrow(dataForUse), function(i){
  print(i/4830948)
  timediff <- dataForUse$TimeDiff[i]
  m <- (timediff - (timediff %% 30))/30
  return(m)
})

dataWithMonth <- cbind(dataForUse, LT = timeToMonth)

timelength <- seq(min(timeToMonth), max(timeToMonth))
uniPG <- as.character(unique(dataWithMonth$PG))
PGMonth <- sapply(1:length(uniPG), function(j){
  print(j/length(uniPG))
  dataByPG <- subset(dataWithMonth, PG == uniPG[j])  
  ult <- unique(dataByPG$LT)
  PG.US.Amt <- sapply(1:length(timelength), function(i){
    dt <- timelength[i]
    if (dt %in% ult){
      dataByLT <- subset(dataByPG, LT == dt)  
      out <- sum(dataByLT$US.Amt)    
    }else{
      out <- 0
    }
    return (out)
  })
  
  return(PG.US.Amt)
})

colnames(PGMonth) <- uniPG
PGMonthCumsum <- cumsum(as.data.frame(PGMonth))
PGMonthPercent <- sweep(PGMonthCumsum,2,colSums(PGMonth),`/`)

matplot(1:nrow(PGMonthPercent), PGMonthPercent, type = "l", lty = 1) 
matplot(timelength[1:9], PGMonthPercent[1:9, ], type = "l", lty = 1) 
matplot(timelength[1:5], PGMonthPercent[1:5, ], type = "l", lty = 1) 

################
################
################
dataWithOrderNO <- cbind(order_no = raw_data$order_no, dataCleaning[, c("Region", "PG", "Qty", "US.Amt", "ModePayment", "NewSector", "OrderSeason", "TimeDiff", 
                                                                        "OrderDate", "YMD")])
nrow(dataWithOrderNO)
dataWithOrderNO$order_no <- as.factor(dataWithOrderNO$order_no)
dataWithOrderNO$Qty <- as.numeric(dataWithOrderNO$Qty)
dataWithOrderNO$US.Amt <- as.numeric(dataWithOrderNO$US.Amt)
dataWithOrderNO$ModePayment <- as.factor(dataWithOrderNO$ModePayment)
dataWithOrderNO$NewSector <- as.factor(dataWithOrderNO$NewSector)
dataWithOrderNO$OrderSeason <- as.factor(dataWithOrderNO$OrderSeason)
dataWithOrderNO$TimeDiff <- as.numeric(dataWithOrderNO$TimeDiff)
if (length(which(is.na(dataWithOrderNO), arr.ind = T)[, 1]) > 0) dataWithOrderNO <- dataWithOrderNO[-which(is.na(dataWithOrderNO), arr.ind = T)[, 1], ]
if (length(which(as.numeric(dataWithOrderNO$US.Amt) <= 0)) > 0) dataWithOrderNO <- dataWithOrderNO[-which(as.numeric(dataWithOrderNO$US.Amt) < 0), ]
if (length(which(as.numeric(dataWithOrderNO$Qty) <= 0)) > 0) dataWithOrderNO <- dataWithOrderNO[-which(as.numeric(dataWithOrderNO$Qty) < 0), ]

nrow(dataWithOrderNO)
timeToMonth2 <- sapply(1:nrow(dataWithOrderNO), function(i){
  print(i/nrow(dataWithOrderNO))
  timediff <- dataWithOrderNO$TimeDiff[i]
  m <- (timediff - (timediff %% 30))/30
  return(m)
})
dataOrderWithMonth <- cbind(dataWithOrderNO, LT = timeToMonth2)

uniT <- unique(dataOrderWithMonth$LT)
totalTimeLength <- seq(min(uniT), max(uniT))
uniOrderNO <- as.character(unique(dataOrderWithMonth$order_no))
uniPG <- as.character(unique(dataOrderWithMonth$PG))
PG.OrderNo <- sapply(1:length(uniPG), function(j){
  print(j/length(uniPG))
  orderByPG <- subset(dataOrderWithMonth, PG == uniPG[j])  

  uniOrderByPG <- as.character(unique(orderByPG$order_no))
  orderPG <- sapply(1:length(uniOrderByPG), function(i){
    subdata <- subset(orderByPG, order_no == uniOrderByPG[i])
    vec <- rep(NA, length(totalTimeLength))
    uniLT <- subdata$LT
    for (h in 1:length(uniLT)){
      subdataLT <- subset(subdata, LT == uniLT[h])
      vec[which(uniLT[h] == totalTimeLength)] <- sum(subdataLT$US.Amt)
    }  
    return(vec)
  })
  
  return(orderPG)
  
  })









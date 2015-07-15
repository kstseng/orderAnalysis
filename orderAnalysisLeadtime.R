# library(RODBC)
# conn <- odbcConnect(dsn = "ACL_EAI_ACL", uid = "david79", pwd = "dtG79")
# raw_data <- sqlQuery(conn, " select order_no,Order_Date,FirstDate,CmtDate1st,efftive_date,Customer_id,
#                             aa.item_no,fact_entity,fact_zone,egroup,edivision,sales_id,Sector,sum(qty),
#                             sum(us_amt) from shipmentBacklog aa  (nolock)  where  order_date between 
#                             '2015/1/1' and '2015/6/30'  and fact_1234 = '1'  and bomseq >= 0  and 
#                             breakdown >= 0  and itp_find <> 2  and itp_find <> 9   and ( qty <> 0 or 
#                             us_amt <> 0 or cancel_flag = ' ' ) group by order_no,Order_Date,FirstDate,
#                             CmtDate1st,efftive_date,Customer_id,aa.item_no,fact_entity,fact_zone,egroup,
#                             edivision,sales_id,Sector order by 1,2,3,4,5
#                             ")
# save.image("C:/Users/David79.Tseng/Dropbox/David79.Tseng/git-respository/orderAnalysis/order_20150101_20150630.RData")
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\order_2013_201506.rdata")
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\order_20150101_20150630.rdata")
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\order_20150601_20150610.rdata")
nrow(raw_data)
colnames(raw_data)
#dataUse <- raw_data[, c("order_no", "Order_Date", "FirstDate", "CmtDate1st", "efftive_date", "Customer_id", "item_no", "fact_entity", 
#                       "fact_zone", "egroup", "edivision", "sales_id", "Sector", "", ".1")]
# dataUse <- as.data.frame(as.matrix(raw_data))
dataUse <- raw_data
colnames(dataUse) <- c("ORDER", "OrderDate", "X1st.Date", "CmtDate1st", "YMD", "Cust", "Part", "RBU", 
                       "Region", "PG", "PD", "Sales.id", "Sector", "Qty", "US.Amt")

## sector re-classifed, modePayment, season of orderDate 
sectorClassified <- function(sector){
  sector <- tolower(sector)
  if(sector %in% c('aonline', 'ia-aonline', "adcp", "ia-adcp")){
    out <- "IAAOnline"
  }else if(sector %in% "ia-csf"){
    out <- "iAutomationChannel"
  }else if(sector %in% "ia-ka"){
    out <- "iAutomationKA"
  }else if(sector %in% "ma"){
    out <- "MA"
  }else if(sector %in% "p&e"){
    out <- "PandE"
  }else if(sector %in% "isys-aonline"){
    out <- "iSYSOnline"
  }else if(sector %in% "isys-csf"){
    out <- "iSys_CSF"
  }else if(sector %in% c('isys-ka', 'icloud-bcd', 'icloud-napd', 'equipment builder', 'icloud ka', 'icloud csf')){
    out <- "iSystemKA"
  }else if(sector %in% "its(ita)"){
    out <- "ITSITA"
  }else if(sector %in% "ec-aonline"){
    out <- "EmbeddedAOnline"
  }else if(sector %in% "ec-csf"){
    out <- "EmbcoreChannel"
  }else if(sector %in% c('ec-ka', 'ec-ka2', 'ec-ka3', 'mbds-dtos', 'com', 'ec-dms', 'ms')){
    out <- "EmbCoreKA"
  }else if(sector %in% "gaming"){
    out <- "Gaming"
  }else if(sector %in% "is-aonline"){
    out <- "iServiceAOnline"
  }else if(sector %in% c('medical equipment builder', 'ihospital', 'd. healthcare', 'd. healthcare fcp', 'd. healthcare ka')){
    out <- "DHealthcare"
  }else if(sector %in% c("d. logistic", "d. logistic fcp", "d. logistic ka")){
    out <- "DLogisticandFleetManagement"
  }else if(sector %in% c('iretail', 'ucity', 'ibuilding')){
    out <- "DRetail"
  }else if(sector %in% c('is-ka', 'is-csf')){
    out <- "iServiceGeneral"
  }else if(sector %in% "ac-dms(e2e)"){
    out <- "AC_DMS"
  }else if(sector %in% c('nc-ka', 'nc-dms(e2e)')){
    out <- "NC_DMS"
  }else if(sector %in% 'iconnectivity'){
    out <- "iConnectivityTotal"
  }else if(sector %in% "caps"){
    out <- "CAPS"
  }else if(sector %in% "others(rma)"){
    out <- "Others"
  }else{
    out <- "misfit"
  }
  
  return (out)
}
modePayment <- function(sector){
  sector <- tolower(sector)
  if (sector %in% c("ia-ka", 'isys-ka', 'icloud-bcd', 'icloud-napd', 'ec-ka', 'ec-ka2', 'ec-ka3', 'mbds-dtos', 'com', 'ec-dms', 'ms', 'medical equipment builder', 'is-ka', 'nc-ka', 'icloud ka', 'd. healthcare ka', 'd. logistic ka')){
    out <- "KA"
  }else if(sector %in% c('aonline', 'ia-aonline', "isys-aonline", "ec-aonline", "is-aonline")){
    out <- "AOnline"
  }else if (sector %in% c("ia-csf", "isys-csf", "ec-csf", 'ihospital', 'is-csf', 'icloud csf')){
    out <- "Channel"
  }else{
    out <- "Others"
  }
  return (out)
}
seasonDefine <- function(month){
  if (month <= 3){
    out <- 1
  }else if(month <= 6){
    out <- 2
  }else if(month <= 9){
    out <- 3
  }else{
    out <- 4
  }
  return(out)
}

## old version
dataCleaning <- dataUse
for(i in 1:nrow(dataUse)){
  print(i/nrow(dataUse))
  d <- dataUse[i, ]
  ##
  ## define delay or not
  ##
  if (strptime(d[, "OrderDate"], "%Y-%m-%d") - strptime(d[, "YMD"], "%Y-%m-%d") >= 0){
    timediff <-  0
  }else{
    if (!is.na(d[, "CmtDate1st"]) & !is.na(d[, "YMD"])){
      timediff <- strptime(d[, "CmtDate1st"], "%Y-%m-%d") - strptime(d[, "YMD"], "%Y-%m-%d") 
    }else if(is.na(d[, "CmtDate1st"]) & !is.na(d[, "YMD"])){
      if (!is.na(d[, "X1st.Date"])){
        timediff <- strptime(d[, "X1st.Date"], "%Y-%m-%d") - strptime(d[, "YMD"], "%Y-%m-%d") 
      }else{
        timediff <- NA
      }
    }else{
      timediff <- NA
    }
  }
  
  if (is.na(timediff)){
    delay <- NA
  }else if(timediff > 30){
    delay <- 1
  }else{
    delay <- 0
  }
  
  ##
  ## order season
  ##
  om <- as.numeric(strsplit(as.character(d$OrderDate), split = "-")[[1]][2])
  orderSeason <- seasonDefine(om)
  ##
  ## sector classified and adding new feature 
  ## 
  dataCleaning[i, "ModePayment"] <- modePayment(d[, "Sector"])
  dataCleaning[i, "NewSector"] <- sectorClassified(d[, "Sector"])
  dataCleaning[i, "OrderSeason"] <- orderSeason
  dataCleaning[i, "Delay"] <- delay
}
## new version
t1 <- proc.time()
OrderSeason <- sapply(1:nrow(dataUse), function(i){
  seasonDefine(as.numeric(strsplit(as.character(dataUse$OrderDate[i]), split = "-")[[1]][2]))
})
t2 <- proc.time()
ModePayment <- sapply(1:nrow(dataUse), function(i){
  modePayment(dataUse[i, "Sector"])
})
t3 <- proc.time()
NewSector <- sapply(1:nrow(dataUse), function(i){
  sectorClassified(dataUse[i, "Sector"])
})
t4 <- proc.time()
TimeDiff <- sapply(1:nrow(dataUse), function(i){
  d <- dataUse[i, ]
  if (strptime(d$OrderDate, "%Y-%m-%d") - strptime(d$YMD, "%Y-%m-%d") >= 0){
    timediff <-  0
  }else{
    if (!is.na(d$CmtDate1st) & !is.na(d$YMD)){
      timediff <- strptime(d$CmtDate1st, "%Y-%m-%d") - strptime(d$YMD, "%Y-%m-%d") 
    }else if(is.na(d$CmtDate1st) & !is.na(d$YMD)){
      if (!is.na(d$X1st.Date)){
        timediff <- strptime(d$X1st.Date, "%Y-%m-%d") - strptime(d$YMD, "%Y-%m-%d") 
      }else{
        timediff <- NA
      }
    }else{
      timediff <- NA
    }
  }
  
  return(timediff)
}) # Response variable
t5 <- proc.time()
LeadTime <- sapply(1:nrow(dataUse), function(i){
  d <- dataUse[i, ]
  #strptime(d$CmtDate1st, "%Y-%m-%d") - strptime(d$OrderDate, "%Y-%m-%d")
  if (!is.na(d$CmtDate1st) & !is.na(d$OrderDate)){
    lt <- d$CmtDate1st - d$OrderDate  
  }else if(is.na(d$CmtDate1st) & !is.na(d$X1st.Date) & !is.na(d$OrderDate)){
    lt <- d$X1st.Date - d$OrderDate  
  }else{
    lt <- NA
  }
  return(lt)
})
t6 <- proc.time()

dataCleaning <- cbind(dataUse, "ModePayment" = ModePayment, "NewSector" = NewSector, 
                      "OrderSeason" = OrderSeason, "LeadTime" = LeadTime, "Delay" = Delay)

head(dataCleaning)
table(dataCleaning$PG)
##
#dataForUse <- dataCleaning[, c("Cust", "RBU", "Region", "PG", "PD", "Sales.id", "Qty", "US.Amt", "NewSector", "Delay")]
dataForUse <- dataCleaning[, c("Region", "PG", "Qty", "US.Amt", "ModePayment", "NewSector", "OrderSeason", "LeadTime", "Delay", 
                               "OrderDate", "X1st.Date", "CmtDate1st", "YMD")]
dataForUse$Qty <- as.numeric(dataForUse$Qty)
dataForUse$US.Amt <- as.numeric(dataForUse$US.Amt)
dataForUse$ModePayment <- as.factor(dataForUse$ModePayment)
dataForUse$NewSector <- as.factor(dataForUse$NewSector)
dataForUse$OrderSeason <- as.factor(dataForUse$OrderSeason)
dataForUse$LeadTime <- as.numeric(dataForUse$LeadTime)
dataForUse$Delay <- as.factor(dataForUse$Delay)
dataForUse <- dataForUse[-which(is.na(dataForUse), arr.ind = T)[, 1], ]
if (length(which(as.numeric(dataForUse$US.Amt) < 0)) > 0) dataForUse <- dataForUse[-which(as.numeric(dataForUse$US.Amt) < 0), ]
if (length(which(as.numeric(dataForUse$Qty) < 0)) > 0) dataForUse <- dataForUse[-which(as.numeric(dataForUse$Qty) < 0), ]
str(dataForUse)

####
table(dataForUse[which(dataForUse$Delay == 1), "Region"])
dataForUse[which(dataForUse$PG == "Industrial Automation"), "US.Amt"]
dataForUse[which(dataForUse$PG == "Industrial Automation" & dataForUse$Delay == 1), "US.Amt"]
sum(dataForUse[which(dataForUse$Sales.id %in% c('41310014', '11120303', '41160011')), "Delay"])
sum(dataForUse[which(dataForUse$PG %in% c("Industrial Automation", "Intelligent System", "Embcore", "PAPS")), "Delay"])

tabOrd <- table(dataForUse[which(dataForUse$Delay == 1), "OrderDate"])
####
#### linear regression
####



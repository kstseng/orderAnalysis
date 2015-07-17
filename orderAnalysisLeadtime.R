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
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\order_2010_201506.rdata")
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\order_2013_201506.rdata")
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\order_20150101_20150630.rdata")
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\order_20150601_20150610.rdata")
load("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\git-respository\\orderAnalysis\\dataForUse_2010.rdata")
# nrow(raw_data)
# colnames(raw_data)
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
timeCompute <- sapply(1:nrow(dataUse), function(i){
  d <- dataUse[i, ]
  orderdate <- d$OrderDate
  reqdate <- d$X1st.Date
  cmtdate <- d$CmtDate1st
  ymddate <- d$YMD
  
  #
  # timediff
  #
  timeDiff <- ymddate - orderdate  
  
  #
  # cmtDiff
  #
  if (!is.na(cmtdate)){
    cmtDiff <- cmtdate - orderdate 
  }else if(!is.na(reqdate)){
    cmtDiff <- reqdate - orderdate  
  }else{
    cmtDiff <- NA
  }
  
  #
  # reqDiff
  #
  if (!is.na(reqdate)){
    reqDiff <- reqdate - orderdate 
  }else{
    reqDiff <- NA
  }
  
  return(c(timeDiff, cmtDiff, reqDiff))
})
TimeDiff <- timeCompute[1, ] 
CmtDiff <- timeCompute[2, ] 
ReqDiff <- timeCompute[3, ] 
t5 <- proc.time()

dataCleaning <- cbind(dataUse, "ModePayment" = ModePayment, "NewSector" = NewSector, 
                      "OrderSeason" = OrderSeason, 
                      "TimeDiff" = TimeDiff, "CmtDiff" = CmtDiff, "ReqDiff" = ReqDiff)

dataForUse <- dataCleaning[, c("Region", "PG", "Qty", "US.Amt", "ModePayment", "NewSector", "OrderSeason", "TimeDiff", "CmtDiff", "ReqDiff", 
                               "OrderDate", "X1st.Date", "CmtDate1st", "YMD")]
dataForUse$Qty <- as.numeric(dataForUse$Qty)
dataForUse$US.Amt <- as.numeric(dataForUse$US.Amt)
dataForUse$ModePayment <- as.factor(dataForUse$ModePayment)
dataForUse$NewSector <- as.factor(dataForUse$NewSector)
dataForUse$OrderSeason <- as.factor(dataForUse$OrderSeason)
dataForUse$TimeDiff <- as.numeric(dataForUse$TimeDiff)
dataForUse$CmtDiff <- as.numeric(dataForUse$CmtDiff)
dataForUse$ReqDiff <- as.numeric(dataForUse$ReqDiff)
#####
#dataForUse <- dataForUse[-which(is.na(dataForUse), arr.ind = T)[, 1], ]
dataForUse <- dataForUse[-which(is.na(dataForUse[, 1:10]), arr.ind = T)[, 1], ] # remove the row which CmtDiff or ReqDiff is na.
if (length(which(as.numeric(dataForUse$US.Amt) < 0)) > 0) dataForUse <- dataForUse[-which(as.numeric(dataForUse$US.Amt) < 0), ]
if (length(which(as.numeric(dataForUse$Qty) < 0)) > 0) dataForUse <- dataForUse[-which(as.numeric(dataForUse$Qty) < 0), ]


save.image("C:/Users/David79.Tseng/Dropbox/David79.Tseng/git-respository/orderAnalysis/dataForUse_2010.RData")


###
### Initial data analysis
###

## <<Transformation>>
## TimeDiff: log
## US.Amt: log
## 
str(dataForUse)
hist(dataForUse$Qty)

plot(dataForUse$TimeDiff ~ dataForUse$Region)
plot(dataForUse$TimeDiff ~ dataForUse$PG)
plot(dataForUse$TimeDiff ~ dataForUse$Qty)
plot(dataForUse$TimeDiff ~ dataForUse$US.Amt)
plot(dataForUse$TimeDiff ~ dataForUse$ModePayment)
plot(dataForUse$TimeDiff ~ dataForUse$NewSector)
plot(dataForUse$TimeDiff ~ dataForUse$OrdeSeason)
plot(dataForUse$TimeDiff ~ dataForUse$CmtDiff)
plot(dataForUse$TimeDiff ~ dataForUse$ReqDiff)


x <- dataForUse$TimeDiff
x <- dataForUse$CmtDiff
x <- dataForUse$ReqDiff
num <- ceiling(0.0005*length(x))
y <- sort(x)
summary(x)
summary(y[c(-c(1:num), -(length(x):(length(x) - num)))])
hist(y[c(-c(1:num), -(length(x):(length(x) - num)))])
hist(log(y[c(-c(1:num), -(length(x):(length(x) - num)))]))
hist(log(x))

d <- dataForUse$TimeDiff - dataForUse$CmtDiff
summary(d)
length(which(d > 4000))

sum(dataForUse[(which(dataForUse$CmtDate1st >= strptime("2015-08-01", "%Y-%m-%d") & 
               dataForUse$CmtDate1st <= strptime("2015-08-31", "%Y-%m-%d"))), "US.Amt"])
####
#### linear regression
####
mod1 <- lm(TimeDiff ~ Region + PG + Qty + US.Amt + ModePayment + NewSector + OrderSeason, data = dataForUse)
mod2 <- lm(TimeDiff ~ Region + PG + Qty + US.Amt + ModePayment, data = dataForUse)
mod3 <- lm(TimeDiff ~ (Region + PG + Qty + US.Amt + ModePayment)^2, data = dataForUse)
summary(mod3)









install.packages("data.table")
install.packages("Metrics")
install.packages("forecast")
install.packages("dplyr")


library(data.table)  
library(Metrics)
library(forecast)
require(dplyr)

train <- fread("train.csv",stringsAsFactors = T)
test  <- fread("test.csv",stringsAsFactors = T)

summary(train)
summary(test)

train1 = train[train$Sales>0,]
preds=c('Store','DayOfWeek','Promo')
mdl = train1 %>% group_by_(.dots=preds) %>% summarise(PredSales=exp(mean(log(Sales)))) %>% ungroup()

pred = train %>% left_join(mdl,by=preds) %>% rename(mSales=PredSales)
pred$mSales[is.na(pred$mSales)]=0
train <- pred

pred = test %>% left_join(mdl,by=preds) %>% rename(mSales=PredSales)
pred$mSales[is.na(pred$mSales)]=0
test <- pred

train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

train[,logSales:=log1p(Sales)]

ar_fit = function(x) {
  Sales <- ts(x$logSales)
  lambda <- BoxCox.lambda(Sales)
  tsclean(Sales, replace.missing = TRUE, lambda=lambda)
  xreg <- cbind(DayOfWeek = x$DayOfWeek , 
                Open = x$Open,
                Promo = x$Promo,
                StateHoliday = x$StateHoliday,
                SchoolHoliday = x$SchoolHoliday,
                mSales = x$mSales
  )
  fit <- auto.arima(Sales, xreg=xreg)
  return(fit)
}

#results <- data.frame(Date=as.Date("01/01/2000", format="%m/%d/%Y"), Store=NA, Sales=0, predSales=0, stringsAsFactors=FALSE)
results <- data.frame(Id=NA, Sales=NA)

for (store in 559:1115) {
  strn <- train[Store == store]
  stst <- test[Store == store]
  stst$Open[is.na(stst$Open)] <- 1
  cat(sprintf("Store %d has training %d rows and test %d rows\n", store, nrow(strn), nrow(stst)  ))
  if (nrow(stst) > 0) { 
    strn <- strn[order(Date)]
    #sval <- strn[Date >= as.Date("07/01/2015", "%m/%d/%Y")]
    #strn <- strn[Date < as.Date("07/01/2015", "%m/%d/%Y")]
    stst <- stst[order(Date)]
    max_sales <- max(strn$Sales, na.rm = TRUE)
    out <- ar_fit(strn)
    #print(residuals(out))
    #print(out)
    
    xreg1 <- cbind(DayOfWeek = stst$DayOfWeek , 
                   Open = stst$Open,
                   Promo = stst$Promo,
                   StateHoliday = stst$StateHoliday,
                   SchoolHoliday = stst$SchoolHoliday,
                   mSales = stst$mSales
    )
    
    print(nrow(xreg1))
    
    pred <- data.frame(forecast(out, xreg=xreg1, lambda=out$lambda))
    pred$Point.Forecast <- expm1(pred$Point.Forecast)
    
    pred$Point.Forecast <- ifelse(pred$Point.Forecast < 0, 0, pred$Point.Forecast)
    pred$Point.Forecast <- ifelse(pred$Point.Forecast > max_sales, max_sales, pred$Point.Forecast)
    
    #print(pred)
    
    stst[,predSales:=round(Open * pred$Point.Forecast, digits=0)]
    
    #print(sprintf(" MSE is %f \n", mse(stst$Sales, stst$predSales)))
    #print(cbind(Store=stst$Store, Date=stst$Date, Sales=stst$Sales, predSales=stst$predSales))
  }
  results <- rbind(results, data.frame(Id=stst$Id, Sales=stst$predSales))
  #results <- rbind(results, data.frame(Date=stst$Date, Store=stst$Store, Sales=stst$Sales, predSales=stst$predSales))
}
results <- na.omit(results)
results <- results[order(results$Id),]
write.csv(results, "rossmann_arima.csv",row.names=F)

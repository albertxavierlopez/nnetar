#################################################
######### NNETAR to predict stock prices ########
#################################################

##### SP 
require(quantmod)
getSymbols('^GSPC',from='2007-06-01')
GSPC<-as.ts(GSPC$GSPC.Adjusted)

      
      # confidence intervals done MANUALLY
      sim <- ts(matrix(0, nrow=150L, ncol=10L),
                start=end(GSPC)[1L]+1L)
      for(i in seq(10)){
        sim[,i] <- simulate(fit3, nsim=150L)}
      autoplot(GSPC) + autolayer(sim)

    
# AUTOMATICALLY
fit <- nnetar(GSPC, lambda=0)
fcast <- forecast(fit, PI=TRUE, h=60)
autoplot(fcast)

      #NVDA
      getSymbols('NVDA',from='2016-06-01')
      NVDA<-as.ts(NVDA$NVDA.Adjusted)
      fit <- nnetar(NVDA, lambda=0)
      fcast <- forecast(fit, PI=TRUE, h=30)
      autoplot(fcast)
      
      #MSFT
      getSymbols('MSFT',from='2017-01-01')
      MSFT<-as.ts(MSFT$MSFT.Adjusted)
      fit <- nnetar(MSFT, lambda=0)
      fcast <- forecast(fit, PI=TRUE, h=35)
      autoplot(fcast)
     
      ###########################################
      
      #JD w/ VOLUME AS A REGRESSOR
      getSymbols('JD',from='2016-01-01')
      price<-as.ts(JD$JD.Adjusted)
      vol<-as.ts(JD$JD.Volume)
      
          #to get the predictions of regressors
          fit_vol<-nnetar(vol,lambda = "auto")
          vol_pred<-forecast(fit_vol, PI=TRUE, h=30)
          autoplot(vol_pred)
          
          vol_fcst<-as.numeric(vol_pred[["mean"]])
          length(vol_fcst)

      fit <- nnetar(price,xreg =vol,  lambda="auto")
      fcast <- forecast(fit,xreg=vol_fcst, PI=TRUE, h=30)
      autoplot(fcast)
      
    
      
      ###########################################
      
      #NVDA w/ VOLUME AS A REGRESSOR
      getSymbols('NVDA',from='2017-01-01')
      price<-as.ts(NVDA$NVDA.Adjusted)
      vol<-as.ts(NVDA$NVDA.Volume)
      
      #to get the predictions of regressors
      fit_vol<-nnetar(vol,lambda = "auto")
      vol_pred<-forecast(fit_vol, PI=TRUE, h=30)
      autoplot(vol_pred)
      
      vol_fcst<-as.numeric(vol_pred[["mean"]])
      length(vol_fcst)
      
      fit <- nnetar(price,xreg =vol,  lambda="auto")
      fcast <- forecast(fit,xreg=vol_fcst, PI=TRUE, h=30)
      autoplot(fcast)
      
      ###########################################
      
      #AAPL w/ VOLUME AS A REGRESSOR W/ RETROSPECTIVE
      getSymbols('AAPL',from='2017-01-01')
      price<-as.ts(AAPL$AAPL.Adjusted)
      vol<-as.ts(AAPL$AAPL.Volume)
      
      
      price_train<-window(price, end=550)
      vol_train<-window(vol, end=550)

      vol_test<-window(vol, start=551)
      
      fit <- nnetar(price_train,xreg =vol_train,  lambda="auto")
      fcast <- forecast(fit,xreg=vol_test, PI=TRUE, h=30)
      autoplot(fcast)+
        autolayer(price)
      
      ###########################################
      
      #AAPL w/ VOLUME AS A REGRESSOR W/ RETROSPECTIVE BIGGER HISTORY
      getSymbols('AAPL',from='2012-01-01')
      price<-as.ts(AAPL$AAPL.Adjusted)
      vol<-as.ts(AAPL$AAPL.Volume)
      
      
      price_train<-window(price, end=1798)
      vol_train<-window(vol, end=1798)
      
      vol_test<-window(vol, start=1799)
      
      fit <- nnetar(price_train,xreg =vol_train,  lambda="auto")
      fcast <- forecast(fit,xreg=vol_test, PI=TRUE, h=40)
      autoplot(fcast)+
        autolayer(price)
      
      #GE w/ VOLUME AS A REGRESSOR W/ RETROSPECTIVE BIGGER HISTORY
      getSymbols('GE',from='2012-01-01')
      price<-as.ts(GE$GE.Adjusted)
      vol<-as.ts(GE$GE.Volume)
      
      
      price_train<-window(price, end=1798)
      vol_train<-window(vol, end=1798)
      
      vol_test<-window(vol, start=1799)
      
      fit <- nnetar(price_train,xreg =vol_train,  lambda="auto")
      fcast <- forecast(fit,xreg=vol_test, PI=TRUE, h=40)
      autoplot(fcast)+
        autolayer(price)
      
      
      #GE w/ VOLUME AS A REGRESSOR W/ RETROSPECTIVE BIGGER HISTORY
      getSymbols('GE',from='2012-01-01')
      price<-as.ts(GE$GE.Adjusted)
      vol<-as.ts(GE$GE.Volume)
      
      
      price_train<-window(price, end=1798)
      vol_train<-window(vol, end=1798)
      
      vol_test<-window(vol, start=1799)
      
      fit <- nnetar(price_train,xreg =vol_train,  lambda="auto")
      fcast <- forecast(fit,xreg=vol_test, PI=TRUE, h=40)
      autoplot(fcast)+
        autolayer(price)
      
      #GE w/ VOLUME AS A REGRESSOR W/ RETROSPECTIVE BIGGER HISTORY
      getSymbols('AXP',from='2012-01-01')
      price<-as.ts(AXP$AXP.Adjusted)
      vol<-as.ts(AXP$AXP.Volume)
      
      
      price_train<-window(price, end=1798)
      vol_train<-window(vol, end=1798)
      
      vol_test<-window(vol, start=1799)
      
      fit <- nnetar(price_train,xreg =vol_train,  lambda="auto")
      fcast <- forecast(fit,xreg=vol_test, PI=TRUE, h=40)
      autoplot(fcast)+
        autolayer(price)
      
      
      #AMD w/ With NVDA AS A REGRESSOR W/ RETROSPECTIVE BIGGER HISTORY
      getSymbols('AMD',from='2017-01-01')
      price<-as.ts(AMD$AMD.Adjusted)
      vol<-as.ts(AMD$AMD.Volume)
      getSymbols('NVDA',from='2017-01-01')
      price2<-as.ts(NVDA$NVDA.Adjusted)
      vol2<-as.ts(NVDA$NVDA.Volume)
      
      
      price_train<-window(price, end=566)
      price_train2<-window(price2, end=566)
      
      price_train2_test<-window(price2, start=567)
      
      fit <- nnetar(price_train, xreg = price_train2,  lambda="auto")
      fcast <- forecast(fit, xreg = price_train2_test, PI=TRUE, h=15)
      autoplot(fcast)+
        autolayer(price)
      
      
      
      
      
      
      
      
      


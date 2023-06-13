# R wrapper around [OANDA](https://www.oanda.com/us-en/) FX trading V20 [API](https://developer.oanda.com/rest-live-v20/introduction/)
This project is an attepmt to bring research, backtest, trading, and monitoring using R wrapper around OANDA broker's HTTP API . It was motivated by a need to quickly turn research in to executable trades. There are three components:
- [Research](https://github.com/thelawrencekhan/oanda/blob/main/README.md#trading-api)
- [Monitoring](https://github.com/thelawrencekhan/oanda/blob/main/README.md#shiny-dashboard)

## QUICK GUIDE
## Installation
1. Prerequisites: Ensure that the following packages are installed
  ``` R
    > install.packages(c("curl","jsonlite","devtools")) # only the package
    > install.packages(c("curl","jsonlite","devtools","shiny","shinythemes","ini")) # package + dashboard
    > install.packages(c("curl","jsonlite","devtools","shiny","shinythemes","ini","zoo")) #package + dashboard + demo backtest & toy algo trading
  ```
2. Install oanda's api
  ``` R
    > library(devtools)
    > install_github(repo="ltekengineering/oanda", subdir="oanda")
  ```
3. Additional features:
  ``` console
    foo@bar: git clone git@github.com:ltekengineering/oanda.git    
  ```

## Trading API
- [x] Implementation of the trading [API](http://developer.oanda.com/rest-live-v20/introduction/) in R for algorithmic trading. A complete [tutorial](https://github.com/ltekengineering/oanda/wiki/D.-Trading-Api-Tutorial) is here.
  ### Using library  
  ``` R
    > library(oanda)    
  ```
  ### Setup
  - [x] Point to the practice account and authenticate with code generated on https://www1.oanda.com/demo-account/login
    ``` R
    > oanda.init("practice",'44667a4c1a94db7f245ea21fe2f82341-04efe2ed548b91e6bb2d7d7707668d8a')
    ```
  ### Time series of bid prices
  ``` R
  > eur_usd <- oanda.instruments.bid("EUR_USD","M1",from = "1608064200") #2020-12-15 03:30 PM
  > usd_cad <- oanda.instruments.bid("USD_CAD","S5",from="2020-12-15T15:30:00") #with a resolution of 5 seconds
  > head(usd_cad$candles)
    complete volume       time     b_o     b_h     b_l     b_c
  1     TRUE      2 1608064200 1.26891 1.26891 1.26890 1.26890
  2     TRUE      1 1608064210 1.26887 1.26887 1.26887 1.26887
  3     TRUE      1 1608064215 1.26889 1.26889 1.26889 1.26889
  4     TRUE      2 1608064230 1.26892 1.26892 1.26889 1.26889
  5     TRUE      2 1608064245 1.26891 1.26891 1.26890 1.26890
  6     TRUE      2 1608064260 1.26891 1.26891 1.26889 1.26889

  > libray(plotly)
  > usd_cad$candles$time <- as.POSIXct(usd_cad$candles$time,origin="1970-01-01")
  > fig <- usd_cad$candles[1:50,] %>% plot_ly(x = ~time, type="candlestick",open = ~b_o, close = ~b_c, high = ~b_h, low = ~b_l) 
  > fig <- fig %>% layout(title = "USD/CAD Candlestick Chart: Bid")
  > fig
  ```
  ![Bid candlestick](https://github.com/ltekengineering/oanda/blob/main/media/bid.png)

  ### Create a market order
  ```R
  > oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",10000) #long 10000 USD/MXN
  > oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",-10000) #short 10000 USD/MXN
  > oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",10000,priceBound=19.86263) #worst pirce of 19.86263 else kill the order
  > info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
  > oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",10000,priceBound=19.86263, info)  #add customization
  ```       

## Shiny Dashboard  
- [x] A shiny app to monitor account activity in real time. Visit the wiki page for a detailed [tutorial.](https://github.com/ltekengineering/oanda/wiki/C.-Dashboard)  

1. Go to dashboard directory
``` console
  foo@bar: cd dasboard
```
2. Edit token and accounts parameters in config_ppe.ini or config_prod.ini with your token and account numbers
    ![Ini file](https://github.com/ltekengineering/oanda/blob/main/media/key.png)
3. Start the dashboard
``` console
  foo@bar: ln -s config_ppe.ini config.ini 
  foo@bar: Rscript --no-save dash.r        
```
Point to config_prod.ini when you're ready to go live

4. Go here http://localhost:8989/

## Backtest & Algo Trading  
- Back-testing demo scripts have been removed

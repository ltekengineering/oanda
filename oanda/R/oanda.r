
#Start with the following default parameters
config<-new.env()
config$baseurl <- "https://api-fxpractice.oanda.com/v3/"
config$basestreamingurl <- "https://stream-fxpractice.oanda.com/v3"
config$token <- ""
#' Initialize and authenticate account
#'
#' Users of this package must run this function first in order for rest of the package to work. This prevents the need to
#' set url and authentication token redundantly for each function.
#' 
#' @param accountLevel The base url that points to either the practice or the production account
#' @param token The token generated on OANDA's website
#' @examples oanda.init("practice",'44667a4c1a94db7f245ea21fe2f82341-04efe2ed548b91e6bb2d7d7707668d8a')
#' @export
oanda.init<-function(accountLevel,token){
    if( tolower(accountLevel) == "practice" ){
        config$baseurl <- "https://api-fxpractice.oanda.com/v3/"
        config$basestreamingurl <- "https://stream-fxpractice.oanda.com/v3"
        config$token <- token
        # config<<-list(baseurl = "https://api-fxpractice.oanda.com/v3/", basestreamingurl = "https://stream-fxpractice.oanda.com/v3", token = token)
    }else if(tolower(accountLevel) == "production"){
        config$baseurl <- "https://api-fxtrade.oanda.com/v3/"
        config$basestreamingurl <- "https://stream-fxtrade.oanda.com/v3"
        config$token <- token
        # config<<-list(baseurl = "https://api-fxtrade.oanda.com/v3/", basestreamingurl = "https://stream-fxtrade.oanda.com/v3", token = token)
    }else{
        print("Accountlevel is either 'pratice' or 'production'")
    }      
}


oanda.parseDateTime <- function(dateTime){
    if( is.character(dateTime)){
        if(grepl("T",dateTime)){        
            return(as.numeric( strptime(dateTime,format="%Y-%m-%dT%H:%M:%S") ))            
        }else{
            return(as.numeric(dateTime))
        }
    }else{
        return(dateTime)
    }     
}

#' Accounts
#'
#' Brings backs all of the accounts and thier respective IDs associated with the token
#' @examples oanda.accounts()
#' @export
oanda.accounts<-function(){
    base.url<-config$baseurl
    a<-curl::new_handle()
    curl::handle_setheaders(a,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")    
    b<-curl::curl_fetch_memory(url=paste(base.url,"accounts",sep=""),handle=a)    
    if(is.null(b)) {
        print("Please check input parameters")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Account summary
#'
#' This function brings back summary such as unrealized PnL, account balance, account position, margin position, 
#' and as well as reference data as such account creation date and risk management settings for the account.
#' 
#' @param account The string representation of an Account Identifier.
#' @return A vector of account IDs
#' @examples oanda.account.summary('101-001-3704066-001')
#' @export
oanda.account.summary<-function(account){
    base.url<-config$baseurl
    a<-curl::new_handle()
    curl::handle_setheaders(a,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    if(missing(account)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/summary",account),sep=""),handle=a)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Accounts tradable instruments
#'
#' The function returns a list of tradeable instruments for the given Account. The list of tradeable instruments is dependent 
#' on the regulatory division that the Account is located in, thus should be the same for all Accounts owned by a single user.
#'  
#' @param account The string representation of an Account Identifier.
#' @return A list of instruments and details
#' @examples oanda.account.instruments('101-001-3704066-001')
#' @export
oanda.account.instruments<-function(account){
    base.url<-config$baseurl
    a<-curl::new_handle()
    curl::handle_setheaders(a,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    if(missing(account)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/instruments",account),sep=""),handle=a)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Accounts tradable instruments
#'
#' The function returns a data frame of tradeable instruments for the given Account. The list of tradeable instruments is dependent 
#' on the regulatory division that the Account is located in, thus should be the same for all Accounts owned by a single user.
#'  
#' @param account The string representation of an Account Identifier.
#' @return A data frame of instruments and details
#' @examples oanda.account.instruments.data.frame('101-001-3704066-001')
#' @export
oanda.account.instruments.data.frame<-function(account){
    #retrieve instruments from oanda server
    instruments<-oanda.account.instruments(account)
    
    if(!is.null(instruments) && class(instruments) == 'list'){
        a.list.of.data.frames <- lapply(instruments$instruments,function(x){
            #specs
            specs<-as.data.frame.list(x[1:13])
            #rates
            rates <- t(as.matrix(x$financing[1:2]))
            colnames(rates) <- paste("Financing",colnames(rates),sep=".")
            #days
            days <- t(matrix(unlist(x$financing$financingDaysOfWeek),byrow = T,nrow=length(x$financing$financingDaysOfWeek)))
            colnames(days)<-paste("Financing.days",days[1,],sep=".")
            days <- as.data.frame(days)[-1,]

            temp<-cbind(specs,rates,days)
            row.names(temp)<-NULL
            return(temp)
        })
    
        return(do.call("rbind",a.list.of.data.frames))#return the rbind of the list of data frames
    }else{
        print("Please enter a valid account id")        
        return(NULL)
    }
    
}

#' Account changes
#'
#' The function is used to poll an Account for its current state and changes since a specified TransactionID.
#'  
#' @param account The string representation of an Account Identifier.
#' @param transactionId The last transaction id for the account.
#' @return A list 
#' @examples oanda.account.changes('101-001-3704066-001',4567)
#' @export
oanda.account.changes<-function(account,transactionId){
    a<-curl::new_handle()
    curl::handle_setheaders(a,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    if(missing(account)){
        b<-NULL        
    }else{
        url.str<-sprintf("%saccounts/%s/changes?sinceTransactionID=%s",config$baseurl,account,transactionId)
        b<-curl::curl_fetch_memory(url=url.str,handle=a)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id and transaction id")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}


#' Instrument bid candlestick 
#' @description
#' This function will return open, high, low and closing prices from the bid side of the market. The return time series are represented in UTC no matter the time zone.
#' The following settings are set either by default or with purpose to reduce too many variations of one function. \cr
#' #price = B \cr
#' #count = 5000 \cr
#' #smooth = False \cr
#' #includeFirst = True //implicitly set by oanda \cr
#' #dailyAlignment = 17 //Optional parameter otherwise oanda sets it to 17 \cr
#' #weeklyAlignment = Friday //Optional parameter otherwise oanda sets it to Friday \cr
#' #alignmentTimezone = America/New_York //Optional parameter otherwise oanda sets it to America/New_York \cr
#' 
#' @source \url{http://developer.oanda.com/rest-live-v20/instrument-ep/}
#' @param instrument The name of the instrument such as EUR_USD
#' @param granularity Frequency of the candlestick such as S5,S10,S15, etc. Please visit www.oanda.com for more details.
#' @param from The start of the time range in unix timestamp to fetch candlesticks for. Optional if 'to' is set.
#' @param to The end of the time range in unix timestamp to fetch candlesticks for. Optional if 'from' is set.
#' @param dailyAlignment The hour of the day (in the specified timezone) to use for granularities that have daily alignments. [default=17, minimum=0, maximum=23]
#' @param weeklyAlignment The day of the week used for granularities that have weekly alignment. [default=Friday]
#' @param alignmentTimezone The timezone to use for the dailyAlignment parameter. Candlesticks with daily alignment will be aligned to the dailyAlignment hour within the alignmentTimezone. Note that the returned times will still be represented in UTC. [default=America/New_York]
#' @examples oanda.instruments.bid("EUR_USD","M1",from = 1608064200) #2020-12-15 03:30 PM
#' @examples oanda.instruments.bid("EUR_USD","M1",from = "1608064200") #2020-12-15 03:30 PM
#' @examples oanda.instruments.bid("USD_CAD","S5",from="2020-12-15T15:30:00")
#' @export
oanda.instruments.bid<-function(instrument,granularity,from,to,dailyAlignment,weeklyAlignment,alignmentTimezone){
    curl.handle<-curl::new_handle() #create a handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers    
    parameters = list()#create an empty list    
    parameters[length(parameters)+1]<-"count=5000"
    parameters[length(parameters)+1]<-"price=B"
    parameters[length(parameters)+1]<-sprintf("granularity=%s",granularity)
    if(!missing(from)) parameters[length(parameters)+1]<-sprintf("from=%i", oanda.parseDateTime(from))
    if(!missing(to)) parameters[length(parameters)+1]<-sprintf("to=%i", oanda.parseDateTime(to))
    if(!missing(dailyAlignment)) parameters[length(parameters)+1]<-sprintf("dailyAlignment=%i",dailyAlignment)
    if(!missing(weeklyAlignment)) parameters[length(parameters)+1]<-sprintf("weeklyAlignment=%s",weeklyAlignment)
    if(!missing(alignmentTimezone)) parameters[length(parameters)+1]<-sprintf("alignmentTimezone=%s",alignmentTimezone)
    
    url.str<-sprintf("%sinstruments/%s/candles?%s",config$baseurl,instrument,paste(parameters,collapse="&"))     
                
    b <- curl::curl_fetch_memory(url=url.str,handle=curl.handle)
    
    if (b$status !=200){
        print(paste("HTTP Status Code:", b$status))
        print(paste("Server Message:", rawToChar(b$content)))
        return(NULL)
    }else{
        parsed.content<-jsonlite::parse_json(rawToChar(b$content))
#         return(parsed.content)
        instrument.names<-c("complete","volume","time","b_o","b_h","b_l","b_c")
        #unlist the candles, makes N length rows and divide the data equally
        parsed.content$candles<-matrix(unlist(parsed.content$candles),nrow=length(parsed.content$candles),byrow = T)
        colnames(parsed.content$candles)<-instrument.names
        #convert the data
        parsed.content$candles<-sapply(colnames(parsed.content$candles),function(x){
            if(x!="complete"){
                as.numeric(parsed.content$candles[,x])
            }else{
                as.logical(parsed.content$candles[,x])
            }
        })
        parsed.content$candles<-as.data.frame(parsed.content$candles)
        parsed.content$candles$complete<-as.logical(parsed.content$candles$complete)
        return(parsed.content)
    }    
}

#' Instrument mid candlestick 
#' @description
#' This function will return open, high, low and closing prices from the bid side of the market. The return time series are represented in UTC no matter the time zone.
#' The following settings are set either by default or with purpose to reduce too many variations of one function. \cr
#' #price = M \cr
#' #count = 5000 \cr
#' #smooth = False \cr
#' #includeFirst = True //implicitly set by oanda \cr
#' #dailyAlignment = 17 //Optional parameter otherwise oanda sets it to 17 \cr
#' #weeklyAlignment = Friday //Optional parameter otherwise oanda sets it to Friday \cr
#' #alignmentTimezone = America/New_York //Optional parameter otherwise oanda sets it to America/New_York \cr
#' @source \url{http://developer.oanda.com/rest-live-v20/instrument-ep/}
#' @param instrument The name of the instrument such as EUR_USD
#' @param granularity Frequency of the candlestick such as S5,S10,S15, etc. Please visit www.oanda.com for more details.
#' @param from The start of the time range in unix timestamp to fetch candlesticks for. Optional if 'to' is set.
#' @param to The end of the time range in unix timestamp to fetch candlesticks for. Optional if 'from' is set.
#' @param dailyAlignment The hour of the day (in the specified timezone) to use for granularities that have daily alignments. [default=17, minimum=0, maximum=23]
#' @param weeklyAlignment The day of the week used for granularities that have weekly alignment. [default=Friday]
#' @param alignmentTimezone The timezone to use for the dailyAlignment parameter. Candlesticks with daily alignment will be aligned to the dailyAlignment hour within the alignmentTimezone. Note that the returned times will still be represented in UTC. [default=America/New_York]
#' @examples oanda.instruments.mid("EUR_USD","M1",from = 1608064200) #2020-12-15 03:30 PM
#' @examples oanda.instruments.mid("EUR_USD","M1",from = "1608064200") #2020-12-15 03:30 PM
#' @examples oanda.instruments.mid("USD_CAD","S5",from="2020-12-15T15:30:00")
#' @export
oanda.instruments.mid<-function(instrument,granularity,from,to,dailyAlignment,weeklyAlignment,alignmentTimezone){
    curl.handle<-curl::new_handle() #create a handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")            
    
    parameters = list()#create an empty list    
    parameters[length(parameters)+1]<-"count=5000"
    parameters[length(parameters)+1]<-"price=M"
    parameters[length(parameters)+1]<-sprintf("granularity=%s",granularity)
    if(!missing(from)) parameters[length(parameters)+1]<-sprintf("from=%i", oanda.parseDateTime(from))
    if(!missing(to)) parameters[length(parameters)+1]<-sprintf("to=%i", oanda.parseDateTime(to))
    if(!missing(dailyAlignment)) parameters[length(parameters)+1]<-sprintf("dailyAlignment=%i",dailyAlignment)
    if(!missing(weeklyAlignment)) parameters[length(parameters)+1]<-sprintf("weeklyAlignment=%s",weeklyAlignment)
    if(!missing(alignmentTimezone)) parameters[length(parameters)+1]<-sprintf("alignmentTimezone=%s",alignmentTimezone)
    
    url.str<-sprintf("%sinstruments/%s/candles?%s",config$baseurl,instrument,paste(parameters,collapse="&")) 
                
    b <- curl::curl_fetch_memory(url=url.str,handle=curl.handle)    
    if (b$status !=200){
        print(paste("HTTP Status Code:", b$status))
        print(paste("Server Message:", rawToChar(b$content)))
        return(NULL)
    }else{
        parsed.content<-jsonlite::parse_json(rawToChar(b$content))
        instrument.names<-c("complete","volume","time","m_o","m_h","m_l","m_c")
        #unlist the candles, makes N length rows and divide the data equally
        parsed.content$candles<-matrix(unlist(parsed.content$candles),nrow=length(parsed.content$candles),byrow = T)
        colnames(parsed.content$candles)<-instrument.names
        #convert the data
        parsed.content$candles<-sapply(colnames(parsed.content$candles),function(x){
            if(x!="complete"){
                as.numeric(parsed.content$candles[,x])
            }else{
                as.logical(parsed.content$candles[,x])
            }
        })
        parsed.content$candles<-as.data.frame(parsed.content$candles)
        parsed.content$candles$complete<-as.logical(parsed.content$candles$complete)
        return(parsed.content)
    }    
}

#' Instrument ask candlestick 
#' @description
#' This function will return open, high, low and closing prices from the bid side of the market. The return time series are represented in UTC no matter the time zone.
#' The following settings are set either by default or with purpose to reduce too many variations of one function. \cr
#' #price = A \cr
#' #count = 5000 \cr
#' #smooth = False \cr
#' #includeFirst = True //implicitly set by oanda \cr
#' #dailyAlignment = 17 //Optional parameter otherwise oanda sets it to 17 \cr
#' #weeklyAlignment = Friday //Optional parameter otherwise oanda sets it to Friday \cr
#' #alignmentTimezone = America/New_York //Optional parameter otherwise oanda sets it to America/New_York \cr
#' @source \url{http://developer.oanda.com/rest-live-v20/instrument-ep/}
#' @param instrument The name of the instrument such as EUR_USD
#' @param granularity Frequency of the candlestick such as S5,S10,S15, etc. Please visit www.oanda.com for more details.
#' @param from The start of the time range in unix timestamp to fetch candlesticks for. Optional if 'to' is set.
#' @param to The end of the time range in unix timestamp to fetch candlesticks for. Optional if 'from' is set.
#' @param dailyAlignment The hour of the day (in the specified timezone) to use for granularities that have daily alignments. [default=17, minimum=0, maximum=23]
#' @param weeklyAlignment The day of the week used for granularities that have weekly alignment. [default=Friday]
#' @param alignmentTimezone The timezone to use for the dailyAlignment parameter. Candlesticks with daily alignment will be aligned to the dailyAlignment hour within the alignmentTimezone. Note that the returned times will still be represented in UTC. [default=America/New_York]
#' @examples oanda.instruments.ask("EUR_USD","M1",from = 1608064200) #2020-12-15 03:30 PM
#' @examples oanda.instruments.ask("EUR_USD","M1",from = "1608064200") #2020-12-15 03:30 PM
#' @examples oanda.instruments.ask("USD_CAD","S5",from="2020-12-15T15:30:00")
#' @export
oanda.instruments.ask<-function(instrument,granularity,from,to,dailyAlignment,weeklyAlignment,alignmentTimezone){
    curl.handle<-curl::new_handle() #create a handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")            
    
    parameters = list()#create an empty list    
    parameters[length(parameters)+1]<-"count=5000"
    parameters[length(parameters)+1]<-"price=A"
    parameters[length(parameters)+1]<-sprintf("granularity=%s",granularity)
    if(!missing(from)) parameters[length(parameters)+1]<-sprintf("from=%i", oanda.parseDateTime(from))
    if(!missing(to)) parameters[length(parameters)+1]<-sprintf("to=%i", oanda.parseDateTime(to))
    if(!missing(dailyAlignment)) parameters[length(parameters)+1]<-sprintf("dailyAlignment=%i",dailyAlignment)
    if(!missing(weeklyAlignment)) parameters[length(parameters)+1]<-sprintf("weeklyAlignment=%s",weeklyAlignment)
    if(!missing(alignmentTimezone)) parameters[length(parameters)+1]<-sprintf("alignmentTimezone=%s",alignmentTimezone)
    
    url.str<-sprintf("%sinstruments/%s/candles?%s",config$baseurl,instrument,paste(parameters,collapse="&")) 
    
    b <- curl::curl_fetch_memory(url=url.str,handle=curl.handle)    
    if (b$status !=200){
        print(paste("HTTP Status Code:", b$status))
        print(paste("Server Message:", rawToChar(b$content)))
        return(NULL)
    }else{
        parsed.content<-jsonlite::parse_json(rawToChar(b$content))
        instrument.names<-c("complete","volume","time","a_o","a_h","a_l","a_c")
        #unlist the candles, makes N length rows and divide the data equally
        parsed.content$candles<-matrix(unlist(parsed.content$candles),nrow=length(parsed.content$candles),byrow = T)
        colnames(parsed.content$candles)<-instrument.names
        #convert the data
        parsed.content$candles<-sapply(colnames(parsed.content$candles),function(x){
            if(x!="complete"){
                as.numeric(parsed.content$candles[,x])
            }else{
                as.logical(parsed.content$candles[,x])
            }
        })
        parsed.content$candles<-as.data.frame(parsed.content$candles)
        parsed.content$candles$complete<-as.logical(parsed.content$candles$complete)
        return(parsed.content)
    }    
}

#' Instrument Bid, Mid, Ask candlestick 
#' @description
#' This function will return open, high, low and closing prices from the bid side of the market. The return time series are represented in UTC no matter the time zone.
#' The following settings are set either by default or with purpose to reduce too many variations of one function. \cr
#' #price = BMA \cr
#' #count = 5000 \cr
#' #smooth = False \cr
#' #includeFirst = True //implicitly set by oanda \cr
#' #dailyAlignment = 17 //Optional parameter otherwise oanda sets it to 17 \cr
#' #weeklyAlignment = Friday //Optional parameter otherwise oanda sets it to Friday \cr
#' #alignmentTimezone = America/New_York //Optional parameter otherwise oanda sets it to America/New_York \cr
#' @source \url{http://developer.oanda.com/rest-live-v20/instrument-ep/}
#' @param instruments The name of the instrument such as EUR_USD
#' @param granularity Frequency of the candlestick such as S5,S10,S15, etc. Please visit www.oanda.com for more details.
#' @param from The start of the time range in unix timestamp to fetch candlesticks for. Optional if 'to' is set.
#' @param to The end of the time range in unix timestamp to fetch candlesticks for. Optional if 'from' is set.
#' @param dailyAlignment The hour of the day (in the specified timezone) to use for granularities that have daily alignments. [default=17, minimum=0, maximum=23]
#' @param weeklyAlignment The day of the week used for granularities that have weekly alignment. [default=Friday]
#' @param alignmentTimezone The timezone to use for the dailyAlignment parameter. Candlesticks with daily alignment will be aligned to the dailyAlignment hour within the alignmentTimezone. Note that the returned times will still be represented in UTC. [default=America/New_York]
#' @examples oanda.instruments.all("EUR_USD","M1",from = 1608064200) #2020-12-15 03:30 PM
#' @examples oanda.instruments.all("EUR_USD","M1",from = "1608064200") #2020-12-15 03:30 PM
#' @examples oanda.instruments.all("USD_CAD","S5",from="2020-12-15T15:30:00")
#' @export
oanda.instruments.all<-function(instrument,granularity,from,to,dailyAlignment,weeklyAlignment,alignmentTimezone){
    curl.handle<-curl::new_handle() #create a handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")            
    
    parameters = list()#create an empty list    
    parameters[length(parameters)+1]<-"count=5000"
    parameters[length(parameters)+1]<-"price=BMA"
    parameters[length(parameters)+1]<-sprintf("granularity=%s",granularity)
    if(!missing(from)) parameters[length(parameters)+1]<-sprintf("from=%i", oanda.parseDateTime(from))
    if(!missing(to)) parameters[length(parameters)+1]<-sprintf("to=%i", oanda.parseDateTime(to))
    if(!missing(dailyAlignment)) parameters[length(parameters)+1]<-sprintf("dailyAlignment=%i",dailyAlignment)
    if(!missing(weeklyAlignment)) parameters[length(parameters)+1]<-sprintf("weeklyAlignment=%s",weeklyAlignment)
    if(!missing(alignmentTimezone)) parameters[length(parameters)+1]<-sprintf("alignmentTimezone=%s",alignmentTimezone)
    
    url.str<-sprintf("%sinstruments/%s/candles?%s",config$baseurl,instrument,paste(parameters,collapse="&")) 
                
    b <- curl::curl_fetch_memory(url=url.str,handle=curl.handle)
    
    if (b$status !=200){
        print(paste("HTTP Status Code:", b$status))
        print(paste("Server Message:", rawToChar(b$content)))
        return(NULL)
    }else{
        parsed.content<-jsonlite::parse_json(rawToChar(b$content))
#         return(parsed.content)
        instrument.names<-c("complete","volume","time","b_o","b_h","b_l","b_c","m_o","m_h","m_l","m_c","a_o","a_h","a_l","a_c")
        #unlist the candles, makes N length rows and divide the data equally
        parsed.content$candles<-matrix(unlist(parsed.content$candles),nrow=length(parsed.content$candles),byrow = T)
        colnames(parsed.content$candles)<-instrument.names
        #convert the data
        parsed.content$candles<-sapply(colnames(parsed.content$candles),function(x){
            if(x!="complete"){
                as.numeric(parsed.content$candles[,x])
            }else{
                as.logical(parsed.content$candles[,x])
            }
        })
        parsed.content$candles<-as.data.frame(parsed.content$candles)
        parsed.content$candles$complete<-as.logical(parsed.content$candles$complete)
        return(parsed.content)
    }    
}

#' Instrument orderbook
#'
#' This function returns the orderbook for an instrument. Visit http://developer.oanda.com/rest-live-v20/instrument-ep/ or www.oanda.com for details
#' 
#' @param instrument The name of the instrument such as EUR_USD
#' @return A named list that includes the orderbook dataframe 
#' @examples oanda.instrument.orderbook("EUR_USD")
#' @export
oanda.instrument.orderbook<-function(instrument){
    #create header handlers
    curl.handle<-curl::new_handle()
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    #make the connection
    url.str<-sprintf("%sinstruments/%s/orderBook",config$baseurl,instrument)    
    b<-curl::curl_fetch_memory(url=url.str,handle=curl.handle)
    
    if (b$status !=200){
        print(paste("HTTP Status Code:", b$status))
        print(paste("Server Message:", rawToChar(b$content)))
        return(NULL)
    }else{
        content<-jsonlite::parse_json(rawToChar(b$content))#parse json data
    
        #unlist the candles, makes N length rows and divide the data equally
        temp<-matrix(unlist(content$orderBook$buckets),nrow=length(content$orderBook$buckets),byrow = T)
        temp<-apply(temp,2,as.numeric) #2 here means do the do the job by columns
        content$orderBook$buckets<-data.frame(temp)
        names(content$orderBook$buckets)<-c("price","longCountPercent","shortCountPercent")
                
        return(content$orderBook)
    }   
}

#' Instrument positionbook
#'
#' This function returns the positionbook for an instrument. Visit http://developer.oanda.com/rest-live-v20/instrument-ep/ or www.oanda.com for details.
#'
#' @param instrument The name of the instrument such as EUR_USD
#' @return A named list that includes the positionbook dataframe 
#' @examples oanda.instrument.positionbook("EUR_USD")
#' @export
oanda.instrument.positionbook<-function(instrument){
    #create header handlers
    curl.handle<-curl::new_handle()
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    #make the connection
    url.str<-sprintf("%sinstruments/%s/positionBook",config$baseurl,instrument)    
    b<-curl::curl_fetch_memory(url=url.str,handle=curl.handle)
    
    if (b$status !=200){
        print(paste("HTTP Status Code:", b$status))
        print(paste("Server Message:", rawToChar(b$content)))
        return(NULL)
    }else{
        content<-jsonlite::parse_json(rawToChar(b$content))#parse json data
    
        #unlist the candles, makes N length rows and divide the data equally
        temp<-matrix(unlist(content$positionBook$buckets),nrow=length(content$positionBook$buckets),byrow = T)
        temp<-apply(temp,2,as.numeric) #2 here means do the do the job by columns
        content$positionBook$buckets<-data.frame(temp)
        names(content$positionBook$buckets)<-c("price","longCountPercent","shortCountPercent")
                
        return(content$positionBook)
    }   
}

#' Instrument Pricing
#'
#' This function returns pricing information for a specified list of Instruments within an Account. Visit http://developer.oanda.com/rest-live-v20/instrument-ep/ or www.oanda.com for details.
#'
#' @param account The string representation of an Account Identifier.
#' @param instruments The name of the instrument(s)
#' @return A named R list of instruments latest pricing data
#' @examples oanda.instrument.pricing('101-001-3704066-001','EUR_USD')
#' @examples oanda.instrument.pricing('101-001-3704066-001','EUR_USD,USD_CAD')
#' @export
oanda.instrument.pricing<-function(account,instruments){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    if(missing(account) && missing(instruments)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/pricing?instruments=%s",account,instruments),sep=""),handle=curl.handle)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id")
        return(NULL)
    }else{         
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Instrument price stream
#' 
#' Get a stream of Account Prices starting from when the request is made. This pricing stream does not include every single price created for the Account, but instead will provide at most 4 prices per second (every 250 milliseconds) for each instrument being requested.
#' If more than one price is created for an instrument during the 250 millisecond window, only the price in effect at the end of the window is sent. This means that during periods of rapid price movement, subscribers to this stream will not be sent every price.
#' Pricing windows for different connections to the price stream are not all aligned in the same way (i.e. they are not all aligned to the top of the second). This means that during periods of rapid price movement, different subscribers may observe different prices depending on their alignment.
#' 
#' @param account The string representation of an Account Identifier.
#' @param instruments The name of the instrument(s)
#' @param func A custom function
#' @return This is a void function therefore does not return any value. Without a function the default behavior of this function is to print the stream to console.
#' @examples oanda.instrument.stream('101-001-3704066-001','EUR_USD')
#' @examples 
#' oanda.instrument.stream('101-001-3704066-001','EUR_USD',function(x){
#'    #do something at each stream
#'    print(x) #or simply print it
#' })
#' @export
oanda.instrument.stream<- function(account,instrument,func){
    curl.handle<-curl::new_handle()#create a handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    if( missing(func)){        
        curl::curl_fetch_stream(url=sprintf("%s/accounts/%s/pricing/stream?instruments=%s",config$basestreamingurl,account,instrument),handle=curl.handle,fun=function(x){
        print(jsonlite::prettify(rawToChar(x))) })
    }else{
        curl::curl_fetch_stream(url=sprintf("https://stream-fxpractice.oanda.com/v3/accounts/%s/pricing/stream?instruments=%s",account,instrument),handle=curl.handle,fun=func)
    }
}

#' Create client extensions
#' 
#' A ClientExtensions object allows a client to attach a clientID, tag and comment to Orders and Trades in their Account.  
#' Do not set, modify, or delete this field if your account is associated with MT4.
#'
#' @param comment A comment associated with the Order/Trade
#' @param tag A tag associated with the Order/Trade
#' @param id The Client ID of the Order/Trade
#' @return A JSON object of ClientExtensions
#' @examples oanda.orders.createClientExtensions("A Comment","strategy_9","my_order_10000")
#' @export
oanda.orders.createClientExtensions <- function(comment, tag, id){
    clientExtensions <- list(comment = comment, tag = tag, id = id)
    return(jsonlite::toJSON(clientExtensions,auto_unbox=T))   
}

#' Create Take profit details
#' 
#' TakeProfitDetails specifies the details of a Take Profit Order to be created on behalf of a client. This may happen when an Order is filled
#' that opens a Trade requiring a Take Profit, or when a Trade’s dependent. Take Profit Order is modified directly through the Trade.
#'
#' @param price The price that the Take Profit Order will be triggered at
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional
#' @return A JSON object of TakeProfitDetails
#' @examples oanda.orders.createTakeProfitDetails(1.2345)
#' info <- oanda.orders.createClientExtensions("100 pips away from trade open","strategy_9","my_order_10000")
#' oanda.orders.createTakeProfitDetails(1.2345, info)
#' @export
oanda.orders.createTakeProfitDetails <- function(price, clientExtensions){
    TakeProfitDetails = list()
    TakeProfitDetails[length(TakeProfitDetails)+1]<-sprintf('"price":"%s"',price)
    TakeProfitDetails[length(TakeProfitDetails)+1]<-'"timeInForce":"GTC"'     
    if(!missing(clientExtensions)) TakeProfitDetails[length(TakeProfitDetails)+1]<-sprintf('"clientExtensions":%s',clientExtensions)   
    
    x<-sprintf("{%s}", paste(TakeProfitDetails,collapse=","))#setup trade specifier
    return(x) 
}

#' Create Stop Loss details
#' 
#' StopLossDetails specifies the details of a Stop Loss Order to be created on behalf of a client. This may happen when an Order is filled that opens
#' a Trade requiring a Stop Loss, or when a Trade’s dependent Stop Loss Order is modified directly through the Trade.
#'
#' @param price The price that the Take Profit Order will be triggered at
#' @param distance  Specifies the distance (in price units) from the Trade’s open price to use as the Stop Loss Order price. Only one of the distance and price fields may be specified.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional
#' @return A JSON object of StopLossDetails
#' @examples oanda.orders.createStopLossDetails(price = 1.2345)
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createStopLossDetails(1.2345, info)
#' oanda.orders.createStopLossDetails(distance = 0.0100)
#' oanda.orders.createStopLossDetails(distance = 0.0100,info)
#' @export
oanda.orders.createStopLossDetails <- function(price, distance, clientExtensions){
    StopLossDetails = list()    
    if( !missing(price) && !missing(distance)){
        StopLossDetails[length(StopLossDetails)+1]<-sprintf('"price":"%s"',price)
    }else if ( !missing(distance)){
        StopLossDetails[length(StopLossDetails)+1]<-sprintf('"distance":"%s"',distance)
    }else{
        StopLossDetails[length(StopLossDetails)+1]<-sprintf('"price":"%s"',price)
    }
    StopLossDetails[length(StopLossDetails)+1]<-'"timeInForce":"GTC"' 
    if(!missing(clientExtensions)) StopLossDetails[length(StopLossDetails)+1]<-sprintf('"clientExtensions":%s',clientExtensions)   
    
    x<-sprintf("{%s}", paste(StopLossDetails,collapse=","))#setup trade specifier
    return(x) 
}

#' Create Trailing Stop Loss details
#' 
#' TrailingStopLossDetails specifies the details of a Trailing Stop Loss Order to be created on behalf of a client. 
#' This may happen when an Order is filled that opens a Trade requiring a Trailing Stop Loss, or 
#' when a Trade’s dependent Trailing Stop Loss Order is modified directly through the Trade.
#'
#' @param distance  Specifies the distance (in price units) from the Trade’s open price to use as the Stop Loss Order price. Only one of the distance and price fields may be specified.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional.
#' @return A JSON object of StopLossDetails.
#' @examples 
#' oanda.orders.createTrailingStopLossDetails(distance = 0.0500)
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createStopLossDetails(distance = 0.0100, info)
#' @export
oanda.orders.createTrailingStopLossDetails <- function(distance, clientExtensions){
    TrailingStopLossDetails = list()
    TrailingStopLossDetails[length(TrailingStopLossDetails)+1]<-sprintf('"distance":"%s"',distance)
    TrailingStopLossDetails[length(TrailingStopLossDetails)+1]<-'"timeInForce":"GTC"' 
    if(!missing(clientExtensions)) TrailingStopLossDetails[length(TrailingStopLossDetails)+1]<-sprintf('"clientExtensions":%s',clientExtensions)   
    
    x<-sprintf("{%s}", paste(TrailingStopLossDetails,collapse=","))#setup trade specifier
    return(x) 
}

#' Create a market order
#' 
#' This function initiates a market order
#'
#' @param account The string representation of an Account Identifier.
#' @param instrument  The Market Order’s Instrument.
#' @param units The quantity requested to be filled by the Market Order. A positive number of units results in a long Order, and a negative number of units results in a short Order.
#' @param priceBound The worst price that the client is willing to have the Market Order filled at. This is optional.
#' @param clientExtensions The Client Extensions to add to the market order when created. This is optional.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",10000) #long 10000 USD/MXN
#' oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",-10000) #short 10000 USD/MXN
#' oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",10000,priceBound=19.86263)
#' #long 10000 USD/MXN with a worst pirce of 19.86263 else kill the order
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createMarketOrder('101-001-3704066-001',"USD_MXN",10000,priceBound=19.86263,info)
#' #long 10000 USD/MXN with a worst pirce of 19.86263 else kill the order
#' @export
oanda.orders.createMarketOrder<-function(account,instrument,units,priceBound,clientExtensions){        
    if(missing(account) || missing(instrument) || missing(units)){
        print("Please check your input parameters")
        return(NULL)           
    }else{
        base.url<-config$baseurl
        order = list()
        order[length(order)+1]<-'"type":"MARKET"'
        order[length(order)+1]<-'"positionFill":"DEFAULT"'        
        order[length(order)+1]<-'"timeInForce":"FOK"'        
        order[length(order)+1]<-sprintf('"instrument":"%s"',instrument)
        order[length(order)+1]<-sprintf('"units":"%s"',units)
        if(!missing(priceBound)) order[length(order)+1]<-sprintf('"priceBound":"%s"',priceBound)
        if(!missing(clientExtensions)) order[length(order)+1]<-sprintf('"clientExtensions":%s',clientExtensions)
        
        x <- sprintf('{"order":{ %s }}',paste(order,collapse=","))
        curl.handle<-curl::new_handle()#create a curl handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers    
        curl::handle_setopt(curl.handle, copypostfields = x)#create the post request
        
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders",account),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

#' Create a limit order
#' 
#' This function initiates a limit order
#'
#' @param account The string representation of an Account Identifier.
#' @param instrument  The Market Order’s Instrument.
#' @param units The quantity requested to be filled by the Market Order. A positive number of units results in a long Order, and a negative number of units results in a short Order.
#' @param price he price threshold specified for the Limit Order. The Limit Order will only be filled by a market price that is equal to or better than this price.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.createLimitOrder('101-001-3704066-001',"USD_HKD",10000,7.75405) 
#' #long 10000 limit order of 7.75405 of USD/HKD
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createLimitOrder('101-001-3704066-001',"USD_HKD",10000,7.75405, info)
#' #long 10000 limit order of 7.75405 of USD/HKD with client information
#' @export
oanda.orders.createLimitOrder<-function(account,instrument,units,price,clientExtensions){        
    if(missing(account) || missing(instrument) || missing(units) || missing(price)){
        print("Please check your input parameters")
        return(NULL)           
    }else{
        base.url<-config$baseurl
        order = list()
        order[length(order)+1]<-'"type":"LIMIT"'
        order[length(order)+1]<-'"positionFill":"DEFAULT"'
        order[length(order)+1]<-'"triggerCondition":"DEFAULT"'
        order[length(order)+1]<-'"timeInForce":"GTC"'        
        order[length(order)+1]<-sprintf('"instrument":"%s"',instrument)
        order[length(order)+1]<-sprintf('"units":"%s"',units)
        order[length(order)+1]<-sprintf('"price":"%s"',price)
        if(!missing(clientExtensions)) order[length(order)+1]<-sprintf('"clientExtensions":%s',clientExtensions)
        
        x <- sprintf('{"order":{ %s }}',paste(order,collapse=","))
#         print(prettify(x))
        curl.handle<-curl::new_handle()#create a curl handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers    
        curl::handle_setopt(curl.handle, copypostfields = x)#create the post request
        
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders",account),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

#' Create a stop order
#' 
#' This function initiates a stop order
#'
#' @param account The string representation of an Account Identifier.
#' @param instrument  The Market Order’s Instrument.
#' @param units The quantity requested to be filled by the Market Order. A positive number of units results in a long Order, and a negative number of units results in a short Order.
#' @param price he price threshold specified for the Limit Order. The Limit Order will only be filled by a market price that is equal to or better than this price.
#' @param priceBound The worst price that the client is willing to have the Market Order filled at. This is optional.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.createStopOrder('101-001-3704066-001',"USD_NOK",10000,8.62792)
#' #long 10000 stop order of 8.62792 of USD/NOK
#' oanda.orders.createStopOrder('101-001-3704066-001',"USD_NOK",10000,8.62792, priceBound = 8.72792)
#' #long 10000 of USD/NOK stop order at 8.62792 with slippage/gap protection at 8.72792
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createStopOrder('101-001-3704066-001', "USD_NOK", 10000, 8.62792, 8.72792, info)
#' #long 10000 of USD/NOK stop order at 8.62792 with slippage/gap protection at 8.72792 with client information
#' @export
oanda.orders.createStopOrder<-function(account, instrument, units, price, priceBound, clientExtensions){        
    if(missing(account) || missing(instrument) || missing(units) || missing(price)){
        print("Please check your input parameters")
        return(NULL)           
    }else{
        base.url<-config$baseurl
        order = list()
        order[length(order)+1]<-'"type":"STOP"'
        order[length(order)+1]<-'"positionFill":"DEFAULT"'
        order[length(order)+1]<-'"triggerCondition":"DEFAULT"'
        order[length(order)+1]<-'"timeInForce":"GTC"'        
        order[length(order)+1]<-sprintf('"instrument":"%s"',instrument)
        order[length(order)+1]<-sprintf('"units":"%s"',units)
        order[length(order)+1]<-sprintf('"price":"%s"',price)
        if(!missing(priceBound)) order[length(order)+1]<-sprintf('"priceBound":"%s"',priceBound)
        if(!missing(clientExtensions)) order[length(order)+1]<-sprintf('"clientExtensions":%s',clientExtensions)
        
        x <- sprintf('{"order":{ %s }}',paste(order,collapse=","))
#         print(prettify(x))
        curl.handle<-curl::new_handle()#create a curl handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers    
        curl::handle_setopt(curl.handle, copypostfields = x)#create the post request
        
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders",account),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}


#' Create a market if touch order
#' 
#' This function initiates market order once a price touches a point
#'
#' @param account The string representation of an Account Identifier.
#' @param instrument  The Market Order’s Instrument.
#' @param units The quantity requested to be filled by the Market Order. A positive number of units results in a long Order, and a negative number of units results in a short Order.
#' @param price he price threshold specified for the Limit Order. The Limit Order will only be filled by a market price that is equal to or better than this price.
#' @param priceBound The worst price that the client is willing to have the Market Order filled at. This is optional.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.createMarketIfTouchedOrder('101-001-3704066-001', "USD_SGD", 10000, 1.32821)
#' #long 10000 market if touch at 1.32821 of USD/SGD
#' oanda.orders.createMarketIfTouchedOrder('101-001-3704066-001', "USD_SGD", 10000, 1.32821, 1.42821)
#' #long 10000 of USD/SGD market if touch order at 1.32821 with slippage/gap protection at 1.42821
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createStopOrder('101-001-3704066-001', "USD_SGD", 10000, 1.32821, 1.42821, info)
#' 
#' @export
oanda.orders.createMarketIfTouchedOrder<-function(account, instrument, units, price, priceBound, clientExtensions){        
    if(missing(account) || missing(instrument) || missing(units) || missing(price)){
        print("Please check your input parameters")
        return(NULL)           
    }else{
        base.url<-config$baseurl
        order = list()
        order[length(order)+1]<-'"type":"MARKET_IF_TOUCHED"'
        order[length(order)+1]<-'"positionFill":"DEFAULT"'
        order[length(order)+1]<-'"triggerCondition":"DEFAULT"'
        order[length(order)+1]<-'"timeInForce":"GTC"'        
        order[length(order)+1]<-sprintf('"instrument":"%s"',instrument)
        order[length(order)+1]<-sprintf('"units":"%s"',units)
        order[length(order)+1]<-sprintf('"price":"%s"',price)
        if(!missing(priceBound)) order[length(order)+1]<-sprintf('"priceBound":"%s"',priceBound)
        if(!missing(clientExtensions)) order[length(order)+1]<-sprintf('"clientExtensions":%s',clientExtensions)
        
        x <- sprintf('{"order":{ %s }}',paste(order,collapse=","))
#         print(prettify(x))
        curl.handle<-curl::new_handle()#create a curl handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers    
        curl::handle_setopt(curl.handle, copypostfields = x)#create the post request
        
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders",account),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}


#' Create a take profit order
#' 
#' This function initiates take profit order. A TakeProfitOrder is an order that is linked 
#' to an open Trade and created with a price threshold.
#'
#' @param account The string representation of an Account Identifier.
#' @param tradeID  The ID of the Trade to close when the price threshold is breached.
#' @param price The price threshold specified for the Limit Order. The Limit Order will only be filled by a market price that is equal to or better than this price.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.createTakeProfitOrder('101-001-3704066-001', "6368", 1.32821) 
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createTakeProfitOrder('101-001-3704066-001', "6368", 1.32821, clientExtensions = info) 
#' @export
oanda.orders.createTakeProfitOrder<-function(account, tradeID, price, clientExtensions){        
    if(missing(account) || missing(tradeID) || missing(price)){
        print("Please check your input parameters")
        return(NULL)           
    }else{
        base.url<-config$baseurl
        order = list()
        order[length(order)+1]<-'"type":"TAKE_PROFIT"'        
        order[length(order)+1]<-'"timeInForce":"GTC"'                
        order[length(order)+1]<-sprintf('"tradeID":"%s"',tradeID)
        order[length(order)+1]<-sprintf('"price":"%s"',price)
        if(!missing(clientExtensions)) order[length(order)+1]<-sprintf('"clientExtensions":%s',clientExtensions)
        
        x <- sprintf('{"order":{ %s }}',paste(order,collapse=","))
#         print(prettify(x))
        curl.handle<-curl::new_handle()#create a curl handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers    
        curl::handle_setopt(curl.handle, copypostfields = x)#create the post request
        
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders",account),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

#' Create a stop loss order
#'
#' A StopLossOrder is an order that is linked to an open Trade and created with a price threshold. The Order will be filled (closing the Trade) by the first price that is equal to or worse than the threshold. A StopLossOrder cannot be used to open a new Position.
#'
#' @param account The string representation of an Account Identifier.
#' @param tradeID  The ID of the Trade to close when the price threshold is breached.
#' @param price The price threshold specified for the Limit Order. The Limit Order will only be filled by a market price that is equal to or better than this price.
#' @param distance  Specifies the distance (in price units) from the Trade’s open price to use as the Stop Loss Order price. Only one of the distance and price fields may be specified.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", price = 1.32821)
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", price = 1.32821, clientExtensions = info)
#' oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", distance = 0.02821)
#' oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", distance = 0.02821, clientExtensions = info)
#' @export
oanda.orders.createStopLossOrder<-function(account,tradeID,price,distance,clientExtensions){        
    if(missing(account) || missing(tradeID)){
        print("Please check your input parameters")
        return(NULL)           
    }else{
        base.url<-config$baseurl
        order = list()
        order[length(order)+1]<-'"type":"STOP_LOSS"'        
        order[length(order)+1]<-'"timeInForce":"GTC"'        
        order[length(order)+1]<-sprintf('"tradeID":"%s"',tradeID)
        
        if(!missing(price) && !missing(distance)) order[length(order)+1]<-sprintf('"price":"%s"',price)
        else if(!missing(distance)) order[length(order)+1]<-sprintf('"distance":"%s"',distance)
        else order[length(order)+1]<-sprintf('"price":"%s"',price)
            
        if(!missing(clientExtensions)) order[length(order)+1]<-sprintf('"clientExtensions":%s',clientExtensions)
        
        x <- sprintf('{"order":{ %s }}',paste(order,collapse=","))
#         print(prettify(x))
        curl.handle<-curl::new_handle()#create a curl handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers    
        curl::handle_setopt(curl.handle, copypostfields = x)#create the post request
        
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders",account),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

#' Create a trailing stop loss order
#' 
#' A TrailingStopLossOrder is an order that is linked to an open Trade and created with a price distance. The price distance is usate ed to calcula trailing stop value for the order that is in the losing direction from the market price at the time of the order’s creation. 
#' The trailing stop value will follow the market price as it moves in the winning direction, and the order will filled (closing the Trade) by the first price that is equal to or worse than the trailing stop value. A TrailingStopLossOrder cannot be used to open a new Position.
#'
#' @param account The string representation of an Account Identifier.
#' @param tradeID  The ID of the Trade to close when the price threshold is breached.
#' @param distance  Specifies the distance (in price units) from the Trade’s open price to use as the Stop Loss Order price. Only one of the distance and price fields may be specified.
#' @param clientExtensions The Client Extensions to add to the Take Profit Order when created. This is optional.
#' @return R list object detailing the response from the server.
#' @examples 
#' info <- oanda.orders.createClientExtensions("A comment","strategy_9","my_order_10000")
#' oanda.orders.createTrailingStopLossOrder('101-001-3704066-001', "6368", distance = 0.02821)
#' oanda.orders.createTrailingStopLossOrder('101-001-3704066-001', "6368", distance = 0.02821, clientExtensions = info)
#' @export
oanda.orders.createTrailingStopLossOrder<-function(account, tradeID, distance, clientExtensions){        
    if(missing(account) || missing(tradeID) || missing(distance)){
        print("Please check your input parameters")
        return(NULL)           
    }else{
        base.url<-config$baseurl
        order = list()
        order[length(order)+1]<-'"type":"TRAILING_STOP_LOSS"'        
        order[length(order)+1]<-'"timeInForce":"GTC"'        
        order[length(order)+1]<-sprintf('"tradeID":"%s"',tradeID)
        order[length(order)+1]<-sprintf('"distance":"%s"',distance)
        
        if(!missing(clientExtensions)) order[length(order)+1]<-sprintf('"clientExtensions":%s',clientExtensions)        
        x <- sprintf('{"order":{ %s }}',paste(order,collapse=","))
#         print(prettify(x))
        curl.handle<-curl::new_handle()#create a curl handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers    
        curl::handle_setopt(curl.handle, copypostfields = x)#create the post request
        
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders",account),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

#' Pending Orders
#' 
#' List all pending orders in an account
#'
#' @param account The string representation of an Account Identifier.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.pending('101-001-3704066-001')
#' @export
oanda.orders.pending<-function(account){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()#create a curl handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers
    if(missing(account)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/pendingOrders",account),sep=""),handle=curl.handle)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Pending Orders by Instrument
#' 
#' Query all pending orders in an account by instrument.
#'
#' @param account The string representation of an Account Identifier.
#' @param instrument The instrument to filter the requested orders by.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.byInstrument('101-001-3704066-001',"USD_MXN")
#' @export
oanda.orders.byInstrument<-function(account,instrument){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()#create a curlhandle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers
    
    if(missing(account) || missing(instrument)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders?instrument=%s",account,instrument),sep=""),handle=curl.handle)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id and/or instrument")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Pending Orders by ID
#' 
#' Query all pending orders in an account by an order ID.
#'
#' @param account The string representation of an Account Identifier.
#' @param id The order ID.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.byId('101-001-3704066-001',"6985")
#' @export
oanda.orders.byId<-function(account,id){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()#create a curl handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers
    
    if(missing(account) || missing(id)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders/%s",account,id),sep=""),handle=curl.handle)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id and/or order id")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Cancel Order by ID
#' 
#' Cancel a pending order in an account.
#'
#' @param account The string representation of an Account Identifier.
#' @param id The order ID.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.orders.cancelById('101-001-3704066-001',"6985")
#' @export
oanda.orders.cancelById<-function(account,id){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()#create a curl handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers
    curl::handle_setopt(curl.handle, customrequest = "PUT")#Make a put request
    
    if(missing(account) || missing(id)){
        print("Please enter a valid account id and/or order id")
        return(NULL)
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/orders/%s/cancel",account,id),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }                
}

#' Open Trades
#' 
#' List all open trades for an account.
#'
#' @param account The string representation of an Account Identifier.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.trades.open('101-001-3704066-001')
#' @export
oanda.trades.open<-function(account){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    if(missing(account)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/openTrades",account),sep=""),handle=curl.handle)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id")
        return(NULL)
    }else{        
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Query open trades by Instrument
#' 
#' Filter open trades for an account by instrument.
#'
#' @param account The string representation of an Account Identifier.
#' @param instrument The name of the instrument to query by.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.trades.byIntrument('101-001-3704066-001',"USD_SGD")
#' @export
oanda.trades.byIntrument<-function(account,instrument){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    if(missing(account)|| missing(instrument)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/trades?instrument=%s",account,instrument),sep=""),handle=curl.handle)
    }    
    if(is.null(b)) {
        return("Please enter a valid account id and/or instrument")
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Query open trade by ID
#' 
#' Filter open trades for an account by trade ID.
#'
#' @param account The string representation of an Account Identifier.
#' @param tradeId The ID to filter the open trades by.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.trades.byId('101-001-3704066-001',"5878")
#' @export
oanda.trades.byId<-function(account,tradeId){
    base.url<-config$baseurl
    curl.handle<-curl::new_handle()
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")
    
    if(missing(account)|| missing(tradeId)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/trades/%s",account,tradeId),sep=""),handle=curl.handle)
    }    
    
    if(is.null(b)) {
        print("Please enter a valid account id and/or trade id")
        return(NULL)
    }else{
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' Close a trade by ID
#' 
#' Close an open trade by ID
#'
#' @param account The string representation of an Account Identifier.
#' @param tradeId The trade ID.
#' @param quantity Indication of how much of the Trade to close. Either the string “ALL” (indicating that all of the Trade should be closed), 
#' or a DecimalNumber representing the number of units of the open Trade to Close using a TradeClose MarketOrder
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.trades.closeById('101-001-3704066-001',"6985") #close all trades
#' oanda.trades.closeById('101-001-3704066-001',"6988", 5000)
#' #close 5000 units assuming there are 10000 units in the trade
#' oanda.trades.closeById('101-001-3704066-001',"6987", 0.5) #will close half of the position held.
#' @export
oanda.trades.closeById<-function(account, tradeId, quantity){    
    if(missing(account) || missing(tradeId)){
        print("Please enter a valid account id and/or trade id")
        return(NULL)
    }else{
        base.url<-config$baseurl
        
        if( missing(quantity) ){
            x <- '{"units":"ALL"}'        
        }else{
            x <- sprintf('{"units":"%s"}',quantity)
        }    
        
        curl.handle<-curl::new_handle()#create a handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers
        #create the post request
        curl::handle_setopt(curl.handle, copypostfields=x)#start with a POST request
        curl::handle_setopt(curl.handle, customrequest = "PUT")#convert it to a PUT request
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/trades/%s/close",account,tradeId),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

#' Edit an open trade
#' 
#' Create, replace and cancel a Trade’s dependent Orders (Take Profit, Stop Loss and Trailing Stop Loss) 
#' hrough the Trade itself
#'
#' @param account The string representation of an Account Identifier.
#' @param tradeId The trade ID.
#' @param takeProfit The take profit price.
#' @param stopLoss The stop loss price.
#' @param trailingStopLoss The trailing stop loss distance. 
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.trades.editById('101-001-3704066-001', "6985", takeProfit = 1.34567)
#' oanda.trades.editById('101-001-3704066-001', "6985", stopLoss = 1.23456)
#' oanda.trades.editById('101-001-3704066-001', "6985", distance = .00250)
#' @export
oanda.trades.editById<-function(account,tradeId,takeProfit,stopLoss,trailingStopLoss){            
    if(missing(account) || missing(tradeId)){
        print("Please check your input parameters")
        return(NULL)
    }else{
        base.url<-config$baseurl
        tradeSpecifier<-list()#create an empty list
        if( !missing(takeProfit) ){
            tradeSpecifier[[length(tradeSpecifier) + 1]] <- sprintf('"takeProfit": {"price": "%f"}',takeProfit)
        }
        if( !missing(stopLoss) ){
            tradeSpecifier[[length(tradeSpecifier) + 1]] <- sprintf('"stopLoss": {"price": "%f"}',stopLoss)
        }
        if( !missing(trailingStopLoss) ){
            tradeSpecifier[[length(tradeSpecifier) + 1]] <- sprintf('"trailingStopLoss": {"distance": "%f"}',trailingStopLoss)
        }
        # if( !missing(guaranteedStopLoss) ){
        #     tradeSpecifier[[length(tradeSpecifier) + 1]] <- sprintf('"guaranteedStopLoss": {"price": "%f"}',guaranteedStopLoss)
        # }

        x<-sprintf("{%s}", paste(tradeSpecifier,collapse=","))#setup trade specifier

        curl.handle<-curl::new_handle()#create a new handle
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")#set headers
        curl::handle_setopt(curl.handle, copypostfields=x)#start with POST request
        curl::handle_setopt(curl.handle, customrequest = "PUT")#then convert it to PUT request
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/trades/%s/orders",account,tradeId),sep=""),handle=curl.handle)
        
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

#' List Positions
#' 
# List all history of positions for an Account. The Positions returned are for every instrument that has had a position during the lifetime of an the Account.
#'
#' @param account The string representation of an Account Identifier.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.positions('101-001-3704066-001')
#' @export
oanda.positions<-function(account){
    base.url<-config$baseurl#set base url
    curl.handle<-curl::new_handle()#create a new handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers
    
    if(missing(account)){#if input parameters are missing
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/positions",account),sep=""),handle=curl.handle)
    }    
    
    if(is.null(b)) {
        print("Please enter a valid account id")
        return(NULL)
    }else{        
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' List open positins
#' 
# List all open Positions for an Account. An open Position is a Position in an Account that currently has a Trade opened for it.
#'
#' @param account The string representation of an Account Identifier.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.positions.open('101-001-3704066-001')
#' @export
oanda.positions.open<-function(account){
    base.url<-config$baseurl#set base url
    curl.handle<-curl::new_handle()#create a new handle    
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers
    
    if(missing(account)){#if input parameters are missing
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/openPositions",account),sep=""),handle=curl.handle)
    }    
    
    if(is.null(b)) {
        print("Please enter a valid account id")
        return(NULL)
    }else{        
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}

#' List position by instrument.
#' 
# Get the details of a single Instrument’s Position in an Account. The Position may by open or not.
#'
#' @param account The string representation of an Account Identifier.
#' @param instrument The name of the instrument to query positions by.
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.positions.instrument('101-001-3704066-001',"USD_MXN")
#' @export
oanda.positions.instrument<-function(account,instrument){
    base.url<-config$baseurl#set base url
    curl.handle<-curl::new_handle()#create a new handle
    curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX")#set headers
    
    if(missing(account) || missing(instrument)){
        b<-NULL        
    }else{
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/positions/%s",account,instrument),sep=""),handle=curl.handle)
    }    
    if(is.null(b)) {
        print("Please enter a valid account id and/or instrument")
        return(NULL)
    }else{        
        return(jsonlite::parse_json(rawToChar(b$content)))
    }    
}


#' Close position by instrument.
#' 
# Closeout the open position for a specific instrument in an Account.
#'
#' @param account The string representation of an Account identifier.
#' @param instrument The name of the instrument.
#' @param longUnits Indication of how much of the long Position to closeout. Either the string “ALL”, the string “NONE”, 
#' or a DecimalNumber representing how many units of the long position to close using a PositionCloseout MarketOrder. 
#' he units specified must always be positive.
#' @param shortUnits Indication of how much of the short Position to closeout. Either the string “ALL”, the string “NONE”, 
#' or a DecimalNumber representing how many units of the short position to close using a PositionCloseout MarketOrder. 
#' The units specified must always be positive. 
#' @return R list object detailing the response from the server.
#' @examples 
#' oanda.positions.close('101-001-3704066-001',"USD_NOK") #close all positions of USD/NOK
#' oanda.positions.close('101-001-3704066-001',"USD_NOK", shortUnits = 5000)
#' #close 5000 units of short position in USD/NOK
#' oanda.positions.close('101-001-3704066-001',"USD_SGD", longUnits = 5000)
#' #close 5000 units of long position in USD/SGD
#' @export
oanda.positions.close<-function(account,instrument,longUnits,shortUnits){         
    if(missing(account) || missing(instrument)){
        print("Please enter a valid account id and/or a instrument name.")
        return(NULL)
    }else{
        base.url<-config$baseurl 
        positionspecifier<-list()#create an empty list for position specifier later
        if( !missing(longUnits) ){# if longUnits are set then use that specifier
            positionspecifier[[length(positionspecifier) + 1]] <- sprintf('"longUnits": "%s"',longUnits)
        }
        if( !missing(shortUnits) ){# if longUnits are set then use that specifier
            positionspecifier[[length(positionspecifier) + 1]] <- sprintf('"shortUnits": "%s"',shortUnits)
        }    
        x<-sprintf("{%s}", paste(positionspecifier,collapse=","))    
        
        curl.handle<-curl::new_handle()#create a curl handle
        #set headers
        curl::handle_setheaders(curl.handle,"Authorization" = paste("Bearer", config$token),"Accept-Datetime-Format" = "UNIX","Content-Type"="application/json")
        #create the post request
        curl::handle_setopt(curl.handle, copypostfields=x)#first create a POST request
        curl::handle_setopt(curl.handle, customrequest = "PUT")#convert the request to a PUT request.
        b<-curl::curl_fetch_memory(url=paste(base.url,sprintf("accounts/%s/positions/%s/close",account,instrument),sep=""),handle=curl.handle)
        return(jsonlite::parse_json(rawToChar(b$content)))
    }        
}

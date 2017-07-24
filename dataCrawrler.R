# This module is used to fetch all the historical data of 
# a certain symbol by using http calls to Yahoo's YQL API.
# Due to YQL is limited to send one year of historical data
# more than http call is needed in order to get data from a 
# period of time longer than one year.


data.crawler <- function(startDate = "2014-01-01", endDate   = "2015-01-01"){
  
  library("httr");
  library("jsonlite");
  
  #################################
  # PRIVATE FUNCTIONS DECLARATION #
  #################################
  
  
  fetch.stock.data <- function(symbol    = "^IBEX",
                               startDate,
                               endDate){

    
    url_1 <- "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol%20%3D%20%22"
    url_2 <- "%22%20and%20startDate%20%3D%20%22";
    url_3 <- "%22%20and%20endDate%20%3D%20%22";
    url_4 <- "%22&format=json&diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=";
    
    # Extract the years of the dates in numeric values.
    year_1 = strtoi(substring(startDate, 1, 4));
    year_2 = strtoi(substring(endDate,   1, 4));
    
    sub_startDate <- startDate;
    
    print(paste("> Fetching", symbol, "data ..."));
    
    returnList <- list();
    
    while (year_1 <= year_2) {
      
      # For fetching in chunks we need  to calculate the finalDate of the period.
      if (year_1 == year_2) {
        sub_endDate   <- paste(year_1, substring(endDate, 5), sep = "");
      } else {
        sub_endDate <- paste(year_1, "-12-31", sep = "");
      }
            
      year_1 <- year_1 + 1;
      
      # Generate the URL to get the historical data with the symbol parameter given.
      url <- paste(url_1, symbol, url_2, sub_startDate, url_3, sub_endDate, url_4, sep = "");
      
      # We prepare the sub_startDate for the next iteraion.
      sub_startDate <- paste(year_1, "-01-01", sep = "");
      
      # GET HTTP Request.
      response <- GET(url);
      
      # Parse JSON response.
      jsonRequest <- content(response);
      
      # Returns historical data from YQL answer structure.
      returnList <- c(jsonRequest$query$results$quote, returnList);
      
    }
    
    returnList;
    
  }
  
  #################################
   
  # Example of request [{"symbol":"TSE", "parameters":"OPEN, CLOSE, HIGH, LOW"}]
  
  # Parse from JSON to List object.
  requestList <- fromJSON(jsonRequest);
  
  symbols <- requestList[["symbol"]];
  
  returnLists <- list();
  
  for (symbol in symbols) {
    returnLists <- c(returnLists, list(fetch.stock.data(symbol, startDate, endDate)));
  }
  
  returnLists;
  
}



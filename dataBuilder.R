data.builder <- function (stockLists) {
  
  #################################
  # PRIVATE FUNCTIONS DECLARATION #
  #################################
  
  build.stock.matrix <- function (dataList, columns) {
    
    
    # Transform dataList in a matrix of nx8 elements.
    output <- matrix(unlist(dataList), ncol = 8, byrow=TRUE, dimnames = 
                       list(
                         NULL , c("SYMBOL", "DATE" , "OPEN" , "HIGH", "LOW", "CLOSE", "VOLUME", "ADJ_CLOSE")  #Column names
                       ));
    
    # Save the symbol of the data that we are processing.
    symbol <- substring(output[1][1], 4);
    
    # Extract date and symbol column
    DATE <- c(output[, "DATE"]);
       
    # Calculates the sesion variations (SESVAR) variable.
    SESVAR <- (as.numeric(output[, "CLOSE"]) - as.numeric(output[, "OPEN"]))/as.numeric(output[, "OPEN"]) * 100;
        
    # Extract columns defined by column parameter (SESVAR not considered).
    output <- cbind(output, SESVAR);
    output <- output[, columns, drop=FALSE];

         
    # Calculate the porcentual increments of values.
    m <- matrix(as.numeric(output), ncol = ncol(output));
    m <- m[nrow(m): 1, ];
    d <- data.frame(diff(m))/m * 100;
    
    colnames(d) <- colnames(output);
    
    output <- d;
    
     
    # If SESVAR is in the configuration string, then we append the calculated vector.
    if(sum(columns == "SESVAR")) {
      
      SESVAR <- SESVAR[(length(DATE) - 1):1];
      output["SESVAR"] <- SESVAR;
      
    }
    
    # Add suffix to column names.
    colnames(output) <- paste(symbol, colnames(output), sep="_");
    
    # Add date column.
    DATE   <- DATE[(length(DATE) - 1):1];
    output <- cbind(DATE, output);
    
    
    output;
    
  }
  
  #################################
  
  # Informative message.
  print("> Building data...");
  
  # Parse from JSON to List object.
  requestList <- fromJSON(jsonRequest);
  
  # Extract the list of parameters
  paramLists <- requestList[["parameters"]];
  
  # Create the matrix to be filled with stock data-
  returnMatrix <- matrix(, nrow = 0, ncol = 0);
  
  # Index to run all stockLists;
  i <- 1;
  
  for (paramList in paramLists) {
    
    # Take the stockList that correspond to the paramList. (if order is preserved we can use a simple index)
    stockList <- stockLists[[i]];
    
    # We calculate the stockMatrix for each list.
    stockMatrix <- build.stock.matrix(stockList, paramList);
    
    if (ncol(returnMatrix) == 0) {
      
      # If returnMatrix is null we assign the stockMatrix.
      returnMatrix <- stockMatrix;
      
    } else {
      
      # ...else we merge by date with previous data and delete those fields uncomplete, 
      returnMatrix <- merge(returnMatrix, stockMatrix, by="DATE", all.x = FALSE);
      
    }
    
    # We set the index to point the next stockList.
    i <- i + 1; 
    
  }
  
  # Elimination of outliers.
  outlierVal <- 2.5;
  
  returnMatrix <- returnMatrix[(rowSums(returnMatrix[-1] < -outlierVal | returnMatrix[-1] > outlierVal) == 0),];
  
#   # Normalize data in a range of [-1:1]
#   oldMin = min(returnMatrix[-1]);
#   oldMax = max(returnMatrix[-1]);
#   
#   returnMatrix[-1] <- scale((((returnMatrix[-1] - oldMin) * 2) / (oldMax - oldMin)) - 1, scale=FALSE);
#   

  returnMatrix;
  
}


data.builder <- function (stockLists, jsonRequest) {
  
  #################################
  # PRIVATE FUNCTIONS DECLARATION #
  #################################
  
  build.stock.matrix <- function (dataList, columns) {
    
    # Transform dataList in a matrix of nx8 elements.
    output <- matrix(unlist(dataList), ncol = 8, byrow=TRUE, dimnames = 
                       list(
                         NULL , c("SYMBOL", "DATE" , "OPEN" , "CLOSE", "HIGH", "LOW", "VOLUME", "ADJ_CLOSE")  #Column names
                       ));
    
    # Save the symbol of the data that we are processing.
    symbol <- substring(output[1][1], 4);
    
    # Extract date and symbol column
    DATE <- c(output[, "DATE"]);
    
    # Format field Date as int with format YYYYMMDD.
    DATE <- strtoi(gsub("-", "", DATE));  
    
    # Extract columns defined by column parameter.
    output <- output[, columns, drop=FALSE];
    
    # Calculate the derivative (day variations) of values.
    m <- matrix(as.numeric(output), ncol = ncol(output));
    m <- m[nrow(m): 1, ];
    d <- data.frame(diff(m));
    
    colnames(d) <- colnames(output);
    
    output <- d;
    
    # Calculate diference of values in the matrix
    
    # Add suffix to column names.
    colnames(output) <- paste(symbol, colnames(output), sep="_");
    
    # Add date column.
    DATE   <- DATE[(length(DATE) - 1):1];
    output <- cbind(output, DATE);
    
    
    output;
    
  }
  
  #################################
  
  # Informing message.
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
  
  # Return all stock matrix.
  returnMatrix;
  
}



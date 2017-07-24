# This module is used to split the original datasets in two differents 
# datasets (training dataset and test dataset) in the proportion of
# 70/30 respectively.

data.partitioner <- function (stockDatatable, trainingRatio = 0.7) {
  
  print(paste("> Generating TRAINING SET (", trainingRatio,") and TEST SET (", 
              (1 - trainingRatio),") ..."));
  
  #Number of each set that will be generated
  sampleNum = 5;
  
  # Get the number of records of the whole dataset.
  setSize <- nrow(stockDataTable);
  
  # Creation of the list of sets 
  testSets     <- list();
  trainingSets <- list();
  
  for (i in 1:sampleNum) {
    
    # Generate a random subset with a proportional size of the trainingRatio.
    randomSet <- sample(1:setSize, trainingRatio*setSize, replace=FALSE);
    
    # We store the trainingSubset...
    trainingSets[i] <- list(stockDataTable[randomSet,]);
    
    # ...and the testSubset.
    testSets[i]     <- list(stockDataTable[-randomSet,]);
                      
  }
  
  # Return of both subsets.
  list(trainingSets, testSets);
  
}


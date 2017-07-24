
model.validator <- function (neuralNets, testSets, info = TRUE) {
  
  
  library("quantmod");
  
  #################################
  
  modelStats     <- matrix(NA, length(neuralNets), 3);
  
  convergedCount <- 0;
  
  colnames(modelStats) <- c("RECM", "R2", "%Correct");
  
  if (info == TRUE) {
    
    print("-----------------------------");
    
  }

  
  for (i in 1:length(testSets)) {
    
    
    if (length(neuralNets[[i]][["net.result"]]) != 0) {
      
      convergedCount <- convergedCount + 1;
      
      testPrediction <- compute(neuralNets[[i]], testSets[[i]][2:(ncol(testSets[[i]]) - 1)])$net.result
      
      realValues     <- testSets[[i]][length(testSets[[i]])];
      
      modelError     <- testSets[[i]][length(testSets[[i]])] - testPrediction[,1];
      
      modelStats[i, "RECM"] <- sqrt(sum(modelError^2)/nrow(testSets[[i]]));
      
      modelStats[i, "R2"]  <- summary(lm(as.matrix(realValues) ~ testPrediction))$r.squared;
      modelStats[i, "%Correct"] <- sum(sign(realValues) == sign(testPrediction))/nrow(realValues);
      
      if (info == TRUE) {
        
        print(paste("ANN", i, " --> RECM ="      ,format(round(modelStats[i, "RECM"],      5), nsmall=5),
                                 ", R2 ="        ,format(round(modelStats[i,   "R2"],      5), nsmall=5),
                                 ", %Correct =" , format(round(modelStats[i,  "%Correct"], 5), nsmall=5), sep=" "))  
        
      }
      
    } else {
      
      modelStats[i, "RECM"] <- 0;
      modelStats[i,   "R2"] <- 0;
      modelStats[i,  "%Correct"] <- 0;
      
      if (info == TRUE) {
        
        print(paste("ANN", i, " --> [DID NOT CONVERGED] ", sep=" "));
        
      }
    }
    
  }
  
  if (info == TRUE) {
    
    print("-----------------------------");
    print(paste("Mean perf: RECM ="     , format(round(sum(modelStats[,     "RECM"])/convergedCount, 5), nsmall=5), 
                         ", R2 ="       , format(round(sum(modelStats[,       "R2"])/convergedCount, 5), nsmall=5),
                         ", %Correct =" , format(round(sum(modelStats[, "%Correct"])/convergedCount, 5), nsmall=5), sep=" "))  
    print("-----------------------------");
  }
  
  averageStats  <- round(colSums(modelStats[,])/convergedCount, 5);
  
  modelStats;
  
}

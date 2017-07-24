
data.analyzer <- function (trainingSets, hnum = 5, algo = "rprop+") {
  
  library("neuralnet");
      
  #################################
  
  # Extract the variables of the market
  varNames <- colnames(trainingSets[[1]])[-1]
  
  neuralStruct = "";
  count = 0;
  
  # Compute the string that generates the neural network  (ie. "OUTPUT~INPUT1+INPUT2+...+INPUTN")
  for (i in 1:length(varNames)) {
        
    if (i == length(varNames)){
      neuralStruct <- substring(neuralStruct, 2);
      
      neuralStruct <- paste(varNames[i], neuralStruct, sep="~");
    } else {  
      neuralStruct <- paste(neuralStruct, varNames[i], sep="+");
    }
    
  }
  
  # Creation of the list of sets 
  neuralNets     <- list();
  weights        <- NULL;
  
  for (i in 1:length(trainingSets)) {    
    
    # Informative message.
    print(paste("> Analyzing data with ", algo, "... [",i,"/",length(trainingSets),"]", sep=""));
    
    # For all repetitions, same startweights.
    if (length(neuralNets) != 0) {
        weights <- neuralNets[[1]]$startweights;
    }
    
    # Train the neural net
    neuralNets[i] <- list(neuralnet(neuralStruct,  trainingSets[[i]][-1], hidden = hnum, algorithm = algo,
                                    learningrate = 0.0005,  linear.output = TRUE, startweights=weights));
    
  }
    
  neuralNets;
  
}


## Symbol Glosary ##
#
#  Ibex35      = ^IBEX
#  NASDAQ      = ^IXIC
#  Frankfurt   = ^GDAXI
#  Tokio       = ^N225
#
## --------------- ##

PATH <- dirname(sys.frame(1)$ofile) 

source(paste(PATH, "/dataCrawrler.r", sep=""))
source(paste(PATH, "/dataBuilder.r", sep=""))
source(paste(PATH, "/dataPartitioner.r", sep=""))
source(paste(PATH, "/dataAnalyzer.r", sep=""))
source(paste(PATH, "/modelValidator.r", sep=""))

## --------------- ##

jsonRequest <- '[{"symbol":"^N225",    "parameters":["OPEN", "HIGH", "LOW", "CLOSE"]},
                 {"symbol":"^GDAXI",   "parameters":["OPEN"]},
                 {"symbol":"^IBEX",    "parameters":["OPEN", "SESVAR"]}]'

stockList      <- data.crawler("2014-01-01", "2014-12-31");

stockDataTable <- data.builder(stockList);

subSets        <- data.partitioner(stockDataTable, 0.7);

trainingSets   <- subSets[[1]];
testSets       <- subSets[[2]];

neuralNets <- data.analyzer(trainingSets, c(1,1), "rprop+");  

model.validator(neuralNets, testSets);

# Informing message.
print("> DONE!")

## Symbol Glosary ##
#
#  Ibex35      = ^IBEX
#  NASDAQ      = ^IXIC
#  Frankfurt   = ^GDAXI
#  Tokyo (TSE) = ^N225
#
## --------------- ##


jsonRequest <- '[{"symbol":"^N225",  "parameters":["OPEN" , "CLOSE", "HIGH", "LOW"]},
                 {"symbol":"^GDAXI", "parameters":["OPEN"]},
                 {"symbol":"^IBEX" , "parameters":["OPEN" , "VOLUME"]}]'

stockList      <- data.crawler(jsonRequest);
stockDataTable <- data.builder(stockList, jsonRequest);

# Informing message.
print("> DONE!");
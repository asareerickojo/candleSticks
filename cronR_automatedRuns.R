
library(cronR)

cmd <- cron_rscript("patternAlert.R")
cron_add(command = cmd, frequency = 'daily', at='7AM', id = 'alert1')

#cron_njobs()
#cron_ls()
#cron_clear(ask = TRUE)

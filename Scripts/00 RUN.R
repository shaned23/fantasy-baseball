

# Run scripts

## References
source(paste0(.scripts,'Clean references.R'))
source(paste0(.scripts,'Load and clean master projections.R'))
source(paste0(.scripts,'Reliever sheet prep.R'))
source(paste0(.scripts,'id_xwalk.R'))

## Saves and Holds
# source(paste0(.scripts,'01a Load and clean.R'))
# source(paste0(.scripts,'02a Saves model.R'))

## NFBC

#Set variables
leagues <- c('OC','DC')
# leagues <- c('OC')
cats <- c('r','hr','rbi','sb','avg','w','k','sv','era','whip')
hit.cats <- c('r','hr','rbi','sb','avg')
pitch.cats <- c('w','k','sv','era','whip')
hitlist.count <- c('r','hr','rbi','sb')
pitlist.count <- c('w','k','sv')
dc.book = '1PjKvrQ6l4ZegRx5hUhmjZD9eJkB3lK2Ar-ZkvSB5Dwk'
oc.book = '1Mulvq-EdPubvXxhdQ_8eigYfZus74GiTeG3wIkxukQo'
#
# # Run scripts
source(paste0(.scripts,'01c Last year results.R'))
source(paste0(.scripts,'02c Logistic regressions.R'))
source(paste0(.scripts,'03c NFBC prep.R'))
source(paste0(.scripts,'04c NFBC WAR.R'))
source(paste0(.scripts,'05c Export stats.R'))
source(paste0(.scripts,'06c Reliever sheet.R'))

## ottoneu
source(paste0(.scripts,'02b ottoneu prep.R'))
source(paste0(.scripts,'03b ottoneu values per game.R'))

## Mendoza
source(paste0(.scripts,'01d Mendoza prep.R'))
source(paste0(.scripts,'02d Mendoza value.R'))
source(paste0(.scripts,'03d Reliever sheet.R'))

## HC Baseballers
cats <- c('r','hr','rbi','sb','avg','ops','qs','k','svhld','era','hip','bb9')
hit.cats <- c('r','hr','rbi','sb','avg','ops')
pitch.cats <- c('qs','k','svhld','era','hip','bb9')
hitlist.count <- c('r','hr','rbi','sb')
pitlist.count <- c('qs','k','svhld')
hc.book = '1MCDdhMBssWlwEvLeR0FEnjmYZpcenSbliGd_lT0eOVA'
source(paste0(.scripts,'01e HC Last year results.R'))
source(paste0(.scripts,'02e HC logistic regressions.R'))
source(paste0(.scripts,'03e HC prep.R'))
source(paste0(.scripts,'04e HC WAR.R'))
source(paste0(.scripts,'05e HC export.R'))

#source(paste0(.scripts,'01e winslogit.R'))


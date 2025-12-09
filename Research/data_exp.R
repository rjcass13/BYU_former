dt <- read.csv('agg_1x1_2015_2024_temp_precip.csv')

provo_lat <- 40
provo_lon <- 248

pro <- dt[which(dt$Lat == 40.5 & dt$Lon == 248.5), ]

pro$week <- sub(".*-", "", pro$Year_Week)

plot(pro$week, pro$Temp)

# Look at raw prior to agg: check temperature start/end dates per year
# Check provo temps in raw data: do they look as expected?
# Look for ways to plot things that give me the globe in the back, etc.
### image.plot function from fields library
### library(fields)
### Can then overlay a world map (packages: rworldmap, maps)

# Research question: 
# Model temperature as a function of Loc/Elev/Time
# Look at different splits of the dependents, etc.
# after accounting for latitude and time, is the effect of elevation linear or not
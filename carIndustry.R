# Car industry  script 14/7/17

library(seasonal)
library(reshape2)
library(ggseas)
library(ggthemes)

setwd("/Users/alex/Documents/car-industry")

# xls downloaded from  http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/9314.0April%202017?OpenDocument
# on Saturday 15/7/17
# csv extracted form "Data1" tab

# cribbed from https://stackoverflow.com/questions/15860071/read-csv-header-on-first-line-skip-second-line
# import CSV and skip top rows
all_content = readLines("AU_CarIndustry_sales.csv")
skip_toprows = all_content[-(2:10)]
carsales <- read.csv(textConnection(skip_toprows), header = TRUE, stringsAsFactors = FALSE)

carsales$Date <- as.Date(as.yearmon(carsales$X,"%b-%Y"))

carsales.ts <- ts(carsales[,5], frequency=12, start=c(1994,1))
carsales.ts.suv <- ts(carsales[,3],frequency=12, start=c(1994,1))
carsales.ts.passenger <- ts(carsales[,2],frequency=12, start=c(1994,1))


# base R plot
carsales.season <- decompose(carsales.ts)
plot(carsales.season)
axis.Date(1,at=carsales.ts,labels=format(df$timestamp,"%b-%d"),las=2)

# nicer plot
df.carsales <- tsdf(carsales.ts)
df.carsales.suv <- tsdf(carsales.ts.suv)
df.carsales.passenger <- tsdf(carsales.ts.passenger)

df.carsales.all <- list(df.carsales,df.carsales.suv,df.carsales.passenger) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="x"), .)
names(df.carsales.all) <- c("Date","Total","SUV","Passenger")

# change wide to long
df.carsales.long <- melt(df.carsales.all, id.vars=c("Date"))

ggsdc(df.carsales.long, aes(x = Date, y = value, colour=variable),
      method = "decompose", start = c(1994, 1), frequency = 12) +
  geom_line() +
  labs(x = "   \n  ", colour = "") +
  scale_x_continuous(minor_breaks = seq(1994,2017,1), breaks=seq(1994,2017,1),labels=seq(1994,2017,1)) + 
  scale_y_continuous(name="Number of vehicles sold\n", labels = scales::comma) +
  #theme_hc() + #theme_gdocs() +
  theme(panel.grid.minor.x = element_line(colour = "black", size = 0.5)) + 
  theme(panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank()) +
  ggtitle("Total car sales") #+theme(legend.position = c(0.17, 0.92))







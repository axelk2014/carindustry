# Car industry  script 14/7/17

library(zoo)
library(xts)
library(seasonal)
library(ggseas)
library(ggthemes)

setwd("/Users/alex/Documents/datasets/car_industry")

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
carsales.season <- decompose(carsales.ts)

plot(carsales.season)
axis.Date(1,at=carsales.ts,labels=format(df$timestamp,"%b-%d"),las=2)

#as.dataframe?
#df.carsales.ts <- tapply(carsales.ts, list(year=floor(time(carsales.ts)), month=month.abb[cycle(carsales.ts)]),c)

dfTime = attributes(carsales.ts)[[1]]
dfTime = seq(dfTime[1],dfTime[2], length.out=(dfTime[2]-dfTime[1])*dfTime[3])

# nicer plot
df.carsales <- tsdf(carsales.ts)

ggsdc(df.carsales, aes(x = x, y = y),
      method = "decompose", start = c(1994, 1), frequency = 12) +
  geom_line() +
  labs(x = "   \n  ", colour = "") +
  scale_x_continuous(minor_breaks = seq(1994,2017,1), breaks=seq(1994,2017,1),labels=seq(1994,2017,1)) + 
  scale_y_continuous(name="Number of vehicles sold\n", labels = scales::comma) +
  #theme_hc() + #theme_gdocs() +
  theme(panel.grid.minor.x = element_line(colour = "black", size = 0.5)) + 
  theme(panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank()) +
  ggtitle("Total car sales") #+theme(legend.position = c(0.17, 0.92))


old_hc <- theme_set(theme_hc())
add_el <- theme_hc() +
  theme(panel.grid.minor = element_line(colour = "gray", size = 0.5) )
add_el$panel.grid.minor

#grid.text("Source: Statistics New Zealand, Balance of Payments", 0.7, 0.03,
        #  gp = gpar(fontfamily = "myfont", fontface = "italic", cex = 0.7))   


# > min(carsales$Date.s)
[1] "1994-01-01"
#ts(data=carsales[,c(11,10)],start="1994-01-01", frequency=1)
#stocks <- xts(df[,-1], order.by=as.Date(df[,1], "%m/%d/%Y"))

carsales.xts <- xts(carsales[,10], order.by=carsales$Date.s, "%Y-%m-%d")
plot.xts(as.xts(carsales.xts))



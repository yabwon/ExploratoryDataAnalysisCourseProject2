## This first line will likely take a few seconds. Be patient!
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

## PLOT 2 ##
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.
# Upload a PNG file containing your plot addressing this question.

library(data.table)
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- data.table(readRDS("Source_Classification_Code.rds"))

library(pryr)
object_size(NEI)
object_size(SCC)


AGGR <- NEI[fips == 24510 # select Baltimore City, Maryland
            , list(sum.of.emissions = sum(Emissions))
            , by = year]


# open device
png(filename = "plot2.png"
    , width = 1000
    , height = 1000
    , units = "px"
    , pointsize = 12
    , bg = "transparent"
)

# setup margins
par(mar=c(5,8,4,2) + 0.5)
# create plot
plot(AGGR[, year]
     , AGGR[, sum.of.emissions]
     , type = "p"
     , main = expression(paste("Total emissions from ", plain(PM)[2.5], " in the Baltimore City, Maryland from 1999 to 2008"))
     , xlab = "Year"
     , ylab = NA
     , ylim=c(0, max(AGGR[, sum.of.emissions]) * 1.1 )
     , xlim=c(min(AGGR[, year]) - 1, max(AGGR[, year]) + 1)
     , col  = rgb(100, 150, 50, maxColorValue = 255)
     , lty  = 5 # "longdash"
     , lwd  = 2
     , axes = FALSE     
)

# add Y axis label
mtext('Sum of emissions'
      , side=2
      , line=6
)
# horizontal lines for every value
abline(h = AGGR[, sum.of.emissions]
       , col = rgb(200, 200, 200, maxColorValue = 255)
       , lty = 3
)

# make lines read whem PM2.5 increases else make lines greem
my.length <- length(AGGR[, year])
for (i in seq(from = 2, to = my.length, by = 1)) {
  if (AGGR[i, sum.of.emissions] - AGGR[i - 1, sum.of.emissions] > 0) {
    kolor <- rgb(200, 50, 50, maxColorValue = 255)
  } else {
    kolor <- rgb(100, 150, 50, maxColorValue = 255)
  }
  
  selector <- c(i, i - 1)
  
  lines(AGGR[selector, year]
        , AGGR[selector, sum.of.emissions]
        , type = "l"
        , col  = kolor
        , lty  = 5 # "longdash"
        , lwd  = 2
  )  
}

# add points for sum.of.emissions 
points(AGGR[, year]
       , AGGR[, sum.of.emissions]
       , col = rgb(50, 100, 150, maxColorValue = 255)
       , lwd  = 6
)
# add Y axis labels
axis(side = 2
     , at = AGGR[, sum.of.emissions]
     , labels=format(AGGR[, sum.of.emissions]
                     , big.mark = "\ "
                     , digits = 2)
     , las = 1
)
# add X axis labels
axis(side=1
     , at=AGGR[, year]
     , labels = AGGR[, year]
)

# reggresion line
model <- lm(sum.of.emissions ~ year, AGGR)
abline(model
       , lwd = 2
       , col = rgb(50, 100, 150, alpha = 100, maxColorValue = 255)
       )

box() # to make it look "as usual"

# close device
dev.off()
rm(list = ls()); gc()
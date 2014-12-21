## This first line will likely take a few seconds. Be patient!
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

## PLOT 3 ##
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
# a plot answer this question.
# Upload a PNG file containing your plot addressing this question.

library(data.table)
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- data.table(readRDS("Source_Classification_Code.rds"))

library(pryr)
object_size(NEI)
object_size(SCC)

# underlying set for plotting
AGGR <- NEI[fips == 24510 # select Baltimore City, Maryland
            , list(sum.of.emissions = sum(Emissions))
            , by = list(year
                        , as.ordered(type)
                        )
            ]
setnames(AGGR, 2, "type")  


# open device
png(filename = "plot3.png"
    , width = 1000
    , height = 1000
    , units = "px"
    , pointsize = 12
    , bg = "transparent"
)
# load libraries
library(ggplot2)
library(grid)
# make base plot
G <- qplot(year
      , sum.of.emissions
      , data = AGGR
      )

G +
# add aesthetics for points 
geom_point(aes(color = type), size = 4) +
# add smooth aesthetics for smooth 
geom_smooth(aes(color = type), alpha = I(0.7), size = I(1), linetype = 1, method = "lm", se = FALSE) +
# add title
labs(title = expression(paste("Total emissions from ", plain(PM)[2.5], " by TYPE in the Baltimore City, Maryland from 1999 to 2008")) ) + 
# add axis labels
labs(x = "Year", y = "Sum of emissions") +
# grey theme
theme_grey() +
# setup range for x axis
coord_cartesian(xlim = c(min(AGGR[, year]) - 0.1, max(AGGR[, year]) + 0.1)) +
# put facets in 2 x 2 grid
facet_wrap(~ type, nrow = 2, ncol = 2) +
# wider margins
theme(panel.margin = unit(2, "lines")) +
# removes legend
theme(legend.position = "none") +
# set up grid lines
scale_x_continuous(breaks = AGGR[, year], minor_breaks = seq(min(AGGR[, year]) , max(AGGR[, year]), 1))

# close device
dev.off()
rm(list = ls()); gc()
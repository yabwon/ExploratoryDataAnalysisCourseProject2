## This first line will likely take a few seconds. Be patient!
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

## PLOT 6 ##
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
# sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes 
# over time in motor vehicle emissions?
# Upload a PNG file containing your plot addressing this question.



library(data.table)
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- data.table(readRDS("Source_Classification_Code.rds"))

library(pryr)
# object_size(NEI)
# object_size(SCC)


AGGR <- NEI[fips == "24510" | fips == "06037"] # select Baltimore City, Maryland and Los Angeles County, California 
# select  'Highway Veh' for analisys
temp.select <- grep("^Highway Veh", as.character(SCC[, Short.Name]))
subset.SCC <- SCC[temp.select]

# set keys for join
setkey(AGGR, SCC)
setkey(subset.SCC, SCC)

#
AGGR1 <- AGGR[subset.SCC, nomatch=0] # inner join

AGGR1[, area:=as.factor(ifelse(fips == "24510", "Baltimore City, Maryland", "Los Angeles County, California") )]


# set for the first plot
AGGR2 <- AGGR1[, list(sum.of.emissions = sum(Emissions))
               , by = list(year, area)]


# labels for percent of change
LA.percent.of.change <- AGGR2[as.numeric(area) == 2][year %in% c(1999,2008)]
BC.percent.of.change <- AGGR2[as.numeric(area) == 1][year %in% c(1999,2008)]

LA.change <- (LA.percent.of.change[year == 2008, sum.of.emissions] - LA.percent.of.change[year == 1999, sum.of.emissions])/LA.percent.of.change[year == 1999, sum.of.emissions]
BC.change <- (BC.percent.of.change[year == 2008, sum.of.emissions] - BC.percent.of.change[year == 1999, sum.of.emissions])/BC.percent.of.change[year == 1999, sum.of.emissions]

LA.labele <- paste("For Los Angeles County, California percent of change from 1999 to 2008 equals ", round(LA.change*100,digits=2),"%",sep="")
BC.labele <- paste("For Baltimore City, Maryland percent of change from 1999 to 2008 equals ", round(BC.change*100,digits=2),"%",sep="")


# set for the second plot
AGGR3 <- AGGR1[, list(sum.of.emissions = sum(Emissions))
               , by = list(year, area, EI.Sector)
               ]

######################################################################


# make first base plot
G2 <- qplot(year
            , sum.of.emissions
            , data = AGGR2
            , col = area
            , alpha = I(0.7)
            , size = I(4)
) +
  # add points labels  
  geom_text(aes(label=format(sum.of.emissions
                             , big.mark = "\ "
                             , digits = 2
                             )
               )
               , hjust=0.5
               , vjust=-1
               , show_guide=FALSE
  ) +
  # add smooth aesthetics for smooth 
  geom_smooth(size = .5
              , linetype = 2
              , method = "lm"
              , se = FALSE
              , formula = y ~ x) +
  # add axis labels
  labs(title = 'Total emissions from motor vehicle sources from 1999 to 2008 in Baltimore City and Los Angeles County'
       , x = "Year"
       , y = "Sum of emissions from motor vehicles") +
  # grey theme
  theme_grey() +
  # setup range for x axis
  coord_cartesian(xlim = c(min(AGGR2[, year]), max(AGGR2[, year]))) +
  # setup range for y axis
  coord_cartesian(ylim = c(0, max(AGGR2[, sum.of.emissions]) * 1.1)) +
  # removes legend
  theme(legend.position=c(1,1)
        , legend.justification=c(1,1)
        ) +
  # set up x grid and lines
  scale_x_continuous(breaks = AGGR2[, year]
                     , minor_breaks = seq(min(AGGR2[, year]), max(AGGR2[, year]), 3)
  ) +
  annotate("text", x = 2005, y = 4000 , label = as.character(LA.labele), parse = FALSE) +

  annotate("text", x = 2003, y = 500 ,  label = as.character(BC.labele), parse = FALSE) 

#############################################################################

# make second base plot
G3 <- qplot(year
            , sum.of.emissions
            , data = AGGR3
            , col = area
            , alpha = I(0.7)
            , size = I(4) 
) +
  # group by type of vehicle
  facet_wrap(~ EI.Sector, nrow = 4, ncol = 1, scales = "free_y") +
  # add points labels  
  geom_text(aes(label=format(sum.of.emissions
                             , big.mark = "\ "
                             , digits = 2
                            )
                , size = 2
                )
  , hjust=-0.15
  , vjust=0.5
  ) +
  # add smooth aesthetics for smooth 
  geom_smooth(size = .2
              , linetype = 1
              , method = "lm"
              , se = FALSE
              , formula = y ~ x
              ) +
  # add axis labels
  labs(title = ''
       , x = "Year"
       , y = "Sum of emissions grouped by vehicle type from 1999 to 2008 in Baltimore City and Los Angeles County") +
  # grey theme
  theme_grey() +
  # setup range for x axis
  coord_cartesian(xlim = c(min(AGGR3[, year]) - 3, max(AGGR3[, year]) + 3)) +
  # setup range for y axis
  # coord_cartesian(ylim = c(0, max.sum.of.emissions * 1.2)) +
  # removes legend
  theme(legend.position = "none") +
  # set up x grid and lines
  scale_x_continuous(breaks = AGGR3[, year]
                     , minor_breaks = seq(min(AGGR3[, year]), max(AGGR3[, year]), 3)
  )
###########################################################


vp.G2 <- viewport(width = 0.75, height = 1, x = 0.25+0.75/2, y = 0.5)
vp.G3 <- viewport(width = 0.25, height = 1, x = 0.125, y = 0.5)


# open device
png(filename = "plot6.png"
    , width = 1200
    , height = 1000
    , units = "px"
    , pointsize = 12
    , bg = "transparent"
)

print(G2, vp = vp.G2)
print(G3, vp = vp.G3)

# close device
dev.off()
rm(list = ls()); gc()
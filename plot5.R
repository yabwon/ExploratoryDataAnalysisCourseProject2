## This first line will likely take a few seconds. Be patient!
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

## PLOT 5 ##
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 
# Upload a PNG file containing your plot addressing this question.



library(data.table)
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- data.table(readRDS("Source_Classification_Code.rds"))

library(pryr)
object_size(NEI)
object_size(SCC)

library(scales)  # for the alpha() function


AGGR <- NEI[fips == 24510] # select Baltimore City, Maryland
# select  'Highway Veh' for analisys
temp.select <- grep("^Highway Veh", as.character(SCC[, Short.Name]))
subset.SCC <- SCC[temp.select]

# set keys for join
setkey(AGGR, SCC)
setkey(subset.SCC, SCC)

#
AGGR1 <- AGGR[subset.SCC, nomatch=0] # inner join



# set for the first plot
AGGR2 <- AGGR1[, list(sum.of.emissions = sum(Emissions))
               , by = list(year)]

# set for the second plot
AGGR3 <- AGGR1[, list(sum.of.emissions = sum(Emissions))
               , by = list(year, EI.Sector)
               ][, max.sum.of.emissions := max(sum.of.emissions)
                 , by = list(year)
                 ]
######################################################################


# make first base plot
G2 <- qplot(year
            , sum.of.emissions
            , data = AGGR2
            , col = I('blue')
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
          ) +
# add smooth aesthetics for smooth 
geom_smooth(color = alpha("orange", 0.2)
            , size = 2
            , linetype = 1
            , method = "lm"
            , se = FALSE) +
# add axis labels
labs(title = 'Total emission from motor vehicles from 1999 to 2008 in Baltimore City'
     , x = "Year"
     , y = "Sum of emission from motor vehicles") +
# grey theme
theme_grey() +
# setup range for x axis
coord_cartesian(xlim = c(min(AGGR2[, year]), max(AGGR2[, year]))) +
# setup range for y axis
coord_cartesian(ylim = c(0, max(AGGR2[, sum.of.emissions]) * 1.1)) +
# removes legend
theme(legend.position = "none") +
# set up x grid and lines
scale_x_continuous(breaks = AGGR2[, year]
                   , minor_breaks = seq(min(AGGR2[, year]), max(AGGR2[, year]), 3)
                   )
#############################################################################

# make second plot
G3 <- qplot(year
            , sum.of.emissions
            , data = AGGR3
            , col = I('blue')
            , alpha = I(0.7)
            , size = I(4) 
            ) +
# group by type of vehicle
facet_wrap(~ EI.Sector
           , nrow = 4
           , ncol = 1
           , scales = "free_y"
           ) +
# add points labels  
geom_text(aes(label=format(sum.of.emissions
                           , big.mark = "\ "
                           , digits = 2
                           )
             )
         , hjust=-0.2
         , vjust=0.5
         ) +
# add smooth aesthetics for smooth 
geom_smooth(color = alpha("orange", 0.2)
            , size = 2
            , linetype = 1
            , method = "lm"
            , se = FALSE
            ) +
# add axis labels
labs(title = ''
     , x = "Year"
     , y = "Sum of emission grouped by vehicle type from 1999 to 2008 in Baltimore City"
     ) +
# grey theme
theme_grey() +
# setup range for x axis
coord_cartesian(xlim = c(min(AGGR3[, year]) - 2, max(AGGR3[, year]) + 2)) +
# removes legend
theme(legend.position = "none") +
# set up x grid and lines
scale_x_continuous(breaks = AGGR3[, year]
                   , minor_breaks = seq(min(AGGR3[, year])
                                        , max(AGGR3[, year])
                                        , 3
                                        )
                   )
###########################################################


vp.G2 <- viewport(width = 0.75, height = 1, x = 0.25+0.75/2, y = 0.5)
vp.G3 <- viewport(width = 0.25, height = 1, x = 0.125, y = 0.5)


# open device
png(filename = "plot5.png"
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
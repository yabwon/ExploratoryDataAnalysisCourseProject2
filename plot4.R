## This first line will likely take a few seconds. Be patient!
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

## PLOT 4 ##
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# Upload a PNG file containing your plot addressing this question.

library(data.table)
NEI <- data.table(readRDS("summarySCC_PM25.rds"))
SCC <- data.table(readRDS("Source_Classification_Code.rds"))

library(pryr)
# object_size(NEI)
# object_size(SCC)

# for current analysis we decided to use following selection rule for coal combustion:
# grep("[Cc][Oo][Aa][Ll]", as.character(unique(SCC[, EI.Sector])), value = T)

# create subset of SCC, to select observations for analysis
temp.select <- grep("[Cc][Oo][Aa][Ll]", as.character(SCC[, EI.Sector]))
subset.SCC <- SCC[
  temp.select, list(SCC = as.character(SCC), EI.Sector = as.character(EI.Sector) )
  ][,
    EI.Sector := as.factor(EI.Sector)
    ]

# set keys for join
setkey(NEI, SCC)
setkey(subset.SCC, SCC)
# str(subset.SCC)


#AGGR <- NEI[subset.SCC][
#            sub("^[[:space:]]+|[[:space:]]+$", "", fips) != 'NA'
#            , list(sum.of.emissions = sum(Emissions))
#            , by = list(year
#                        , fips
#            )
#            ]

# set for main plot
AGGR1 <- NEI[subset.SCC][
            sub("^[[:space:]]+|[[:space:]]+$", "", fips) != 'NA'
            , list(sum.of.emissions = sum(Emissions))
            , by = list(year)
            ]

# set for 'top ten'
AGGR2 <- NEI[subset.SCC][
            sub("^[[:space:]]+|[[:space:]]+$", "", fips) != 'NA'
            , list(sum.of.emissions = sum(Emissions))
            , by = list(year,fips)
            ][sum.of.emissions > 0
              ]
setorder(AGGR2, year, -sum.of.emissions)

top_10_ <- function(x) {
  x[1:10]
}

setkeyv[AGGR2, c("year", "sum.of.emissions")]

# library(plyr)
top_ten <- rbindlist(lapply(split(AGGR2,AGGR2$year),top_10_))
top_ten[, fips:=as.factor(fips)]
setorder(top_ten, year, sum.of.emissions)


# set fo secondary plot
AGGR3 <- NEI[subset.SCC][
  sub("^[[:space:]]+|[[:space:]]+$", "", fips) != 'NA'
  , list(average.of.emissions = mean(Emissions))
  , by = list(year)
  ]

##############################################

# open device
png(filename = "plot4.png"
    , width = 1200
    , height = 1000
    , units = "px"
    , pointsize = 12
    , bg = "transparent"
)

# load libraries
library(ggplot2)
library(grid)

# make first base plot
G1 <- qplot(year
           , sum.of.emissions
           , data = AGGR1
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
geom_smooth(alpha = 1/2
            , size = 2
            , linetype = 1
            , method = "lm"
            , se = FALSE) +
# add axis labels
labs(title = 'Emissions from coal combustion-related sources changes from 1999 to 2008'
     , x = "Year"
     , y = "Sum of emissions from coal combustion") +
# grey theme
theme_grey() +
# setup range for x axis
coord_cartesian(xlim = c(min(AGGR1[, year]), max(AGGR1[, year]))) +
# setup range for y axis
coord_cartesian(ylim = c(0, max(AGGR1[, sum.of.emissions]) * 1.1)) +
# removes legend
theme(legend.position = "none") +
# set up x grid and lines
scale_x_continuous(breaks = AGGR1[, year]
                   , minor_breaks = seq(min(AGGR1[, year]), max(AGGR1[, year]), 3)) +
# set up y grid and lines
scale_y_continuous(breaks = seq(0 , 1.1*max(AGGR1[, sum.of.emissions]), 100000)
                   , labels = format(seq(0 , 1.1*max(AGGR1[, sum.of.emissions]), 100000)
                                    , big.mark = "\ "
                                    , digits = 2
                                    , scientific = F
                                    )
                   )
#############################################################################



# make secondary plot
G3 <- qplot(year
            , average.of.emissions
            , data = AGGR3
            , col = I('red')
            , alpha = I(0.7)
            , size = I(4)
) +
# add smooth aesthetics for smooth 
geom_smooth(alpha = 1/2
            , size = 2
            , col = I('red')
            , linetype = 1
            , method = "lm"
            , se = FALSE) +
# add axis labels
labs(title = 'Average emissions from coal combustion from 1999 to 2008'
     , x = "Year"
     , y = "Average of emissions from coal combustion") +
# grey theme
theme_grey() +
# setup range for x axis
coord_cartesian(xlim = c(min(AGGR3[, year]), max(AGGR3[, year]))) +
# setup range for y axis
coord_cartesian(ylim = c(0, max(AGGR3[, average.of.emissions]) * 1.1)) +
# removes legend
theme(legend.position = "none") +
# set up x grid and lines
scale_x_continuous(breaks = AGGR3[, year]
                   , minor_breaks = seq(min(AGGR3[, year]), max(AGGR3[, year]), 3)) +
# set up y grid and lines
scale_y_continuous(breaks = seq(0, 120, 10)
, label = format(seq(0, 120, 10)
, big.mark = "\ "
, digits = 2
)
)


# make "top ten" plot
t10 <- qplot(year
            , sum.of.emissions
            , data = top_ten
            #, col = as.factor(fips)
            , fill = as.factor(runif(length(fips))) #as.numeric(as.factor(fips))
            , geom = 'bar'
            , stat="identity"
            ) +
guides(fill=FALSE) +
labs(title = 'Top 10 emiters from 1999 to 2008'
     , x = "Year"
     , y = "Sum of top 10 emiters for coal combustion") +
# set up x grid and lines
scale_x_continuous(breaks = top_ten[, year]
                   , minor_breaks = seq(min(top_ten[, year]), max(top_ten[, year]), 3)) +
# set up y grid and lines
scale_y_continuous(breaks = seq(0 , 120000, 20000)
                  , label = format(seq(0 , 120000, 20000) 
                                  , big.mark = "\ "
                                  , digits = 2)
                                  )

# setup plots size and locallizationf 
vp.G1 <- viewport()
vp.t10 <- viewport(width = 0.3, height = 0.4, x = 0.25, y = 0.3)
vp.G3 <- viewport(width = 0.4, height = 0.4, x = 0.6, y = 0.3)

print(G1, vp = vp.G1)
print(t10, vp = vp.t10)
print(G3, vp = vp.G3)

# close device
dev.off()
rm(list = ls()); gc()
# OpenAir
This is just some graphs from OpenAir Package using "mydata"

# Libraries
library(zoo)
library(openair)
library(plyr)
library(dplyr)
library(ggplot2)
library(DT)        # reading datatable()
library(date)      # as.date()
library(DT)
library(Rmisc)     # summarySE()
require(ggplot2)
require(RColorBrewer)
data(mydata)
str(mydata)
head(mydata)
tail(mydata)
# =================== #
# Organizing the Data #
# =================== #


mydata <- mydata %>% 
  mutate(Day = substring(mydata$date,9,10))     # Adding Days
mydata$Day  <- as.numeric(mydata$Day)


mydata <- mydata %>% 
  mutate(Month = substring(mydata$date,6,7))     # Adding Months
mydata$Month  <- as.numeric(mydata$Month)


mydata <- mydata %>% 
  mutate(Year = substring(mydata$date,1,4))     # Adding Years
mydata$Year  <- as.numeric(mydata$Year)


mydata <- mydata %>% 
  mutate(Date.only = substring(mydata$date,1,10))   # Date Only
mydata$Date.only <- as.Date(mydata$Date.only,format="%Y-%m-%d")


mydata <- mydata %>% 
  mutate(Months = month.abb[mydata$Month])      # Months by name


mydata <- mydata %>% 
  mutate(Days = weekdays(mydata$Date.only, abbreviate = TRUE)) # Day by names


mydata <- mydata %>% 
  mutate(Hour = substring(mydata$date,12,13))   # Hour
mydata$Hour  <- as.numeric(mydata$Hour)
datatable(mydata)

mydata <- mydata %>% 
  mutate(weekinyear = strftime(mydata$Date.only, format = "%V"))  # Adding weeks
mydata$weekinyear <- as.numeric(mydata$weekinyear)

mydata <- mydata %>% 
  mutate(weekinmonth = as.integer(mydata$Day/7)+1)
mydata$weekinmonth <- as.integer(mydata$Day/7)+1


mydata <-mydata[order(mydata$weekinmonth),] #it was week

mydata <- mydata %>% 
  mutate(yearmonth = as.yearmon(mydata$Date.only))
mydata$yearmonth <- factor(mydata$yearmonth)

mydata <- ddply(mydata,.(mydata$yearmonth), transform, monthweek = weekinmonth)  

head(mydata)

mydatacalender <- mydata[, c("Year", "yearmonth", "Months", "weekinmonth", "monthweek", "Days", "co")]

ggplot(mydatacalender, aes(monthweek, Days, fill = co)) + 
  geom_tile(colour = "white") + 
  facet_grid(Year~Months) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "CO Level in The Calender", 
       subtitle="1998-2005", 
       fill="CO Level")
       
![Image of Yaktocat](https://raw.githubusercontent.com/amerie2013/OpenAir/master/Calender.bmp)   

# ==================================== #
# Function of Wind Speed and Direction #
# ==================================== #
plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "BrBG",
                          countmax = NA,
                          debug = 0){
  
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd, dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
  
  }  
  
  
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))    # Tidy up input data ----
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)   # The wind speed bins ----
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  
  spd.colors <- colorRampPalette(brewer.pal(min(max(3, n.colors.in.range), 
                                                min(9, n.colors.in.range)),
                                            palette))(n.colors.in.range)  # the color in the map
  if (max(data[[spd]],na.rm = TRUE) > spdmax){
    spd.breaks <- c(spdseq, max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq])),
                    paste(spdmax, "-", max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq]))
  }
  data$spd.binned <- cut(x = data[[spd]], breaks = spd.breaks, 
                         labels = spd.labels, ordered_result = TRUE)
  
  data. <- na.omit(data)    # clean up the data
  

  dir.breaks <- c(-dirres/2,seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres), "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))

  dir.binned <- cut(data[[dir]], breaks = dir.breaks, ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  if (debug>0){
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
  }
  if(packageVersion("ggplot2") > "2.2"){
    cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  

  p.windrose <- ggplot(data = data, aes(x = dir.binned, fill = spd.binned)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE, labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", values = spd.colors, drop = FALSE) +
    #theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_line(colour="grey65"))     # creating the plot
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  print(p.windrose)  
 
  return(p.windrose)     # return the handle to the wind rose
}
plot.windrose(data = mydata, 
                   spd =mydata$ws , dir = mydata$wd)
![Image of Yaktocat](https://raw.githubusercontent.com/amerie2013/OpenAir/master/Wind.bmp)  
# ========
# Groups # 
# ========
# S.D, S.E. of the mean, and a 95% C.I.
By_Years <- summarySE(mydata, measurevar="no2", groupvars=c("Days","Year","Hour"), na.rm = TRUE)
By_Years
#===================
#
pd <- position_dodge(0.1) # move them .05 to the left and right
p <- ggplot(By_Years, aes(x=Hour, y=no2, colour=Year, group=Year)) + 
  geom_errorbar(aes(ymin=no2-ci, ymax=no2+ci), colour="black", width=.1, 
                position=pd) +
  geom_line(position=pd) +
  facet_wrap(~ Days) +
  geom_point(position=pd, size=0.5, fill="white") 
print(p)
![Image of Yaktocat](https://raw.githubusercontent.com/amerie2013/OpenAir/master/By_Years.bmp) 

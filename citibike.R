setwd('C:/Users/barriosb/Desktop/citibike-tripdata')
citibike_tripdata_201501<-read.csv('201501-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201502<-read.csv('201502-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201503<-read.csv('201503-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201504<-read.csv('201504-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201505<-read.csv('201505-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201506<-read.csv('201506-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201507<-read.csv('201507-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201508<-read.csv('201508-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201509<-read.csv('201509-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201510<-read.csv('201510-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201511<-read.csv('201511-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')
citibike_tripdata_201512<-read.csv('201512-citibike-tripdata.csv', header = TRUE, sep = ",", colClasses='character')

citibike_tripdata<-rbind(citibike_tripdata_201501,citibike_tripdata_201502,citibike_tripdata_201503,
                         citibike_tripdata_201504,citibike_tripdata_201505,citibike_tripdata_201506,
                         citibike_tripdata_201507,citibike_tripdata_201508,citibike_tripdata_201509,
                         citibike_tripdata_201510,citibike_tripdata_201511,citibike_tripdata_201512)

#Cast the Variables
citibike_tripdata$tripduration<-as.integer(citibike_tripdata$tripduration)
citibike_tripdata$starttime<-as.POSIXct(citibike_tripdata$starttime,format = "%m/%d/%Y %H:%M")
citibike_tripdata$stoptime<-as.POSIXct(citibike_tripdata$stoptime,format = "%m/%d/%Y %H:%M")
citibike_tripdata$birth.year<-as.integer(citibike_tripdata$birth.year)
citibike_tripdata$gender<-as.integer(citibike_tripdata$gender)
citibike_tripdata$start.station.latitude<-as.numeric(citibike_tripdata$start.station.latitude)
citibike_tripdata$start.station.longitude<-as.numeric(citibike_tripdata$start.station.longitude)
citibike_tripdata$end.station.latitude<-as.numeric(citibike_tripdata$end.station.latitude)
citibike_tripdata$end.station.longitude<-as.numeric(citibike_tripdata$end.station.longitude)


# What is the median trip duration, in seconds?
median(citibike_tripdata$tripduration)

# What fraction of rides start and end at the same station
citibike_tripdata$start_end_same=ifelse(citibike_tripdata$start.station.id==citibike_tripdata$end.station.id,1,0)
sum(citibike_tripdata$start_end_same)/length(citibike_tripdata$start_end_same)

# We say a bike has visited a station if it has a ride that either started or ended at that station. 
# Some bikes have visited many stations; others just a few. What is the standard deviation
# of the number of stations visited by a bike?
library(plyr)
df1<-data.frame(bikeid= citibike_tripdata$bikeid,stationid = citibike_tripdata$start.station.id)
df2<-data.frame(bikeid = citibike_tripdata$bikeid,stationid = citibike_tripdata$end.station.id)
df_visted<-rbind(df1,df2)
summary_df<-ddply(df_visted,~bikeid,summarise,number_of_distinct_stations=length(unique(stationid)))
sd(summary_df$number_of_distinct_stations)
mean(summary_df$number_of_distinct_stations)

### What is the average length in kilometers of a trip? Assume trips follow great circle
##  arcs from the start station to the end station. Ignore trips that start and end at the same station,
### as well as those with obviously wrong data
library('Hmisc')
# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)

gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  # Convert degrees to radians
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

subset_citibike_tripdata<-subset(citibike_tripdata,citibike_tripdata$start_end_same==0)
subset_citibike_tripdata$triplength<-gcd.slc(subset_citibike_tripdata$start.station.longitude,subset_citibike_tripdata$start.station.latitude,
        subset_citibike_tripdata$end.station.longitude,subset_citibike_tripdata$end.station.latitude)
mean(subset_citibike_tripdata$triplength)


### Calculate the average duration of trips for each month in the year.
### (Consider a trip to occur in the month in which it starts.) What is the
### difference, in seconds, between the longest and shortest average durations?
citibike_tripdata$Month<-months(citibike_tripdata$starttime)
citibike_tripdata_month<-ddply(citibike_tripdata,~Month,summarise,average_tripduration=mean(tripduration))
max(citibike_tripdata_month$average_tripduration)-min(citibike_tripdata_month$average_tripduration)

### Let us define the hourly usage fraction of a station to be the fraction of all rides starting at that station
### that leave during a specific hour. A station has surprising usage patterns if it has 
### an hourly usage fraction for an hour significantly different from the corresponding
### hourly usage fraction of the system as a whole. What is the largest ratio of station hourly usage
## fraction (hence corresonding to the most "surprising" station-hour pair)?
citibike_tripdata$Hour <-format(citibike_tripdata$starttime, format="%H")
citibike_tripdata$Day<-format(citibike_tripdata$starttime, format="%d")
citibike_tripdata_by_station_M_D_H<-ddply(citibike_tripdata,c("Hour","start.station.id"), summarise,number_of_unique_bike_ids=length(bikeid))
citibike_tripdata_by_M_D_H<-ddply(citibike_tripdata,c("Hour"), summarise,number_of_unique_bike_ids=length(bikeid))
citibike_tripdata_hourly_usage_fraction<-merge(citibike_tripdata_by_M_D_H,citibike_tripdata_by_station_M_D_H, by = c("Hour"))
citibike_tripdata_hourly_usage_fraction$hourly_usage_fraction<-citibike_tripdata_hourly_usage_fraction$number_of_unique_bike_ids.y/citibike_tripdata_hourly_usage_fraction$number_of_unique_bike_ids.x
citibike_tripdata_hourly_usage_fraction$hourly_usage_fraction_system<-citibike_tripdata_hourly_usage_fraction$number_of_unique_bike_ids.x/length(citibike_tripdata$bikeid)
citibike_tripdata_hourly_usage_fraction$hourly_usage_fraction_ratio<-citibike_tripdata_hourly_usage_fraction$hourly_usage_fraction/citibike_tripdata_hourly_usage_fraction$hourly_usage_fraction_system
library(plotly)
ggplot(citibike_tripdata_hourly_usage_fraction, aes(x=Hour, y=hourly_usage_fraction_ratio))+
  geom_boxplot()+
  labs(x="Hour")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))

ggplotly()

subset(citibike_tripdata_hourly_usage_fraction,citibike_tripdata_hourly_usage_fraction$hourly_usage_fraction_ratio==max(citibike_tripdata_hourly_usage_fraction$hourly_usage_fraction_ratio))
## station-hour pair that is most surprising is 04AM at station 312.

## There are two types of ridesrs: "Customers" and "Subscribers".
### Customers buy a short-time pass which allows 30 minute rides.
#### Subscribers buy yearly passes that allows 45 minute rides.
### What fraction of rides exceed their corresponding time limit?
citibike_tripdata$exceedtimelimit=ifelse((((citibike_tripdata$usertype=="Customer")&(citibike_tripdata$tripduration>30*60))|
                                           ((citibike_tripdata$usertype=="Subscriber")&(citibike_tripdata$tripduration>45*60))),1,0)


sum(citibike_tripdata$exceedtimelimit)/length(citibike_tripdata$exceedtimelimit)

### Most of the time, a bike will begin a trip at the same station where its previous
## trip ended. Sometimes a bike will be moved by the program, either for maintenance or to
## rebalance the distribution of bikes. What is the average number of times a bike is moved
## during this period, as detected by seeing if it starts at a different station than where the 
## previous ride ended

library(DataCombine)
sort.citibike_tripdata<-citibike_tripdata[order(citibike_tripdata$bikeid,citibike_tripdata$starttime),]
citibike_tripdata2<-slide(sort.citibike_tripdata, Var="start.station.id", GroupVar ="bikeid", slideBy=1)
citibike_tripdata2$moved<-ifelse(citibike_tripdata2$end.station.id == citibike_tripdata2$start.station.id1,0,1)
citibike_tripdata2$moved[is.na(citibike_tripdata2$moved)]<-0
citibike_tripdata2_moved<-ddply(citibike_tripdata2,~bikeid,summarise,number_of_times_moved=sum(moved))
mean(citibike_tripdata2_moved$number_of_times_moved)
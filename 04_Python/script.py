---
title: "BART Transit System"
author: "Jonathan Bouchet"
date: "`r Sys.Date()`"
output:
 html_document:
    fig_width: 10
    fig_height: 7
    toc: yes
    number_sections : yes
    code_folding: show
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,message=FALSE,warning=FALSE)
```

<center><img src="https://i2.wp.com/www.iliveinthebayarea.com/wp-content/uploads/2012/12/bart.jpg?resize=380%2C210"></center>

<hr>

<strong>History :</strong>

* _version 1 : initial commit_ 
* _version 2 : added name to start/end station_ 
* _version 3 : added heatmap with daily sum of departures for the 2017 data_
* _version 4 : redid map of connections_

<hr>

```{r}
#load packages and csv file
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(rworldmap)
library(ggthemes)
library(gridExtra)
library(geosphere)
library(lubridate)
library(viridis)
library(ggrepel)
```

# Motivation

* recreate the BART Line system visualization as the [real one](https://www.bart.gov/sites/all/themes/bart_desktop/img/system-map.gif)
* study line's and station's occupancy(vs. month) to see which line is the busiest

# Data preparation

The data is pretty clean and does not need much work. I will only extract the `Longitude` and `Latitude` from the `Location` feature.

Most of the work in this section is dedicated to prepare the maps. The issue is to plot both point(=station) and segment(=connection between stations) because `ggplot` tries to connect all points together.

To circumvent this, I use `gcIntermediate` package that needs to define a `start`, an `end` for each segment. Then for each line, I make a dataframe with all these segments.

I got the stations details per line on [wikipedia](https://en.wikipedia.org/wiki/Pittsburg/Bay_Point%E2%80%93SFO/Millbrae_line).

## San Francisco / Bay area Map

```{r}
counties<-map_data("county")
ca_county <- subset(counties, region == "california")
mCal<-ggplot() + 
  geom_map(data = ca_county, map = ca_county,aes(x = long, y = lat, map_id = region, group = group),fill = "white", color = "black", size = 0.2) + 
  theme_fivethirtyeight() +
  coord_fixed(1.3)
```

## Getting the geo-locations

* straightforward: I split the `Location` feature by `,` and convert to numeric.
* I also remove the Abbreviation from the name, it's easier to join dataframes later.

```{r}
stations<-read.csv('../input/bart-ridership/station_info.csv',sep=',',stringsAsFactors=F)
stations$Longitude<-sapply(stations$Location, function(x) as.numeric(strsplit(x,',')[[1]][1]))
stations$Latitude<-sapply(stations$Location, function(x) as.numeric(strsplit(x,',')[[1]][2]))
stations$Elevation<-sapply(stations$Location, function(x) as.numeric(strsplit(x,',')[[1]][3]))
stations$Name2<-sapply(stations$Name, function(x) trimws(strsplit(x,'\\(')[[1]][1]))
```

## Define stations by line

* I use a list to store individual list corresponding to each line:
* I impute the color code and name.

```{r}
listStations<-list()
richmondStations = c('Richmond','El Cerrito del Norte','El Cerrito Plaza','North Berkeley','Downtown Berkeley','Ashby','West Oakland','Embarcadero','Montgomery St.','Powell St.','Civic Center/UN Plaza','Daly City','Colma','South San Francisco','San Bruno','Millbrae')

fremontStations<-c('Fremont','Union City','South Hayward','Hayward','Bay Fair','San Leandro','Coliseum/Oakland Airport','Fruitvale','Lake Merritt','12th St. Oakland City Center','19th St. Oakland','MacArthur','Ashby','Downtown Berkeley','North Berkeley','El Cerrito Plaza','El Cerrito del Norte','Richmond')

pittsburgStations <- c('Pittsburg/Bay Point','North Concord/Martinez','Concord','Walnut Creek','Lafayette','Orinda','Rockridge','MacArthur','19th St. Oakland','12th St. Oakland City Center','West Oakland','Embarcadero','Montgomery St.','Powell St.','Civic Center/UN Plaza','16th St. Mission','24th St. Mission','Glen Park','Balboa Park','Daly City','Colma','South San Francisco','San Bruno','San Francisco Int\'l Airport','Millbrae') 

dublinStations <- c('Dublin/Pleasanton','West Dublin/Pleasanton','Castro Valley','Bay Fair','San Leandro','Coliseum/Oakland Airport','Fruitvale','Lake Merritt','West Oakland','Embarcadero','Montgomery St.','Powell St.','Civic Center/UN Plaza','16th St. Mission','24th St. Mission','Glen Park','Balboa Park','Daly City')

warmspringsStations<-c('Warm Springs/South Fremont','Fremont','Union City','South Hayward','Hayward','Bay Fair','San Leandro','Coliseum/Oakland Airport','Fruitvale','Lake Merritt','West Oakland','Embarcadero','Montgomery St.','Powell St.','Civic Center/UN Plaza','16th St. Mission','24th St. Mission','Glen Park','Balboa Park','Daly City')

l1<-list(args=richmondStations,label='RICHMOND',color='red2')
l2<-list(args=fremontStations,label='FREMONT',color='orange')
l3<-list(args=pittsburgStations,label='PITTSBURG/BAY POINT',color='yellow2')
l4<-list(args=dublinStations,label='DUBLIN/PLEASANTON',color='steelblue')
l5<-list(args=warmspringsStations,label='WARM SPRINGS/ SOUTH FREMONT',color='green4')

listStations[[1]]<-l1
listStations[[2]]<-l2
listStations[[3]]<-l3
listStations[[4]]<-l4
listStations[[5]]<-l5
```

## Making segments

* The function below loops over all rows of a `Line` dataframe (ie having the stations details)
* then it makes segments connecting the `i+1` and `i` rows

```{r}
makeLines<-function(mydf){
	connections_df<-data.frame(lon=double(), lat=double())
	for(i in 2:(nrow(mydf))){
		start<-c(mydf$Longitude[i-1],mydf$Latitude[i-1])
		end<-c(mydf$Longitude[i],mydf$Latitude[i])
		inter <- data.frame(gcIntermediate(start,  end, n=100, addStartEnd=TRUE, breakAtDateLine=F))
		connections_df<-rbind(connections_df,inter)
    }
    return(connections_df)
}
```

There are 5 lines in the BART system, so it's better to automate everything. The loop is:

* making a dataframe based on the stations, per Line
* join it with the `stations` file that has the geo-locations
* make segments from it
* save both segments and points in a list ... and voilà.

```{r}
listSegments<-list()
listPoints<-list()
 for (i in 1:5){
 	temp <- data.frame(Name2 = c(listStations[[i]]$args))
 	temp$Name2<-as.character(temp$Name2)
 	temp$LineName<-rep(listStations[[i]]$label,nrow(temp))
 	temp<-data.frame(left_join(temp,stations,by='Name2'))
 	temp_segments<-makeLines(temp)
 	temp_segments$color<-rep(listStations[[i]]$color,nrow(temp_segments))
 	temp <- temp %>% mutate(stationType = ifelse(row_number()==1 | row_number()== nrow(temp),'Start/End','transit'))
 	
 	listPoints[[i]]<-temp
 	listSegments[[i]]<-temp_segments
}
```

## Example{.tabset .tabset-fade .tabset-pills}

```{r}
# does not work with geom_label_repel --> fix me later
singleLine<-list()

for(i in 1:5){
	singleLine[[i]]<-mCal + geom_point(data=listSegments[[i]],aes(x=lon, y= lat,color=color),size=2) +
	  geom_point(data=listPoints[[i]],aes(x=Longitude, y= Latitude),size=2,color='white') +
	  theme_fivethirtyeight() + xlim(-122.6,-121.8) + ylim(37.55,38.1) + 
	  coord_fixed(1.5) + scale_color_identity() + labs(subtitle=listStations[[i]]$label) + 
	  theme(axis.text = element_blank()) + theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))
}
```

```{r,eval=F}
grid.arrange(singleLine[[1]], singleLine[[2]],singleLine[[5]], singleLine[[3]], singleLine[[4]],ncol=3)
#do.call(grid.arrange,c(singleLine,ncol=1))
```

### `RICHMOND` line

```{r}
i<-1
mCal + geom_point(data=listSegments[[i]],aes(x=lon, y= lat,color=color),size=2) +
	  geom_point(data=listPoints[[i]],aes(x=Longitude, 
	                                      y= Latitude),
	                                      size=2,color='white') +
	  theme_fivethirtyeight() + xlim(-122.6,-121.8) + ylim(37.5,38.1) + 
	  coord_fixed(1.5) + scale_color_identity() + labs(subtitle=listStations[[i]]$label) + 
	  theme(axis.text = element_blank()) + theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+ 
    geom_label_repel(data=listPoints[[i]],aes(x=Longitude,y=Latitude,label=ifelse(listPoints[[i]]$stationType=='Start/End',listPoints[[i]]$Name,'')),force=10)
```

### `FREMONT` line

```{r}
i<-2
mCal + geom_point(data=listSegments[[i]],aes(x=lon, y= lat,color=color),size=2) +
	  geom_point(data=listPoints[[i]],aes(x=Longitude, 
	                                      y= Latitude),
	                                      size=2,color='white') +
	  theme_fivethirtyeight() + xlim(-122.6,-121.8) + ylim(37.5,38.1) + 
	  coord_fixed(1.5) + scale_color_identity() + labs(subtitle=listStations[[i]]$label) + 
	  theme(axis.text = element_blank()) + theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+ 
    geom_label_repel(data=listPoints[[i]],aes(x=Longitude,y=Latitude,label=ifelse(listPoints[[i]]$stationType=='Start/End',listPoints[[i]]$Name,'')),force=10)
```

### `PITTSBURG/BAY POINT` line

```{r}
i<-3
mCal + geom_point(data=listSegments[[i]],aes(x=lon, y= lat,color=color),size=2) +
	  geom_point(data=listPoints[[i]],aes(x=Longitude, 
	                                      y= Latitude),
	                                      size=2,color='white') +
	  theme_fivethirtyeight() + xlim(-122.6,-121.8) + ylim(37.5,38.1) + 
	  coord_fixed(1.5) + scale_color_identity() + labs(subtitle=listStations[[i]]$label) + 
	  theme(axis.text = element_blank()) + theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+ 
    geom_label_repel(data=listPoints[[i]],aes(x=Longitude,y=Latitude,label=ifelse(listPoints[[i]]$stationType=='Start/End',listPoints[[i]]$Name,'')),force=10)
```

### `DUBLIN/PLEASANTON` line

```{r}
i<-4
mCal + geom_point(data=listSegments[[i]],aes(x=lon, y= lat,color=color),size=2) +
	  geom_point(data=listPoints[[i]],aes(x=Longitude, 
	                                      y= Latitude),
	                                      size=2,color='white') +
	  theme_fivethirtyeight() + xlim(-122.6,-121.8) + ylim(37.5,38.1) + 
	  coord_fixed(1.5) + scale_color_identity() + labs(subtitle=listStations[[i]]$label) + 
	  theme(axis.text = element_blank()) + theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+ 
    geom_label_repel(data=listPoints[[i]],aes(x=Longitude,y=Latitude,label=ifelse(listPoints[[i]]$stationType=='Start/End',listPoints[[i]]$Name,'')),force=10)
```

### `WARM SPRINGS/ SOUTH FREMONT` line

```{r}
i<-5
mCal + geom_point(data=listSegments[[i]],aes(x=lon, y= lat,color=color),size=2) +
	  geom_point(data=listPoints[[i]],aes(x=Longitude, 
	                                      y= Latitude),
	                                      size=2,color='white') +
	  theme_fivethirtyeight() + xlim(-122.6,-121.8) + ylim(37.5,38.1) + 
	  coord_fixed(1.5) + scale_color_identity() + labs(subtitle=listStations[[i]]$label) + 
	  theme(axis.text = element_blank()) + theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+ 
    geom_label_repel(data=listPoints[[i]],aes(x=Longitude,y=Latitude,label=ifelse(listPoints[[i]]$stationType=='Start/End',listPoints[[i]]$Name,'')),force=10)
```


# 2017 data analysis

This dataset covers the 5 first months of 2017. The columns are the `departure` and `Destination` station, as well as the number of rides, on a hourly basis.

## Data preparation

The data preparation will consist here in extracting the `Day` and `Month` for a later data analysis.

```{r}
bart_2017<-read.csv('../input/bart-ridership/date-hour-soo-dest-2017.csv',sep=',',stringsAsFactors=F)
bart_2017$Month<-month(bart_2017$DateTime)
bart_2017$Day<-day(bart_2017$DateTime)
```


```{r}
#get the summary count for 2017 entirely
departingStations_2017 <- data.frame(bart_2017 %>% group_by(Origin) %>% summarise(count=n(), sum = sum(Throughput)))
#merge with the stations dataframe
departingStations_2017_v2<- data.frame(
  left_join(
    departingStations_2017 %>% rename(Abbreviation = Origin), 
    stations %>% select(Longitude, Latitude, Name2, Abbreviation), by='Abbreviation'))
```

##Results

```{r}
mCal + geom_point(data=departingStations_2017_v2, aes(x=Longitude, y=Latitude, size=sum,color=sum),alpha=.75) + 
  xlim(-122.6,-121.8) + ylim(37.5,38.1) + 
  scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + theme_fivethirtyeight() + 
  scale_radius(range=c(0,8)) + 
  theme(axis.text=element_blank(), 
        legend.position='right',
        legend.direction='vertical') + 
  labs(title='2017 BART Transit',
       subtitle='represented is the overall count of single rides by departing station') + 
  guides(size=F)
```

San Francisco area seems pretty busy as expected. Let's have a closer look. I will use a `shapefile` of the city from another Kaggle dataset.

```{r}
library(rgdal)
shapefile <- readOGR('../input/sf-restaurant-inspection-scores/Shapefiles (2)/geo_export_38fd3153-0303-488e-9f3c-0f81e8e00115.shp')
sfMap<-ggplot() +
  geom_path(data = shapefile,aes(x = long, y = lat, group = group),color = 'black',size = .5) + 
  coord_fixed(1.3)
```

```{r}
sfMap + 
  geom_point(data=departingStations_2017_v2, aes(x=Longitude, y=Latitude, size=sum,color=sum),alpha=.75) + 
  xlim(-122.55,-122.35) + ylim(37.7,37.85) + 
  scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") +
  theme_fivethirtyeight() + scale_radius(range=c(0,8)) + 
  labs(title='2017 BART transit, San Francisco Downtown',
       subtitle='represented is the overall count of single rides by departing station') + 
  theme(axis.text=element_blank(), 
        legend.position='right',
        legend.direction='vertical') + 
  guides(size=F)
```

# 2017 Overview

```{r}
bart_2017$weekdays<-weekdays(as.Date(bart_2017$DateTime))
bart_2017$week<-week(as.Date(bart_2017$DateTime))
bart_2017$weekdays <- factor(bart_2017$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))
bart_2017$ymd<-ymd(as.Date(bart_2017$DateTime))
```

```{r fig.width=12, fig.height=12, fig.align='center'}
left_join(bart_2017, stations %>% rename(Origin = Abbreviation), by=c('Origin')) %>% 
dplyr::group_by(Name2, ymd) %>% dplyr::summarize(count=n(),sum= sum(Throughput)) %>% 
ggplot(aes(x=ymd,y=sum,group=Name2)) + geom_line(aes(color=sum), size=.5) + scale_color_gradientn(name="",colors=rev(viridis::inferno(5))) + 
theme_fivethirtyeight() + theme(axis.text = element_text(size=6)) + facet_wrap(~Name2,ncol=5) + 
labs(title = 'BART 2017 activity', subtitle='Count of daily rides per stations') + guides(color=F)
```

We clearly see which `stations` were the busiest in 2017.

# 2017 Breakdown{.tabset .tabset-fade .tabset-pills}

* I join the `2017` datafile with the `stations` datafile to get the full `Station` name
* I then `groupby` the `2017` data by day and station and make a heatmap for each

```{r}
tt<-data.frame(left_join(bart_2017 %>% select(Origin, week, weekdays, Throughput), stations %>% select(Abbreviation, Name2) %>% rename(Origin = Abbreviation), by=c('Origin')))
```

```{r}
makePlot<-function(mystation,myname){
 	mydf <- data.frame(tt %>% filter(Origin==mystation) %>% group_by(week,weekdays) %>%summarize(week_count=n(),week_sum=sum(Throughput)))
 	pal='B'
 	g<-ggplot(mydf, aes(x = week, y = weekdays, fill=week_sum)) + 
 	  scale_fill_viridis(name="", 
 	                     option = pal,  
 	                     direction = -1,
 	                     na.value = "grey93", 
 	                     limits = c(0, max(mydf$week_sum))) + 
 	  geom_tile(color = "white", size = 0.4) +
 	  scale_x_continuous(expand = c(0, 0),
 	                     breaks = seq(1, 52, length = 12),
 	                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov","Dec")) + 
 	  theme(axis.title = element_blank(),
 	        axis.ticks = element_blank(),
 	        axis.text.y = element_text(size=6),
 	        legend.position = "right",
 	        legend.direction='vertical',
 	        legend.key.width = unit(1, "cm"),
 	        strip.text = element_text(hjust = 0.01, face = "bold", size = 10),
 	        panel.grid=element_blank()) + 
 	  ggtitle(paste0(myname,':count of departure rides')) + 
 	  theme_fivethirtyeight() + 
 	  guides(fill = guide_colorbar(barwidth = 30, barheight = 1))
 	return(g)
}

mylist<-list()
 for(i in 1:length(unique(tt$Origin))){
 	curStation <- unique(tt$Origin)[i]
 	curName <- unique(tt$Name2)[i]
 	mylist[[i]]<-makePlot(curStation, curName)
 }
```

## 12th St. Oakland City Center

```{r fig.width=12, fig.height=6, fig.align='center'}
mylist[[1]]
```

## Montgomery St.

```{r fig.width=12, fig.height=6, fig.align='center'}
mylist[[26]]
```

## Civic Center/UN Plaza

```{r fig.width=12, fig.height=6, fig.align='center'}
mylist[[9]]
```

## Downtown Berkeley

```{r fig.width=12, fig.height=6, fig.align='center'}
mylist[[14]]
```

As expected, the activity is max. during weekdays, except for January,1st and other holidays.

# Map of correspondances
 
* I grouped the data per (`Origin`, `Destination`) by taking the sum of `throughput` for the whole 2017 year.
* I also used the `stations` data as a look-up table to impute the geo-coordinates

```{r}
# look up table
ref <- data.frame(stations %>% select(Name2, Abbreviation, Longitude, Latitude) %>% rename(NameDestination = Name2))
tt<- data.frame(bart_2017 %>% group_by(Origin, Destination) %>% summarise(count=n(), sum = sum(Throughput)))
tt$NameDestination <- sapply(tt$Destination, function(x) ref$NameDestination[match(x, ref$Abbreviation)])
tt$NameOrigin <- sapply(tt$Origin, function(x) ref$NameDestination[match(x, ref$Abbreviation)])
tt$long_origin <- sapply(tt$Origin, function(x) ref$Longitude[match(x, ref$Abbreviation)])
tt$lat_origin <- sapply(tt$Origin, function(x) ref$Latitude[match(x, ref$Abbreviation)])
tt$long_destination <- sapply(tt$Destination, function(x) ref$Longitude[match(x, ref$Abbreviation)])
tt$lat_destination <- sapply(tt$Destination, function(x) ref$Lat[match(x, ref$Abbreviation)])
```

```{r}
curList<-list()
curPlot<-list()
cnt<-0
for (i in 1:length(unique(tt$Origin))){
	cnt<-cnt+1
	curList[[cnt]]<-data.frame(tt %>% filter(Origin ==unique(tt$Origin)[cnt]))
}
pp <- data.frame(do.call("rbind",curList))
pp <- data.frame(pp %>% filter(NameOrigin!='NA'))
```

```{r}
mCal + 
  geom_point(data=pp,aes(x=long_destination, y = lat_destination),size=3,alpha=.75) +
  geom_curve(data=filter(pp,Origin!=Destination), 
             aes(x = long_origin, y = lat_origin, xend = long_destination, yend = lat_destination, color=Origin), size=.25, alpha=.35, curvature = .2 ,arrow = arrow(length = unit(0.02, "npc"))) + 
  xlim(-122.6,-121.8) + ylim(37.55,38.1) + 
  scale_color_manual(values=viridis::viridis(length(unique(pp$Origin)))) + 
  guides(color = FALSE) + theme(axis.text=element_blank()) + labs(title='BART: all connections')
```

```{r fig.width=14, fig.height=10, fig.align='center'}
mCal + 
  geom_point(data=pp,aes(x=long_destination, y = lat_destination),size=1,alpha=1) +
  geom_curve(data=filter(pp,Origin!=Destination), 
             aes(x = long_origin, y = lat_origin, xend = long_destination, yend = lat_destination, color=Origin), size=.25, alpha=.75, curvature = .2 ,arrow = arrow(length = unit(0.02, "npc"))) + 
  xlim(-122.6,-121.8) + ylim(37.55,38.1) + 
  scale_color_manual(values=viridis::viridis(length(unique(pp$Origin)))) + 
  guides(color = FALSE) + facet_wrap(~NameOrigin,ncol=8)+ theme(axis.text=element_blank())
```
#BISM Lecture 2
#Script for reploting lecture 2

#Install the following packages in case you do not have them already:
#install.packages("flextable") 
#install libraries
#install.packages("gplots")
#install.packages("RColorBrewer")
#install.packages("directlabels")
#install.packages("ggtext")

#These are the libraries used during this script:
library(flextable) 
library(officer)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(gcookbook)
library(dplyr)
library(scales)
library(plotly)
library(directlabels)
library(grid)
library(ggtext)



### Section 1: Tables


#Create a table using "iris" dataset
df_table1 = flextable(head(iris))
df_table1 = bold(df_table1, part = "header") # bold header
df_table1 = fontsize(df_table1, part = "header", size = 12)  #font size


##  Section 1.1: heavy borders
#define borders
heavy_border_outer = fp_border(color="black", width = 2)
heavy_border_v = fp_border(color="black", width = 2)
heavy_border_h = fp_border(color="black", width = 2)

#apply the borders to df_table1
df_table1 =  bg(df_table1, bg = "black", part = "header") # change header background colour 
df_table1 = color(df_table1, color = "white",part="header") #change header font colour
df_table1 = border_remove(x = df_table1)
df_table1 = border_outer(df_table1, part="all", border = heavy_border_outer )
df_table1 = border_inner_h(df_table1, part="all", border = heavy_border_h )
df_table1 = border_inner_v(df_table1, part="all", border = heavy_border_v )
df_table1


##  Section 1.2: light borders
#define borders
light_size=0.1
light_border_outer = fp_border(color="gray77", width = light_size)
light_border_v = fp_border(color="gray77", width = light_size)
light_border_h = fp_border(color="gray77", width = light_size)

#apply the borders to df_table1
df_table1 = bg(df_table1, bg = "gray77", part = "header") # change header background colour 
df_table1 = color(df_table1, color = "white",part="header") #change header font colour
df_table1 = border_remove(x = df_table1)
df_table1 = border_outer(df_table1, part="all", border = light_border_outer )
df_table1 = border_inner_h(df_table1, part="all", border = light_border_h )
df_table1 = border_inner_v(df_table1, part="all", border = light_border_v )
df_table1


## Section 1.3: Minimal borders
#define borders
minimal_size=0.03
minimal_border_outer = fp_border(color="gray80", width = minimal_size)
minimal_border_v = fp_border(color="gray80", width = minimal_size)
minimal_border_h = fp_border(color="gray80", width = minimal_size)

#apply the borders to df_table1
df_table1 = bg(df_table1, bg = "white", part = "header") # change header background colour 
df_table1 = color(df_table1, color = "black",part="header") #change header font colour
df_table1 = border_remove(x = df_table1)
df_table1 = border_outer(df_table1, part="all", border = minimal_border_outer )
df_table1 = border_inner_h(df_table1, part="all", border = minimal_border_h )
df_table1 = border_inner_v(df_table1, part="all", border = minimal_border_v )
df_table1



### Section 2: Heatmaps



## Section 2.1: Plotting a heatmap


#set session directory
#Session -> Set Working Directory -> Choose Directory
#for example:
#setwd("C:/Users/DELL/OneDrive - Griffith University/Data Science/Internships/Insight RSA/BISM PPT")

#Read the file Heatmat_DB2.csv, which contains a database to plot heatmap
df_heatmap1 = read.csv(file = 'Heatmat_DB2.csv',header=TRUE)

#plot a heatmap
plot_heatmap1=ggplot(df_heatmap1, aes(x = Product_name , y = Customer_name , fill = Acceptance))
plot_heatmap1 +
  geom_tile() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(limits=rev(levels(df_heatmap1$Customer_name)),expand = c(0,0))+
  scale_fill_gradient(low = "lightcyan2", high = "skyblue3", scale(0,10)) +
  geom_text(aes(label=sprintf("%2.0f%%", 100*Acceptance)))


## Section 2.2: plotting a table with heatmap data


#I had issues here transforming the table. Therefore, I created another CSV with table structure
#Help in here!
#plot a table of the same data
df_heatmap2 = read.csv(file = 'Heatmat_DB.csv',header=TRUE)
df_table2 = flextable(df_heatmap2)
df_table2



### Section 3: Scatterplots



## Section 3.1: First scatterplot


#Dataset: Height and weight of schoolchildren
x_mean=mean(heightweight$ageYear) #calculate x mean
y_mean=mean(heightweight$heightIn) #calculate y mean
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + #ggplot of the data set
  geom_point(colour="grey77",size=1.5)+  #plot points in colour grey77
  geom_point(x=x_mean,y=y_mean,colour="black",size=2.4)+ #plot the average point
  annotate("text",x=x_mean-0.25,y=y_mean+0.3,colour="black",label="AVG")+ #include the annotation of the point
  ggtitle("Height and weight of schoolchildren") + #Modify the title of the plot
  scale_x_continuous(name = "Age in years")+ #modify x axis
  scale_y_continuous(name = "Height in inches")+ #modify y axis
  theme(
    axis.text = element_text(colour="grey55"), #modify x axis colour
    axis.title = element_text(colour="grey55",face="bold"), #modify x axis title colour
    panel.background = element_rect(fill = 'white',colour="grey77")) #modify background colour


## Section 3.2 second scatterplot


#modify original dataset to create a new variable to filter height on the average
df_scatter1 = heightweight %>%
  mutate(height_avg = ifelse(heightIn < y_mean, "< avg", ">= avg"))

ggplot(df_scatter1, aes(x = ageYear, y = heightIn,colour=height_avg)) + #ggplot of the data set
  geom_hline(yintercept = y_mean,linetype = "dashed")+
  geom_point(size=1.5) + #plot points in colour grey77
  scale_colour_manual(values = c("grey77","orange"),guide=FALSE)+
  geom_point(x=x_mean,y=y_mean,colour="black",size=2.4)+ #plot the average point
  annotate("text",x=x_mean-0.25,y=y_mean+0.3,colour="black",label="AVG")+ #include the annotation of the point
  ggtitle("Height and weight of schoolchildren") + #Modify the title of the plot
  scale_x_continuous(name = "Age in years")+ #modify x axis
  scale_y_continuous(name = "Height in inches")+ #modify y axis
  theme(
    axis.text.x = element_text(colour="grey55"), #modify x axis colour
    axis.text.y = element_text(colour="grey55"), #modify y axis colour
    axis.title.x = element_text(colour="grey55",face="bold"), #modify x axis title colour
    axis.title.y = element_text(colour="grey55", face="bold"), #modify y axis title colour
    panel.background = element_rect(fill = 'white',colour="grey77")) #modify background colour
  


### Section 4: line graphs



## Section 4.1: preparing the dataset


#create a new dataset with the countries Australia, Argentina, and New Zealand
df_line1=countries %>%
  filter(Name  %in% c("Australia","Argentina","New Zealand"))
#check which years do not have GDP data
df_line1 %>%
  filter(is.na(GDP)) %>%
  select(Year)
#Only using year interval of 2005 - 2009
df_line1= df_line1 %>%
  filter(Year >= 2005 & Year <=2009)
  

## Section 4.2: Single serie


#filter a dataset with only Australia's GDP
line_sng=df_line1 %>%
  filter(Name=="Australia")

ggplot(line_sng, aes(x = Year, y = GDP, colour = Name)) +
  geom_line(size=1)+
  scale_colour_manual(values = c("dark blue"),guide=FALSE)+
  annotate("text",x=2009,y=41000,colour="dark blue",label="Australia",hjust = 0) + #Australia
  scale_x_continuous(name = "",limits=c(2005,2010.5),breaks=seq(2005,2009))+ #modify x axis
  scale_y_continuous(name = "",limits=c(0,max(df_line1$GDP)))+ #modify y axis
  theme(
    axis.text.x = element_text(colour="grey55"), #modify x axis colour
    axis.text.y = element_text(colour="grey55"), #modify y axis colour
    axis.title.x = element_text(colour="grey55",face="bold"), #modify x axis title colour
    axis.title.y = element_text(colour="grey55", face="bold"), #modify y axis title colour
    panel.background = element_rect(fill = 'white',colour="grey77")) #modify background colour


## Section 4.3: Two series


#filter a dataset with Australia and New Zealand GDP
line_two=df_line1 %>%
  filter(Name  %in% c("Australia","New Zealand"))

ggplot(line_two, aes(x = Year, y = GDP, colour = Name)) +
  geom_line(size=1)+
  scale_colour_manual(values = c("dark blue","grey77"),guide=FALSE)+
  annotate("text",x=2009,y=41000,colour="dark blue",label="Australia",hjust = 0) + #Australia
  annotate("text",x=2009,y=30000,colour="grey55",label="New Zealand",hjust = 0)+  #New Zealand
  scale_x_continuous(name = "",limits=c(2005,2010.5),breaks=seq(2005,2009))+ #modify x axis
  scale_y_continuous(name = "",limits=c(0,max(df_line1$GDP)))+ #modify y axis
  theme(
    axis.text.x = element_text(colour="grey55"), #modify x axis colour
    axis.text.y = element_text(colour="grey55"), #modify y axis colour
    axis.title.x = element_text(colour="grey55",face="bold"), #modify x axis title colour
    axis.title.y = element_text(colour="grey55", face="bold"), #modify y axis title colour
    panel.background = element_rect(fill = 'white',colour="grey77")) #modify background colour


## Section 4.4 Multiple series


ggplot(df_line1, aes(x = Year, y = GDP, colour = Name)) +
  geom_line(size=1)+
  scale_colour_manual(values = c("grey77","dark blue","grey77"),guide=FALSE)+
  annotate("text",x=2009,y=41000,colour="dark blue",label="Australia",hjust = 0) + #Australia
  annotate("text",x=2009,y=9000,colour="grey55",label="Argentina",hjust = 0)+  #Argentina
  annotate("text",x=2009,y=30000,colour="grey55",label="New Zealand",hjust = 0)+  #New Zealand
  scale_x_continuous(name = "",limits=c(2005,2010.5),breaks=seq(2005,2009))+ #modify x axis
  scale_y_continuous(name = "",limits=c(0,max(df_line1$GDP)))+ #modify y axis
  theme(
    axis.text.x = element_text(colour="grey55"), #modify x axis colour
    axis.text.y = element_text(colour="grey55"), #modify y axis colour
    axis.title.x = element_text(colour="grey55",face="bold"), #modify x axis title colour
    axis.title.y = element_text(colour="grey55", face="bold"), #modify y axis title colour
    panel.background = element_rect(fill = 'white',colour="grey77")) #modify background colour


## Section 4.5: Confidence region line plot


#filter the climate data set with data between 1950 and 1960
df_line3 = climate %>%
  filter(Source == "Berkeley" & Year >=1950 & Year <=1960) %>%
  select(Year, Anomaly10y, Unc10y)

#plot the confidence region of the Global temperature anamoly 
ggplot(df_line3, aes(x = Year, y = Anomaly10y)) +
  geom_ribbon(aes(ymin = Anomaly10y - Unc10y, ymax = Anomaly10y + Unc10y), alpha = 0.2) +
  geom_line(size=1)+
  annotate("text",x=1950,y=0.01,colour="black",label="AVG",hjust = 0) + #AVG
  annotate("text",x=1950,y=0.08,colour="grey55",label="MAX",hjust = 0)+  #MAX
  annotate("text",x=1950,y=-0.06,colour="grey55",label="MIN",hjust = 0)+  #MIN
  ggtitle("Global temperature anamoly from 1950-1960 ") + #Modify the title of the plot
  scale_x_continuous(name = "Year",limits=c(1950,1960),breaks=seq(1950,1960))+ #modify x axis
  scale_y_continuous(name = "Temperature anomaly (Celcius)",limits=c(-0.10,0.10))+ #modify y axis
  theme(
    axis.text.x = element_text(colour="grey55"), #modify x axis colour
    axis.text.y = element_text(colour="grey55"), #modify y axis colour
    axis.title.x = element_text(colour="grey55",face="bold"), #modify x axis title colour
    axis.title.y = element_text(colour="grey55", face="bold"), #modify y axis title colour
    panel.background = element_rect(fill = 'white',colour="grey55"), #modify background colour
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"))



###  Section 5: Slopegraph



#read CSV dataset
df_slope1=read.csv(file = 'Slopegraph_DB.csv',header=TRUE)


## Section 5.1: slopegraph 1 - all values as grey


ggplot(df_slope1) + 
  geom_segment(aes(x=2008,xend=2009,y=Year.2008,yend=Year.2009),size=.5)+ #plot slopegraphs
  #clean the background
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        panel.border=element_blank())+
  ggtitle("GDP in USD by country from 2008 to 2009") + #add title
  xlab("") + #define x title
  ylab("")+ #define y label
  ylim((0.9*(min(df_slope1$Year.2008,df_slope1$Year.2009))),(1.05*(max(df_slope1$Year.2008,df_slope1$Year.2009))))+ #define y limits
  xlim(2007.5,2009.5)+ #define x limits
  geom_text(label=df_slope1$Country,y=df_slope1$Year.2008,x=rep.int(2007.7,4),
            size=3,hjust=1,vjust=0,colour="grey50" )+ #plot country labels
  geom_text(label=comma_format()(round(df_slope1$Year.2008)), #plot 2008 values
            y=df_slope1$Year.2008,x=rep.int(2008,4),size=3,hjust=+1.2,vjust=0,colour="grey50")+
  geom_text(label=comma_format()(round(df_slope1$Year.2009)), #plot 2009 values
            y=df_slope1$Year.2009,x=rep.int(2009,4),size=3,hjust=-0.8,vjust=0,colour="grey50" )+
  #plot points in the slopegraphs
  geom_point(y=df_slope1$Year.2008,x=rep.int(2008,4),size=3,colour="grey50")+
  geom_point(y=df_slope1$Year.2009,x=rep.int(2009,4),size=3,colour="grey50")+
    #plot columns
  geom_text(label="2008",x=2008,y=0.9*(min(df_slope1$Year.2008,df_slope1$Year.2009)),colour="grey50")+
  geom_text(label="2009",x=2009,y=0.9*(min(df_slope1$Year.2008,df_slope1$Year.2009)),colour="grey50")
  

## Section 5.2: slopegraph 2 - focus on Australia


col_list=c("dark blue",rep("grey50",3))
ggplot(df_slope1) + 
  geom_segment(aes(x=2008,xend=2009,y=Year.2008,yend=Year.2009),size=.5)+ 
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        panel.border=element_blank())+
  ggtitle("GDP in USD by country from 2008 to 2009") +
  xlab("") + 
  ylab("")+
  #theme(axis.title.y=theme_text(vjust=3))+
  ylim((0.9*(min(df_slope1$Year.2008,df_slope1$Year.2009))),(1.05*(max(df_slope1$Year.2008,df_slope1$Year.2009))))+
  xlim(2007.5,2009.5)+
  geom_text(label=df_slope1$Country,y=df_slope1$Year.2008,x=rep.int(2007.7,4),
            size=3,hjust=1,vjust=0,colour=col_list )+
  geom_text(label=comma_format()(round(df_slope1$Year.2008)),
            y=df_slope1$Year.2008,x=rep.int(2008,4),size=3,hjust=+1.2,vjust=0,colour=col_list)+
  geom_text(label=comma_format()(round(df_slope1$Year.2009)),
            y=df_slope1$Year.2009,x=rep.int(2009,4),size=3,hjust=-0.8,vjust=0,colour=col_list )+
  geom_point(y=df_slope1$Year.2008,x=rep.int(2008,4),size=3,colour=col_list)+
  geom_point(y=df_slope1$Year.2009,x=rep.int(2009,4),size=3,colour=col_list)+
  geom_text(label="2008",x=2008,y=0.9*(min(df_slope1$Year.2008,df_slope1$Year.2009)),colour=col_list)+
  geom_text(label="2009",x=2009,y=0.9*(min(df_slope1$Year.2008,df_slope1$Year.2009)),colour=col_list)



###   Section 6: Bar charts



#create a dataset with Australia GDP in 2008 and 2009
df_bar1=line_sng %>%
  filter(Year %in% c(2008,2009))


## Section 6.1: bar chart 1 - without y limit 0
ggplot(df_bar1,aes(x=factor(Year),y=GDP))+
  geom_col(fill="orange")+ #colour: orange
  coord_cartesian(ylim=c(40000,50000))+ #y limit (40k,50k)
  geom_text(aes(label=comma_format()(round(GDP)),vjust=-0.2))+ #bar chart label
  #clean the background
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(colour="grey55"),
        plot.title = element_text(colour="grey55"),
        panel.border=element_blank())+
  ggtitle("Decrease on Australia's GDP in 2009")+  #add title
  xlab(NULL) + 
  ylab(NULL)
  

## Section 6.2: bar chart 2 - with y limit 0


ggplot(df_bar1,aes(x=factor(Year),y=GDP))+
  geom_col(fill="orange")+ #colour: orange
  coord_cartesian(ylim=c(0,50000))+ #y limit (40k,50k)
  geom_text(aes(label=comma_format()(round(GDP)),vjust=+1.5),colour="white")+ #bar chart label
  #clean the background
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(colour="grey55"),
        plot.title = element_text(colour="grey55"),
        panel.border=element_blank())+
  ggtitle("Decrease on Australia's GDP in 2009")+  #add title
  xlab("") + 
  ylab("")


## Section 6.3: bar widths too thin


ggplot(line_sng,aes(x=Year,y=GDP))+
  geom_col(width =0.3,fill="grey60")+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL)

## Section 6.4: bar widths too thick


ggplot(line_sng,aes(x=Year,y=GDP))+
  geom_col(width =0.9,fill="grey60")+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab("") + 
  ylab("")

## Section 6.5: bar widths just right


ggplot(line_sng,aes(x=Year,y=GDP))+
  geom_col(width =0.7,fill="grey60")+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab("") + 
  ylab("")


## Section 6.6: Simple bar graph


#create a dataframe to plot the bar chart
df_bar5=data.frame(values=c(0.34,0.31,0.26,0.09), 
                    supplier=c("Supplier A","Supplier B","Supplier C","Supplier D"))

ggplot(df_bar5,aes(x=supplier,y=values))+
  geom_col(fill="grey50")+
  geom_text(aes(label=sprintf("%1.0f%%", 100*values),vjust=0,hjust=1.4),colour="white")+
  labs(title="Supplier Market Share",
       caption ="Total 100%")+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_line(colour="grey70"),
        plot.caption = element_text(colour="grey70")
  )+
  scale_x_discrete(limits=rev(levels(df_bar5$supplier))) +
  scale_y_continuous(breaks=NULL) +
  xlab(NULL) + 
  ylab(NULL) + coord_flip() #swap the axis



## Section 7 Vertical bar charts



#create a dataset with 4 countries
df_bar2=countries %>%
  filter(Name %in% c("Australia","United States","Japan","New Zealand") 
         & Year >=2005 & Year <=2009)


## Section 7.1: plot the bar chart with 1 country


ggplot(line_sng,aes(x=Year,y=GDP))+
  geom_col(width =0.7,fill="steelblue3")+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab("") + 
  ylab("")  


## Section 7.2: plot the bar chart with 2 countries


ggplot(line_two, aes(x = Year, y = GDP, fill = Name)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("steelblue3","steelblue1"),guide=FALSE)+
  annotate("text",x=2005-0.25,y=38000,colour="steelblue3",label="AUS",size=2.5)+ 
  annotate("text",x=2005+0.20,y=29000,colour="steelblue1",label="NZL",size=2.5)+ 

  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL)


## Section 7.3: plot the bar chart with 4 countries


ggplot(df_bar2, aes(x = Year, y = GDP, fill = Name)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("steelblue3","steelblue1","grey55","grey70"),guide=FALSE)+
  annotate("text",x=2005-0.35,y=38000,colour="steelblue3",label="AUS",size=2)+ 
  annotate("text",x=2005-0.12,y=40000,colour="steelblue1",label="JPN",size=2)+ 
  annotate("text",x=2005+0.12,y=29000,colour="grey55",label="NZL",size=2)+ 
  annotate("text",x=2005+0.35,y=45000,colour="grey70",label="USA",size=2)+ 
  
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL)



## Section 8 Stacked bar charts



## Section 8.1: bottom stacked bar filled
ggplot(df_line1, aes(x = Year, y = GDP, fill = Name)) +
  geom_col(colour="white")+
  scale_fill_manual(values = c("grey70","grey70","steelblue3"),guide=FALSE)+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL)


## Section 8.2: second bottom stacked bar filled


ggplot(df_line1, aes(x = Year, y = GDP, fill = Name)) +
  geom_col(colour="white")+
  scale_fill_manual(values = c("grey70","steelblue3","grey70"),guide=FALSE)+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL)



### Section 9: Waterfall chart



#define the data frame for the waterfall chart
df_bar3=data.frame(
  x_val=c("Beginning HC","Hires","Transfers In","Transfers Out","Exits","Ending HC"),
  y_val=c(100,30,8,-12,-10,116)
  )

#modify the data frame to make it easier to plot the water fall chart
df_bar3$x_val=factor(df_bar3$x_val,levels = df_bar3$x_val) #make x_val categorial variable
df_bar3$id=seq_along(df_bar3$y_val) #create a column "id" to make a numerical x axis
df_bar3$type=ifelse(df_bar3$y_val>0, "in","out") #if you want to change the colour of the chart
df_bar3[df_bar3$x_val %in% c("Beginning HC","Ending HC"),
                "type"] = "net" #in -> increase, out-> decrease, net-> totals
df_bar3$end=cumsum(df_bar3$y_val) #calculate the end position
df_bar3$end=c(head(df_bar3$end,-1),0)
df_bar3$start=c(0,head(df_bar3$end,-1)) #calculate the start position

ggplot(df_bar3, aes(x_val, fill = type)) + 
  geom_rect(aes(x = x_val, xmin = id - 0.45, xmax = id + 0.45, 
                ymin = end,ymax = start), fill="steelblue3")+ #plotting the bar charts
  geom_text(label=df_bar3$y_val,y=c(100,130,138,138,126,116), #add text labels
            x=df_bar3$x_val,size=4,colour="white",vjust=1)+
  #add title and subtitle
  labs(title="2014 Headcount math", 
       subtitle = "Through more employees transferred out of the team than transferred in, 
aggresive hiring means overall headcount(HC) increased 16% over the course of the year")+
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=10),
        panel.background = element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL)



### Section 10: Horizontal bar charts



## Section 10.1 plot the bar chart with 1 country


ggplot(line_sng,aes(x=Year,y=GDP))+
  geom_col(width =0.7,fill="steelblue3")+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab("") + 
  ylab("") + #same as vertical charts 
  coord_flip() + #coord_flip swaps the axis
  scale_x_reverse() 
  

## Section 10.2 plot the bar chart with 2 countries


ggplot(line_two, aes(x = Year, y = GDP, fill = Name)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("steelblue3","steelblue1"),guide=FALSE)+
  annotate("text",x=2005+0.25,y=38000,colour="steelblue3",label="AUS",size=2.5)+ 
  annotate("text",x=2005-0.25,y=29000,colour="steelblue1",label="NZL",size=2.5)+ 
  
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL) + coord_flip() + scale_x_reverse() 


## Section 10.3 plot the bar chart with 4 countries


ggplot(df_bar2, aes(x = Year, y = GDP, fill = Name)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("steelblue3","steelblue1","grey55","grey70"),guide=FALSE)+
  annotate("text",x=2005+0.35,y=38000,colour="steelblue3",label="AUS",size=2)+ 
  annotate("text",x=2005+0.12,y=40000,colour="steelblue1",label="JPN",size=2)+ 
  annotate("text",x=2005-0.12,y=29000,colour="grey55",label="NZL",size=2)+ 
  annotate("text",x=2005-0.35,y=45000,colour="grey70",label="USA",size=2)+ 
  
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_text(colour="grey55"),
        panel.border=element_blank())+
  xlab(NULL) + 
  ylab(NULL) + coord_flip() + scale_x_reverse() 



## Section 11: Stacked horizontal bar charts



#read the dataset of the survey results
df_bar4 = read.csv(file = 'StackedH_DB.csv',header=TRUE)
#reorder the results 
df_bar4 = df_bar4 %>%
  mutate(Result = factor(Result,levels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")))


library(grid)

xdist=0.5
ydist=0.9
t1 = textGrob(expression(bold("Strongly Disagree | Disagree | ") * phantom(bold("Neutral | ")) * phantom(bold("Agree | Strongly Agree"))),
               x = xdist, y = ydist, gp = gpar(col = "grey40"))

t2 = textGrob(expression(phantom(bold("Strongly Disagree | Disagree | ")) * bold("Neutral | ") * phantom(bold("Agree | Strongly Agree"))),
               x = xdist, y = ydist, gp = gpar(col = "grey70"))

t3 = textGrob(expression(phantom(bold("Strongly Disagree | Disagree | ")) * phantom(bold("Neutral | ")) * bold("Agree | Strongly Agree")),
               x = xdist, y = ydist, gp = gpar(col = "dodgerblue4"))

#plot a stacked horizontal bar chart
ggplot(df_bar4,aes(x = Item , y = Category, fill = Result)) +
  geom_col(position=position_fill(reverse = TRUE),colour="white")+ 
  #modify bar colour
  scale_fill_manual(values = c("grey40","grey40","grey70","dodgerblue4","dodgerblue4")  ,guide=FALSE)+
  #modify the scale of y axis and define breaks
  scale_y_continuous(labels = scales::percent, breaks=seq(0,1,0.2)) +
  #change the order of the x axis
  scale_x_discrete(limits=rev(levels(df_bar4$Item)),expand=c(0, 1.5)) +
  #define the title and subtitle
  labs(title="Survey results",
       subtitle = "")+
  
  annotation_custom(grobTree(t1, t2, t3)) +

  theme(panel.background = element_blank(),
      panel.grid=element_blank(),
      axis.text.x = element_text(colour="grey55"),
      panel.border=element_blank(),
      axis.line.x = element_line(colour="grey55"),
      plot.subtitle = element_text(size=9,face="bold",hjust=0.5)
      )+
  xlab(NULL) + 
  ylab(NULL) + coord_flip() #swap the axis

  




### Section 12: Simple text



df_bar6=data.frame(year=factor(c(1970,2012)),values=c(41,20))

ggplot(df_bar6,aes(x=year,y=values))+
  geom_col(fill="turquoise4")+
  geom_text(aes(label=values,vjust=-0.5,hjust=1),colour="black")+
  labs(title="Children with a
'Traditional' Stay-at-Home Mother",
       subtitle = "% of children with a married
stay-at-home mother with a 
working husband",
       caption ="Note: Based on children younger than 18.
Their mothers are categorized based on 
employment status in 1970 and 2012.

Source: Pew Research Center analysis of
March Current Population Surveys
Integrated Public Use MIcrodata Series
(IPUMS-CPS), 1971 and 2013
Adapted from PEW RESEARCH CENTER")+

  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(colour="grey55",hjust=0),
        plot.caption = element_text(colour="grey55",hjust=0))+
  scale_y_continuous(breaks = NULL,limits=c(0,50))+
  scale_x_discrete(breaks = NULL)+
  
  xlab(NULL) + 
  ylab(NULL) 



#Section 12.1 plain text



#turquoise4


#define the text properties
df <- tibble(
  label = "<span style='font-size:60pt; color:turquoise4'>**20%**</span>
<br>of children had a
<br><span style='color:turquoise4'>traditional stay-at-home mom</span>
<br>in 2012, compared to 41% in 1970"
  ,
  x = 0,
  y = 0,
  hjust = 0,
  vjust = 0,
  colour = "grey55"
)

#plot the text 
ggplot(df) +
  aes(
    x, y, label = label, color = colour, fill = fill,
    hjust = hjust, vjust = vjust, size=10
  ) +
  geom_richtext(fill = NA, label.color = NA) +
  scale_color_identity() +
  scale_fill_identity() +
  xlim(0, 0.2) + ylim(0, 0.2)+
  theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.text = element_blank(),
        legend.position='none',
        axis.ticks = element_blank())+
  xlab(NULL) + 
  ylab(NULL) 


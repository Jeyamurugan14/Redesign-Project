### IMPORTING DATA AND LIBRARIES
library(ggplot2)
library(ggforce)
library(grid)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(reshape)
library("ggmap")
library(maptools)
library(maps)
library(devtools)
library(rworldmap)
library(RColorBrewer)
library(viridisLite)
DS <- read.csv(file = "Data.csv")
train <- read_csv("Dataset.csv")
home <- read_csv("DATA1.csv")
mydata2 <- read.csv(file="data2.csv", header=TRUE, sep=",")
mydata <- read.csv(file="data.csv", header=TRUE, sep=",")



### GLOBAL DATA

overallyearlymedians <- DS %>%
  group_by(Question_Year) %>%
  summarize(medianGRI = median(GRI), medianSHI = median(SHI))
melt <- melt(as.data.frame(overallyearlymedians),id="Question_Year")



### GLOBAL PLOTS
#Scatter for GRI and SHI combined


ggplot(melt,aes(x = Question_Year,y = value, color=factor(variable,labels=c("Median GRI","Median SHI"))))+ 
geom_line(size=1.2) +
ylim(1,3) +
  geom_label(data = melt, aes(label = value),
             nudge_y = 0, label.padding = unit(0.25, "lines"),
             label.r = unit(1, "mm"),show.legend = FALSE) +
  scale_x_discrete(name="Question Year",overallyearlymedians$Question_Year,
                   breaks = overallyearlymedians$Question_Year,
                   labels = (c("2007","'08","'09","'10","'11","'12","'13","'14","'15","'16")))+
  labs(title="Global Government Restrictions and Social Hostility Index median scores",
       subtitle="(GRI using 10-point scale based on 20 indicators)
(SHI using 10-point scale based on 13 indicators)",
       x="Question Year",
       y="Median Index Value",
       caption="Pew Research Center") +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  guides(color=guide_legend(title="Measure"))+
  scale_color_manual(values=c("Tomato2","blue"))



### Average SHI and GRI per country visualization sorted <3 <3 <3



cm1 <- DS %>%
  group_by(Ctry_EditorialName) %>%
  summarize(avgGRI = mean(GRI), avgSHI = mean(SHI)) %>%
  arrange(avgGRI,-avgSHI) %>%
  mutate(Ctry_EditorialName = factor(Ctry_EditorialName, unique(Ctry_EditorialName)))
cm1 <- cm1[,-3]
cm1 <- as.data.frame(cm1)
cm2 <- DS %>%
  group_by(Ctry_EditorialName) %>%
  summarize(avgGRI = mean(GRI), avgSHI = mean(SHI)) %>%
  arrange(avgSHI,-avgGRI) %>%
  mutate(Ctry_EditorialName = factor(Ctry_EditorialName, unique(Ctry_EditorialName)))
cm2 <- cm2[,-2]
cm2 <- as.data.frame(cm2)
meltedcountry <- melt(cbind(cm1,cm2),id.vars="Ctry_EditorialName")




#Average GRI and SHI per country



ggplot(meltedcountry, aes(x = value, y = Ctry_EditorialName,
                          color=factor(variable,labels=c("Median GRI","Median SHI"))))+
  geom_segment(aes(yend = Ctry_EditorialName), xend = 0, colour="grey50") +
  geom_point(size=3) +
  scale_colour_brewer(palette = "Set1", limits=c("avgGRI","avgSHI"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(variable ~ .,scales="free_y", space="free_y")+
  labs(title="Global Government Restrictions and Social Hostility Index Average scores per Country",
       subtitle="(GRI using 10-point scale based on 20 indicators)
(SHI using 10-point scale based on 13 indicators)",
       x="Average Index Value",
       y="Country",
       caption="Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  guides(color=guide_legend(title="Measure"))+
  scale_color_manual(values=c("Tomato2","blue"))+  
  ggsave("myplot.pdf",width=25,height=130,units="cm",scale=TRUE,limitsize=FALSE)



### REGIONAL DATA


regions <- c("Americas", "Europe","Sub-Saharan Africa","Asia-Pacific", "Middle East-North Africa")
regiondf <- data.frame()
regiondf2 <- data.frame()
for (i in regions){
    regiondf <- DS %>%
    group_by(Question_Year, Region5) %>%
      filter(Region5 %in% i) %>%
    summarize(medianGRI = median(GRI), medianSHI = median(SHI))
    regiondf2 <- bind_rows(regiondf2,regiondf)
}

Americas <- melt(regiondf2[1:10,],id=c("Question_Year","Region5"))
Europe <- melt(regiondf2[11:20,],id=c("Question_Year","Region5"))
SubSaharanAfrica <- melt(regiondf2[21:30,],id=c("Question_Year","Region5"))
AsiaPacific <- melt(regiondf2[31:40,],id=c("Question_Year","Region5"))
MiddleEastNorthAfrica <- melt(regiondf2[41:50,],id=c("Question_Year","Region5"))


#ALL PLOTS 
#Scatter for GRI and SHI for each region


p1 <- ggplot(Americas, aes(x = Question_Year,y = value,color = factor(variable,labels=c("Median GRI","Median SHI"))))+
  geom_line(size=1)+
  geom_label(data = Americas, aes(label = value),
             nudge_y = 0, label.padding = unit(0.10, "lines"),
             label.r = unit(1, "mm"),show.legend = FALSE) +
  scale_x_discrete(name="Question Year",overallyearlymedians$Question_Year,
                   breaks = overallyearlymedians$Question_Year,
                   labels = (c("2007","'08","'09","'10","'11","'12","'13","'14","'15","'16")))+
  labs(title="Americas",
       x="Question Year",
       y="Median Index Value",
       caption="Pew Research Center") +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(color=guide_legend(title="Measure"))+
  scale_color_manual(values=c("Tomato2","blue"))

p2 <- ggplot(Europe, aes(x = Question_Year,y = value,color = factor(variable,labels=c("Median GRI","Median SHI"))))+
  geom_line(size=1)+
  geom_label(data = Europe, aes(label = value),
             nudge_y = 0, label.padding = unit(0.10, "lines"),
             label.r = unit(1, "mm"),show.legend = FALSE) +
  scale_x_discrete(name="Question Year",overallyearlymedians$Question_Year,
                   breaks = overallyearlymedians$Question_Year,
                   labels = (c("2007","'08","'09","'10","'11","'12","'13","'14","'15","'16")))+
  labs(title="Europe",
       x="Question Year",
       y="Median Index Value",
       caption="Pew Research Center") +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(color=guide_legend(title="Measure"))+
  scale_color_manual(values=c("Tomato2","blue"))

p3 <- ggplot(SubSaharanAfrica, aes(x = Question_Year,y = value,color = factor(variable,labels=c("Median GRI","Median SHI"))))+
  geom_line(size=1)+
  geom_label(data = SubSaharanAfrica, aes(label = value),
             nudge_y = 0, label.padding = unit(0.10, "lines"),
             label.r = unit(1, "mm"),show.legend = FALSE) +
  scale_x_discrete(name="Question Year",overallyearlymedians$Question_Year,
                   breaks = overallyearlymedians$Question_Year,
                   labels = (c("2007","'08","'09","'10","'11","'12","'13","'14","'15","'16")))+
  labs(title="Sub-Saharan Africa",
       x="Question Year",
       y="Median Index Value",
       caption="Pew Research Center") +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(color=guide_legend(title="Measure"))+
  scale_color_manual(values=c("Tomato2","blue"))

p4 <- ggplot(AsiaPacific, aes(x = Question_Year,y = value,color = factor(variable,labels=c("Median GRI","Median SHI"))))+
  geom_line(size=1)+
  geom_label(data = AsiaPacific, aes(label = value),
             nudge_y = 0, label.padding = unit(0.10, "lines"),
             label.r = unit(1, "mm"),show.legend = FALSE) +
  scale_x_discrete(name="Question Year",overallyearlymedians$Question_Year,
                   breaks = overallyearlymedians$Question_Year,
                   labels = (c("2007","'08","'09","'10","'11","'12","'13","'14","'15","'16")))+
  labs(title="Asia Pacific",
       x="Question Year",
       y="Median Index Value",
       caption="Pew Research Center") +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(color=guide_legend(title="Measure"))+
  scale_color_manual(values=c("Tomato2","blue"))

p5 <- ggplot(MiddleEastNorthAfrica, aes(x = Question_Year,y = value,color = factor(variable,labels=c("Median GRI","Median SHI"))))+
  geom_line(size=1)+
  geom_label(data = MiddleEastNorthAfrica, aes(label = value),
             nudge_y = 0, label.padding = unit(0.10, "lines"),
             label.r = unit(1, "mm"),show.legend = FALSE) +
  scale_x_discrete(name="Question Year",overallyearlymedians$Question_Year,
                   breaks = overallyearlymedians$Question_Year,
                   labels = (c("2007","'08","'09","'10","'11","'12","'13","'14","'15","'16")))+
  labs(title="Middle East and North Africa",
       x="Question Year",
       y="Median Index Value",
       caption="Pew Research Center") +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(color=guide_legend(title="Measure"))+
  scale_color_manual(values=c("Tomato2","blue"))

grid.arrange(p1,p2,p3,p4,p5,nrow = 3,ncol = 2, 
             top = textGrob(
              "Global Government Restrictions and Social Hostility Index median scores per Region",
              gp=gpar(fontsize=15, fontface="bold")))


## GRI and SHI summary
## Percentages are hard to interpret,so plotting bar plots with number


as.factor(DS$Question_Year)
Summary_GRI<- DS %>%
  filter(GRI>=7) %>%
  group_by(Question_Year)%>%
  summarize(countries= length(unique(Ctry_EditorialName)))
Summary_SHI<- DS %>%
  filter(SHI>=6) %>%
  group_by(Question_Year)%>%
  summarize(countries= length(unique(Ctry_EditorialName)))


GRISHI <- merge(Summary_GRI,Summary_SHI,by='Question_Year')
names(GRISHI)<-c('Year','GRI','SHI')

x <- GRISHI %>%
  gather('Measure','Number of countries',c('GRI','SHI'))

totalc<-length(unique(DS$Ctry_EditorialName))
x$Percentage<-x$`Number of countries`/totalc
x$Percentage<-round((x$Percentage*100),0)

total<-ggplot(x,aes(x=Year,y=x$Percentage,group=Measure))+
  geom_col(width = 0.6,
           position = 'dodge2',
           aes(fill=Measure))+
  geom_text(aes(label=paste0(Percentage,'% (',`Number of countries`,' countries)')),
             inherit.aes=TRUE,
             position = position_dodge(width = 0.6), 
             hjust = -.04,
             stat='identity') +
  labs(title="Number of countries with high level of Govt Restrictions and Overall Hostilities",
       subtitle="Countries above 6.0 SHI and 7.0 GRI",
       y="Percentage of 198 total countries",
       x="Year",
caption="Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_text(face="bold", size=12),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("Tomato2","turquoise"))+
  scale_x_discrete(name="Question Year",overallyearlymedians$Question_Year,
                   breaks = overallyearlymedians$Question_Year,
                   labels = (c("2007","'08","'09","'10","'11","'12","'13","'14","'15","'16")))+
  scale_y_continuous(limits = c(0, 20),expand = c(0,0))+
  coord_flip()
total


#Bar+line plot for yearly regional harrasments per religion



g1 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2007), y=mydata2$X2007,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2007, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2007", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g2 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2008), y=mydata2$X2008,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2008, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2008", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g3 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2009), y=mydata2$X2009,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2009, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2009", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g4 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2010), y=mydata2$X2010,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2010, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2010", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g5 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2011), y=mydata2$X2011,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2011, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2011", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g6 <- ggplot(mydata2,aes(x=reorder(mydata2$Religion, -mydata2$X2012), y=mydata2$X2012,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2012, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2012", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g7 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2013), y=mydata2$X2013,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2013, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2013", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g8 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2014), y=mydata2$X2014,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2014, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2014", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g9 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2015), y=mydata2$X2015,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2015, vjust=-0.6))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2015", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 155),expand = c(0,0))

g10 <- ggplot(mydata2, aes(x=reorder(mydata2$Religion, -mydata2$X2016), y=mydata2$X2016,group=1)) +
  geom_point()+geom_line(size=1)+geom_text(aes(label=mydata2$X2016, vjust=-0.5))+
  labs(y="Number of incidents", 
       x="Religion", 
       title="Government harrasments in 2016", 
       caption = "Source: Pew Research Center")+
  theme(axis.text.x = element_text(face="bold", size = 8),
        title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(face="bold", size = 12)) +
  geom_bar(width = 0.5, fill="tomato2",stat="identity")+
  scale_y_continuous(limits = c(0, 158),expand = c(0,0))
  
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,nrow = 5)



#Targeted religious groups by politicians or political parties from list to bar chart



ggplot(train, aes(x=train$`Targeted Religious Group`, fill =Region))+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_text(face="bold", size=12),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  geom_bar()+
  labs(x="Religion",
       y="Number of country", 
       title= "Muslims in Europe were primary target of nationalist political parties or politicians",
       caption = "Source: Pew Research Center")+
  scale_fill_manual(values=c("yellow","turquoise","Tomato2"))

#Targeted religious groups by politicians or political parties from list to bar chart
ggplot(train, aes(x=train$`Targeted Religious Group`, y = train$Country,shape =Region, color =Region))+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_text(face="bold", size=12),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  geom_point(size=3)+
  labs(x="Religion",
       y="Country", 
       title= "Muslims in Europe were primary target of nationalist political parties or politicians",
       caption = "Source: Pew Research Center")+
  scale_fill_manual(values=c("yellow","turquoise","Tomato2"))


#Targeted religious groups by nationalists in Europe from list to bar chart


ggplot(home, aes(x=home$'Targeted Religious Group', fill =Region))+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_text(face="bold", size=12),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  geom_bar()+
  labs(x="Religion",
       y="Number of country",
       title= "Religious groups targeted by nationalists in Europe",
       caption = "Source: Pew Research Center")+
  scale_fill_manual(values=c("yellow","turquoise","Tomato2"))



#Targeted religious groups by nationalists in Europe from list to bar chart


ggplot(home, aes(x=home$`Targeted Religious Group`, y = home$Country,
                 shape =Region, color =Region))+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_text(face="bold", size=11),
        title = element_text(face = "bold.italic", color = "Black"),
        axis.title = element_text(face = "bold.italic", color = "Black"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  geom_point(size=3)+
  labs(x="Religion",
       y="Country", 
       title= "Religious groups targeted by nationalists in Europe",
       caption = "Source: Pew Research Center")
  scale_fill_manual(values=c("yellow","turquoise","Tomato2"))

  
  
###HEATMAP
  
  
mapped_data <- joinCountryData2Map(DS, joinCode = "NAME",
                                   nameJoinColumn = "Ctry_EditorialName")
mapdatanew_name <- "Government restrictions on religion around the world"
mapdatanew_Year <- 2016
op <- palette(c('yellow','pink','orange','red'))
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapped_data[["GRI"]] <- cut(mapped_data[["GRI"]]
                            , cutVector
                            , include.lowest=TRUE )
levels(mapped_data[["GRI"]]) <- c('low','medium','high','very high')
cutVector <- quantile(mapped_data[["GRI"]],na.rm = TRUE)
mapdatanew <- mapCountryData (mapped_data, nameColumnToPlot = "GRI",
                              mapTitle="Government restrictions on religion around the world",
                              missingCountryCol = "White",
                              catMethod='categorical',
                              oceanCol = 'lightblue',
                              #mapRegion = 'Asia',
                              colourPalette='palette',
                              addLegend=FALSE
)
do.call(addMapLegendBoxes,
        c(mapdatanew
          ,x='bottomleft'
          ,cex=0.8
          ,ncol=3
          ,title="Density"))

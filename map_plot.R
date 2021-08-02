library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("dplyr")
library("mapproj")
library("fiftystater")



world <- ne_countries(scale = "medium",type = 'map_units', returnclass = "sf")
class(world)
Dataset<-read.csv("E:/code_lastest/Dataset/Gather_dataset_week_lastest0622_max3_1.csv",stringsAsFactors = F)
strata<-unique(Dataset[,c("CountryCode","Group")])
class(strata)
colnames(strata)<-c("adm0_a3","Group")
world<-full_join(world,strata,by="adm0_a3")
world$Group<-as.factor(world$Group)
world<-world[-which(world$adm0_a3=="ATA"),]
p1<-ggplot() +geom_sf(data = world,aes(fill=Group), inherit.aes = F)+  
  scale_fill_manual(values = c("#FFBF03","#bcd200","#99bbde","#c4bbe9","#f5f5f5"))+ 
  coord_sf(expand = FALSE,crs=4326) +
  scale_y_continuous(breaks = seq(-90, 90, 30))+
  scale_x_continuous(breaks = c(-180,-90,0,90,180))+
  theme_bw()+
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size = 9,family="Times New Roman"),
        legend.background = element_rect(fill="transparent",color=NA),
        legend.key=element_rect(fill=NA),
        plot.title = element_text(color="black",hjust = 0,vjust=0, size=9,family="Times New Roman"),
        axis.ticks= element_line(color="black"),
        axis.text = element_text(color="black",vjust=0,hjust=0, size=9,family="Times New Roman"),
        panel.background=element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(size=0.05),
        axis.title = element_text(color="black", size=9,family="Times New Roman"))
ggsave(p1, filename = "E:/code_lastest/picture/FIG. 1/worldgroup.pdf",width=140,heigh=90,unit="mm",
       bg = "transparent",device = cairo_pdf)
#WorldData<-read.csv("E:/code_lastest/Dataset/Dataset_0622NPIs.csv",stringsAsFactors = F)

d <- Dataset[, c(4,3,2,7,23,25,17,31)]
d$New_cases_pop<-d$New_cases_smoothed/d$Pop*100
#d2 <- d %>% group_by(Week, Wave, Group) %>% summarize_all(funs(sum))
#d2 <- d[,-8] %>% group_by(Week,Wave, Group, Country) %>% summarize_all(funs(sum))

## a start time of each wave
starttime <- Dataset[, c(1:4,7)]
starttime <- starttime[which(starttime$Week == 1),]
starttime$Start <- as.Date(starttime$Start )
#meanst <- starttime %>% group_by(Wave, Group) %>% summarise_all(funs(first))
meanst <- starttime %>% group_by(Wave, Group,Country) %>% summarise_all(funs(first))
#Y1<- d[, c(1,3,4,8)]
#yt <- Y1 %>% group_by(Wave, Group,Country) %>% summarise_all(funs(mean))
d3 <- merge(d, meanst[, 1:4], by = c("Wave", "Group","Country"))
#d3 <- merge(d2, yt, by = c("Wave", "Group","Country"))
#d3 <- merge(d2, meanst[, 1:3], by = c("Wave", "Group"))
d3$date <- d3$Start
k <- which(d3$Week != 1)
d3$date[k] <- d3$Start[k] + (d3$Week[k] - 1) * 7 

d5 <- d3[order(d3$Wave, d3$Group, d3$date), ] 
d6 <- d5[!duplicated(d5[,c(1:2)]),]
d5$Group<-as.factor(d5$Group)
d5$Wave<-as.factor(d5$Wave)

#y1<-d5[,c("date","Group","Wave","Y1")]
#d7<-y1 %>% group_by(date,Group,Wave) %>% summarise_all(funs(mean))
y1<-d5[,c("date","Group","Wave","New_cases_pop")]
d7<-y1 %>% group_by(date,Group,Wave) %>% summarise_all(funs(mean))
d6 <- d7[!duplicated(d7[,c(2:3)]),]
p3<-ggplot(d5)+geom_step(aes(x=date,y=New_cases_pop,fill=Country),size=0.05,alpha=0.2,color="#e1eaef")+
  theme(legend.position ="none")+
  #geom_smooth(method="loess",se=F,size=1,span=0.4)+ 
  #geom_line()+
  scale_x_continuous(breaks = seq(min(d5$date),max(d5$date),120))+
  scale_y_sqrt(name ="Ajusted new cases (pre hundred people/week)",
               breaks = c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),
               limits = c(0, 1))+
  geom_smooth(data=d7,aes(x=date,y=New_cases_pop,color=Group),se=F,method="loess",size=0.6,span=0.1)+
  #geom_segment(data=d6,aes(x=date,y=New_cases_pop,xend=date, yend = New_cases_pop*2))
  #geom_vline(d6, mapping = aes(xintercept =  date,color=Group,linetype=Wave),size=0.6)+
  geom_point(d6, mapping = aes(x =  date,y=New_cases_pop*2,color=Group,shape=Wave),size=1)+
  theme_bw()+
  scale_color_manual(values = c("#e6ab00","#93ac00","#7daad8","#c1b5ef","#f5f5f5"))+
  scale_linetype_manual(values=c("1"=6,"2"=1,"3"=62))+ labs(x=NULL)+
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size = 9,family="Times New Roman"),
        legend.background = element_rect(fill="transparent",color=NA),
        legend.key=element_rect(fill=NA),
        plot.title = element_text(color="black",size=9,family="Times New Roman"),
        axis.ticks= element_line(color="black"),
        axis.text = element_text(color="black", size=9,family="Times New Roman"),
        panel.background=element_rect(fill = "transparent",colour = NA),
        panel.grid = element_blank(),
        axis.line = element_line(size=0.05),
        axis.title = element_text(color="black", size=9,family="Times New Roman"))
ggsave(p3, filename = "E:/code_lastest/picture/FIG. 1/worldgroup_newcases2.pdf",width=140,heigh=90,unit="mm",
       bg = "transparent",device = cairo_pdf)

# US
us_data <- read.csv("E:/code_lastest/Dataset/US_dataset/US_V2/US_Gather_dataset_week_lastest_max3_withG.csv")
us_group <-unique(us_data[,c("State","StateID","Group")])
us_group$Group <-factor(us_group$Group)
colnames(us_group)<-c("state","StateID","Group")
#state <- map_data("state")
us_group$state<-tolower(us_group$state)
state <- data.frame(state = tolower(rownames(USArrests)), USArrests)
us<-full_join(us_group,state ,by="state")
p2<-ggplot(us, aes(map_id = state,fill=Group)) + 
  geom_map(map = fifty_states,color="grey40") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+ scale_fill_manual(values = c("#65272f","#ffc21a","#c75c48"))+
  theme_bw()+labs(x=NULL,y=NULL)+
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size = 9,family="Times New Roman"),
        legend.background = element_rect(fill="transparent",color=NA),
        legend.key=element_rect(fill=NA),
        plot.title = element_text(color="black",hjust = 0,vjust=0, size=9,family="Times New Roman"),
        axis.ticks= element_line(color="black"),
        axis.text = element_text(color="black",vjust=0,hjust=0, size=9,family="Times New Roman"),
        panel.background=element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(size=0.05),
        axis.title = element_text(color="black", size=9,family="Times New Roman"))
ggsave(p2, filename = "E:/code_lastest/picture/FIG. 1/USgroup.pdf",width=140,heigh=90,unit="mm",
       bg = "transparent",device = cairo_pdf)

d <- us_data[, c(2,3,4,15,21,23,28,31)]
d$New_cases_pop<-d$New_cases_smoothed/(d$Pop*10000)
#d2 <- d %>% group_by(Week, Wave, Group) %>% summarize_all(funs(sum))
#d2 <- d[,-8] %>% group_by(Week,Wave, Group, Country) %>% summarize_all(funs(sum))

## a start time of each wave
starttime <- us_data[, c(1:4,31)]
starttime <- starttime[which(starttime$Week == 1),]
starttime$Start <- as.Date(starttime$Start)
#meanst <- starttime %>% group_by(Wave, Group) %>% summarise_all(funs(first))
meanst <- starttime %>% group_by(Wave, Group,State) %>% summarise_all(funs(first))
#Y1<- d[, c(1,3,4,8)]
#yt <- Y1 %>% group_by(Wave, Group,Country) %>% summarise_all(funs(mean))
d3 <- merge(d, meanst[, 1:4], by = c("Wave", "Group","State"))
#d3 <- merge(d2, yt, by = c("Wave", "Group","Country"))
#d3 <- merge(d2, meanst[, 1:3], by = c("Wave", "Group"))
d3$date <- d3$Start
k <- which(d3$Week != 1)
d3$date[k] <- d3$Start[k] + (d3$Week[k] - 1) * 7 

d5 <- d3[order(d3$Wave, d3$Group, d3$date), ] 
d6 <- d5[!duplicated(d5[,c(1:2)]),]
d5$Group<-as.factor(d5$Group)
d5$Wave<-as.factor(d5$Wave)

#y1<-d5[,c("date","Group","Wave","Y1")]
#d7<-y1 %>% group_by(date,Group,Wave) %>% summarise_all(funs(mean))
y1<-d5[,c("date","Group","Wave","New_cases_pop")]
d7<-y1 %>% group_by(date,Group,Wave) %>% summarise_all(funs(mean))
d6 <- d7[!duplicated(d7[,c(2:3)]),]
p5<-ggplot(d5)+geom_step(aes(x=date,y=New_cases_pop,fill=State),size=0.05,alpha=0.2,color="#e1eaef")+
  theme(legend.position ="none")+
  #geom_smooth(method="loess",se=F,size=1,span=0.4)+ 
  #geom_line()+
  scale_x_continuous(breaks = seq(min(d5$date),max(d5$date),120))+
  scale_y_sqrt(name ="Ajusted new cases (pre hundred people/week)",
               breaks = c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1.2),
               limits = c(0, 1.2))+
  geom_smooth(data=d7,aes(x=date,y=New_cases_pop,color=Group),se=F,method="loess",size=0.6,span=0.1)+
  #geom_segment(data=d6,aes(x=date,y=New_cases_pop,xend=date, yend = New_cases_pop*2))
  #geom_vline(d6, mapping = aes(xintercept =  date,color=Group,linetype=Wave),size=0.6)+
  geom_point(d6, mapping = aes(x =  date,y=New_cases_pop*2,color=Group,shape=Wave),size=1)+
  theme_bw()+
  scale_color_manual(values = c("#65272f","#ffc21a","#c75c48"))+ labs(x=NULL)+
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size = 9,family="Times New Roman"),
        legend.background = element_rect(fill="transparent",color=NA),
        legend.key=element_rect(fill=NA),
        plot.title = element_text(color="black",size=9,family="Times New Roman"),
        axis.ticks= element_line(color="black"),
        axis.text = element_text(color="black", size=9,family="Times New Roman"),
        panel.background=element_rect(fill = "transparent",colour = NA),
        panel.grid = element_blank(),
        axis.line = element_line(size=0.05),
        axis.title = element_text(color="black", size=9,family="Times New Roman"))
ggsave(p5, filename = "E:/code_lastest/picture/FIG. 1/USgroup_newcases.pdf",width=140,heigh=90,unit="mm",
       bg = "transparent",device = cairo_pdf)


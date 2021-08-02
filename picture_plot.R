theme_set(theme_minimal(base_size=10, base_family="Times New Roman"))
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
#######figure 2##########
fig2data<-function(fig2data_list,policy_variable,gather,inputpath){
  fig1_list<-list()
  num<-c("All waves","Wave 1","Wave 2","Wave 3")
  #num = c("Group 1","Group 2","Group 3","Group 4" )
  for(m in 1:length(fig1data_list)){
    #fitall<-readRDS(paste0("wave12-10result/waveresult/",fig1data_list[m]))
    fitall<-readRDS(paste0(inputpath,fig1data_list[m]))
    out<-rstan::extract(fitall)
    #gatherdata<-subset(gather,gather$Group==m)
    if(m>1){gatherdata<-subset(gather,gather$Wave==m-1)}else{gatherdata<-gather}
    X<-colMeans(gatherdata[,c("School.closing", "Workplace.closing","Close.Public.transport",
                              "International.travel.controls", "Facial.Coverings",
                               "Restrict.Movement","Restrict.Gathering")])
    if(length(policy_variable)>7){
        CCN<-unique(gatherdata$Country)
        maxD<-lapply(seq(1:length(CCN)),FUN = function(v){
          V1<-subset(gatherdata,gatherdata$Country==CCN[v])
          max(V1$recovered)})
        V<-colMeans(do.call(rbind,maxD))
        X<-rbind(as.data.frame(X),V)
        X<-as.numeric(X$X)}
    alpha<-as.matrix(out$alpha)
    alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
    alpha<-do.call(cbind,alphalist)
    allNPI<-apply(alpha,1,sum)
    list<-list(alpha,allNPI)
    alpha<-do.call(cbind,list)
    alpha<-(1-exp(-alpha))*100
    colnames(alpha)<-c(policy_variable,"All")
    data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
    #write.csv(data,paste0(outputpath,num[m],".csv"),row.names = F)
    data$wave<-num[m]
    fig1_list[[m]]<-data}
  data<-do.call(rbind,fig1_list)
  return(data)
}

fig2<-function(gather,policy_variable,wavefitlist,inputpath="result/wave/"){
  M1<-do.call(rbind,lapply(split(gather,gather$Wave),FUN = function(g1){
    X<-as.data.frame(t(colMeans(g1[,c("School.closing", "Workplace.closing",
                                      "Close.Public.transport",
                                      "International.travel.controls", 
                                      "Facial.Coverings", "Restrict.Movement",
                                      "Restrict.Gathering")])))
    colnames(X)<-policy_variable
    X$wave<-paste("Wave",unique(g1$Wave))
    return(X)}))
  NPImean<-as.data.frame(t(colMeans(gather[,c("School.closing", "Workplace.closing",
                                              "Close.Public.transport",
                                              "International.travel.controls", 
                                              "Facial.Coverings", "Restrict.Movement",
                                              "Restrict.Gathering")])))
  colnames( NPImean)<-policy_variable
  NPImean$wave<-"All waves"
  mean<-do.call(rbind,list(NPImean,M1))
  mean<-melt(mean,"wave")
  colnames(mean)<-c("Wave","parameter","strength")
  Result<-fig2data(wavefitlist,policy_variable,gather,inputpath)
  #Result<-read.csv("result/wave/Overall.csv")
  R<-subset(Result,Result$parameter!="All waves")
  data2<-merge(mean, R, by = c("Wave","parameter"))
  fig<-ggplot(data2)+  geom_col(mapping = aes_(x=~strength*100,y=~parameter,fill=~Wave),position=position_dodge(width = 0.9),alpha=0.4,color="white")+
    geom_linerange(mapping = aes_(xmin =~ll, xmax=~hh, y=~parameter,color= ~Wave),show.legend = F,alpha=0.4,
                   size = 1, position=position_dodge(width = 0.9))+
    geom_hline(show.legend = T,aes(yintercept = -1, color=Wave),size = 1,alpha=0.4)+
    geom_linerange(mapping = aes_(xmin =~l, xmax=~h, y=~parameter,color=~Wave),show.legend = F,
                   size =1.8,position=position_dodge(width = 0.9))+
    geom_point(mapping = aes_(x = ~m, y = ~parameter, color = ~Wave),show.legend = T,alpha=0.7,
               size =2.5,position= position_dodge(width = 0.9))+
    labs(x = paste0("\u0394","\u03C9","t","(%)"),y = NULL,title =NULL)+theme_bw()+
    scale_x_continuous(expand=c(0,0),limits=range(c(0:100)))+
    scale_color_manual(name=NULL,values =c("#4798b3","#8d5b51","#697ea8","#b6a681"))+
    scale_fill_manual(name=NULL,values =c("#dde6e6","#dde6e6","#dde6e6","#dde6e6"))+
    theme(legend.position = "top",
          legend.title=element_blank(),
          legend.key.height=unit(0.2,'cm'),
          legend.key.width=unit(1.5,'cm'),
          legend.text = element_text(size = 9,family="Times New Roman"),
          legend.background = element_rect(fill="transparent",color=NA),
          legend.key=element_rect(fill=NA),
          plot.title = element_text(color="black",hjust = 0,vjust=0, size=9,family="Times New Roman"),
          axis.ticks.y= element_line(color="black"),
          axis.text.y = element_text(color="black",vjust=0.5,hjust=1, size=9,family="Times New Roman"),
          panel.grid=element_blank(),
          plot.margin=unit(c(0.01,0.2,0.01,0.2),"cm"),
          panel.background=element_rect(fill = "transparent",colour = NA),
          axis.line = element_line(size=0.05),
          axis.title = element_text(color="black", size=9,family="Times New Roman"),
          axis.text.x = element_text(color="black", size=9,family="Times New Roman"))
  ggsave(fig, filename = "picture/fig1_new.pdf",width=160,heigh=140,unit="mm",
         bg = "transparent",device = cairo_pdf)
}
############figure 3##############
fig3<-function(gather,policy_variable,
               outputpath="result/all/",
               inputpath="result/all/")
{
  g1<-list()
  for(i in 1:4){
    data_list<-list()
    for(j in 1:3){
      all<-readRDS(paste0(inputpath,"G",i,"-W",j,"all.rds"))
      out<-rstan::extract(all)
      gatherdata<-subset(gather,gather$Group==i)
      gatherdata<-subset(gatherdata,gatherdata$Wave==j)
      X<-colMeans(gatherdata[,c("School.closing", "Workplace.closing","Close.Public.transport", "International.travel.controls", "Facial.Coverings","Restrict.Movement","Restrict.Gathering")])
      if(length(policy_variable)>7){
        CCN<-unique(gatherdata$Country)
        maxD<-lapply(seq(1:length(CCN)),FUN = function(v){
          V1<-subset(gatherdata,gatherdata$Country==CCN[v])
          max(V1$recovered)})
        V<-colMeans(do.call(rbind,maxD))
        X<-rbind(as.data.frame(X),V)
        X<-as.numeric(X$X)}
      alpha<-as.matrix(out$alpha)
      alphalist<-lapply(seq(1:ncol(alpha)),FUN = function(k){alpha[,k]*X[k]})
      alpha<-do.call(cbind,alphalist)
      allNPI<-apply(alpha,1,sum)
      list<-list(alpha,allNPI)
      alpha<-do.call(cbind,list)
      alpha<-(1-exp(-alpha))*100
      colnames(alpha)<-c(policy_variable,"All NPIs")
      data <- mcmc_intervals_data(alpha,prob = .5,prob_outer= .95,point_est="median")
      #write.csv(data,paste0(outputpath,"W",j,"-G",i,".csv"),row.names = F)
      data$wave<-paste0("Wave ",j)
      data_list[[j]]<-data }
    data<-do.call(rbind,data_list)
    data$wave<-factor(data$wave,levels=c("Wave 3","Wave 2","Wave 1"))
    g1[[i]]<-ggplot()+
      geom_linerange(mapping = aes_(xmin =~ll, xmax=~hh, y=~parameter,color= ~wave),show.legend = F,
                     data=data, size = 1,alpha=0.4, position=position_dodge(width = 0.8))+
      geom_hline(show.legend = T,aes(yintercept = -1, color=wave),data=data,alpha=0.4,size = 1)+
      geom_linerange(mapping = aes_(xmin =~l, xmax=~h, y=~parameter,color= ~wave),show.legend = F,
                     data=data, size = 1.8,alpha=1, position=position_dodge(width = 0.8))+
      geom_point(mapping = aes_(x = ~m, y = ~parameter, color = ~wave),
                 data = data, size = 2.5,alpha=0.7,position=position_dodge(width = 0.8))+
      scale_color_manual(name=NULL,values =c("#8d5b51","#697ea8","#b6a681"),limits=c("Wave 1","Wave 2","Wave 3"))+
      scale_x_continuous(expand=c(0,0),limits=range(c(0:100)))+theme_bw()+
      labs( x =paste0("\u0394","\u03C9","t","(%)"),y = NULL,title = paste0("Group ",i))+
      theme(legend.position = "none",
            plot.title = element_text(color="black",hjust = 0,vjust=0,size = 8,family="Times New Roman"),
            axis.title.x= element_text(color="black",size = 8,family="Times New Roman"),
            axis.text.x= element_text(color="black",size = 8,family="Times New Roman"),
            axis.text.y= element_blank(),
            axis.ticks.y= element_blank(),
            plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm"),
            panel.grid=element_blank(),
            panel.background=element_rect(fill = "transparent",colour = NA))}
  g1[[1]]<-g1[[1]]+theme(axis.ticks.y=element_line(color="black"),
                         axis.text.y= element_text(color="black",hjust=1,size = 8,family="Times New Roman"))
  legend<-g1[[1]]+theme(legend.position = "top",
                        legend.title=element_blank(),
                        legend.text = element_text(size = 8,family="Times New Roman"),
                        legend.key.height=unit(0.2,'cm'),
                        legend.key.width=unit(1,'cm'),
                        legend.background = element_rect(fill="transparent"),
                        legend.key=element_rect(fill="transparent"))
  mylegend<-g_legend(legend)
  plot<-grid.arrange(mylegend,arrangeGrob(grobs =g1,ncol =4,widths=c(3.6,2,2,2)),heights = c(0.5,10))
  ggsave(paste0(outputpath,"fig2.pdf"),plot,height=75,width=200,unit="mm",device = cairo_pdf)
}


##################picture in SI###################
#########R-hat and RSS#############
modeltranplot<-function(alloutput,waveoutput){
  rhat<-list()
  R_ESS<-list()
  allfit<-readRDS(alloutput)
  out_rhat<-as.data.frame(rhat(allfit))#Rhat
  colnames(out_rhat)<-"x"
  Rneff<-as.data.frame(neff_ratio(allfit))####ESS
  colnames(Rneff)<-"x"
  rhat<-ggplot(out_rhat,aes(x))+geom_histogram(bins=50,colour="#a23434",fill="#a23434")+
    labs(x=NULL,y=NULL,title=paste0("Group ",G," Wave ",W))+
    scale_x_continuous(expand=(c(0,0)),limits=c(0.995,1.005))+
    scale_y_continuous(expand=(c(0,0)))+
    geom_vline(xintercept = 1,linetype ="dotdash",size=1)+
    theme(axis.line.y= element_blank(),axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),axis.title.y = element_blank(),
          legend.position = "none", axis.text.x = element_blank(),
          plot.title=element_text(size = 10,family="Times New Roman",hjust=0),
          panel.grid=element_blank(), axis.line.x = element_line(colour = "black"),
          axis.ticks.x = element_line(colour = "black"),
          panel.background=element_rect(fill = "transparent",colour = NA),
          plot.margin=unit(c(0.1,0.2,0.1,0.2),"cm"),
          axis.text.x=element_text(size = 10,family="Times New Roman"))
  R_ESS<-ggplot(Rneff,aes(x))+geom_histogram(bins=50,colour="#a23434",fill="#a23434")+
    labs(x=NULL,y=NULL,title=paste0("Group ",G," Wave ",W))+
    scale_x_continuous(expand=(c(0,0)),limits=c(0,1.1))+
    scale_y_continuous(expand=(c(0,0)))+
    geom_vline(xintercept = 1,linetype ="dotdash",size=1)+
    theme(axis.line.y= element_blank(),axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),axis.title.y = element_blank(),
          legend.position = "none",axis.text.x = element_blank(),
          plot.title=element_text(size = 10,family="Times New Roman",hjust=0),
          panel.grid=element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.ticks.x = element_line(colour = "black"),
          panel.background=element_rect(fill = "transparent",colour = NA),
          plot.margin=unit(c(0.1,0.2,0.1,0.2),"cm"),
          axis.text.x=element_text(size = 10,family="Times New Roman"))
  plot<-grid.arrange(arrangeGrob(grobs=list(rhat,R_ESS),nrow =1,
                                 gp=gpar(fontsize=10,hjust=0,fontfamily="Times New Roman")))
  ggsave("picture/rhat_ress.pdf",plot,width=160,height=75,units="mm",device = cairo_pdf)
}

#######NPIs correlation analysis###########
COR<-function(gather){
  x1<-gather[,c(8:16,18:22)]
  x1$Aging<-as.numeric(gsub("\\%", "",x1$Aging))
  colnames(x1)<- c("School closures", "Workplace closures", "Public transport closures",
                   "Gatherings restrictions", "Close public transport",
                   "Stay at home requirements","Internalmovement restrictions",
                   "International travel control", "Facial coverings",
                   "Health index", "Pop density", "Aging", "Humdity", "Air temp")
  p1<-quickcor(x1, cor.test = TRUE)+geom_star(data = get_data(type = "upper" ,show.diag = FALSE))+
    geom_mark(data = get_data(type = "lower", show.diag = FALSE),size=2,family="T")+
    geom_abline(size=0.5,slope=-1,intercept =15)+
    scale_fill_gradient2(midpoint = 0, low = "#00498d", mid = "white", high = "#74230a",space="Lab")+
    theme(legend.position = "right",
          legend.text = element_text(color="black",size=12,family="Times New Roman"),
          legend.title = element_text(color="black",size=12,family="Times New Roman"),
          legend.key.height=unit(10,'mm'),
          legend.key.width=unit(2,'mm'),
          axis.text = element_text(color="black",size=12,family="Times New Roman"))
  return(p1)
  ggsave("picture/cor.pdf",p1,width=200,height=200,units="mm",device = cairo_pdf)
}

############plot new cases report and new cases#####
DailyCases<-function(Dataset){
  ppd<-Dataset[,c("New_cases_report","New_cases","Date","StatesID","wave")]
  ppd$Date<-as.Date(ppd$Date)
  v<-melt(ppd,id=c("Date","wave","StatesID"))
  infde<-ggplot(v,aes(x=Date,y=value,fill=variable))+geom_bar(position="dodge",stat="identity",alpha=0.6)+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    scale_y_continuous(labels = comma)+ 
    labs(x=NULL,y=NULL)+ scale_fill_cyclical(values = c("#8d535b","#b8c4d6"),
                                             breaks = c("New_cases_report","New_cases"),
                                             labels = c('New_cases_report'="Daily number of reported infections",'New_cases'="Daily number of inferred new infections"),
                                             name = NULL,guide=guide_legend(nrow = 1))+theme_bw()+
    theme(legend.position = "top",
          legend.text = element_text(size = 9,family="Times New Roman",color="black",),
          legend.key.height=unit(0.2,'cm'), legend.key.width=unit(1,'cm'),
          strip.text = element_text(color="black",size = 10,family="Times New Roman"),
          plot.title = element_text(color="black",hjust = 0,vjust=0,size = 10,family="Times New Roman"),
          axis.title.x= element_text(color="black",vjust=0.5,size = 10,family="Times New Roman"),
          axis.text.x= element_text(color="black",hjust = 0.5,vjust=0.5,angle= 90,size = 10,family="Times New Roman"),
          plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
          legend.background = element_blank(),
          legend.key=element_rect(fill="transparent"),
          panel.background=element_rect(fill = "transparent",colour = "black"))
  ggsave(infde,file=paste0("result/picture/daily_cases.pdf"),height=12,width=14,unit="cm",device = cairo_pdf)
}
###################plot policy intensity###############
policyfig<-function(Countrylist,gather){
  Pfiglist<-list()
  for(n in 1:length(Countrylist)){
    Countrydata<-gather[which(gather$Country==Countrylist[n]),c("Start","School.closing", "Workplace.closing",
                                                                "Close.Public.transport", "International.travel.controls",
                                                                "Facial.Coverings","Restrict.Movement","Restrict.Gathering")]
    colnames(Countrydata)<-c("Start","School closures", "Workplace closures",
                             "Public transport closures","International travel restrictions",
                             "Facial coverings", "Movement restrictions","Gathering restrictions")
    Countrydata$Start<-substring(Countrydata$Start,6,10)
    DD<-melt(Countrydata,id="Start")
    Pfiglist[[n]]<-ggplot(DD,aes(x=Start,y=variable,fill=value))+geom_tile(color="white")+
      scale_fill_gradient2(midpoint = 0, low = "white",
                           mid = "#f0e5cc", high = "#723021",space="Lab")+
      labs(title=paste0("Group ",n,": ",Countrylist[n]),y=NULL,x=NULL)+
      theme(axis.text.x=element_text(color="black",hjust=1,angle=90,family="Times New Roman",size=10),
            axis.ticks.y= element_line(color="black"),
            plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm"),
            axis.text.y=element_blank(),
            plot.title=element_text(color="black",family="Times New Roman",size=10,hjust=0.5),
            legend.position = "none")}
  for(n in c(1,3)){Pfiglist[[n]]<-Pfiglist[[n]]+
    theme(axis.text.y=element_text(color="black",family="Times New Roman",size=10))}
  legend<-Pfiglist[[1]]+theme(legend.position = "top",
                              legend.title=element_blank(),
                              legend.text = element_text(size=10,family = "Times New Roman"),
                              legend.box = "horizontal",
                              legend.key.height=unit(0.2,'cm'),
                              legend.key.width=unit(2,'cm'),
                              legend.background = element_rect(color="transparent",fill="transparent"),
                              legend.key=element_rect(fill="transparent"))+guides(color=guide_legend(byrow=TRUE))
  leg<-g_legend(legend)
  plot<-grid.arrange(leg,arrangeGrob(grobs=Pfiglist,ncol=2,widths = c(6,4)),heights=c(1,10))
  return(plot)
}


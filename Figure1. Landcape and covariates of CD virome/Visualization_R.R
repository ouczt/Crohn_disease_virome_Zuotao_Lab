#Chinamap
china <- map("china", plot = F)
p=ggplot() + 
    geom_path(data = china, aes(long, lat, group = group), color = 'black', show.legend = F) +
    geom_point(data = mat.cities, aes(x = long, y = lat, size = 8), alpha = 0.8, color = 'darkorange') +
    geom_text(data = mat.cities, aes(x = long, y = lat, label = names)) +
    labs(x = 'Longitude', y = 'Latitude', title = 'China', size = 'Sample size') + 
    theme_bw() +
    theme(panel.border = element_blank())+
    theme(legend.position="none")+
    theme(panel.border = element_blank(),
          axis.title = element_text(size=15),
          #text = element_text(family = "STHeiti"),
          plot.title = element_text(hjust = 0.5,size=20))
		  
#Stacked Bar Chart
p=ggplot(data, aes(x = sample,weight = relative_abundance, fill=Genus_Level,color=Genus_Level))+  
    geom_bar(position = "fill",width=1,size=0)+
    theme_bw()+
    ylab("Relative abundance")+
    xlab("Subjects (n=208)")+
    scale_fill_manual(values=mycolors)+
    scale_color_manual(values=mycolors)+
    scale_y_continuous(expand = c(0,0))+
    #coord_fixed(ratio=400/4) +
    #ylim(0,1)+
    #xlab("HL group")+
    #facet_grid(disease~geo)+
    guides(fill = guide_legend(byrow = TRUE)) +
    theme(legend.spacing.y = unit(0.3, "lines"))+ 
    #theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+ 
    theme(panel.border = element_blank())+ 
    theme(panel.grid.major=element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          legend.title = element_text(size = 20,color="black"),
          legend.text = element_text(size = 15,color="black"),
          axis.text = element_text(size = 10,color="black"),
          axis.title = element_text(size = 20,color="black")))

     ##output_legend
     (legend = as_ggplot(ggpubr::get_legend(p1)))
     legend
	 
	 
#boxplot 
setwd('C:/Desktop/Mucosal virus_CD&HC/')
data = read.table("Prokaryotic_eukaryotic_virus.txt",sep='\t',  comment.char='',header=TRUE )
otu_melt <- melt(data,id.vars='taxonomy',measure.vars = colnames(data[,2:ncol(data)]),
                 variable.name = "sample",value.name = "relative_abundance") 
data <- dcast(otu_melt, taxonomy ~ sample, value.var=c("relative_abundance"), fun.aggregate = sum)
row.names(data)=data[,1]
data = data[,-1]
data=t(data)
#statistics
rmax <- vector()
rmin <- vector()
rmean <- vector()
rmed <- vector()
rvar <- vector()
rsd <- vector()
for (i in 1:2)
{ rmax = c(rmax,max(data[,i]))
  rmin = c(rmin,min(data[,i]))
  rmean = c(rmean,mean(data[,i]))
  rmed = c(rmed,median(data[,i]))
  rvar = c(rvar,var(data[,i]))
  rsd = c(rsd,sd(data[,i]))}

d = rbind(rmax,rmin,rmean,rmed,rvar,rsd)
colname=c("Eukaryotic","Prokaryotic")
colnames(d)=colname

otu_melt <- melt(data,id.vars='taxonomy',measure.vars = colnames(data[,2:ncol(data)]),
                 variable.name = "sample",value.name = "relative_abundance") 

p = ggplot(data=otu_melt,aes(x=Var2,y=relative_abundance,color=Var2))+
    geom_boxplot(alpha=0.8,width=0.5,position=position_dodge(width=2),size=0.8,outlier.colour = NA,color="black",fill=c('#9400D3','#6495ED'))+
    theme_bw() +
    theme(legend.position="none") +
    theme(text = element_text(size=1)) +
    ylab("Relative abundance (%)") +
    theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+ 
    theme(panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),  
          panel.grid.minor = element_blank(),) +
    #xlim("Eukaryotic virus","Prokaryotic virus")+
    theme(plot.title = element_text(hjust = 0.5,size = 25,face="bold"))+
    theme(axis.text.x = element_text(size = 15,color="black"), 
          axis.text.y = element_text(size = 10,color="black"),
          axis.ticks.x=element_line(color="black",size=1,lineend = 30), 
          #axis.line.x=element_line(linetype=1,color="black",size=1), 
          axis.ticks.y=element_line(color="black",size=1,lineend = 30), 
          #axis.line.y=element_line(linetype=1,color="black",size=1),
          axis.title= element_text(size=20),
          axis.title.x=element_blank(),
          legend.position = 'none'))
		  
#barplot
p =(ggplot(env_effect, aes(group,r2,fill=group))+
    geom_col()+
    scale_y_continuous(guide = guide_axis(position = "top"))+
    coord_flip()+
    scale_fill_manual(values=color2)+
    theme_classic()+
    theme(legend.position="none")+
    ylab(expression(paste("R"^"2")))+
    theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1))+
    theme(axis.title = element_blank(),
    axis.text=element_text(colour='black',size=15)))

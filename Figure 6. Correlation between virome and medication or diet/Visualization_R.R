#boxplot
p_coffee = ggplot(data=diversity_merge_coffee,aes(x=Coffee_consumption_frequency,y=richness,color=Coffee_consumption_frequency))+
    geom_jitter(alpha=0.2,size=6,position=position_jitterdodge(jitter.width = 0.6,jitter.height = 0,dodge.width = 0.4))+
    geom_boxplot(alpha=0.8,width=0.5,
                 position=position_dodge(width=2),
                 size=0.8,outlier.colour = NA,color="black",fill=c("#08306B","#2171B5","#6BAED6","#C6DBEF"))+ ##4ED2D6',
    scale_fill_manual(values=alpha(c("#08306B","#2171B5","#6BAED6","#C6DBEF"), 0.2))+
    scale_color_manual(values=c("#08306B","#2171B5","#6BAED6","#C6DBEF"))+
    theme_bw() +
    theme(legend.position="none") +
    theme(text = element_text(size=1)) +
    ylim(40,200)+ 
    ylab("Rihcness") +
    xlab("")+
    theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA), 
          panel.grid.minor = element_blank(),) +
    theme(plot.title = element_text(hjust = 0.5,size = 20,face="bold"))+
    theme(axis.text.x = element_text(size = 20,color="black",angle=30,hjust=1,vjust=1), 
          axis.text.y = element_text(size = 15,color="black"),
          axis.ticks.x=element_line(color="black",size=1,lineend = 30),
          axis.ticks.y=element_line(color="black",size=1,lineend = 30), 
          axis.title= element_text(size=20),
          legend.position = 'none')+
    stat_compare_means(comparisons = list(c("Each week","Each year")),
                       method = "wilcox.test",label = "p.signif",
                       size = 10,
                       label.y =170,
                       tip.length = 0, 
                       bracket.size = 1)+
    stat_compare_means(comparisons = list(c("Each week","Each month")),
                       method = "wilcox.test",label = "p.signif",
                       size = 10,
                       label.y =155,
                       tip.length = 0, 
                       bracket.size = 1)+
    stat_compare_means(comparisons = list(c("Each week","Never")),
                       method = "wilcox.test",label = "p.signif",
                       size = 10,
                       label.y =180,
                       tip.length = 0, 
                       bracket.size = 1),
    stat_compare_means(comparisons = list(c("Each day","Never")),
                       method = "wilcox.test",label = "p.signif",
                       size = 10,
                       label.y =150, 
                       tip.length = 0, 
                       bracket.size = 1))
					   
#heatmap
setwd('C:/Desktop/Mucosal virus_CD&HC/Fig4. Feature species_Maslin_HC_Rimission_Flare-up')
df = read.table(file = "Feature_diet_Heatmap.txt", header = TRUE, sep = "\t",stringsAsFactors = FALSE)
df2 = read.table(file = "Feature_diet_Heatmap2.txt", header = TRUE, sep = "\t",stringsAsFactors = FALSE)

Diet_list = df$feature
Diet_list2 = Diet_list[c(8,9,10,11,14,17,21,30,32,38)]
Diet_list = Diet_list[-c(8,9,10,11,14,17,21,30,32,38)]
Diet_list3 = df2$feature
Diet_list3 = c(Diet_list2,Diet_list3)

setwd('C:/Desktop/Mucosal virus_CD&HC/Maaslin')
result_diet = read.table(file = "all_results.tsv", header = TRUE, sep = "\t",stringsAsFactors = FALSE)
result_diet = result_diet[which(result_diet$feature %in% Diet_list),]
result_diet2 = result_diet
result_diet2$value = gsub('CD','HC',result_diet2$value)
result_diet2$coef = as.numeric(result_diet2$coef)
result_diet2$coef = (-1)*result_diet2$coef
result_diet = rbind(result_diet,result_diet2)
facet = c('a')
result_diet = cbind(result_diet,facet = facet)

setwd('C:/Desktop/Mucosal virus_CD&HC/Maaslin/Diet')
result_diet3 = read.table(file = "all_results2.txt", header = TRUE, sep = "\t",stringsAsFactors = FALSE)
result_diet3$value = gsub('_',' ',result_diet3$value)
result_diet3 = result_diet3[which(result_diet3$feature %in% Diet_list),]
facet2 = c('b')
result_diet3 = cbind(result_diet3,facet = facet2)

result_diet4 = rbind(result_diet,result_diet3) 
Prevalence = result_diet4$N.not.0/result_diet4$N
association = -log(result_diet4$qval)*result_diet4$coef
result_diet4 = cbind(result_diet4,Prevalence = Prevalence, association = association)
result_diet4$Prevalence[which(result_diet4$qval >=0.2)] <- NA
result_diet4$value = gsub(' frequency','',result_diet4$value)
result_diet4$association[which(result_diet4$association <= -6)] = -6
result_diet4$association[which(result_diet4$association >= 6)] = 6

setwd('C:/Desktop/Mucosal virus_CD&HC/Fig4. Feature species_Maslin_HC_Rimission_Flare-up/Maaslin/Diet')
result_diet3 = read.table(file = "all_results2.txt", header = TRUE, sep = "\t",stringsAsFactors = FALSE)
result_diet3$value = gsub('_',' ',result_diet3$value)
result_diet5 = result_diet3[which(result_diet3$feature %in% Diet_list3),]


Prevalence = result_diet5$N.not.0/result_diet5$N
association = -log(result_diet5$qval)*result_diet5$coef
result_diet5 = cbind(result_diet5,Prevalence = Prevalence, association = association)
result_diet5$Prevalence[which(result_diet5$qval >=0.2)] <- NA
result_diet5$value = gsub(' frequency','',result_diet5$value)
result_diet5$association[which(result_diet5$association <= -6)] = -6
result_diet5$association[which(result_diet5$association >= 6)] = 6

p_diet1 = ggplot() +
  geom_point(data = result_diet4, aes(feature, value, size = Prevalence, fill = association), shape = 21, color = "#c4bcba")+
  scale_fill_gradientn(colors=brewer.pal(11,'RdBu')[c(10,9,6,3,2)],space = "Lab",limits = c(-6,6),breaks=seq(-6,6,3),
                       name="Significant associations (−log10(qval)*sign(coeff))",
                       guide = guide_colorbar(title.position = "top",ticks = FALSE,title.hjust=0,barwidth = 10))+
  guides(size = guide_legend(name="a",title.position = "top",label.position = "bottom",title.hjust=0.5))+
  facet_nested(facet~.,scales = 'free_y',space = 'free',
               strip = strip_themed(background_y = element_blank(),text_y=element_blank()))+
  force_panelsizes(rows = c(length(unique(result_diet4[which(result_diet4$facet=='a'),'value'])),
                            length(unique(result_diet4[which(result_diet4$facet=='b'),'value']))),
                   cols = length(unique(result_diet4$feature)),respect = TRUE)+
  geom_vline(xintercept =seq(0.5, length(unique(result_diet4$feature))+0.5, 1), color = "#bbbbbb")+
  geom_hline(yintercept=seq(0.5, length(unique(result_diet4$metadata))+0.5, 1), color = "#bbbbbb")+
  theme_bw() +
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.grid.major = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        axis.line = element_line(color = 'black'),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 15,color="black"),
        axis.text.x = element_text(size = 15,color="black",angle = 90,vjust = 0.5,hjust = 1),
        axis.ticks.y= element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(colour="black", size=15,face="bold"),
        legend.text = element_text(colour="black", size=15))

p_diet2 = ggplot() +
  geom_point(data =result_diet5, 
             aes(feature,value, size = Prevalence, fill = association), shape = 21, color = "#c4bcba")+
  scale_fill_gradientn(colors=brewer.pal(11,'RdBu')[c(10,9,6,3,2)],space = "Lab",limits = c(-6,6),breaks=seq(-6,6,3),
                       name="Significant associations (−log10(qval)*sign(coeff))",
                       guide = guide_colorbar(title.position = "top",ticks = FALSE,title.hjust=0,barwidth = 10,size = 10))+
  guides(size = guide_legend(name="a",title.position = "top",label.position = "bottom",title.hjust=0.5))+
  geom_vline(xintercept =seq(0.5, length(unique(result_diet5$feature))+0.5, 1), color = "#bbbbbb")+
  geom_hline(yintercept=seq(0.5, length(unique(result_diet5$value))+0.5, 1), color = "#bbbbbb")+
  force_panelsizes(rows = length(unique(result_diet5$value)),
                   cols = length(unique(result_diet5$feature)),respect = TRUE)+
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        axis.line = element_line(color = 'black'),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 15,color="black"),
        axis.text.x = element_text(size = 15,color="black",angle = 90,vjust = 0.5,hjust = 1),
        axis.ticks.y= element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(colour="black", size=15,face="bold"),
        legend.text = element_text(colour="black", size=15))

p=p_diet + p_diet2 + plot_layout(ncol = 1, heights = c(1, 3))

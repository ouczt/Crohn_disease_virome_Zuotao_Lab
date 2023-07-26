#bubble plot

p = ggplot() +
  geom_point(data =result_inflammation4, 
             aes(factor(value,levels = c("HC","CD","Remission","Flare-up")),feature, size = Prevalence, fill = association), shape = 21, color = "#c4bcba")+
  scale_fill_gradientn(colors=brewer.pal(11,'RdBu')[c(10,9,6,3,2)],space = "Lab",limits = c(-6,6),breaks=seq(-6,6,3),
                       name="Significant associations (−log10(qval)*sign(coeff))",
                       guide = guide_colorbar(title.position = "top",ticks = FALSE,title.hjust=0,barwidth = 10,size = 10))+
  guides(size = guide_legend(name="a",
                             title.position = "top",label.position = "bottom",title.hjust=0.5))+
  geom_vline(xintercept =seq(0.5, length(unique(result_inflammation4$value))+0.5, 1), color = "#bbbbbb")+
  geom_hline(yintercept=seq(0.5, length(unique(result_inflammation4$feature))+0.5, 1), color = "#bbbbbb")+
  force_panelsizes(rows = length(unique(result_inflammation4$feature)),
                   cols = length(unique(result_inflammation4$value)),respect = TRUE)+
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
		
#lefse_barplot
setwd('C:/Desktop/Mucosal virus_CD&HC/Host prediction/lefse_host bacterial erichment')
lefse = read.table('Significant result_LDA_over_2.txt', header = TRUE, sep = '\t')
lefse = lefse[order(lefse[,3],lefse[,4],decreasing = T),]
lefse_remission = lefse[lefse$EnrichedGroups=='remission',]
lefse_HC = lefse[lefse$EnrichedGroups=='HC',]
lefse_flare = lefse[lefse$EnrichedGroups=='flare',]
lefse=rbind(lefse_HC,lefse_remission)
lefse$Biomarkernames = gsub("_"," ",lefse$Biomarkernames)
lefse$Biomarkernames = factor(lefse$Biomarkernames,levels = as.character(rev(lefse$Biomarkernames)))
colors <- c('#82A493','#EDCD8E')
p = ggplot(lefse,aes(x = Biomarkernames,y=LDA,fill = EnrichedGroups)) +
    scale_fill_manual(values = colors)+
    scale_y_continuous(breaks=seq(0, 4, 2))+
    geom_bar(alpha=1,stat = 'identity',width = 0.9,position = position_dodge(0.7))+  
    xlab('') + ylab('HC vs. remssion predicted viral bacterial host enrichment \nLDA SCORE (log 10)') + coord_flip() + 
    theme_bw() +
    theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+
    theme(axis.text.x = element_text(vjust=1,hjust=1))+
    theme(panel.grid.major.y = element_blank()) +
    theme(axis.text.y = element_blank(),axis.ticks = element_blank(),
          axis.text.x = element_text(face = 'bold'),axis.title.x = element_text(face='bold')) +
    #theme(panel.border = element_blank()) +
    theme(legend.position = 'none')+
    guides(fill=guide_legend(title=NULL))+
    geom_text(aes(y = ifelse(lefse$LDA >0,-0.03,0.03),label=Biomarkernames),size=3,hjust = ifelse(lefse$LDA>0,1,0))

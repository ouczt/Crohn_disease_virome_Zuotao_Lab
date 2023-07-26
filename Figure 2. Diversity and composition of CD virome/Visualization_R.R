#boxplot1

p=ggplot(data=diversity,aes(x=Group,y=richness,color=Group))+
  geom_jitter(alpha=0.2,size=5,position=position_jitterdodge(jitter.width = 1,jitter.height = 0,dodge.width = 0.4))+
  geom_boxplot(alpha=0.8,width=0.5,
                 position=position_dodge(width=2),
                 size=0.8,outlier.colour = NA,color="black",fill=alpha(c('#3CAF20','#F9874E','#0A5A60','#F73D0E'), 0.2))+
  theme_bw() +
  theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+ #外边框粗细
  theme(legend.position="none") +
  theme(text = element_text(size=1)) +
  scale_fill_manual(values=alpha(c('#3CAF20','#F9874E','#0A5A60','#F73D0E'), 0.2))+
  scale_color_manual(values=c('#3CAF20','#F9874E','#0A5A60','#F73D0E'))+
  ylim(50,200)+
  ylab("Richness") +
  xlab("")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        panel.grid.minor = element_blank(),) +
  theme(plot.title = element_text(hjust = 0.5,size = 30,face="bold"))+
  theme(axis.text = element_text(size = 20,color="black"), 
        axis.ticks.x=element_line(color="black",size=1,lineend = 30), 
        axis.line.x=element_line(linetype=1,color="black",size=1),
        axis.ticks.y=element_line(color="black",size=1,lineend = 30), 
        axis.line.y=element_line(linetype=1,color="black",size=1),
        axis.title= element_text(size=20),
        legend.position = 'none')+
  stat_compare_means(comparisons = list(c("GZ_CD","GZ_HC"),c("KM_CD","KM_HC")),
                                        method = "t.test",label = "p.signif",
                                        size = 10,
                                        label.y =180,
                                        tip.length = 0, 
                                        bracket.size = 1))
										
#boxplot2										
p=ggplot(data=diversity,aes(x=Inflammatory_severity,y=richness,color=Inflammatory_severity))+
  geom_jitter(alpha=0.2,size=5,position=position_jitterdodge(jitter.width = 1,jitter.height = 0,dodge.width = 0.4))+
  geom_boxplot(alpha=0.8,width=0.5,
                 position=position_dodge(width=2),
                 size=0.8,outlier.colour = NA,color="black",fill=alpha(c('#3CAF20','#729DF3','#F2C094'), 0.2))+
  theme_bw() +
  theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+ #外边框粗细
  theme(legend.position="none") +
  theme(text = element_text(size=1)) +
  scale_fill_manual(values=alpha(c('#3CAF20','#729DF3','#F2C094'), 0.2))+
  scale_color_manual(values=c('#3CAF20','#729DF3','#F2C094'))+
  ylim(50,220)+ 
  ylab("Richness") +
  xlab("")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),) +
  theme(plot.title = element_text(hjust = 0.5,size = 30,face="bold"))+
  theme(axis.text = element_text(size = 20,color="black"), 
        axis.ticks.x=element_line(color="black",size=1,lineend = 30), 
        axis.line.x=element_line(linetype=1,color="black",size=1),
        axis.ticks.y=element_line(color="black",size=1,lineend = 30), 
        axis.line.y=element_line(linetype=1,color="black",size=1),
        axis.title= element_text(size=20),
        legend.position = 'none')+
  stat_compare_means(comparisons = list(c("GZ_HC","GZ_remission")), 
                                        method = "wilcox.test",label = "p.signif",
                                        size = 10,
                                        label.y =180, 
                                        tip.length = 0, 
                                        bracket.size = 1)+
  stat_compare_means(comparisons = list(c("GZ_HC","GZ_flare")), 
                       method = "wilcox.test",label = "p.signif",
                       size = 10,
                       label.y =200, 
                       tip.length = 0, 
                       bracket.size = 1))
										
#Pcoa plot
p= beta_pcoa(distance_mat, meta, groupID="Group", ellipse=T, label=F, PCo=13)+
   geom_point(size = 3) + 
   scale_color_manual(values = c('#F9874E','#3CAF20','#F73D0E','#0A5A60')) +
   geom_vline(xintercept = 0,color = 'gray', lty ="dashed",)+
   geom_hline(yintercept = 0,color = 'gray', lty="dashed")+
   theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+
   theme(plot.title = element_text(hjust = 0.5,size = 15))+ #face="bold"
   theme(panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA), 
          panel.grid.minor = element_blank(),) +
   theme(axis.text = element_text(size = 15,color="black"), 
          axis.text.x = element_text(angle=35,hjust=1,vjust=1),
          axis.ticks.x=element_line(color="black",size=1,lineend = 30), 
          axis.line.x=element_line(linetype=1,color="black",size=0.5), 
          axis.ticks.y=element_line(color="black",size=1,lineend = 30), 
          axis.line.y=element_line(linetype=1,color="black",size=0.5),
          axis.title= element_text(size=20))+
   theme(legend.position = c(0.85,0.2))+ 
   theme(legend.title = element_blank())+
   theme(legend.text = element_text(size = 20))
   
#RDA plot
p_RDA = ggplot(otupca_site.scaling1, aes(x=RDA1, y=RDA2,color = group)) +
    geom_point(size=3)+
    ylim(-0.1,0.1)+
    xlim(-0.1,0.2)+
    theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+
    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) + 
    labs(x = paste('PCA1: ', otupca_eig[1], '%'), y = paste('PCA2: ', otupca_eig[2], '%')) +
    geom_vline(xintercept = 0, color = 'gray', size = 0.5) + 
    geom_hline(yintercept = 0, color = 'gray', size = 0.5) +
    geom_segment(data = otupca_env_sign_disease, aes(x = 0,y = 0, xend = PC1*1, yend = PC2*1), arrow = arrow(length = unit(0.2, 'cm')), size = 0.8, color = 'black') +
    geom_text(data = otupca_env_sign_disease, aes(PC1 * 1, PC2* 1, label = group), color = 'black', size = 10,vjust = -0.5,hjust = 1.2)+theme(axis.text = element_text(size = 10,color="black"), 
          axis.ticks.x=element_line(color="black",size=0.5,lineend = 20), 
          axis.ticks.y=element_line(color="black",size=0.5,lineend = 20), 
          axis.title= element_text(size=20))+
    stat_ellipse(data=otupca_site.scaling1,
                 geom = "polygon",level=0.5,linetype = 2,
                 size=0.5,aes(fill=type),alpha=0.2,show.legend = T))
				 
p_box = ggplot(otupca_site.scaling1,aes(group,RDA2),color=group) +
    geom_boxplot(aes(fill = group),outlier.colour = NA) +
    scale_fill_manual(values=color) +
    theme_bw()+
    theme(axis.ticks.length = unit(0.4,"lines"), 
          axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_text(colour='black',size=10),
          axis.text.x=element_text(colour='black',size=20),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    stat_compare_means(comparisons = list(c("Healthy control","CD")),
                       method = "t.test",label = "p.signif",
                       size = 6,
                       label.y =0.15, 
                       tip.length = 0.0, 
                       bracket.size = 0.5)
					   
p = p_RDA + p_box + plot_layout(design = '11112')

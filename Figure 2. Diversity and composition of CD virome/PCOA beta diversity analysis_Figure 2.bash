#PCOA beta diversity analysis
#otu
setwd('C:/Users/86925/Desktop/Mucosal virus_CD&HC/...')
otu = read.table("virus.txt", header=T,row.names = 1, sep="\t", comment.char="") #载入otu表格

#metadata
meta = read.table("metadata.txt",header=T, row.names = 1, sep = '\t', comment.char="",stringsAsFactors = FALSE, check.names = FALSE)
meta1 = read.table("metadata.txt",header=T, sep = '\t', comment.char="",stringsAsFactors = FALSE, check.names = FALSE)
meta_gz = read.table("metadata_gz.txt",header=T, row.names = 1, sep = '\t', comment.char="",stringsAsFactors = FALSE, check.names = FALSE)
meta_yn = read.table("metadata_yn.txt",header=T, row.names = 1, sep = '\t', comment.char="",stringsAsFactors = FALSE, check.names = FALSE)

#Bray-Crutis distance
bray <- vegdist(otu,method = "bray")
bray <- as.matrix(bray)
write.table(bray,"bray-crutis.txt",sep = "\t")

distance_type="bray_curtis"

#result
distance_mat=read.table("bray-crutis.txt", header=T, row.names=1, sep="\t", comment.char="")
distance_mat[1:3, 1:3]

#visualization
(p= beta_pcoa(distance_mat, meta, groupID="Group", ellipse=T, label=F, PCo=13)+
    ylim(-0.1,0.1)+
    xlim(-0.1,0.2)+
    geom_point(size = 3) + 
    scale_color_manual(values = c('#F9874E','#3CAF20','#F73D0E','#0A5A60')) +
    #xlim(-0.3,0.1)+
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
          axis.line=element_line(linetype=1,color="black",size=0.5),
          axis.ticks.y=element_line(color="black",size=1,lineend = 30), 
          axis.title= element_text(size=20))+
    theme(legend.position = c(0.85,0.2))+ 
    theme(legend.title = element_text(size = 15))+
    theme(legend.title = element_blank())+
    theme(legend.text = element_text(size = 20)) 
  
)

ggsave(p,file=paste0('beta_diversity.pdf'),width=190,height=180,units="mm",dpi = 1200,scale=1)
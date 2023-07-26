#meta
setwd('C:/Desktop/Mucosal virus_CD&HC/')
meta = read.table("metadata.txt", header=T, row.names=1,sep="\t", comment.char="")
meta = cbind(ID = rownames(meta),meta)

#diversity
setwd('C:/Desktop/Mucosal virus_CD&HC/Fig6. richness_evenness_shannon_correlationship')
alpha_diversity_virus = read.table("virus_genus_diversity.txt", header=T, row.names=1,sep="\t", comment.char="")
alpha_diversity_virus = cbind(ID = rownames(alpha_diversity_virus),alpha_diversity_virus)

alpha_diversity_bacteria = read.table("mucosa_bacteria_genus_diversity.txt", header=T, row.names=1,sep="\t", comment.char="")
alpha_diversity_bacteria = cbind(ID = rownames(alpha_diversity_bacteria),alpha_diversity_bacteria)

alpha_diversty = merge (alpha_diversity_virus,alpha_diversity_bacteria,by =("ID"))
alpha_diversty_meta = merge (meta,alpha_diversty,by=("ID"))

#grouping
alpha_diversty_meta_Remission = alpha_diversty_meta[grepl('Remission',alpha_diversty_meta$Type),]
rownames(alpha_diversty_meta_Remission) = alpha_diversty_meta_Remission[,c(1)]
alpha_diversty_meta_Remission = alpha_diversty_meta_Remission[,-c(1:5,15)]

alpha_diversty_meta_Flare = alpha_diversty_meta[grepl('Flare',alpha_diversty_meta$Type),]
rownames(alpha_diversty_meta_Flare) = alpha_diversty_meta_Flare[,c(1)]
alpha_diversty_meta_Flare = alpha_diversty_meta_Flare[,-c(1:5,15)]

alpha_diversty_meta_HC = alpha_diversty_meta[grepl('HC',alpha_diversty_meta$Type),]
rownames(alpha_diversty_meta_HC) = alpha_diversty_meta_HC[,c(1)]
alpha_diversty_meta_HC = alpha_diversty_meta_HC[,-c(1:5,15)]


#plot
p.virus_virus_HC <- 
  quickcor(alpha_diversty_meta_HC, type = "lower",show.diag = FALSE,cor.test=T) + 
  geom_circle2()+
  theme(axis.text.y = element_text(size = 15,color="black"),
        axis.text.x = element_text(size = 15,color="black"))+
  geom_mark(r=NA,sig.thres=0.05,size=10,color="black",nudge_y=-0.2,nudge_x = 0)+
  scale_fill_gradientn(colors=brewer.pal(11,'RdBu')[c(9,8,6,4,3)],space = "Lab",limits = c(-1,1),breaks=seq(-1,1,0.5),
                       name="Pearson's r",
                       guide = guide_colorbar(title.position = "top",ticks = FALSE,title.hjust=0,barwidth = 1,size = 10))

p.virus_virus_CD <- 
  quickcor(adj_virus_virus_sort_CD, type = "lower",show.diag = FALSE) + 
  geom_circle2()+
  scale_fill_gradientn(colors=brewer.pal(11,'RdBu')[c(9,8,6,4,3)],space = "Lab",limits = c(-0.8,1),breaks=seq(-0.8,1,0.5),
                       name="Pearson's r",
                       guide = guide_colorbar(title.position = "top",ticks = FALSE,title.hjust=0,barwidth = 1,size = 10))

p.virus_virus_Remission <- 
  quickcor(alpha_diversty_meta_Remission, type = "lower",show.diag = FALSE,cor.test=T) + 
  geom_circle2()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 15,color="black"),
        axis.text.x = element_text(size = 15,color="black"))+
  geom_mark(r=NA,sig.thres=0.05,size=10,color="black",nudge_y=-0.2,nudge_x = 0)+
  scale_fill_gradientn(colors=brewer.pal(11,'RdBu')[c(9,8,6,4,3)],space = "Lab",limits = c(-1,1),breaks=seq(-1,1,0.5),
                       name="Pearson's r",
                       guide = guide_colorbar(title.position = "top",ticks = FALSE,title.hjust=0,barwidth = 1,size = 10))

p.virus_virus_Flare <- 
  quickcor(alpha_diversty_meta_Flare, type = "lower",show.diag = FALSE,cor.test=T) + 
  geom_circle2()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 15,color="black"),
        axis.text.x = element_text(size = 15,color="black"))+
  geom_mark(r=NA,sig.thres=0.05,size=10,color="black",nudge_y=-0.2,nudge_x = 0)+
  scale_fill_gradientn(colors=brewer.pal(11,'RdBu')[c(9,8,6,4,3)],space = "Lab",limits = c(-1,1),breaks=seq(-1,1,0.5),
                       name="Pearson's r",
                       guide = guide_colorbar(title.position = "top",ticks = FALSE,title.hjust=0,barwidth = 1,size = 10))
p = p.virus_virus_HC + p.virus_virus_Remission + p.virus_virus_Flare

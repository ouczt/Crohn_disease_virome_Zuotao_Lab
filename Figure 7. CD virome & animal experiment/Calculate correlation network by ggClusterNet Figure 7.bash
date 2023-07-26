#Calculate correlation network by ggClusterNet

#Virome
setwd('C:/Desktop/Mucosal virus_CD&HC/Animal_study/')
metadata = read.table('metadata_virome.txt', header = TRUE, sep = '\t')
metadata_IBD = metadata [which(metadata$time=="Day0" & metadata$group=="IBD"),][,1:2]
metadata_nonIBD = metadata [which(metadata$time=="Day0" & metadata$group=="nonIBD"),][,1:2]
meta_IBD = metadata_IBD[,1]
meta_nonIBD = metadata_nonIBD[,1]

otu = read.table('OTU_virus.txt', header = TRUE, sep = '\t')

#Relative abundance
for (i in 8:ncol(otu)) {
  otu[,i] = otu[,i] /sum(otu[,i]) *100
}

rownames(otu) = paste0('V_OTU',c(1:nrow(otu)))
taxonomy = otu[,-(8:ncol(otu))]
otutab = otu[,-(1:7)]

otutab_v_IBD = otutab[,which(colnames(otutab) %in% meta_IBD)]
otutab_v_nonIBD = otutab[,which(colnames(otutab) %in% meta_nonIBD)]

###Bacteriome
setwd('C:/Desktop/Mucosal virus_CD&HC/Animal_study/')
metadata_b = read.table('metadata_bacteria.txt', header = TRUE, sep = '\t')

otu = read.table('OTU_bacteria.txt', header = TRUE, sep = '\t')
otu = separate(data=otu, col="clade_name",into=c("kingdom","phylum","class","order","family","genus","species","strains"), sep = "\\|", remove = TRUE)
otu_b=otu[,-8]
rownames(otu_b) = paste0('B_OTU',c(1:nrow(otu_b)))

taxonomy_b = otu_b[,-(8:ncol(otu_b))] 
otutab_b = otu_b[,-(1:7)]


otutab_b_IBD = otutab_b[,which(colnames(otutab_b) %in% metadata_IBD)]
otutab_b_IBD = otutab_b_IBD[which(rowSums(otutab_b_IBD[,7:ncol(otutab_b_IBD)]) > 0),]  
taxonomy_b_IBD = taxonomy_b[which(rownames(taxonomy_b) %in% rownames(otutab_b_IBD)),]

otutab_b_nonIBD = otutab_b[,which(colnames(otutab_b) %in% metadata_nonIBD)]
otutab_b_nonIBD = otutab_b_nonIBD[which(rowSums(otutab_b_nonIBD[,7:ncol(otutab_b_nonIBD)]) > 0),] 
taxonomy_b_nonIBD = taxonomy_b[which(rownames(taxonomy_b) %in% rownames(otutab_b_nonIBD)),]

#IBD group
merge=as.data.frame(rbind(otutab_b_IBD,otutab_v_IBD))
v_list = rownames(otutab_v_IBD)
b_list = rownames(otutab_b_IBD)

tax_merge= as.data.frame(rbind(taxonomy_b,taxonomy))
tax_merge= tax_merge[which(rownames(tax_merge) %in% rownames(merge)),]
ps_IBD = phyloseq(otu_table(as.matrix(merge), taxa_are_rows=TRUE),
              tax_table(as.matrix(tax_merge)))

#nonIBD group
merge=as.data.frame(rbind(otutab_b_nonIBD,otutab_v_nonIBD))
v_list = rownames(otutab_v_nonIBD)
b_list = rownames(otutab_b_nonIBD)

tax_merge= as.data.frame(rbind(taxonomy_b_nonIBD,taxonomy))
tax_merge= tax_merge[which(rownames(tax_merge) %in% rownames(merge)),]

ps_nonIBD = phyloseq(otu_table(as.matrix(merge), taxa_are_rows=TRUE),
              tax_table(as.matrix(tax_merge)))

#Correlation network analysis

#IBD
result = corMicro (ps = ps_IBD,
                   N = 300,
                   method.scale = "TMM",
                   r.threshold= 0.15,
                   p.threshold= 0.05,
                   method = "sparcc"
                   )

#nonIBD
result2 = corMicro (ps = ps_nonIBD,
                   N = 300,
                   method.scale = "TMM",
                   r.threshold= 0.15,
                   p.threshold= 0.05,
                   method = "sparcc")
#Calculate the effect size of metadata factors

#otu
setwd('C:/Users/86925/Desktop/Mucosal virus_CD&HC/...')
otu = read.table("virus_contig.txt", header=T, row.names=1, sep="\t", comment.char="")

#metadata
env = read.table("GZ&YN_metadata_vegan.txt",header=T, row.names = 1, sep = '\t', comment.char="",stringsAsFactors = FALSE, check.names = FALSE)


#Hellinger transformation
otu_hel <- decostand(otu, method = 'hellinger')

#PCA
otupca <- rda(otu_hel,env, scale = FALSE)
summary(otupca, scaling = 1)
plot(otupca, choices = c(1, 2), scaling = 1,  display = c('wa'))

otupca_ef <- envfit(otupca,env,perm = 999, choices = c(1,2), display = 'sites')
otupca_ef

#p-value adjustment with bonferroni method
otupca_ef_adj <- otupca_ef
otupca_ef_adj$factors$pvals <- p.adjust(otupca_ef_adj$factors$pvals, method = 'fdr')
otupca_ef_adj$vectors$pvals <- p.adjust(otupca_ef_adj$vectors$pvals, method = 'fdr')
otupca_ef_adj

#Obtain the analysis result
otupca_site.scaling1 <- scores(otupca, choices = 1:2, scaling = 1, display = 'sites')
#or by
otupca_site.scaling1 <- summary(otupca, scaling = 1)$sites[ ,1:2]
#output the result table
#write.csv(data.frame(otupca_site.scaling1), 'pca_site.scaling1.csv', quote = FALSE)

#Obtain the analysis result2
otupca_species.scaling1 <- summary(otupca, scaling = 1)$species[ ,1:2]
#output the result table
#write.csv(data.frame(otupca_species.scaling1), 'pca_species.scaling1.csv', quote = FALSE)

#Axis and effective size
eig <- otupca$CA$eig
eig_prop <- otupca$CA$eig / sum(otupca$CA$eig)

a=c()
for (i in 1:2) {a=c(a,rep(otupca_ef_adj$factors$r[i],2))}
a=c(a,rep(otupca_ef_adj$factors$r[3],4))
for (i in 4:24) {a=c(a,rep(otupca_ef_adj$factors$r[i],2))}
a

b=c()
for (i in 1:2) {b=c(b,rep(otupca_ef_adj$factors$pvals[i],2))}
b=c(b,rep(otupca_ef_adj$factors$pvals[3],4))
for (i in 4:24) {b=c(b,rep(otupca_ef_adj$factors$pvals[i],2))}
b

#Summarize the result
otupca_env <- data.frame(cbind(otupca_ef_adj$factors$centroids,a,b))
names(otupca_env) <- c('PC1', 'PC2', 'r2', 'p.adj')

otupca_env2 <- data.frame(cbind(otupca_ef_adj$vectors$arrows, otupca_ef_adj$vectors$r, otupca_ef_adj$vectors$pvals))
names(otupca_env2) <- c('PC1', 'PC2', 'r2', 'p.adj')

otupca_env3= rbind(otupca_env,otupca_env2)

write.csv(otupca_env3, 'pca_env.csv', quote = FALSE)

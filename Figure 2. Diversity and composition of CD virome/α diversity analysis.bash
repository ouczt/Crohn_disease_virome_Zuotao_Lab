#α-diversity analysis by vegan
library(vegan)
setwd('C:/Users/Desktop/Mucosal virus_CD&HC/...')
otu <- read.table('otu.txt',sep='\t',  comment.char='',header=TRUE )
meta <- read.table('metadata.txt',sep='\t', comment.char='',header=TRUE )

row.names(otu) = otu [,1]
name = otu [,1]
otu = otu[,-c(1)]
otu = as.data.frame(lapply(otu,as.numeric))
row.names(otu) = name
otu1 = t(otu)

diversity = data.frame(sample =colnames(otu),
                       Richness=specnumber(otu1),
                       Shannon=diversity(otu1, index="shannon"),
                       Simpson=diversity(otu1, index="simpson"),
                       Invsimpson=diversity(otu1, index="invsimpson"),
                       Evenness=diversity(otu1, index="shannon")/log(specnumber(otu1)))
					   
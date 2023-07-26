#inter-individual dissimilarity
library("BiocManager")
library("phyloseq")

setwd('C:/Desktop/Mucosal virus_CD&HC/')
otu_v = read.table("virus.txt", header=T, sep="\t", comment.char="",stringsAsFactors = FALSE, row.names = 1,check.names = FALSE)

#OTU_v
otu_v = t(otu_v)
otu_v = as.matrix(otu_v)
otu_tax = as.matrix(read.table("virus_tax.txt", header = TRUE, sep = "\t", row.names = 1))

OTU =otu_table(otu_v, taxa_are_rows = TRUE)
TAX =tax_table(otu_tax)
physeq_p = phyloseq(OTU, TAX) # phylogenetic tree

reference <- apply(abundances(otu_v), 1, median)
b_v <- divergence(physeq_p, reference, method = "bray")

#OTU_b
otu_b = read.table("bacteria.txt", header=T, sep="\t", comment.char="",stringsAsFactors = FALSE, row.names=1,check.names =FALSE)
otu_b = t(otu_b)
otu_b = as.matrix(otu_b)
otu_tax_b = as.matrix(read.table("bacteria_tax.txt", header = TRUE, sep = "\t", row.names = 1))

OTU =otu_table(otu_b, taxa_are_rows = TRUE)
TAX =tax_table(otu_tax_b)
physeq_p_b = phyloseq(OTU, TAX)

reference = apply(abundances(otu_b), 1, median)
b_b = divergence(physeq_p_b, reference, method = "bray")
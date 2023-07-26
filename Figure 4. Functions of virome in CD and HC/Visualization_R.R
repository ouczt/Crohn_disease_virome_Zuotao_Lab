#heatmap
#function_table
protein_table = read.table("Function_significant_protein.txt", header=T, sep="\t", comment.char="") 
protein_table1 = merge(protein_table, function_protein, by = "Protein_names",all.x = T)
protein_table_abundance = protein_table1[,c(1,6:ncol(protein_table1))]
row.names(protein_table_abundance) = protein_table_abundance[,1]
protein_table_abundance = protein_table_abundance[,-1]
protein_table_abundance = t(protein_table_abundance)
protein_table_abundance = cbind (rownames(protein_table_abundance),protein_table_abundance[,1:ncol(protein_table_abundance)])
colnames(protein_table_abundance)[1] = "Sample"
anno = read.table("metadata.txt",header = T) #Group
group = anno [,c(1,3)]
otu = merge(group,protein_table_abundance,by="Sample")
Group = factor(otu[,2],levels = c("HC","Remission","Flare"))


rownames(otu) = otu[,1]
names = otu[,1]
otu = otu [,-c(1,2)]
otu = apply(otu[,1:ncol(otu)],2,as.numeric)
otu = scale(otu) #normalized
rownames(otu) = names
otu = t(otu)

p = Heatmap(otu,
    name = "gene\ncontent",
    cluster_rows = F,
    cluster_columns = F,
    row_dend_reorder = T,
    column_dend_reorder = T,
    row_order = row_order,
    col = colorRamp2(c(-0.5,0,1,2,3,5), c("#483D8B","slateblue4","white","yellow", "orange","red")), 
    show_row_names = F,
    show_column_names = F,
    column_split = Group,
    column_title = NULL,
    row_title =NULL,
    row_names_gp = gpar(fontsize = 12),
    column_names_gp = gpar(fontsize = 8),
    group = as.vector(Group),
    col = list(group = c("HC" = '#228B22',
                         "Remission" = '#FFA500', 
                         "Flare" = '#B22222')),
    show_annotation_name = c(group = FALSE),
    annotation_legend_param = list(group = list(at = c("HC","Remission","Flare"))))

Group2 = factor(protein_table1[,5])
protein_table2 = protein_table1 [,c(1:4)]
rownames(protein_table2) = protein_table2[,1]
names = protein_table2[,1]
protein_table2 = protein_table2 [,-c(1)]
protein_table2 = apply(protein_table2[,1:ncol(protein_table2)],2,as.numeric)
rownames(protein_table2) = names

type = list(title = "Group", title_gp = gpar(fontsize = 15),
            labels_gp = gpar(fontsize = 10)
            
p2 = Heatmap(
     protein_table2,
     col = colorRamp2(c(-4,-2,0,2,4), c("slateblue4", "#333E9A","#F0F5F9", "#E78A46","#B22222")),
     cluster_rows = T,
     cluster_columns = F,
     show_column_names = T,
     show_row_names = T,
     column_title = NULL,
     show_row_dend = F,
     show_column_dend = F,
     show_heatmap_legend = T,
     row_names_gp = gpar(fontsize = 12),
     column_names_rot = -90,
     width = unit(1.5, "cm"), 
     right_annotation = rowAnnotation(group = as.vector(Group2),width = unit(0.3, "cm"),
     col = list(group = c("Metabolism (anabolism and catabolism)"="darkgreen",  #"#8DD3C7"
                         "DNA replication, recombination, and repair"= "yellow", #"#FFFFB3"
                         "Trancription, translation and post-translational processing"="#BEBADA",
                         "Antibiotic related/Antibiotic resistance function"="#B3DE69",
                         "Signal transduction"="#80B1D3",
                         "Transport of chemicals, molecules,nutrients"="#FDB462",
                         "Mobilization and integration to host"="#FB8072",
                         "Regulation of gene expression"="#FCCDE5",
                         "Viral adhesion and invasion"="#D9D9D9",
                         "Viral DNA genome package, morphogenesis, and virion assembly biogenesis"="#BC80BD",
                         "Lysogenic transformation, lytic development, and viral release "="#CCEBC5",
                         "Macromolecule modification (nucleotide modification)"="#FFED6F",
                         "Host related function (toxin/antitoxin system and stress associated response)"="#1F78B4"))),
      heatmap_legend_param = list(title= "−log(qval)*sign(coeff)",labels_gp = gpar(col = "black", font = 1,fontsize = 12)))
      draw(p2,heatmap_legend_side = "bottom") 
      row_order = row_order(draw(p2))

      p=p1+p2
      draw(p,heatmap_legend_side = "bottom") 

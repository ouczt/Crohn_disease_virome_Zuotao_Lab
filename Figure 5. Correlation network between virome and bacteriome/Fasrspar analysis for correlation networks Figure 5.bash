#Fasrspar analysis for correlation networks

#Pipline install
conda install -c bioconda -c conda-forge fastspar --prefix=/bin/Anaconda3/anaconda3/envs/fastspar
conda activate fastspar
conda env config vars set PATH=/bin/Anaconda3/anaconda3/envs/fastspar/bin/:$PATH #setting path 

###Input file
fastspar --otu_table ./sparcc_out/otu.txt --correlation ./sparcc_out/correlation.tsv --covariance ./sparcc_out/covariance.tsv

###Calculate p-value
mkdir /sparcc_out/bootstrap_counts/
fastspar_bootstrap --otu_table /sparcc_out/result.txt --number 1000 --prefix /sparcc_out/bootstrap_counts/result

mkdir /sparcc_out/bootstrap_correlation
parallel fastspar --otu_table {} --correlation /sparcc_out/bootstrap_correlation/cor_{/} --covariance bootstrap_correlation/cov_{/} -i 5 ::: /sparcc_out/bootstrap_counts/*

fastspar_pvalues --otu_table /sparcc_out/result.txt --correlation /sparcc_out/result_correlation.tsv --prefix bootstrap_correlation/cor_result_ --permutations 1000 --outfile /sparcc_out/result_pvalues.tsv

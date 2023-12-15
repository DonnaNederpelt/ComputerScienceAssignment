This document presents an overview of all the codes used in the research paper that are provided in the gitub.
There are two codes:
1. CS code with optimizing clusters
2. CS code with bootstrapping without LSH

In both code you need a dataset that contains 1,624 products of four Web shops. This dataset is available on https://personal.eur.nl/frasincar/datasets/TVs-all-merged.zip as a JSON file

We elaborate on how the codes should be executed, which results can be obtained by each of the codes and where these can be found in the paper.

1. The first code is used to obtain the F1-measure values for the LSH and the k-means clustering method.
You execute the first code called CS code with optimizing clusters in the following way:
--> load in the data as a matrix
--> create the matrix from the possible band and row combinations based on the size of minhashing
--> initialize all the vectors that need to be filled by the bootstrap and the number of bootstrap iterations
--> run the bootstrap function. In this bootstrap function the following steps are done:
	+ make a data matrix based on the indices chosen to perform boostrap on
	+ clean the data by replacing inconsistencies
	+ make model words from the titles and key value pairs
	+ make binary vectors with these model words and make a binary matrix of all these vectors
	+ create the minhash signature matrix by using hash functions
	+ tune the bands and rows by iterating over the different combinations and apply LSH on the minhash signature matrix for these different bands and rows and calculate the F1-measure values
	  Keep the optimal band and row value for the highest F1-measure value
	+ after tuning implement the optimal band and row value in the LSH again
	+ apply k-means clustering on the LSH and tune the cluster value by determining the F1-measure value for the k-means clustering. Keep the cluster value for which F1 is the highest
	+ apply k-means clustering for the optimal cluster value
	+ compute the F1-measure values for LSH and k-means with their optimal value and store these values in a vector
(This order from the bootstrap function is done 5 times)

2. You execute the second code called CS code with bootstrapping without LSH in the following way:
--> load in the data as a matrix
--> initialize all the vectors that need to be filled by the bootstrap and the number of bootstrap iterations
--> run the bootstrap function. In this bootstrap function the following steps are done:
	+ make a data matrix based on the indices chosen to perform boostrap on
	+ clean the data by replacing inconsistencies
	+ make model words from the titles and key value pairs
	+ make binary vectors with these model words and make a binary matrix of all these vectors
	+ create the minhash signature matrix by using hash functions
	+ apply k-means clustering on the signature matrix and tune the cluster value by determining the F1-measure value for the k-means clustering. Keep the cluster value for which F1 is the highest
	+ apply k-means clustering for the optimal cluster value
	+ compute the F1-measure value for k-means with the optimal value and store this value in a vector
(This order from the bootstrap function is done 5 times)

For further research, other clustering methods can be combined and compared to LSH in order to determine whether LSH remains a good pair shrinking method and whether a combination of LSH with a clustering method results in a lot of true duplicate pairs

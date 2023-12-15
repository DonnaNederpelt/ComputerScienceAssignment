install.packages("jsonlite")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("hash")
library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)
library(hash)

#loading in the dataset and making it a matrix
dataset <- fromJSON("C:\\Users\\Donna\\Documents\\Schooljaar 2023-2024\\BLOK 2\\Computer Science\\TVs-all-merged.json")
data_frame <- bind_rows(lapply(dataset, as.data.frame), .id = "observation_id")
data <- as.matrix(data_frame)

#number of minhashes to which n = r*b must hold
number <- 1500
#initialize a matrix to store pairs of divisors for rows and bands
divisor_pairs_bands_rows <- matrix( ncol = 2)

#iterate from 1 to the square root of the number
for (i in 1:sqrt(number)) {
  #check if i is a divisor of the number of minhashes
  if (number %% i == 0) {
    #calculate the corresponding divisor
    divisor <- number / i
    
    #append the product and thus the possible combination of rows and bands to the matrix
    divisor_pairs_bands_rows <- rbind(divisor_pairs_bands_rows, c(i, divisor))
  }
}
#remove the first row as this is the initialization row
divisor_pairs_bands_rows <- divisor_pairs_bands_rows[-1, ]
#again remove the first row as this contains 1 and 1500 which we will not consider as a rows and bands pair
divisor_pairs_bands_rows <- divisor_pairs_bands_rows[-1, ]

#create a new matrix with columns reversed
matrix_reversed <- divisor_pairs_bands_rows[, ncol(divisor_pairs_bands_rows):1]

#Combine the original matrix with the reversed matrix such that each column contains all rows and bands options
divisor_pairs_for_rows_bands <- rbind(divisor_pairs_bands_rows, matrix_reversed)

#initializing the iterations and the vectors that will be filled with results
iterations <- 5
F1 <- numeric(iterations)
F1_kmeans <- numeric(iterations)
bands_bootstrap_optimal <- numeric(iterations)
rows_bootstrap_optimal <- numeric(iterations)
cluster_bootstrap_optimal <- numeric(iterations)

#bootstrapping
for(iteration in 1:iterations){
  #set seed
  set.seed(iteration)

  #number of observations to select
  num_observations_to_select <- 1000
  
  #randomly select 1000 indices
  selected_indices <- sample(1:nrow(data), num_observations_to_select, replace = FALSE)
  
  #use the selected indices to obtain 1000 products
  data_matrix <- data[selected_indices, ]
  
  
  #DATA CLEANING-----------------------------------------------------------------------------------------------------
  #create the non_numeric_words specification
  non_numeric_words <- unlist(strsplit(gsub("[^A-Za-z]+", " ", data_frame), "\\s+"))
  
  #count the frequency of each word
  word_frequency <- table(non_numeric_words)
  sorted_word_freq <- sort(word_frequency, decreasing = TRUE)
  
  #converting the words to lowercase by this function
  convert_to_lowercase <- function(matrix){
    result_matrix <- matrix(, nrow = nrow(matrix), ncol = ncol(matrix))
    
    for (i in 1:nrow(matrix)) {
      for (j in 1:ncol(matrix)) {
        result_matrix[i, j] <- tolower(matrix[i, j])
      }
    }
    
    return(result_matrix)
  }
  
  #apply the function to the data matrix
  lowercase_matrix <- convert_to_lowercase(data_matrix)
  
  #create the non_numeric_words specification
  non_numeric_words <- unlist(strsplit(gsub("[^A-Za-z]+", " ", lowercase_matrix), "\\s+"))
  
  #count the frequency of each word
  word_frequency_lower <- table(non_numeric_words)
  print(word_frequency_lower)
  #sort the order of the word frequency
  sorted_word_freq_lower <- sort(word_frequency_lower, decreasing = TRUE)
  print(sorted_word_freq_lower)
  
  #provide all different ways in which inch is written in the dataset and convert it to the same word
  inconsistent_inch <- c("Inch", "inches", '"', "-inch", " inch", "inch")
  standardize_inch <- function(text_vector, inconsistent_inch) {
    #use a loop to replace each value in replace the inconsistencies with "inch"
    for (value in inconsistent_inch) {
      text_vector <- gsub(value, "inch", text_vector, ignore.case = TRUE)
    }
    
    #replace double quotes with "inch"
    text_vector <- gsub('["“”]', 'inch', text_vector)
    
    return(text_vector)
  }
  
  #apply the function to the dataset to get all named inch without inconsistencies
  dataset_inches <- standardize_inch(lowercase_matrix, inconsistent_inch)
  
  #apply the function to the dataset to get all named hertz without inconsistencies
  inconsistent_hertz <- c("Hertz", "hertz", "Hz", "HZ", "hz", "-hz", "hz")
  standardize_hertz <- function(text_vector, inconsistent_hertz) {
    # Use a loop to replace each value in replace_values with "inch"
    for (value in inconsistent_hertz) {
      text_vector <- gsub(value, "hz", text_vector, ignore.case = TRUE)
    }
    
    return(text_vector)
  }
  
  #apply the function to the dataset to get all named hertz without inconsistencies
  dataset_inches_hertz <- standardize_hertz(dataset_inches, inconsistent_hertz)
  
  #create a dataset containing no inconsistencies regarding inch and hertz
  dataset_consistent <- as.matrix(dataset_inches_hertz)
  colnames(dataset_consistent) <- colnames(data_matrix)
  title_vector <- dataset_consistent[, 356]
  
  #Model words--------------------------------------------------------------------------------------------------------------
  
  #create the expression for the model wrods of the tilte
  Modelword_title <- "([a-zA-Z0-9](([0-9]+[^0-9,]+)|([^0-9,]+[0-9]+))[a-zA-Z0-9])"
  
  title_tokens <- strsplit(title_vector, "\\s+")
  #apply the model words expression of the title on the title
  model_words_per_title <- sapply(title_tokens, function(tokens) {
    valid_tokens <- tokens[grep(Modelword_title, tokens, perl = TRUE)]
    if (length(valid_tokens) >= 2) {
      cleaned_tokens <- gsub("[^a-zA-Z0-9]", "", valid_tokens)
      cleaned_tokens <- gsub("\\s+", "", cleaned_tokens)
      paste(cleaned_tokens, collapse = " ")
    } else {
      NA
    }
  })
  
  modelwords_title_matrix <- as.matrix(model_words_per_title)
  
  #create the expression for the model words of key value pairs
  Modelword_keyvalue <- "(^\\d+(\\.\\d+)?[a-zA-Z]+$|^\\d+(\\.\\d+)?$)"
  
  #create a key value matrix from the dataset with no inconsistencies regarding inch and hertz
  keyvalue_matrix <- dataset_consistent[, 5:355]
  #apply the model words expression of the key value words on the key value matrix
  model_words_keyvalues_matrix <- apply(keyvalue_matrix, c(1, 2), function(keyvalue_pair) {
  
    valid_pair <- grepl(Modelword_keyvalue, keyvalue_pair, perl = TRUE)
    
    if (valid_pair) {
      #use gsub to remove non-numerical parts
      cleaned_pair <- gsub("[^0-9.]", "", keyvalue_pair)
      cleaned_pair
    } else {
      NA
    }
  })
  
  
  #Binary vectors ---------------------------------------------------------------------------------
  #creating vectors with the unique model words of the titles and the key values and combined
  MW_titles <- unique(unlist(strsplit(as.character(modelwords_title_matrix), " ")))
  MW_keyvalues <- unique(as.vector(model_words_keyvalues_matrix))
  MW <- unique(c(MW_titles, MW_keyvalues))
  MW <- MW[!is.na(MW)]
  
  
  #initialize the binary matrix
  binary_matrix <- matrix(0, nrow = nrow(modelwords_title_matrix), ncol = length(MW), dimnames = list(NULL, MW))
  
  #fill in the model words from the titles
  model_words_split <- strsplit(modelwords_title_matrix, " ")
  
  #find the maximum number of words
  max_words <- max(sapply(model_words_split, length))
  
  #create a matrix to fill in the split model words
  final_modelwords_title_matrix <- matrix("", nrow = length(modelwords_title_matrix), ncol = max_words)
  
  #fill in the matrix with the split model words
  for (i in 1:length(modelwords_title_matrix)) {
    final_modelwords_title_matrix[i, 1:length(model_words_split[[i]])] <- model_words_split[[i]]
  }
  
  #fill in the binary matrix based on the model words from the title
  for (i in 1:nrow(final_modelwords_title_matrix)) {
    model_words_title_i <- unlist(strsplit(as.character(final_modelwords_title_matrix[i, ]), " "))
    binary_matrix[i, MW %in% model_words_title_i] <- 1
  }
  
  #fill in the binary matrix based on model words from key-value pairs
  for (i in 1:nrow(model_words_keyvalues_matrix)) {
    model_words_keyvalues_i <- unlist(strsplit(as.character(model_words_keyvalues_matrix[i, ]), " "))
    binary_matrix[i, MW %in% model_words_keyvalues_i] <- 1
  }

  
  #Defining duplicate candidates ------------------------------------------------------------------
  #function to generate a random prime number greater than k
  #set k equal to the number of minhashes
  k <- 1500
  is_prime <- function(n) {
    if (n <= 1) {
      return(FALSE)
    }
    
    for (i in 2:(floor(sqrt(n)))) {
      if (n %% i == 0) {
        return(FALSE)
      }
    }
    
    return(TRUE)
  }
  #generate a random prime number via this function
  generate_random_prime_greater_than_k <- function(prime){
    prime_candidate <- sample(1500:10000,1)
    while(!is_prime(prime_candidate)){
      prime_candidate <- prime_candidate + 1
    }
    return(prime_candidate)
  }
  
  
  #generate a random hash function via this function
  random_hash_function <- function(k) {
    #generate two random integers a and b
    a <- sample(1:(k-1), 1)
    b <- sample(1:(k-1), 1)
    
    #generate a random prime number greater than k
    p <- generate_random_prime_greater_than_k(k)
    
    #define the hash function
    return(function(x) {
      return((a + b * x) %% p)
    })
  }
  
  #generate the minhash signatures for a set of binary vectors via this function
  generate_minhash_signatures <- function(binary_matrix, num_hashes) {
    num_instances <- nrow(binary_matrix)
    num_features <- ncol(binary_matrix)
    #set k equal to the number of minhashes you want to do, in this case 1500
    k <- floor(num_features *(1500/ncol(binary_matrix)))
    #initialize a matrix for the minhash signatures
    minhash_signatures <- matrix(Inf, nrow = num_hashes, ncol = num_instances)
    #apply the different hashfunctions on the binary matrix to get the minhash signatures
    for (h in 1:num_hashes) {
      hash_function <- random_hash_function(k)
      
      for (i in 1:num_instances) {
        set_indices <- which(binary_matrix[i, ] == 1)
        if (length(set_indices) == 0) {
          minhash_signatures[h, i] <- 0
        } else {
          minhash_signatures[h, i] <- min(hash_function(set_indices))
        }
      }
    }
    
    return(minhash_signatures)
  }
  
  #set seed
  set.seed(123)
  num_hashes <- floor(ncol(binary_matrix) * (1500/ncol(binary_matrix)))
  #creating a minhash signature matrix
  minhash_signatures <- generate_minhash_signatures(binary_matrix, num_hashes)
  
  #LSH---------------------------------------------------------------------
  #fucntion to apply LSH to the minhash signature matrix with bands and rows
  lsh_algorithm <- function(minhash_signatures, bands, rows ) {
    num_hashes <- nrow(minhash_signatures)
    num_instances <- ncol(minhash_signatures)
    
    #check if the number of hashes is divisible by bands
    if (num_hashes %% bands != 0) {
      stop("Number of hashes must be divisible by the number of bands.")
    }
    
    #calculate the number of rows per band
    rows_per_band <- num_hashes / bands
    
    buckets <- vector("list", length = bands)
    for (i in 1:bands) {
      buckets[[i]] <- list()
    }
    
    for (band in 1:bands) {
      #select the band from the minhash signatures matrix
      start_row <- (band - 1) * rows_per_band + 1
      end_row <- band * rows_per_band
      band_signatures <- minhash_signatures[start_row:end_row, ]
      
      #hash each column in the band
      hashed_columns <- apply(band_signatures, 2, function(col) {
        paste(col, collapse = '')
      })
      
      for (product in 1:num_instances) {
        bucket_identifier <- hashed_columns[product]
        buckets[[band]][[bucket_identifier]] <- c(buckets[[band]][[bucket_identifier]], product)
      }
    }
    candidate_pairs <- list()
    for (band in 1:bands) {
      for (bucket in buckets[[band]]) {
        if (length(bucket) > 1) {
          #generate all pairs within the bucket
          pairs <- combn(bucket, 2)
          candidate_pairs <- union(candidate_pairs, list(pairs))
        }
      }
    }
    return(candidate_pairs)
  }
  
  #initializing to which the optimal values will be saved
  F1_optimal_tune <- 0
  F1_optimal_rowsbands <- 0
  band_optimal_tune <- 0
  row_optimal_tune <- 0
  cluster_optimal_tune <- 0
  #start with tuning the bands and rows
  for(i in nrow(divisor_pairs_for_rows_bands)){
    #get the ith row of the matrix with the pairs of rows and bands
    row_sample_tune <- as.integer(divisor_pairs_for_rows_bands[i,1])
    band_sample_tune <- as.integer(divisor_pairs_for_rows_bands[i,2])
    #apply LSH on the min hash signature matrix with the row and band value
    candidate_pairs_tune <- lsh_algorithm(minhash_signatures, band_sample_tune, row_sample_tune)
    
    #flatten the list into a matrix with two columns
    pair_matrix_tune <- matrix(unlist(candidate_pairs_tune), ncol = 2, byrow = TRUE)
    
    model_id_vector <- data_matrix[, 1]
    #count the total number of pairs in the data set
    count <- 0
    model_id_count <- table(model_id_vector)
    model_id_count_matrix <- as.matrix(model_id_count)
    for (i in 1:nrow(model_id_count_matrix)) {
      if (model_id_count_matrix[i, 1] == 2) {
        count <- count + 1
      } else if (model_id_count_matrix[i, 1] == 3) {
        count <- count + 3
      } else if (model_id_count_matrix[i, 1] == 4) {
        count <- count + 6
      }
    }
    #determine which pairs are actual duplicates
    model_id_duplicates_tune <- matrix(0, nrow = nrow(pair_matrix_tune), ncol = ncol(pair_matrix_tune))
    for (i in 1:nrow(pair_matrix_tune)) {
      row_number_true_matrix_pair1_tune <- pair_matrix_tune[i, 1]
      row_number_true_matrix_pair2_tune <- pair_matrix_tune[i, 2]
      model_id_duplicates_tune[i, 1] <- data_matrix[row_number_true_matrix_pair1_tune, 1]
      model_id_duplicates_tune[i, 2] <- data_matrix[row_number_true_matrix_pair2_tune, 1]
    }
    #calculate the correctly found pairs by the LSH
    true_pairs_tune <-0
    model_id_duplicates_counted_tune <- matrix(0, nrow = nrow(model_id_duplicates_tune), ncol = 1)
    for (i in 1:nrow(model_id_duplicates_tune)) {
      if (model_id_duplicates_tune[i, 1] == model_id_duplicates_tune[i, 2]) {
        model_id_duplicates_counted_tune[i, 1] <- 1
        true_pairs_tune <- true_pairs_tune + 1
      } else {
        model_id_duplicates_counted_tune[i, 1] <- 0
      }
    }
    
    #add the counted duplicates as the third column in model_id_duplicates
    model_id_duplicates_tune <- cbind(model_id_duplicates_tune, model_id_duplicates_counted_tune)
    #correclty found pairs
    D_f <- true_pairs_tune
    #total number of found pairs
    N_c <- length(pair_matrix_tune)
    #total amount of pairs in the data
    D_n <- count
    #compute the PC and PQ
    PQ = D_f / N_c
    PC = D_f/D_n
    
    #compute F1 value
    F1value_tune = (2*PC*PQ)/(PQ+PC)
    
    #replacing the optimal tuning value when the F1 value is greater than the optimal value at that moment
    #and collect the corresponding optimal values for the rows and bands
    if(F1value_tune > F1_optimal_rowsbands){
      F1_optimal_rowsbands <- F1value_tune
      row_optimal_tune <- row_sample_tune
      band_optimal_tune<- band_sample_tune
    }
  }  
  #collect the optimal band and row values that gave the highest F1 each bootstrap iteration
  rows_bootstrap_optimal[iteration] <- row_optimal_tune
  bands_bootstrap_optimal[iteration] <- band_optimal_tune
  #perform LSH with the optimal band and row value
  candidate_pairs <- lsh_algorithm(minhash_signatures, band_optimal_tune, row_optimal_tune)
  #flatten the list into a matrix with two columns
  pair_matrix <- matrix(unlist(candidate_pairs), ncol = 2, byrow = TRUE)
  
    #K MEANS--------------------------------------------------------------------------------------------------------------
    #make the pair matrix by LSH as a vector
    vector_of_pairs_by_LSH <- as.vector(pair_matrix)
    
    #find the unique numbers in the vector
    unique_numbers_in_pairs <- unique(vector_of_pairs_by_LSH)
    #initialize an empty matrix to store rows corresponding to unique numbers found in the pairs by LSH
    result_matrix <- matrix(NA, nrow = length(unique_numbers_in_pairs), ncol = nrow(minhash_signatures), dimnames = list(NULL, colnames(minhash_signatures)))
    
    #iterate over the unique numbers found by LSH and put their corresponding minhash signature column in their row
    for (i in 1:length(unique_numbers_in_pairs)) {
      row_number_product <- which(vector_of_pairs_by_LSH == unique_numbers_in_pairs[i])
      result_matrix[i, ] <- minhash_signatures[, row_number_product[1]]  # Assuming you want only the first row for each unique number
    }
    
    #iterate trough the possible cluster values
    for (i in 2:floor(length(unique_numbers_in_pairs)/2)) {
      print(i)
      cluster_sample_tune <- i
      #perform k-means clustering with the cluster value on the result_matrix
      kmeans_result_tune <- kmeans(result_matrix, centers = cluster_sample_tune)
      result_matrix_tune1 <- matrix(nrow = nrow(result_matrix), ncol = ncol(result_matrix) + 1)
      result_matrix_tune1 <- cbind(kmeans_result_tune$cluster,result_matrix)
      result_matrix_tune2 <- matrix(nrow = nrow(result_matrix), ncol = ncol(result_matrix_tune1) + 1)
      result_matrix_tune2 <- cbind(unique_numbers_in_pairs, result_matrix_tune1)
      #get a matrix with the number of the product and the corresponding cluster number
      number_cluster_matrix_tune <- cbind(result_matrix_tune2[,1], result_matrix_tune2[,2])
      
      #get the unique cluster numbers
      unique_clusters_tune <- unique(number_cluster_matrix_tune[, 2])
      
      #initialize a new matrix to store the results of the clusters
      clustered_duplicates_tune <- matrix(0,nrow = length(unique_clusters_tune), ncol = 2)
      
      
      #iterate through each unique cluster number
      for (i in seq_along(unique_clusters_tune)) {
        cluster_number_tune <- unique_clusters_tune[i]
        
        #extract the product numbers for the current cluster
        product_numbers_tune <- number_cluster_matrix_tune[number_cluster_matrix_tune[, 2] == cluster_number_tune, 1]
        
        #store the results in the new matrix
        clustered_duplicates_tune[i, 1] <- cluster_number_tune
        clustered_duplicates_tune[i, 2] <- paste(product_numbers_tune, collapse = ", ")
      }
      
      #get the unique cluster numbers
      unique_clusters_tune <- unique(clustered_duplicates_tune[, 1])
      
      #initialize a new matrix to store the results
      clustered_pairs_tune <- matrix(ncol = 2)
      
      #iterate through each unique cluster
      for (i in seq_along(unique_clusters_tune)) {
        cluster_number_tune <- unique_clusters_tune[i]
        
        #extract the product numbers for the current cluster and split by comma
        product_numbers_tune <- unlist(strsplit(as.character(clustered_duplicates_tune[clustered_duplicates_tune[, 1] == cluster_number_tune, 2]), ", "))
        
        #generate all possible pairs if there are at least 2 products in the same cluster
        if (length(product_numbers_tune) >= 2) {
          pairs_tune <- t(combn(product_numbers_tune, 2))
          
          #append to the new matrix
          clustered_pairs_tune <- rbind(clustered_pairs_tune, pairs_tune)
        }
      }
      
      #remove the first row as this is the initialization row
      clustered_pairs_tune <- clustered_pairs_tune[-1, ]
      
      model_id_vector_tune <- data_matrix[, 1]
      #count the total number of pairs in the dataset
      count_tune <- 0
      model_id_count_tune <- table(model_id_vector_tune)
      model_id_count_matrix_tune <- as.matrix(model_id_count_tune)
      for (i in 1:nrow(model_id_count_matrix_tune)) {
        if (model_id_count_matrix_tune[i, 1] == 2) {
          count_tune <- count_tune + 1
        } else if (model_id_count_matrix_tune[i, 1] == 3) {
          count_tune<- count_tune + 3
        } else if (model_id_count_matrix_tune[i, 1] == 4) {
          count_tune <- count_tune + 6
        }
      }
      #determine which pairs are actual duplicates
      model_id_duplicates_kmeans_tune <- matrix(0, nrow = nrow(clustered_pairs_tune), ncol = ncol(clustered_pairs_tune))
      for (i in 1:nrow(clustered_pairs_tune)) {
        row_number_true_matrix_kmeans_pair1_tune <- as.integer(clustered_pairs_tune[i, 1])
        row_number_true_matrix_kmeans_pair2_tune <- as.integer(clustered_pairs_tune[i, 2])
        model_id_duplicates_kmeans_tune[i, 1] <- data_matrix[row_number_true_matrix_kmeans_pair1_tune, 1]
        model_id_duplicates_kmeans_tune[i, 2] <- data_matrix[row_number_true_matrix_kmeans_pair2_tune, 1]
      }
      #calculate the number of correclty found pairs by kmeans
      true_pairs_kmeans_tune <- 0
      model_id_duplicates_counted_kmeans_tune <- matrix(0, nrow = nrow(model_id_duplicates_kmeans_tune), ncol = 1)
      for (i in 1:nrow(model_id_duplicates_kmeans_tune)) {
        if (model_id_duplicates_kmeans_tune[i, 1] == model_id_duplicates_kmeans_tune[i, 2]) {
          model_id_duplicates_counted_kmeans_tune[i, 1] <- 1
          true_pairs_kmeans_tune <- true_pairs_kmeans_tune + 1
        } else {
          model_id_duplicates_counted_kmeans_tune[i, 1] <- 0
        }
      }
      
      #add the counted duplicates as the third column in model_id_duplicates
      model_id_duplicates_kmeans_tune <- cbind(model_id_duplicates_kmeans_tune, model_id_duplicates_counted_kmeans_tune)
      
    
      #correctly found pairs
      D_f_kmeans_tune <- true_pairs_kmeans_tune
      #total number of found pairs
      N_c_kmeans_tune <- length(clustered_pairs_tune)
      #total amount of pairs in data
      D_n_kmeans_tune <- count_tune
      
      #compute the PQ and PC
      PQ_kmeans_tune <- D_f_kmeans_tune / N_c_kmeans_tune
      PC_kmeans_tune <- D_f_kmeans_tune/D_n_kmeans_tune
      
      if(PQ_kmeans_tune == 0 || PC_kmeans_tune == 0){
        F1_kmeans_value_tune <- 0
      } else{
        #calculate F1 measure
        F1_kmeans_value_tune <- (2*PC_kmeans_tune*PQ_kmeans_tune)/(PC_kmeans_tune+PQ_kmeans_tune)
      }
      #replacing the optimal tuning value when the F1 value is greater than the optimal value at that moment
      #and collect the corresponding optimal cluster value at that moment
      if(F1_kmeans_value_tune > F1_optimal_tune ){
      F1_optimal_tune <- F1_kmeans_value_tune
      row_optimal_tune <- row_sample_tune
      band_optimal_tune<- band_sample_tune
      cluster_optimal_tune <- cluster_sample_tune
      }
    }
    #collect the optimal cluster values that gave the highest F1
  cluster_bootstrap_optimal[iteration] <- cluster_optimal_tune
  
  
  
  #K MEANS--------------------------------------------------------------------------------------------------------------
  #make the pair matrix by LSH as a vector
  vector_of_pairs_by_LSH <- as.vector(pair_matrix)
  
  #find unique numbers in the vector
  unique_numbers_in_pairs <- unique(vector_of_pairs_by_LSH)
  #initialize an empty matrix to store rows corresponding to unique numbers determined by the pair matrix by LSH
  result_matrix <- matrix(NA, nrow = length(unique_numbers_in_pairs), ncol = nrow(minhash_signatures), dimnames = list(NULL, colnames(minhash_signatures)))
  
  #iterate over the unique numbers found by LSH and put their corresponding minhash signature column in their row
  for (i in 1:length(unique_numbers_in_pairs)) {
    row_number_product <- which(vector_of_pairs_by_LSH == unique_numbers_in_pairs[i])
    result_matrix[i, ] <- minhash_signatures[, row_number_product[1]]  # Assuming you want only the first row for each unique number
  }
  
  #perform k-means clustering with k equal to the optimal cluster number
  number_of_clusters <- cluster_optimal_tune
  kmeans_result <- kmeans(result_matrix, centers = number_of_clusters)
  result_matrix <- cbind(kmeans_result$cluster,result_matrix)
  result_matrix <- cbind(unique_numbers_in_pairs, result_matrix)
  #create a matrix with the product number and the cluster number
  number_cluster_matrix <- cbind(result_matrix[,1], result_matrix[,2])
  
  #get the unique cluster numbers
  unique_clusters <- unique(number_cluster_matrix[, 2])
  
  #initialize a new matrix to store the results
  clustered_duplicates <- matrix(0,nrow = length(unique_clusters), ncol = 2)
  
  
  #iterate through each of the unique cluster
  for (i in seq_along(unique_clusters)) {
    cluster_number <- unique_clusters[i]
    
    #extract product numbers for the current cluster
    product_numbers <- number_cluster_matrix[number_cluster_matrix[, 2] == cluster_number, 1]
    
    #store the results in the new matrix
    clustered_duplicates[i, 1] <- cluster_number
    clustered_duplicates[i, 2] <- paste(product_numbers, collapse = ", ")
  }
  
  #obtain the unique cluster numbers
  unique_clusters <- unique(clustered_duplicates[, 1])
  
  #initialize a new matrix to store the results
  clustered_pairs <- matrix(ncol = 2)
  
  #iterate through each of the unique cluster
  for (i in seq_along(unique_clusters)) {
    cluster_number <- unique_clusters[i]
    
    #extract product numbers for the current cluster and split by comma
    product_numbers <- unlist(strsplit(as.character(clustered_duplicates[clustered_duplicates[, 1] == cluster_number, 2]), ", "))
    
    #generate all possible pairs if there are at least 2 products in the same cluster
    if (length(product_numbers) >= 2) {
      pairs <- t(combn(product_numbers, 2))
      
      #add new pairs to the new matrix
      clustered_pairs <- rbind(clustered_pairs, pairs)
    }
  }
  
  #remove the first row as this is the initialization row
  clustered_pairs <- clustered_pairs[-1, ]
  
  
  #Evaluation LSH-------------------------------------------------------------------------------
  #calculating from a matrix how many modelID's are the same
  model_id_vector <- data_matrix[, 1]
  count <- 0
  model_id_count <- table(model_id_vector)
  model_id_count_matrix <- as.matrix(model_id_count)
  for (i in 1:nrow(model_id_count_matrix)) {
    if (model_id_count_matrix[i, 1] == 2) {
      count <- count + 1
    } else if (model_id_count_matrix[i, 1] == 3) {
      count <- count + 3
    } else if (model_id_count_matrix[i, 1] == 4) {
      count <- count + 6
    }
  }
  #determining the actual pairs in the matrix with all the LSH pairs
  model_id_duplicates <- matrix(0, nrow = nrow(pair_matrix), ncol = ncol(pair_matrix))
  for (i in 1:nrow(pair_matrix)) {
    row_number_true_matrix_pair1 <- pair_matrix[i, 1]
    row_number_true_matrix_pair2 <- pair_matrix[i, 2]
    model_id_duplicates[i, 1] <- data_matrix[row_number_true_matrix_pair1, 1]
    model_id_duplicates[i, 2] <- data_matrix[row_number_true_matrix_pair2, 1]
  }
  #calculating the correctly found pairs by kmeans
  true_pairs <-0
  model_id_duplicates_counted <- matrix(0, nrow = nrow(model_id_duplicates), ncol = 1)
  for (i in 1:nrow(model_id_duplicates)) {
    if (model_id_duplicates[i, 1] == model_id_duplicates[i, 2]) {
      model_id_duplicates_counted[i, 1] <- 1
      true_pairs <- true_pairs + 1
    } else {
      model_id_duplicates_counted[i, 1] <- 0
    }
  }
  
  #add the counted duplicates as the third column in model_id_duplicates
  model_id_duplicates <- cbind(model_id_duplicates, model_id_duplicates_counted)
  
  #correctly found pairs
  D_f <- true_pairs
  #total number of found pairs
  N_c <- length(pair_matrix)
  #total amount of pairs in data
  D_n <- count
  
  #compute PQ and PC
  PQ = D_f / N_c
  PC = D_f/D_n
  
  #Compute F1 value
  F1value = (2*PC*PQ)/(PQ+PC)
  #put each obtained F1 value in a vector for each bootstrap interation
  F1[iteration] <- F1value
  
  #Evaluation KMEANS--------------------------------------------------------------------------
  #determining the actual pairs in the matrix with all the clustered pairs
  model_id_duplicates_kmeans <- matrix(0, nrow = nrow(clustered_pairs), ncol = ncol(clustered_pairs))
  for (i in 1:nrow(clustered_pairs)) {
    row_number_true_matrix_kmeans_pair1 <- as.integer(clustered_pairs[i, 1])
    row_number_true_matrix_kmeans_pair2 <- as.integer(clustered_pairs[i, 2])
    model_id_duplicates_kmeans[i, 1] <- data_matrix[row_number_true_matrix_kmeans_pair1, 1]
    model_id_duplicates_kmeans[i, 2] <- data_matrix[row_number_true_matrix_kmeans_pair2, 1]
  }
  #calculating the correctly found pairs by kmeans
  true_pairs_kmeans <- 0
  model_id_duplicates_counted_kmeans <- matrix(0, nrow = nrow(model_id_duplicates_kmeans), ncol = 1)
  for (i in 1:nrow(model_id_duplicates_kmeans)) {
    if (model_id_duplicates_kmeans[i, 1] == model_id_duplicates_kmeans[i, 2]) {
      model_id_duplicates_counted_kmeans[i, 1] <- 1
      true_pairs_kmeans <- true_pairs_kmeans + 1
    } else {
      model_id_duplicates_counted_kmeans[i, 1] <- 0
    }
  }
  
  #add the counted duplicates as the third column in model_id_duplicates
  model_id_duplicates_kmeans <- cbind(model_id_duplicates_kmeans, model_id_duplicates_counted_kmeans)
  
  #correctly found pairs
  D_f_kmeans <- true_pairs_kmeans
  #total number of found pairs
  N_c_kmeans <- length(clustered_pairs)
  #total amount of pairs in data
  D_n_kmeans <- count
  
  #compute PC and PQ
  PQ_kmeans <- D_f_kmeans / N_c_kmeans
  PC_kmeans <- D_f_kmeans/D_n_kmeans
  
  if(PQ_kmeans == 0 || PC_kmeans == 0){
    F1_kmeans_value <- 0
  } else{
    #compute F1 measure
    F1_kmeans_value <- (2*PC_kmeans*PQ_kmeans)/(PC_kmeans+PQ_kmeans)
  }
    #putting each obtained F1 value for kmeans in a vector for each bootstrap iteration
    F1_kmeans[iteration] <- F1_kmeans_value
  
}
#print results
print(F1)
print(F1_kmeans)
avg_F1 <- mean(F1)
avg_F1_kmeans <- mean(F1_kmeans)
print(avg_F1)
print(avg_F1_kmeans)
print(bands_bootstrap_optimal)
print(rows_bootstrap_optimal)
print(cluster_bootstrap_optimal)

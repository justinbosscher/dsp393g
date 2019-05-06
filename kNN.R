# This is a kNN model with a user-defined distance function.

# euc_dist()
# Euclidean Distance
euc_dist <- function(x1, x2) {
    return(sqrt(sum((x1 - x2) ^ 2)))
} # end euc.dist()


# knn_model()
# Takes train and test df's, dist_type
knn_model <- function(train_df, test_df, dist_type, k) {
    
    # Check values of k; k shouldn't be an even integer or greater than the
    # number of columns in train_df
    if(k %% 2 == 0) { stop("Please choose an odd number for k.") }
    if(k >= nrow(train_df)) { stop("Please choose a value for k that is",
                                   "smaller than n-1.", sep="") }
    
    # Create return value, predictions
    predictions <- rep(NA, nrow(test_df))
    
    # Check distance type and call appropriate distance function; save output
    # to vector, distances
    for(row in 1:nrow(test_df)) {
        
        # Use apply() to call appropriate distance formula on train_df by row;
        distances <- apply(train_df[1:(ncol(train_df) - 1)], 1,
                           function(x) euc_dist(x, test_df[row, ]))
        
        # Sort distances maintaining index from min to max
        distances_sorted <- sort(distances, index.return=T)
        
        # Save types from train_df using the indexes in distances_sorted
        nn_k <- train_df[distances_sorted$ix[1:k], ]
        
        # Create frequency table of types in nn_k
        freq_table <- table(nn_k[ , ncol(nn_k)])
        
        # Sort frequency table from min to max
        freq_table_sorted <- sort(freq_table, decreasing=T)
        
        # If there is a tie for most frequent type, take a sample of the types
        # in in nn_k; else, enter the type of first element of sorted frequency
        # table in return value
        if(freq_table_sorted[1] == freq_table_sorted[2]) {
            predictions[row] <- as.character(sample(nn_k[ , ncol(nn_k)]),
                                             size=1)
        } else {
            predictions[row] <- names(freq_table_sorted[1])
        }
    }
    return(predictions)
} # end knn_model()
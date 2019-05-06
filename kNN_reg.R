# This is an kNN Regression model that calls a user-defined distance function

# euc_dist()
euc_dist <- function(x1, x2) {
    return(sqrt(sum((x1 - x2) ^ 2)))
} # end euc.dist()


# knn_model()
# Takes train and test df's, dist_type, and integer k as parameters;
# Returns the average mpg of the k most similar observations
knn_reg_model <- function(train_df, test_df, dist_type, k) {
    
    # Check values of k; k shouldn't be greater than the
    # number of rows in train_df
    if(k >= nrow(train_df)) { stop("Please choose a value for k that is",
                                   "smaller than n-1.", sep="") }
    
    # Create return value, predictions
    predictions <- rep(NA, nrow(test_df))
    
    # Check distance type and call appropriate distance function; save output
    # to vector, distances
    for(row in 1:nrow(test_df)) {
        
        # Use apply() to call appropriate distance formula on train_df by row
        distances <- apply(train_df[1:(ncol(train_df) - 1)], 1,
                           function(x) euc_dist(x, test_df[row, ]))
        
        # Sort distances maintaining index from min to max
        distances_sorted <- sort(distances, index.return=T)
        
        # Save values from train_df using the indexes in distances_sorted
        nn_k <- train_df[distances_sorted$ix[1:k], ncol(train_df)]
        
        predictions[row] <- mean(nn_k)
    }
    return(predictions)
} # end knn_model()
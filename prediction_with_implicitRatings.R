#getting implicit ratings matrix with recosystem package for the MovieLense data

library(recosystem)


#have to: 
#get data into a nx3 matrix where n is total number of ratings
#first columnd is user, second is item, third is rating
#write that df into a txt file
#from there it should be good

num_users<-nrow(data_matrix)
num_item<-ncol(data_matrix)
new_ratings<-matrix(0,nrow=0,ncol=3)

for (u in 1:num_users){
  for (i in 1:num_item){
    if(!is.na(data_matrix[u,i])){
      new_ratings<-rbind(new_ratings,c(u,i,data_matrix[u,i]))
    }
  }
}


# Initialize the recommender object
reco <- Reco()

# Set up training options for SVD++ (takes both explicit & implicit feedback)
opts <- reco$tune(
  train_data = data_memory(user_index=new_ratings[,1],item_index=new_ratings[,2],rating=new_ratings[,3]),
  opts = list(dim = c(10, 20),  # Number of latent factors
              lrate = c(0.05, 0.1),  # Learning rate
              costp_l1 = 0, costp_l2 = c(0.01, 0.1),  # Regularization for user factors
              costq_l1 = 0, costq_l2 = c(0.01, 0.1),  # Regularization for item factors
              niter = 20,  # Number of iterations
              nmf = FALSE,  # Use standard matrix factorization
              svdpp = TRUE)  # Enable SVD++
)

# Train the model using the best parameters
reco$train(data_memory(user_index=new_ratings[,1],item_index=new_ratings[,2],rating=new_ratings[,3]), opts = opts$best)

#now predict over everything
#first: need big matrix

num_entries<-num_users*num_item
pred_matrix<-matrix(0,nrow=num_entries,ncol=2)
for (u in 1:num_users){
  for (i in 1:num_item){
    entry<-u*i
    pred_matrix[entry,1]<-u
    pred_matrix[entry,2]<-i
  }
}

predictions<-reco$predict(data_memory(user_index=pred_matrix[,1],item_index=pred_matrix[,2]))
r_implicit<-matrix(predictions,nrow=num_users,ncol=num_item)

#writing it to a csv so we don't have to go through these steps again
write.csv(r_implicit,file="MovieLense_PredictedRatings_Implicit")

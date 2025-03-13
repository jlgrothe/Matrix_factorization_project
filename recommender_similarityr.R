library(recommenderlab)
data("MovieLense")
data_matrix=as(MovieLense,"matrix")
data_matrix<-data_matrix-rowMeans(data_matrix,na.rm=T)

#doing the funk svd on data_matrix (takes a little bit of time)

fsvd=funkSVD(data_matrix,verbose=TRUE)

r <- tcrossprod(fsvd$U, fsvd$V)

#functions needed

get_cos_sim<-function(i1,i2,data_matrix){
  
  #A function which takes in two indices, 
  #and returns the cosine similarity of the items with those indices
  
  item1<-which(!is.na(data_matrix[,i1]))
  item2<-which(!is.na(data_matrix[,i2]))
  overlap<-intersect(item1,item2)
  if(length(overlap) <= 1){
    return(0)
  }
  i1_overlapr<-data_matrix[overlap,i1]
  i2_overlapr<-data_matrix[overlap,i2]
  dotprod<-as.numeric(i1_overlapr %*% i2_overlapr)
  mag_i1<-sqrt(sum(i1_overlapr*i1_overlapr))
  mag_i2<-sqrt(sum(i2_overlapr*i2_overlapr))
  cosine_sim<-dotprod/(mag_i1*mag_i2)
  return (cosine_sim)
  
}

get_cor<-function(i1,i2,data_matrix){
  
  #A function which takes in two indices,
  #and returns the pearons correlation between the items at those indices
  #(ignoring values not in the overlap)
  
  item1<-which(!is.na(data_matrix[,i1]))
  item2<-which(!is.na(data_matrix[,i2]))
  overlap<-intersect(item1,item2)
  if(length(overlap) <= 1){
    return(0)
  }
  i1_overlapr<-data_matrix[overlap,i1]
  i2_overlapr<-data_matrix[overlap,i2]
  return (cor(i1_overlapr,i2_overlapr,method='pearson'))
  
}

get_jaccard<-function(i1,i2,data_matrix){
  
  #A function which takes in two indices,
  #and returns the Jaccard index between the items at those indices
  
  item1<-which(!is.na(data_matrix[,i1]))
  item2<-which(!is.na(data_matrix[,i2]))
  union<-union(item1,item2)
  overlap<-intersect(item1,item2)
  return (length(overlap)/length(union))
  
}

#TO DO: CREATE FUNCTIONS FOR AMI AND URP

get_rated_items<-function(user,data_matrix){
  
  #A function which takes in a user 
  #and returns the indices of the items that user has rated
  
  rated_indices<-which(!is.na(data_matrix[user,]))
  return(unname(rated_indices))
}

get_unrated_items<-function(user,data_matrix){
  
  #A function which takes in a user
  #and returns the indices of the items that the user hasn't rated
  
  unrated_indices<-which(is.na(data_matrix[user,]))
  return(unname(unrated_indices))
}

get_rec_index<-function(user,Rhat,data_matrix,n){
  
  #A function that takes in a user, 
  #an estimated ratings matrix, 
  #and a number of recommendations to produce
  #and returns the top n items not rated by the user by predicted rating
  
  unrated<-get_unrated_items(user,data_matrix)
  pred_ratings<-Rhat[user,unrated]
  sorted_pred<-order(pred_ratings,decreasing=FALSE)[1:n]
  return(sorted_pred)
  
}

get_rec_sim<-function(user,Rhat,data_matrix,n,similarity){
  
  #A function which takes in a user, 
  #an estimated ratings matrix, 
  #a number of recommendations to produce,
  #and a similarity metric, NOTE: NOT ALL SIMILARITY METRICS ARE ON THE SAME SCALE
  #and produces the recommendations for that user
  #then calculates the similarity between those recommendations
  #and the items the user has already rated
  
  recs<-get_rec_index(user,Rhat,data_matrix,n)
  rated<-get_rated_items(user,data_matrix)
  nrated<-length(rated)
  df <- as.data.frame(matrix(0, nrow = nrated, ncol = n))
  if(similarity == "cosine"){
    for (x in 1:nrated){
      for (i in 1:n){
        df[x,i]=get_cos_sim(rated[x],recs[i],data_matrix)
      }
    }
  }
  if(similarity == "cor"){
    for(x in 1:nrated){
      for (i in 1:n){
        df[x,i]=get_cor(rated[x],recs[i],data_matrix)
      }
    }
  }
  if(similarity == "jaccard"){
    for(x in 1:nrated){
      for (i in 1:n){
        df[x,i]=get_jaccard(rated[x],recs[i],data_matrix)
      }
    }
  }
  return(df)
}

rec_sim_stats<-function(Rhat, data_matrix, n, similarity, stat){
  
  #A function which takes in a predicted ratings matrix,
  #an original data_matrix, a number of top recommendations,
  #a similarity metric and a statistic
  #and returns a data frame with n rows and columns equal to the number of users
  #in the original data_matrix
  #which holds a statistic about the similarity between a top recommendation for a user
  #and all the items that user has rated
  #note: this is kind of computationally intensive, ~3 min for cosine and mean
  #note: more efficient to do this without if statements? 
  #note: need to add more options for similarity and stat
  
  num_users<-length(data_matrix[,1])
  stat_df<-as.data.frame(matrix(0, nrow = 10, ncol = num_users))
  if(similarity=="cosine" && stat=="mean"){
    for (i in 1:num_users){
      sim_df<-get_rec_sim(i,r,data_matrix,10,"cosine")
      for (n in 1:10){
        stat_df[n,i]<-mean(sim_df[,n])
      }
    }
  }
  return(stat_df)
}

num_users<-length(data_matrix[,1])
mean_cosine_df<-as.data.frame(matrix(0, nrow = 10, ncol = num_users))
for (i in 1:num_users){
  sim_df<-get_rec_sim(i,r,data_matrix,10,"cosine")
  for (n in 1:10){
    mean_cosine_df[n,i]<-mean(sim_df[,n])
  }
  print(i)
}

#examples of what the cor, cosine, and jaccard similarity to recommendations matrices look like

u124_cordf<-get_rec_sim(124,r,data_matrix,10,"cor")
u124_simdf<-get_rec_sim(124,r,data_matrix,10,"cosine")
u124_jacdf<-get_rec_sim(124,r,data_matrix,10,"jaccard")

#getting the matrix of the mean cosine similarity between 
#each of the top 10 items recommended for a user
#and all the items that user has rated
#for all the users in the dataset

mean_cos_df<-rec_sim_stats(r,data_matrix,10,"cosine","mean")


  

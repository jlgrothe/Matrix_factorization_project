#Visualizations produced using reommender_similarityr
#Assumes you have already produced dataframes with 
#rows for each user
#n columns for the similarity of the top n ratings, 
#column n+1 is the number of items that user has rated

library(ggplot2)

#creating a plot of the similarity of the top user on the y axis 
#and number of items rated for that user on the x axis

ggplot(mean_cos_df,aes(x=V11,y=V1))+geom_point()

#potential interesting visualizations: 
#difference between the recommendation that is on average most similar to what users rated
#and the recommendation that is on average the least similar to what users rated

get_most_similar<-function(stat_df){
  nrecs<-ncol(stat_df)-1
  num_users<-nrow(stat_df)
  max_sim<-numeric(num_users)
  for (u in 1:num_users){
    max<-max(stat_df[u,1:nrecs])
    max_sim[u]<-max
  }
  return (max_sim)
  
}

get_least_similar<-function(stat_df){
  nrecs<-ncol(stat_df)-1
  num_users<-nrow(stat_df)
  min_sim<-numeric(num_users)
  for (u in 1:num_users){
    min<-min(stat_df[u,1:nrecs])
    min_sim[u]<-min
  }
  return (min_sim)
}

most_similar<-get_most_similar(mean_cos_df)
least_similar<-get_least_similar(mean_cos_df)
diff<-most_similar-least_similar
diff_df<-data.frame(mean_cos_df[,11],most_similar,least_similar,diff)
colnames(diff_df)<-c("Num_rated","Most_similar","Least_similar","Difference")

ggplot(diff_df,aes(x=Num_rated,y=Difference))+geom_point()
ggplot(diff_df,aes(x=Num_rated,y=Most_similar))+geom_point()
ggplot(diff_df,aes(x=Num_rated,y=Least_similar))+geom_point()


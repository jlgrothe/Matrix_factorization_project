#Visualizations produced using reommender_similarityr
#Assumes you have already produced dataframes with 
#rows for each user
#n columns for the similarity of the top n ratings, 
#column n+1 is the number of items that user has rated

library(ggplot2)
library(ggpubr)

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

get_diff_vis<-function(stat_df){
  num_ratings_index<-ncol(stat_df)
  most_similar<-get_most_similar(stat_df)
  least_similar<-get_least_similar(stat_df)
  diff<-most_similar-least_similar
  diff_df<-data.frame(stat_df[,num_ratings_index],most_similar,least_similar,diff)
  colnames(diff_df)<-c("Num_rated","Most_similar","Least_similar","Difference")
  
  diff_plot<-ggplot(diff_df,aes(x=Num_rated,y=Difference))+geom_point()+ylim(0,0.75)
  most_plot<-ggplot(diff_df,aes(x=Num_rated,y=Most_similar))+geom_point()+ylim(0,0.75)
  least_plot<-ggplot(diff_df,aes(x=Num_rated,y=Least_similar))+geom_point()+ylim(0,0.5)
  ggarrange(most_plot,least_plot,diff_plot,
    labels=c("Most similar on average","Least similar on average","Difference between most and least similar"),
    ncol=2,nrow=2)
}
#to do: rescale so that each plot is the same scale

get_diff_vis(mean_cos_df)
get_diff_vis(mean_cor_df)
get_diff_vis(mean_jac_df)

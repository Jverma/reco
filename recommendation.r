#load data
#data = item,user,rating

train <- read.csv("input.csv", header=TRUE)

#find common rators of two items
common_reviewers <- function(item1, item2){
	reviews2 <- subset(train, item==item2)
 	reviews1 <- subset(train, item==item1)
 	reviewers_sameset <- intersect(reviews1[,'user'], reviews2[, 	'user'])
 	if (length(reviewers_sameset) == 0){
 		NA
	 	}
 	else{
	 	reviewers_sameset
	 	}
 }

#get ratings of an item by a set of users. For distance similarity
#which is used in this file, userset is just one user. But similarity
#with a user set is helpful e.g. in correlation based similarity. 

get_reviews <- function(item1, userset){
	item.data <- subset(train, item==item1 & user_id %in% userset)
 	ordered <- order(item.data$user)
 	item.data <- item.data[ordered,]
 	duplicates <- duplicated(item.data$user)==FALSE
 	item.data <- item.data[duplicates,]
 	item.data[,'rating']
 }

#distance similarity between two items

sim_distance <- function(b1, b2){
	common_users <- common_reviewers(b1, b2)
 	sq_sum = 0
 	for (user in common_users){
 	sq_diff = (get_reviews(b1, user) - get_reviews(b2, user))**2
 	sq_sum = sq_sum + sq_diff
	 }
 	return (sq_sum)
 }


#similarity between all pairs of item 
item.pairs <- expand.grid(b1=train$items, b2=train$items)
item.pairs <- subset(item.pairs, b1!=b2)
library(plyr)
similarity <- ddply(item.pairs, .(b1,b2), function(x){
	c("sim" = sim_distance(b1,b2))
}
)


#most similar items
similar_items <- function("myitem", n=10){
	most_sim <- subset(similarity, b1=="myitem")[1:n, ]	
	return (most_sim)
	}
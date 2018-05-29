# Read the files first

rm(list = ls())
#install.packages("recommenderlab")
#install.packages("recosystem")
# install.packages("ggthemes")
library("ISLR")
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(plyr)
library(devtools)


gender = read.table("C:/Users/jadha/Desktop/Project/libimseti/gender.dat", header = FALSE, sep = ",")
gender = data.frame(gender)
names(gender) = c("UserID", "Gender")
summary(gender)
gender_count = count(gender, c('Gender'))
gender_count
x11()
barplot(prop.table(table(gender$Gender)),xlab = "Gender",ylab = "Probabbility", main = "Gender Probability", col = c("Pink","Blue","Black"))
#hist(as.numeric(gender$Gender), main = "Gender Frequency", xlab = "Gender")+ theme(axis.text.x= element_text(family, face, colour, size))
#axis(1, at = xticks,col.axis="blue", las=2, levels = 1:3,
 #    labels = c("First", "Second", "Third"))

ratings1 = read.table("C:/Users/jadha/Desktop/Project/libimseti/ratings.dat", header = FALSE, sep = ",")
ratings = data.frame(ratings1)
rating_matrix <- as.matrix(ratings1)
names(ratings) = c("UserID", "Profile", "Rating")
summary(ratings)

# Histogram

#is.na.data.frame(ratings)
#x = na.omit(ratings)
merged_dataset = merge(ratings, gender, by.x = "UserID", by.y = "UserID")

full_dataset = merge(merged_dataset, gender, by.x = "Profile", by.y = "UserID")

full = full_dataset[,c("Profile","UserID", "Gender.y","Gender.x","Rating")]
names(full) = c("UserID","Profile","Gender_user","Gender_profile","Ratings")
# The computational is very intense and high
#dataset = merged_dataset[sample(nrow(merged_dataset), 100000), ] # 100 thousand records

# mean of ratings
mean_ratings_user = aggregate(full$Ratings~full$Gender_user, full, mean)
mean_ratings_profile = aggregate(full$Ratings~full$Gender_profile, full, mean)

# Create separate dataframes
male_male = full[full$Gender_user =="M" & full$Gender_profile =="M",]
female_female = full[full$Gender_user =="F" & full$Gender_profile =="F",]
male_female = full[full$Gender_user =="M" & full$Gender_profile =="F",]
female_male = full[full$Gender_user =="F" & full$Gender_profile =="M",]

######Creating Graphs for the data 
male_male_mean = aggregate(male_male$Ratings~male_male$Gender_profile, male_male, mean)
female_female_mean = aggregate(female_female$Ratings~female_female$Gender_profile, female_female, mean)
male_female_mean = aggregate(male_female$Ratings~male_female$Gender_profile, male_female, mean)
female_male_mean = aggregate(female_male$Ratings~female_male$Gender_profile, female_male, mean)

######Plotting the scores according to Gender-Gender 
# Male - Male 
x11()
ggplot(data=male_male, aes(male_male$Ratings)) + 
  geom_histogram( breaks=seq(1, 10, by = 1),
                 col="black", 
                 fill=("blue4"), 
                 alpha = .2,bins = 10) + 
  labs(title="Histogram for Male - Male Ratings") +
  labs(x="Ratings", y="Count")

#Female - Female
x11()
ggplot(data=female_female, aes(female_female$Ratings)) + 
  geom_histogram( 
    col="black", 
    fill=I("deeppink"), 
    alpha = .2,bins = 10) + 
  labs(title="Histogram for Female - Female Ratings") +
  labs(x="Ratings", y="Count")

#Male - Female

x11()
ggplot(data=male_female, aes(male_female$Ratings)) + 
  geom_histogram( 
    col="black", 
    fill=I("blueviolet"), 
    alpha = .2,bins = 10) + 
  labs(title="Histogram for Male - Female Ratings") +
  labs(x="Ratings", y="Count")

#Female - Male
x11()
ggplot(data=female_male, aes(female_male$Ratings)) + 
  geom_histogram( 
    col="black", 
    fill=I("darkorchid"), 
    alpha = .2,bins = 10) + 
  labs(title="Histogram for Female - Male Ratings") +
  labs(x="Ratings", y="Count")

###########################################################################################################################
####Building Recommender System 

#Data Normalization 
affinity.matrix <- as(ratings, "realRatingMatrix")
affinity.matrix

dim(getRatingMatrix(affinity.matrix))
getRatingMatrix(affinity.matrix)[500:1050,50:100]

e = evaluationScheme(affinity.matrix[1:1000,],method="split", train=0.9,k=1,given=15)

#Cosine Similarity with no normaltization
c_model_non_normalized <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = NULL, method = "Cosine"))

#Cosine with Centered Normalization 
c_model_center <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = "center", method = "Cosine"))

#Cosine with Z-score normalization
c_model_z_score <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = "Z-score", method = "Cosine"))


#Euclidean Distance as Similarity with no normaltization
e_model_non_normalized <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = NULL, method = "Euclidean"))

#Euclidean Distance as Similarity with Centered Normalization 
e_model_center <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = "center", method = "Euclidean"))

#Euclidean Distance as Similarity with Z-score normalization
e_model_z_score <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = "Z-score", method = "Euclidean"))


#Pearson Correlation as Similarity metrics with no normaltization
p_model_non_normalized <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = NULL, method = "Pearson"))

#Pearson Correlation as Similarity metrics with Centered Normalization 
p_model_center <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = "center", method = "Pearson",n=50))

#Pearson Correlation as Similarity metrics with Z-score normalization
p_model_z_score <- Recommender(getData(e, "train"),method = "UBCF", param = list(normalize = "Z-score", method = "Pearson"))

###########################################################################################################################
# Predict function

#Cosine Similarity with no normaltization
c_non_normalized_predict <- predict(c_model_non_normalized, getData(e, "known"), type="ratings")
#as(c_non_normalized_predict,"matrix")[,1:5]
error_c_non_normalized = rbind(UBCF_c_non_normalized = calcPredictionAccuracy(c_non_normalized_predict, getData(e,"unknown")))
error_c_non_normalized

#Cosine with Centered Normalization 
c_center_normalized_predict <- predict(c_model_center, getData(e, "known"), type="ratings")
as(c_center_normalized_predict,"matrix")[,1:5]
error_c_center_normalized = rbind(UBCF_c_center_normalized = calcPredictionAccuracy(c_center_normalized_predict, getData(e,"unknown")))
error_c_center_normalized

#Cosine with Z-score normalization
c_zscore_normalized_predict <- predict(c_model_z_score, getData(e, "known"), type="ratings")
as(c_zscore_normalized_predict,"matrix")[,1:5]
error_c_zscore_normalized = rbind(UBCF_c_zscore_normalized = calcPredictionAccuracy(c_zscore_normalized_predict, getData(e,"unknown")))
error_c_zscore_normalized

#Euclidean Distance as Similarity with no normaltization
e_non_normalized_predict <- predict(e_model_non_normalized, getData(e, "known"), type="ratings")
#as(c_non_normalized_predict,"matrix")[,1:5]
error_e_non_normalized = rbind(UBCF_e_non_normalized = calcPredictionAccuracy(e_non_normalized_predict, getData(e,"unknown")))
error_e_non_normalized

#Euclidean Distance as Similarity with Centered Normalization 
e_center_normalized_predict <- predict(e_model_center, getData(e, "known"), type="ratings")
as(e_center_normalized_predict,"matrix")[,1:5]
error_e_center_normalized = rbind(UBCF_e_center_normalized = calcPredictionAccuracy(e_center_normalized_predict, getData(e,"unknown")))
error_e_center_normalized

#Euclidean Distance as Similarity with zscore normalization
e_zscore_normalized_predict <- predict(e_model_z_score, getData(e, "known"), type="ratings")
as(e_zscore_normalized_predict,"matrix")[,1:5]
error_e_zscore_normalized = rbind(UBCF_e_zscore_normalized = calcPredictionAccuracy(e_zscore_normalized_predict, getData(e,"unknown")))
error_e_zscore_normalized

#Pearson Correlation as Similarity metrics with no normaltization
p_non_normalized_predict <- predict(p_model_non_normalized, getData(e, "known"), type="ratings")
as(p_non_normalized_predict,"matrix")[,1:5]
error_p_non_normalized = rbind(UBCF_p_non_normalized = calcPredictionAccuracy(p_non_normalized_predict, getData(e,"unknown")))
error_p_non_normalized

#Pearson Correlation as Similarity metrics with Centered Normalization
p_center_normalized_predict <- predict(p_model_center, getData(e, "known"), type="ratings")
as(p_center_normalized_predict,"matrix")[,1:5]
error_p_center_normalized = rbind(UBCF_p_center_normalized = calcPredictionAccuracy(p_center_normalized_predict, getData(e,"unknown")))
error_p_center_normalized

#Pearson Correlation as Similarity metrics with Z-score normalization
p_zscore_normalized_predict <- predict(p_model_z_score, getData(e, "known"), type="ratings")
as(p_zscore_normalized_predict,"matrix")[,1:5]
error_p_zscore_normalized = rbind(UBCF_p_zscore_normalized = calcPredictionAccuracy(p_zscore_normalized_predict, getData(e,"unknown")))
error_p_zscore_normalized


###############################################################################

#Top 10 recommendation for 5 users
# Popularity
recom <- Recommender(affinity.matrix[1:5000],method = "Popular")
names(getModel(recom))
getModel(recom)$topN
x = predict(recom, affinity.matrix[10:14],n=10)
recom_list = as(x,"list")
recom_list

#Top 10 recommendation for 5 users
# Pearson correlation with centerd normalization
x1 = predict(p_model_center, affinity.matrix[10:14],n=10)
recom_list1 = as(x1,"list")
recom_list1


# Predict the ratings for five users
x = predict(recom, affinity.matrix[10:15],type="ratings")
recom_list = as(x,"matrix")[,1:8]
recom_list




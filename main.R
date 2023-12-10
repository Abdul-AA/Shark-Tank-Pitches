df=read.csv("shark_tank.csv")

# Data Pre-processing

df$deal=ifelse(df$deal==TRUE,1,0)
df$deal=as.factor(df$deal)


df$Multiple.Entreprenuers=ifelse(df$Multiple.Entreprenuers==TRUE,1,0)
df$Multiple.Entreprenuers=as.factor(df$Multiple.Entreprenuers)

df$shark1=as.factor(df$shark1)
df$shark2=as.factor(df$shark2)
df$shark3=as.factor(df$shark3)
df$shark4=as.factor(df$shark4)
df$shark5=as.factor(df$shark5)


# Checking the shape of the data
dim(df)
head(df)

# Checking the descriptive stats
summary(df)
library(stargazer)
stargazer(df,type='latex', title = 'Summary Statistics of The Entire Dataset',digits=2)

library(ggplot2)

# Count plot for class distribution
ggplot(df, aes(deal)) +
  geom_bar(fill = "grey")+ labs(title='Class Distribution of Target Variable',x='Deal (1 for Deal, 0 for No Deal)')

# Bivariate and Univariate analyses
library(scales)  # Loading the scales package for formatting axes as currency

ggplot(df, aes(x = valuation, y = exchangeForStake)) +
  geom_point() +
  labs(title = "Scatter Plot of Business Valuation vs. Percentage of Stake Offered",
       x = "Business Valuation (in millions)",
       y = "Percentage of Stake Offered") +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Clustering for further insights

# clustering with business valuation and stake offered
df_c=df[,c(9,10)]

km=kmeans(df_c,5)
km

# validation
km$tot.withinss
km$betweenss

# Checking centroids
km$centers
df_c$cluster=as.factor(km$cluster)
attach(df_c)
df_c
ggplot(df_c, aes(x = valuation, y = exchangeForStake, color = cluster)) +
  geom_point() +
  labs(title = "Clustering based on Business Valuation and Stake Offered",
       x = "Business Valuation (In Millions)",
       y = "Stake Offered")+
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


# Checking cluster group counts
table(df_c$cluster)

# Labeling the original dataframe with their respective cluster labels
df$cluster=as.factor(km$cluster)

# Most frequent category by cluster
library(dplyr)

most_frequent_by_cluster = df %>%
  group_by(cluster, category) %>%
  summarise(count = n()) %>%
  top_n(1, wt = count) %>%
  ungroup()

most_frequent_by_cluster

# Data Manipulation required for the rest of the project
# Checking the group counts of categories
table(df$category)
# Creating a mapping to regroup categories
category_mapping = list(
  "Beverages" = c("Alcoholic Beverages", "Non-Alcoholic Beverages", "Water Bottles", "Wine Accessories"),
  "Apparel and Accessories" = c("Men and Women's Apparel","Men and Women's Accessories","Women's Apparel", "Women's Accessories", "Undergarments and Basics", "Men's Accessories", "Fashion Accessories",  "Men and Women's Shoes", "Women's Shoes"),
  "Health and Wellness" = c( "Fitness Programs","Fitness Apparel and Accessories", "Fitness Equipment", "Health and Well-Being", "Maternity", "Personal Care and Cosmetics", "Homeopathic Remedies"),
  "Services" = c("Consumer Services", "Education", "Online Services", "Professional Services", "Home Security Solutions"),
  "Home Products" = c("Home Accessories","Kitchen Tools","Furniture", "Storage and Cleaning Products", "Home Improvement"),
  "Baby Products" = c("Baby and Children's Bedding","Baby and Children's Food","Baby and Children's Entertainment","Baby and Child Care","Baby and Children's Apparel and Accessories"),
  "Other" = c( "Specialty Food", "Productivity Tools", "Automotive", "Music", "Entertainment", "Toys and Games", "Pet Products", "Golf Products", "Outdoor Recreation", "Gardening",  "Weddings", "Party Supplies", "Pest Control", "Mobile Apps", "Costumes", "Holiday Cheer", "Cycling")
)

# Reassigning the categories to their new groups
for (group in names(category_mapping)) {
  df$category[df$category %in% category_mapping[[group]]] <- group
}
# Checking the new group count
table(df$category)
df$category=as.factor(df$category)

## Frequency plot for business category
ggplot(df, aes(category)) +
  geom_bar(fill = "grey")+ labs(title='Frequency Plot of Business Category')


attach(df)




# Business Valuation
summary(df$valuation)

ggplot(df, aes(x = "", y = valuation)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot of Business Valuation",
       y = "Business Valuation (in millions)") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6)) +
  theme(axis.title.x = element_blank())

# Business Valuation by categories
#install.packages("doBy")
library(doBy)
summary_valuation_by_category <- summaryBy(valuation ~ category, data = df, FUN = c(mean, sd, median, min, max))
summary_valuation_by_category

ggplot(df, aes(x = category, y = valuation)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = " Business Valuation by Business Category",
       x = "Business Category",
       y = "Business Valuation (in millions)") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6)) 

# Stakes offered

# Summary of Stakes
summary(df$exchangeForStake)

# Boxplot of Stakes
ggplot(df, aes(x = "", y = exchangeForStake)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot of Percentage of Stake Offered",
       y = "Percentage of Stake Offered") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.title.x = element_blank())

# Summary of Stakes by Categories
summary_stakes_by_category = summaryBy(exchangeForStake ~ category, data = df, FUN = c(mean, sd, median, min, max))
summary_stakes_by_category

# Boxplot of Stakes by Categories
ggplot(df, aes(x = category, y = exchangeForStake)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot of Percentage of Stake Offered by Business Category",
       x = "Business Category",
       y = "Percentage of Stake Offered") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Further Explorations
table(df$shark1)
table(df$shark2)
table(df$shark3)
table(df$shark4)
table(df$shark5)



# Model Building

## Bagging 

library(caret)
library(randomForest)
formular= deal ~ exchangeForStake + askedFor+ valuation+shark1+shark2+shark3+shark4+shark5 + Multiple.Entreprenuers + category 

classifiedforest=randomForest(formular, cp=0.1, na.action=na.omit, )
classifiedforest

# Feature Importance
importance(classifiedforest)
varImpPlot(classifiedforest)


## Boosting 

set.seed(42)
library(gbm)

# Using validation set cross validation to assess the model's generalization
set.seed(42)

# Splitting the data into training (80%) and validation (20%) sets
set_ratio = 0.7
index = createDataPartition(df$deal, p = set_ratio, list = FALSE)
train_data = df[index, ]
validation_data = df[-index, ]

set.seed(42)
# Training the gbm model on the training set
model_gbm = gbm(formular,
                data = train_data,
                distribution = "multinomial",
                shrinkage = 0.01,
                interaction.depth = 3,
                n.minobsinnode = 3,
                n.trees = 150,
                verbose = TRUE)

# Predicting on the validation set
pred_validation = predict.gbm(object = model_gbm,
                              newdata = validation_data,
                              n.trees = 150,
                              type = "response")

# Giving class names to the highest prediction value.
class_names_validation = colnames(pred_validation)[apply(pred_validation, 1, which.max)]
result_validation = data.frame(validation_data$deal, class_names_validation)

print(result_validation)

# Creating confusion matrix for the validation set
conf_mat_validation = confusionMatrix(validation_data$deal, as.factor(class_names_validation))
print(conf_mat_validation)

## Using LOOCV
set.seed(42)
ctrl <- trainControl(method = "LOOCV")


# Train the gbm model with k-fold cross-validation
model_gbm_cv <- train(formular, 
                      data = df,
                      method = "gbm",
                      trControl = ctrl,
                      metric = "Accuracy",  
                      distribution = "bernoulli"
                      
                      )
print(model_gbm_cv)

# Plot the results of the grid search
ggplot(model_gbm_cv)+labs(x='Number of Trees',title='Grid Search Result',y='Accuracy (LOOCV)' )
# Accessing the final model from LOOCV
final_model_loocv <- model_gbm_cv$finalModel
stargazer(summary(final_model_loocv), type='latex')

# Applying the best set of hyperparameters on the entire dataset
model_gbm = gbm(formular,
                data = df,
                distribution = "multinomial",
                shrinkage = .1,
                interaction.depth = 2,
                n.minobsinnode = 10,
                n.trees = 100)       
summary(model_gbm)

pred_test = predict.gbm(object = model_gbm,
                        newdata = df,
                        n.trees = 500,           
                        type = "response")

pred_test

# Giving class names to the highest prediction value.
class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
result = data.frame(df$deal, class_names)

print(result)
# Creating confusion matrix
conf_mat = confusionMatrix(df$deal, as.factor(class_names))
print(conf_mat)

fourfoldplot(as.table(conf_mat),color=c("red","green"),main = "Confusion Matrix")


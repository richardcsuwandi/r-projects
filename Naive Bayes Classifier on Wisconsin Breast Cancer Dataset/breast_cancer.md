Naive Bayes Classifier on Wisconsin Breast Cancer Dataset
================

This project is one of my R projects series. In this project, we have a Wisconsin breast cancer dataset reported by Dr. Wolberg that consists of 699
observations with 11 variables. A more detailed description of the
dataset can be found
[here](https://www.rdocumentation.org/packages/mlbench/versions/2.1-1/topics/BreastCancer)

The goal of this project is to create a Naive Bayes Classifier model to
predict whether a cancer is malignant or benign from biopsy details. The source code of this project can also be found [here](https://github.com/richardcsuwandi/r-projects/blob/master/Naive%20Bayes%20Classifier%20on%20Wisconsin%20Breast%20Cancer%20Dataset/breast_cancer.R) 

``` r
# Import the libraries
library(mlbench)  # For the dataset
library(mice)  # For handling missing values
library(caret)  # For training the model
library(e1071)  # For implementing the Naive Bayes algorithm
```

## Loading the Data

First, we need to import the dataset from the mlbench library.

``` r
# Import the data
data(BreastCancer)
```

## Exploratory Data Analysis (EDA)

Before begin fitting our model, let’s explore and understand our data.

``` r
# Get the structure of the data
str(BreastCancer)
```

    ## 'data.frame':    699 obs. of  11 variables:
    ##  $ Id             : chr  "1000025" "1002945" "1015425" "1016277" ...
    ##  $ Cl.thickness   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
    ##  $ Cell.size      : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
    ##  $ Cell.shape     : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
    ##  $ Marg.adhesion  : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
    ##  $ Epith.c.size   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
    ##  $ Bare.nuclei    : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
    ##  $ Bl.cromatin    : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
    ##  $ Normal.nucleoli: Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
    ##  $ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
    ##  $ Class          : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...

``` r
# Get the levels of the target 'Class'
levels(BreastCancer$Class)
```

    ## [1] "benign"    "malignant"

We can obtain a more detailed summary of the dataset using the summary()
function.

``` r
# View the summary of the data
summary(BreastCancer)
```

    ##       Id             Cl.thickness   Cell.size     Cell.shape  Marg.adhesion
    ##  Length:699         1      :145   1      :384   1      :353   1      :407  
    ##  Class :character   5      :130   10     : 67   2      : 59   2      : 58  
    ##  Mode  :character   3      :108   3      : 52   10     : 58   3      : 58  
    ##                     4      : 80   2      : 45   3      : 56   10     : 55  
    ##                     10     : 69   4      : 40   4      : 44   4      : 33  
    ##                     2      : 50   5      : 30   5      : 34   8      : 25  
    ##                     (Other):117   (Other): 81   (Other): 95   (Other): 63  
    ##   Epith.c.size  Bare.nuclei   Bl.cromatin  Normal.nucleoli    Mitoses   
    ##  2      :386   1      :402   2      :166   1      :443     1      :579  
    ##  3      : 72   10     :132   3      :165   10     : 61     2      : 35  
    ##  4      : 48   2      : 30   1      :152   3      : 44     3      : 33  
    ##  1      : 47   5      : 30   7      : 73   2      : 36     10     : 14  
    ##  6      : 41   3      : 28   4      : 40   8      : 24     4      : 12  
    ##  5      : 39   (Other): 61   5      : 34   6      : 22     7      :  9  
    ##  (Other): 66   NA's   : 16   (Other): 69   (Other): 69     (Other): 17  
    ##        Class    
    ##  benign   :458  
    ##  malignant:241  
    ##                 
    ##                 
    ##                 
    ##                 
    ## 

As we can see from the summary above, we have 16 NA (missing values) in
the dataset. So, we have to clean our data first.

## Cleaning the Data

We can use the mice library to handle the 16 missing values by imputing
them with the most suited values considering all nine predicting columns
in the dataset.

``` r
# Here, we don't need the 'Id' and 'Class' column
data_cleaned <- mice(BreastCancer[,2:10], print = FALSE)

# Add the 'Class' column to the cleaned data
BreastCancer <- cbind(BreastCancer[,11, drop = FALSE], mice::complete(data_cleaned, 1))
```

Check the summary for the cleaned data.

``` r
summary(BreastCancer)
```

    ##        Class      Cl.thickness   Cell.size     Cell.shape  Marg.adhesion
    ##  benign   :458   1      :145   1      :384   1      :353   1      :407  
    ##  malignant:241   5      :130   10     : 67   2      : 59   2      : 58  
    ##                  3      :108   3      : 52   10     : 58   3      : 58  
    ##                  4      : 80   2      : 45   3      : 56   10     : 55  
    ##                  10     : 69   4      : 40   4      : 44   4      : 33  
    ##                  2      : 50   5      : 30   5      : 34   8      : 25  
    ##                  (Other):117   (Other): 81   (Other): 95   (Other): 63  
    ##   Epith.c.size  Bare.nuclei   Bl.cromatin  Normal.nucleoli    Mitoses   
    ##  2      :386   1      :412   2      :166   1      :443     1      :579  
    ##  3      : 72   10     :133   3      :165   10     : 61     2      : 35  
    ##  4      : 48   2      : 32   1      :152   3      : 44     3      : 33  
    ##  1      : 47   5      : 30   7      : 73   2      : 36     10     : 14  
    ##  6      : 41   3      : 29   4      : 40   8      : 24     4      : 12  
    ##  5      : 39   8      : 21   5      : 34   6      : 22     7      :  9  
    ##  (Other): 66   (Other): 42   (Other): 69   (Other): 69     (Other): 17

## Splitting the Data

Next, we are going to split our data into training and test sets, in a
70:30 ratio.

``` r
# For the sake of reproducibility, we set a random seed number
set.seed(365)

# Perform a random split
train_index <- createDataPartition(BreastCancer$Class, p = 0.7, list = FALSE)
train_set <- BreastCancer[train_index,]
test_set <- BreastCancer[-train_index,]

# Remove the target 'Class' in the test sets
test_set_predict <- test_set[2:10]
```

Let’s see the dimension of our train and test sets

``` r
dim(train_set)
```

    ## [1] 490  10

``` r
dim(test_set_predict)
```

    ## [1] 209   9

## Building the Model

Finally, we can build our Naive Bayes Classifier model and fit it to our
data.

``` r
# Build and fit the model
model <- naiveBayes(Class ~ ., data = train_set)

# Make the predictions
preds <- predict(model, newdata = test_set_predict)
```

To evaluate our model, we can create a confusion matrix.

``` r
conf_matrix <- table(preds, test_set$Class)
confusionMatrix(conf_matrix)
```

    ## Confusion Matrix and Statistics
    ## 
    ##            
    ## preds       benign malignant
    ##   benign       133         1
    ##   malignant      4        71
    ##                                           
    ##                Accuracy : 0.9761          
    ##                  95% CI : (0.9451, 0.9922)
    ##     No Information Rate : 0.6555          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9475          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.3711          
    ##                                           
    ##             Sensitivity : 0.9708          
    ##             Specificity : 0.9861          
    ##          Pos Pred Value : 0.9925          
    ##          Neg Pred Value : 0.9467          
    ##              Prevalence : 0.6555          
    ##          Detection Rate : 0.6364          
    ##    Detection Prevalence : 0.6411          
    ##       Balanced Accuracy : 0.9785          
    ##                                           
    ##        'Positive' Class : benign          
    ## 

The confusion matrix shows that our Naive Bayes Classifier model
predicted 133 benign cases correctly with only 1 wrong prediction.
Similarly, the model predicted 71 malignant cases correctly with 4 wrong
predictions.

The accuracy of our model is 97.61%

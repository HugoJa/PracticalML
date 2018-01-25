---
title: "Practical ML"
author: "Hugo Jaouen"
date: "23 janvier 2018"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


### Summary
We will try to build machine learning algorithm to predicting human activity using data from wearable sensors. We will use HAR dataset: http://groupware.les.inf.puc-rio.br/har. Downloadable data files: [training](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and [testing](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)



Read the training data into a data table.

```{r}
library(data.table)
urltrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(urltrain, destfile = "pml-training.csv", mode = "wb")
Datatrain  <- read.csv("pml-training.csv")
```

Read the testing data into a data table.

```{r}
urltest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(urltest, destfile = "pml-testing.csv", mode = "wb")
Datatest  <- read.csv("pml-testing.csv")
```




### Partitioning and exploration

```{r results='hide', message=F }


library(caret)
set.seed(1234)
inTraining <- createDataPartition(Datatrain$X, p = 0.8, list = F)
train <- Datatrain[inTraining,]
test <- Datatrain[-inTraining,]
```

Some columns almost all values are NA :

```{r}
summary(train[,c('new_window', 'avg_roll_belt', 'min_roll_dumbbell')])
```

Now for the model building we will do first a feature selection.
If the performance is'nt enough we could do imputation.


### Modeling

Let's try the easy way first:

* Let's delete the columns full of NA 

* Let's delete names and time data (assuming you devellop a model for new data and not a model for each individual)

```{r}
usefulness.treshold <- nrow(train) * 0.95
useful.columns <- apply(train, 2, function(x){
      sort(table(x, useNA='always'), decreasing=T)[1] < usefulness.treshold})
useful.columns[1:7] <- F; # remove user names and time data

train <- train[,useful.columns]
```

Let's try Random forest first. Powerful and easy to apply.


```{r}
table(c( complete.cases(Datatrain[,useful.columns]), complete.cases(Datatest[,useful.columns]) ))
```

The data is cylindric, no need to preprocessing.
Let's begin with a first model without feature eingenering

```{r echo=F, message=F}
library(randomForest)
```

```{r cache=T, message=F, results='hold'}
library(randomForest)
system.time(fited <- randomForest(classe ~ ., data=train))
fited
```

Random forest does cross-validation to estimate 0.41% error rate. 
It's really good to start, so let's try on new data :

### New data performance

```{r}
predicted.classe <- predict(fited, test)
confusionMatrix(predicted.classe, test$classe)
```

Out of sample error is: `r round(1 - confusionMatrix(predict(fited, test[, useful.columns]), test$classe)$overall['Accuracy'], 4) * 100`%

It's really good, our model is great at predicting the classe "variable".

### Propagation

```{r results='hide'}
predict(fited, Datatest)
```


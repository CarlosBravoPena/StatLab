library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

read_mnist()
summary(mnist)
mnist
mnist <- read_mnist()
mnist
mnist
ncol(mnist$train$images)
names(mnist)


# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)


# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)


p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#comprehensive test

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
y_female<-dat%>%filter(dat$sex=="Female")
y_female
x_inclass<-dat%>%filter(dat$type=="online")
x_inclass

#compute accuracy Q2 based on type
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)

confusionmatrix<-table(y_hat, y)
confusionMatrix(data=y_hat,reference=y)


#second part complehensive test

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]



#question 8
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	


#question 9
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

#question 10
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

#question 11
plot(iris,pch=21,bg=iris$Species)


#question 12
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)


#linear regression smooth
library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg
#aqui calculo el square loss
mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
#aqui calculo el square loss
mean((y_hat - test_set$son)^2)


#predict function

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

#test 4 pregunta 1
set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

dat
set.seed(1, sample.kind="Rounding")
rmse <- replicate (100, {

  test_index <- createDataPartition(dat$y, times = 1, p = .5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x, data=train_set)
  y_hat <- predict(fit, newdata=test_set)
  sqrt(mean((y_hat-test_set$y)^2))
}, simplify=TRUE)

mean(res)
sd(res)

#question 2
set.seed(1, sample.kind="Rounding")
n <- c(100,500,1000,5000,10000)
ans<-sapply(n,function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n , c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
rmse <- replicate (100, {
test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)
mod <- lm(y~x, data= train_set)
y_hat <- predict(mod, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
}, simplify=TRUE)
c(mean=mean(rmse),stnd=sd(rmse))

})
ans
#question 4
set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)
#question 6
set.seed(1, sample.kind="Rounding") 
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
#repetir 3 veces con x_1 , x_2 y x_1+x_2
rmse <-   test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_2+x_1, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
 
  set.seed(1)
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x_1, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  fit <- lm(y ~ x_1 + x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  #question 8
  set.seed(1)
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  rmse <-   test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))

  
  #regression for a categorical outcome
  library(dslabs)
  library(caret)
  library(dplyr)
  data("heights")
  y <- heights$height
  
  #set.seed(2) #if you are using R 3.5 or earlier
  set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- heights %>% slice(-test_index)
  test_set <- heights %>% slice(test_index)
  
  train_set %>% 
    filter(round(height)==66) %>%
    summarize(y_hat = mean(sex=="Female"))
  
  heights %>% 
    mutate(x = round(height)) %>%
    group_by(x) %>%
    filter(n() >= 10) %>%
    summarize(prop = mean(sex == "Female")) %>%
    ggplot(aes(x, prop)) +
    geom_point()
  lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
  p_hat <- predict(lm_fit, test_set)
  y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
  #confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]
  confusionMatrix(y_hat, test_set$sex)
  
  
  #logistic regression
  
  heights %>% 
    mutate(x = round(height)) %>%
    group_by(x) %>%
    filter(n() >= 10) %>%
    summarize(prop = mean(sex == "Female")) %>%
    ggplot(aes(x, prop)) +
    geom_point() + 
    geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
  
  range(p_hat)
  
  # fit logistic regression model
  glm_fit <- train_set %>% 
    mutate(y = as.numeric(sex == "Female")) %>%
    glm(y ~ height, data=., family = "binomial")
  
  p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
  
  tmp <- heights %>% 
    mutate(x = round(height)) %>%
    group_by(x) %>%
    filter(n() >= 10) %>%
    summarize(prop = mean(sex == "Female")) 
  logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
    mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
  tmp %>% 
    ggplot(aes(x, prop)) +
    geom_point() +
    geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)
  
  y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
  confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]
  
  #case study 2 or 7
  mnist <- read_mnist()
  is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
  titles <- c("smallest","largest")
  tmp <- lapply(1:2, function(i){
    expand.grid(Row=1:28, Column=1:28) %>%
      mutate(label=titles[i],
             value = mnist$train$images[is[i],])
  })
  tmp <- Reduce(rbind, tmp)
  tmp %>% ggplot(aes(Row, Column, fill=value)) +
    geom_raster() +
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label) +
    geom_vline(xintercept = 14.5) +
    geom_hline(yintercept = 14.5)
  
  data("mnist_27")
  mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
  
  is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
  titles <- c("smallest","largest")
  tmp <- lapply(1:2, function(i){
    expand.grid(Row=1:28, Column=1:28) %>%
      mutate(label=titles[i],
             value = mnist$train$images[is[i],])
  })
  tmp <- Reduce(rbind, tmp)
  tmp %>% ggplot(aes(Row, Column, fill=value)) +
    geom_raster() +
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label) +
    geom_vline(xintercept = 14.5) +
    geom_hline(yintercept = 14.5)
  
  fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
  p_hat_glm <- predict(fit_glm, mnist_27$test)
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
  confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]
  
  mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
    geom_raster()
  
  mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster() +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black") 
  
  p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
  mnist_27$true_p %>%
    mutate(p_hat = p_hat) %>%
    ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
    geom_raster() +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black") 
  
  p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
  mnist_27$true_p %>%
    mutate(p_hat = p_hat) %>%
    ggplot() +
    stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
    geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)
  
  
  #question 12
  #set.seed(2) #if you are using R 3.5 or earlier
  set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
  make_data <- function(n = 1000, p = 0.5, 
                        mu_0 = 0, mu_1 = 2, 
                        sigma_0 = 1,  sigma_1 = 1){
    
    y <- rbinom(n, 1, p)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)
    
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    
    list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
         test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
  }
  dat <- make_data()
  dat$train %>% ggplot(aes(x, color = y)) + geom_density()
  
  #mirespuesta
  set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
  delta <- seq(0, 3, len = 25)
  res <- sapply(delta, function(d){
    dat <- make_data(mu_1 = d)
    fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
    y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
    mean(y_hat_glm == dat$test$y)
  })
  qplot(delta, res)
  
  #smoothing
  library(dslabs)
  library(ggplot2)
  library(dplyr)
  data("polls_2008")
  qplot(day, margin, data = polls_2008)
  # bin smoothers
  span <- 7 
  fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
  polls_2008 %>% mutate(smooth = fit$y) %>%
    ggplot(aes(day, margin)) +
    geom_point(size = 3, alpha = .5, color = "grey") + 
    geom_line(aes(day, smooth), color="red")
  
  # kernel
  span <- 7
  fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
  polls_2008 %>% mutate(smooth = fit$y) %>%
    ggplot(aes(day, margin)) +
    geom_point(size = 3, alpha = .5, color = "grey") + 
    geom_line(aes(day, smooth), color="red")
  
  #questions about smoothing
  library(tidyverse)
  library(lubridate)
  library(purrr)
  library(pdftools)
  
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
  dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
    s <- str_trim(s)
    header_index <- str_which(s, "2015")[1]
    tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
    month <- tmp[1]
    header <- tmp[-1]
    tail_index  <- str_which(s, "Total")
    n <- str_count(s, "\\d+")
    out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
    s[-out] %>%
      str_remove_all("[^\\d\\s]") %>%
      str_trim() %>%
      str_split_fixed("\\s+", n = 6) %>%
      .[,1:5] %>%
      as_data_frame() %>% 
      setNames(c("day", header)) %>%
      mutate(month = month,
             day = as.numeric(day)) %>%
      gather(year, deaths, -c(day, month)) %>%
      mutate(deaths = as.numeric(deaths))
  }) %>%
    mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                          "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
    mutate(date = make_date(year, month, day)) %>%
    dplyr::filter(date <= "2018-05-01")
  
  dat
  #respuesta
  span <- 60 / as.numeric(diff(range(dat$date)))
  fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
  dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
    ggplot() +
    geom_point(aes(date, deaths)) +
    geom_line(aes(date, smooth), lwd = 2, col = "red")
  
  #pregunta dos
  span <- 60 / as.numeric(diff(range(dat$date)))
  fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
  dat %>% 
    mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
    ggplot(aes(day, smooth, col = year)) +
    geom_line(lwd = 2)
  
  #pregunta 3
  library(broom)
  mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
  qplot(x_2, y, data = mnist_27$train)
  mnist_27$train %>% 
    mutate(y = ifelse(y=="7", 1, 0)) %>%
    ggplot(aes(x_2, y)) + 
    geom_smooth(method = "loess")
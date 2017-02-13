#====================
# Kevin Tse
# 1/12/2017
#====================

library(readr)
library(mosaic)
library(kknn)
library(gridExtra)

#----------------
# Comparing the benefits and downsides of kNN with non-ML regressions

# Generate Training Set
set.seed(99);
x_training = rnorm(100);
epsilon_training = rnorm(100);
y_training = 1.8*x_training + 2 + epsilon_training;

# Generate Test Set
x_test = rnorm(10000);
epsilon_test = rnorm(10000);
y_test = 1.8*x_test + 2 + epsilon_test;

df_training <- data.frame(x = x_training, y = y_training);
df_test <- data.frame(x = x_test, y = y_test);

## Scatter Plot
sp_linear <- ggplot(df_test, aes(x = x, y = y)) +
  geom_point(shape=1, color = "red") +
  geom_abline(slope = 1.8, intercept = 2)

ggsave("scatterplot_linear.jpeg", plot = sp_linear)

## Ordinary Regression

fit <- lm(y ~ x, data = df_training) # Linear regression
summary(fit) # Display coefficients
# y = b_0  + b_1 * x + e
# b_0 = 1.9204, b_1 = 1.8775, e = -0.1007

lin_reg <- predict(fit, df_test) # Make Prediction on Test 
lin_MSE <- mean((lin_reg - df_test$y)^2) # Linear MSE

lin_lin <-ggplot(df_test, aes(x = x, y = y)) +
  geom_point(shape=1, color = "red") +
  geom_abline(slope = 1.8, intercept = 2, color = "black") +
  #geom_abline(slope = 1.8775, intercept = 1.9204, color = "blue", linetype = "dashed")
  geom_smooth(data = df_training, method='lm', color = "blue", linetype = "dashed", fill = NA, fullrange = TRUE) # Linear Regression

ggsave("linear_on_linear.jpeg", plot = lin_lin)

## Applying knn on data
kf2 = kknn(y~x,df_training,df_test,k=2,kernel = "rectangular")
df_kf2 = data.frame(predicted = kf2$fitted, x = df_test$x)

kf12 = kknn(y~x,df_training,df_test,k=12,kernel = "rectangular")
df_kf12 = data.frame(predicted = kf12$fitted, x = df_test$x)

p1 <- ggplot(df_test, aes(x = x, y = y)) +
  ggtitle("Predicted Values with k = 22") +
  geom_point(shape=1, color = "red") +
  geom_abline(slope = 1.8, intercept = 2, color = "black") + 
  geom_point(aes(y = predicted, x = x), data = df_kf2, color = "blue")

p2 <- ggplot(df_test, aes(x = x, y = y)) +
  ggtitle("Predicted Values with k = 12") +
  geom_point(shape=1, color = "red") +
  geom_abline(slope = 1.8, intercept = 2, color = "black") + 
  geom_point(aes(y = predicted, x = x), data = df_kf12, color = "blue")

kNN_linear <- grid.arrange(p1, p2, nrow=2)

ggsave("kNN_on_linear.jpeg", plot = kNN_linear)


## Finding MSE for each k and lienar regression

# The MSE of linear regression is
lin_MSE <- mean((lin_reg - df_test$y)^2) # 0.9977181

# Loop over values of k, fit on training set, predict on test
kvec=2:15; nk=length(kvec)
outMSE = rep(0,nk) #will put the out-of-sample MSE here
for(i in 1:nk) {
  near = kknn(y~x,df_training,df_test,k=kvec[i],kernel = "rectangular")
  MSE = mean((df_test$y - near$fitted)^2)
  outMSE[i] = MSE
}

# k, log(1/k), MSE of corresponding k
df_MSE = data.frame(k = 2:15, loginv_k = log(1/(2:15)),MSE = outMSE);

kNN_MSE_linear_plot <- ggplot(df_MSE, aes(x = loginv_k, y = MSE)) +
  ggtitle("MSE vs log(1/k) with Linear Regression MSE, Linear Model") +
  geom_point(shape=1) +
  geom_abline(slope = 0, intercept = lin_MSE, color = "blue") +
  ylim(0.9,1.8)

ggsave("kNN_MSE_linear_plot.jpeg", plot = kNN_MSE_linear_plot)

# Linear regression outperforms kNN for all values of k, 
# having a MSE of 0.9977181. Linear regression is the best model
# because it matches the true model exactly in this particular case,
# and the MSE values supports this reasoning. For k values from 5 to 15,
# the MSE ranging from 1.2 to 1.3, which seem reasonably well. 
# In particular, k = 10 did the best for kNN, giving a MSE of 1.198558, 
# but still noticably worse than 0.9977181 of linear regression.


# Reusing previous draws
y_training2 = exp(x_training+1) + 3 + epsilon_training;
y_test2 = exp(x_test+1) + 3 + epsilon_test;

df_training2 <- data.frame(x = x_training, y = y_training2);
df_test2 <- data.frame(x = x_test, y = y_test2);

# Scatter Plot
sp_exp <- ggplot(df_test2, aes(x = x, y = y)) +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) exp(x+1) + 3)

ggsave("scatterplot_exp.jpeg", plot = sp_exp)

# Linear Regression
fit2 <- lm(y ~ x, data = df_training2) # Linear regression
summary(fit2) # Display coefficients
# y = b_0  + b_1 * x + e
# b_0 = 6.6821, b_1 = 2.9020, e = -0.2351

lin_reg2 <- predict(fit2, df_test2) # Make Prediction on Test
lin_MSE2 <- mean((lin_reg2 - df_test2$y)^2) # Linear MSE

lin_exp <- ggplot(df_test2, aes(x = x, y = y)) +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) exp(x+1) + 3) + 
  geom_smooth(data = df_training2, method='lm', color = "blue", linetype = "dashed", fill = NA, fullrange = TRUE)

ggsave("linear_on_exp.jpeg", plot = lin_exp)

#kNN

kf2_exp = kknn(y~x,df_training2,df_test2,k=2,kernel = "rectangular")
df_kf2_exp = data.frame(predicted = kf2_exp$fitted, x = df_test2$x)

kf12_exp = kknn(y~x,df_training2,df_test2,k=12,kernel = "rectangular")
df_kf12_exp = data.frame(predicted = kf12_exp$fitted, x = df_test2$x)

p21 <- ggplot(df_test2, aes(x = x, y = y)) +
  ggtitle("Predicted Values with k = 2, Exponential") +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) exp(x+1) + 3) + 
  geom_point(aes(y = predicted, x = x), data = df_kf2_exp, color = "blue")

p22 <- ggplot(df_test2, aes(x = x, y = y)) +
  ggtitle("Predicted Values with k = 12, Exponential") +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) exp(x+1) + 3) + 
  geom_point(aes(y = predicted, x = x), data = df_kf12_exp, color = "blue")

kNN_exp <- grid.arrange(p21, p22, nrow=2)

ggsave("kNN_on_exp.jpeg", plot = kNN_exp)

# MSE
# The MSE of linear regression is
lin_MSE2 <- mean((lin_reg2 - df_test2$y)^2) # 18.50692

# Loop over values of k, fit on training set, predict on test
kvec=2:15; nk=length(kvec)
outMSE2 = rep(0,nk) #will put the out-of-sample MSE here
for(i in 1:nk) {
  near = kknn(y~x,df_training2,df_test2,k=kvec[i],kernel = "rectangular")
  MSE = mean((df_test2$y - near$fitted)^2)
  outMSE2[i] = MSE
}

# k, log(1/k), MSE of corresponding k
df_MSE_exp = data.frame(k = 2:15, loginv_k = log(1/(2:15)),MSE = outMSE2);

kNN_MSE_exp_plot <- ggplot(df_MSE_exp, aes(x = loginv_k, y = MSE)) +
  ggtitle("MSE vs log(1/k) with Linear Regression MSE, Exponential Model") +
  geom_point(shape=1) +
  geom_abline(slope = 0, intercept = lin_MSE2, color = "blue") 

ggsave("kNN_MSE_exp_plot.jpeg", plot = kNN_MSE_exp_plot)

# The linear regression has a MSE of 18.50692, this is a worse MSE than all k-values, except k = 14, 15.
# For k values from 3 to 8, the MSE ranges from 14.28444 to 15.98250. 
# In particular, k = 2 gives a MSE of 12.85768, significantly better than the MSE from linear regression.
# This is because we have an exponential true model, which is far off from a linear one.


y_training3 = sin(2*x_training) + 2 + epsilon_training;
y_test3 = sin(2*x_test) + 2 + epsilon_test;

df_training3 <- data.frame(x = x_training, y = y_training3);
df_test3 <- data.frame(x = x_test, y = y_test3);

# Scatter Plots
sp_sin <- ggplot(df_test3, aes(x = x, y = y)) +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) sin(2*x) + 2)

ggsave("scatterplot_sin.jpeg", plot = sp_sin)

# Linear Regression
fit3 <- lm(y ~ x, data = df_training3) # Linear regression
summary(fit3) # Display coefficients
# y = b_0  + b_1 * x + e
# b_0 = 2.0264, b_1 = 0.4310, e = 0.09093

lin_reg3 <- predict(fit3, df_test3) # Make Prediction on Test
lin_MSE3 <- mean((lin_reg3 - df_test3$y)^2) # Linear MSE

lin_sin <- ggplot(df_test3, aes(x = x, y = y)) +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) sin(2*x) + 2) + 
  geom_smooth(data = df_training3, method='lm', color = "blue", linetype = "dashed", fill = NA, fullrange = TRUE)

ggsave("linear_on_sin.jpeg", plot = lin_sin)

#kNN

kf2_sin = kknn(y~x,df_training3,df_test3,k=2,kernel = "rectangular")
df_kf2_sin = data.frame(predicted = kf2_sin$fitted, x = df_test3$x)

kf12_sin = kknn(y~x,df_training3,df_test3,k=12,kernel = "rectangular")
df_kf12_sin = data.frame(predicted = kf12_sin$fitted, x = df_test3$x)

p31 <- ggplot(df_test3, aes(x = x, y = y)) +
  ggtitle("Predicted Values with k = 2, sin") +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) sin(2*x) + 2) + 
  geom_point(aes(y = predicted, x = x), data = df_kf2_sin, color = "blue")

p32 <- ggplot(df_test3, aes(x = x, y = y)) +
  ggtitle("Predicted Values with k = 12, sin") +
  geom_point(shape=1, color = "red") +
  stat_function(fun=function(x) sin(2*x) + 2) + 
  geom_point(aes(y = predicted, x = x), data = df_kf12_sin, color = "blue")

kNN_sin <- grid.arrange(p31, p32, nrow=2)

ggsave("kNN_on_sin.jpeg", plot = kNN_sin)

# MSE
# The MSE of linear regression is
lin_MSE3 <- mean((lin_reg3 - df_test3$y)^2) # 1.450791

# Loop over values of k, fit on training set, predict on test
kvec=2:15; nk=length(kvec)
outMSE3 = rep(0,nk) # will put the out-of-sample MSE here
for(i in 1:nk) {
  near = kknn(y~x,df_training3,df_test3,k=kvec[i],kernel = "rectangular")
  MSE = mean((df_test3$y - near$fitted)^2)
  outMSE3[i] = MSE
}

# k, log(1/k), MSE of corresponding k
df_MSE_sin = data.frame(k = 2:15, loginv_k = log(1/(2:15)),MSE = outMSE3);

kNN_MSE_sin_plot <- ggplot(df_MSE_sin, aes(x = loginv_k, y = MSE)) +
  ggtitle("MSE vs log(1/k) with Linear Regression MSE for sin function") +
  geom_point(shape=1) +
  geom_abline(slope = 0, intercept = lin_MSE3, color = "blue") 

ggsave("kNN_MSE_sin_plot.jpeg", plot = kNN_MSE_sin_plot)

# For k values from 7 to 14, the MSE all fall below 1.2.
# k = 9 gives the best result with a MSE of 1.16542, which is significantly better
# than the MSE from linear regression, 1.450791. All but two k-values perform
# better than the linear regression in terms of MSE.
# We see that kNN is a much better model to predict the values from a sin function.
# This is because the function is not monotonic, which causes the linear regression 
# to have less accurate results.


## 1.8

# p = 1 case
plot_list = list()
plot_list[[1]] <- ggplot(df_MSE_sin, aes(x = loginv_k, y = MSE)) +
  ggtitle("MSE vs log(1/k) with Linear Regression MSE for sin function, p = 1") +
  geom_point(shape=1) +
  geom_abline(slope = 0, intercept = lin_MSE3, color = "blue") 


for (j in 2:20) {
  xs = x_training
  xs_test = x_test
  set.seed(500)
  for (i in 2:j) {
    xs = cbind(xs,rnorm(100));
    xs_test= cbind(xs_test,rnorm(10000));
  }
  # Creating data frame
  df_MSE_sin_p = data.frame(y=y_training3,xs);
  df_MSE_sin_p_test = data.frame(y=y_test3,xs);
  
  kvec=2:15; nk=length(kvec);
  outMSEp = rep(0,nk) # will put the out-of-sample MSE here
  for(i in 1:nk) {
    near = kknn(y~.,df_MSE_sin_p,df_MSE_sin_p_test,k=kvec[i],kernel = "rectangular")
    MSE = mean((df_MSE_sin_p_test$y - near$fitted)^2)
    outMSEp[i] = MSE
  }
  df_MSE_sin_p = data.frame(k = 2:15, loginv_k = log(1/(2:15)),MSE = outMSEp);
  
  # Save the plot into an array
  plot_list[[j]]  <- ggplot(df_MSE_sin_p, aes(x = loginv_k, y = MSE)) +
    ggtitle(paste("MSE vs log(1/k) with Linear Regression MSE for sin function, p =", j)) +
    geom_point(shape=1) +
    geom_abline(slope = 0, intercept = lin_MSE3, color = "blue") +
    expand_limits(y=1)
}

MSE_vs_p_1 <- grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], nrow=2, ncol=2)
MSE_vs_p_2 <- grid.arrange(plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]], nrow=2, ncol=2)
MSE_vs_p_3 <- grid.arrange(plot_list[[9]], plot_list[[10]], nrow=2)

ggsave("MSE_vs_p_1.jpeg", plot = MSE_vs_p_1, width = 12)
ggsave("MSE_vs_p_2.jpeg", plot = MSE_vs_p_2, width = 12)
ggsave("MSE_vs_p_3.jpeg", plot = MSE_vs_p_3)

# In general, the performance of kNN will improve as the data set 
# gets larger. This is because the accuracy of kNN relies heavily 
# on the number and clustering of observations. The increase in the 
# number of training observation in particular improves the results of 
# larger k-values (the increase in clustering will allow for a better 
# approximation of the true value at a local point), so a larger training 
# set allows the selection of bigger k-values. In addition, it will 
# also meet we have more training observations that lies on the tail ends 
# of the distributions, this will improve the prediction for the tail ends as well.

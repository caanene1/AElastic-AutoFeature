# Required packages
library(glmnet)
library(dplyr)
library(magrittr)

# Main Elastic net function
AElastic <- function(x, f) {
# Elastic net feature selection
# Args:
#    x: Matrix containing features to extract, first column most be lables
#    f: Type of model, two class or >3 class, "binomial" or "multinomial"
#
# Returns: 
#    Multiple outputs; plots, lambda and Table of selcted featues.
#    Plots are shown in IDE and features saved to working directory.
# Notes: Use one of "f = c("binomial" or "multinomial")
# Set the feature matrix
#
Mods <- function(x, f, a) { 
# Penalised regression
# Args:
#    x: DataFrame with first column as lable and rest as features
#    f: Indicate if it is "binomial" or "multinomial"
#    s: TRUE or FALSE for data centering (Zscores), data must be scaled
# Returns: 
#    Multiple outputs; plots, lambda and Table of selcted featues.
#    Plots are shown in IDE and features saved to working directory.
# Set the feature matrix
Fs <- as.matrix(scale(x[-1], center = TRUE, scale = TRUE))
y <-  as.matrix(x[1])
# FoldIds for 10-fold crossvalidation, can be reduced to 3 if data too small.
foldv = sample(1:10, size = length(y), replace = TRUE) 
# Seed
set.seed(100) 
#Set IDs for crossvalidation

# Fit model
cv.glmnet(Fs, y, 
         family = f,
         nlambda = 100,
         type.measure = "mse",
         foldid = foldv, 
         alpha = a) 
}
# Run model 
c1 <- Mods(x, f, 0.2)
c2 <- Mods(x, f, 0.5)
c3 <- Mods(x, f, 0.8)
#
#
# Plot the model
CompPP <- function(c1, c2, c3) {
# Compares model at three alphas.
# Plots loss(Error) to log(Lambda) three alphas 
# Args:
#    c1 - c3: Model object to extract errors and plot
# Returns: 
#    A 4X4 image of errors, with selected features
# Ploting call
par(mfrow = c(2,2))
plot(c1); plot(c2); plot(c3)
#
plot(log(c1$lambda), c1$cvm, 
       pch = 15, col = "red", 
       xlab = "log(Lambda)", 
       ylab = c2$name)
points(log(c2$lambda), c2$cvm,
       pch = 17, col = "blue")
points(log(c3$lambda), c3$cvm,
       pch = 19, col = "green")
legend("topright", legend = c("0.2","0.5","0.8"), 
       pch = 18, col = c("red","blue","green"))
}
#
CompPP(c1, c2, c3)
#
dff <- function(x) { 
# Extract features from elastic net crossvalidated model
# Args:
#    x: Model object to extract coeffs/selection parameters
# Returns: 
#    Dataframe of name, min and 1se
as.data.frame(as.matrix(coef(x, 
                             s = c(x$lambda.min, x$lambda.1se))))
}
#
#
# Extract the tables and coeffcients for each alpha 
cvres <- do.call(cbind, list(dff(c1), dff(c2), dff(c3))) %>%
        setNames(., c("0.2min", "0.2se", "0.5min", "0.5se", 
                        "0.8min", "0.8se")) %>%
        mutate(Sbeta = rowSums(.)) %>%
        subset(., !Sbeta == 0)
return(cvres)
}


# Usage
# Set data to have lable in first column, rest must be numeric columns
df <- df
# Run 
Res <- AElastic(df,"binomial")

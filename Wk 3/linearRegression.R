# Week 3 - regression
install.packages("scatterplot3d")
require(scatterplot3d)

data("mtcars")
attach(mtcars)
# plot 3d relationship of weight as a function of disp and mpg
sc3d = scatterplot3d(disp,mpg, wt, main="3D Scatterplot", angle = 150)
# angle gives the angle between the x and y axes

# perform regression
# make data frame of only the variables we are interested in
data = data.frame(cbind(wt,disp,mpg))
variables = colnames(data)[c(2:dim(data)[2])]
# define the formula - a string expression of the form y ~ var1+var2+var3...
formula = paste(colnames(data)[1],paste(variables,collapse = '+'),sep='~')
# gives "wt~disp+mpg" which means that wt is some function of disp and mpg 

fit <- lm(formula, data=data)
summary(fit)
# means we are assuming a linear relationship  (lm is a linear model) 
# where wt ~ intercept + c1*disp + c2*mpg 
# where c1 and c2 are coefficients and intercept is the y intercept
# fit is a lm object with many variables like coefficients, residuals, rank, fitted.values...
# we are interested in the coefficients  and the intercept, all of which are found in the
# variable $coefficient of the lm object

beta = fit$coefficients
str(beta) # a named numeric vector - "(Intercept)", "disp" and "mpg"

X <- cbind(1,disp, mpg) # create matrix with first column of 1s with other columns mpg & disp
# calculate the dot product(%*%) of X.beta - the first column of 1s in X is so that the intercept
# is only multiplied by one such that yhat[i,1] = intercept + disp*c1 + mpg*c2
yhat= X%*%beta

fittedLine = as.data.frame(cbind(yhat, disp, mpg)) # create data frame similar to "data" but instead
# of the actual wt we are now using our estimated wt = yhat
fittedLine = fittedLine[order(fittedLine[,1]),] # sort by yhat so we can plot a straight line rather than
 # a jagged line

# plotting
# points3d(x,y,z) is a function that adds points to the existing plot
# plane3d(intercept,coeff...) is a function that adds a plane to the existing plot
# both functions are actually attributes of the sc3d object so we call them on it with
# sc3d$points3d() or sc3d$plane3d()
# the order of the variables is important x = disp, y = mpg, z = wt
sc3d$points3d(fittedLine[,2],fittedLine[,3],fittedLine[,1], type='l', col = 'red')
sc3d$plane3d(fit, col='red')


# coefficient of determination R^2(R-squared) measures how well we fit the data
# whilst it may be good to get a high R-squared(ranges from 0 to 1), it may render
# our model less accurate in predictions due to overfitting

# avoiding OVERFITTING
# separate data into training and test data
# model based on the training data
# then test your function on the test data
# if it does poorly on the test data but good on the training data, you are overfitting,
# you need to use less data points
# if it does poorly on the training data but better on the test data?


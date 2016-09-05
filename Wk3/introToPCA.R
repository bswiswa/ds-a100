require(scatterplot3d)
data("mtcars")
wts <- mtcars$wt
cor(mtcars$wt, mtcars$disp)
# correlation varies from -1 to +1
# 
z <- cor(mtcars)
z
cov(mtcars)
# package lattice plots, these are alternatives to the pairs plot
install.packages("lattice")
library(lattice)
levelplot(z)
levelplot(cov(mtcars))
pairs(mtcars)
# second moment is usually enough to find correlation but pairs plot may show
# a better picture

# remember correlation matrix, level plot of correlation matrix(cor()),
# and covariance matrix

#principal component analysis
mtcars.pca <- princomp(mtcars, cor = T)
plot(mtcars.pca, main = "PCA") # called spectrum of data
#biplot shows position of PCA relative to original axes all variables that are in the same
# direction have a positive correlation
biplot(mtcars.pca)
str(mtcars.pca)
# singular variables do not add anything new to the data and would need to be removed from PCA

plot(mtcars.pca$scores[,1],mtcars.pca$scores[,2], xlab='PC1', ylab='PC2', col=mtcars$wt)

par(mar=c(12,5,1,1))
plot(mtcars.pca$loadings[,1], xaxt='n', xlab="", ylab="PC1 coefficient")
axis(1, at=1:length(mtcars),labels=colnames(mtcars), col.axis="red", las=2)

mtcars[1:5,]
#look at loadings of PCA 1
# loadings give the factors that you would multiply each variable to get its location
# in the component space
mtcars.pca$loadings[,1]
mtcars.pca$scores[1:5,1:5]

#PCA gives you a good idea of the relationship between the matrix
# difficult to explain to clients

# running PCA first helps avoid overfitting because it only gives you the variables that
# correlate the best


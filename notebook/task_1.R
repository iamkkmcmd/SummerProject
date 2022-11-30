# Cleaning the environment
rm(list = ls())
graphics.off()

# Load the packages
library('tidyverse')
library('tidyquant')

# Tickers of public/private sector banks of India

sbi <- tq_get('SBIN.NS',
       from = '2019-01-01',
       to = '2019-12-31', get = 'stock.prices')

plot(X[,1],X[,2], pch = 19, col = 'steelblue',
     xlab = 'X1', ylab = 'X2', 
     main = 'Scatterplot of X1 and X2')

d <- X[,1] - X[,2]
hist(d, prob = T, xlab = 'Generalized distance')
pdf <- density(d)
lines(pdf, col = 'red', lwd = 2)

qqnorm(d, pch = 19)
qqline(d, col = 'red', lwd = 2)

qchisq(0.05,2)
length(d < 1.386294)
length(X[,1])


library(mvtnorm) # References rmvnorm()
library(ellipse) # References ellipse()
# Set the covariance matrix
sigma2 <- matrix(c(5, 2, 2, 5), ncol=2)
# Set the means
mu <- c(5,5)
# Get the correlation matrix
P <- cov2cor(sigma2)
# Generate the data
p <- rmvnorm(n=50, mean=mu, sigma=sqrt(sigma2))
# Plot the data
plot(p)
# Plot the ellipse
lines( ellipse( P, centre = c(5,5)) , col='red')

c <- cov(X[,1],X[,2])
cov_mat <- matrix(c(var(X[,1]),c,c,var(X[,2])), byrow = T, nrow = 2)
plot(X[,1],X[,2], pch = 19, col = 'steelblue',
     xlab = 'X1', ylab = 'X2', 
     main = 'Scatterplot of X1 and X2')
lines(ellipse(cov_mat, centre = c(mean(X[,1]), mean(X[,2])), level = 0.8))




n <- length(X[,1])
p <- 2
alpha <- 0.05
x1bar <- mean(X[,1]); x2bar <- mean(X[,2])
cric_value <- qt(p = alpha/(2*p), df = n-1, lower.tail = FALSE)
s11 <- sd(X[,1]); s22 <- sd(X[,2])
cat('Bonferroni confidence interval for mu1: [', 
    round(x1bar-cric_value*sqrt(s11/n), 4), ',', 
    round(x1bar+cric_value*sqrt(s11/n), 4), ']\n')
cat('Bonferroni confidence interval for mu2: [', 
    round(x2bar-cric_value*sqrt(s22/n), 4), ',', 
    round(x2bar+cric_value*sqrt(s22/n), 4), ']\n')


library(mnormt)
x <- X[,1]
y <- X[,2]
xbar <- c(mean(x), mean(y))
f <- function(x, y) dmnorm(cbind(x, y), xbar, cov_mat)
z <- outer(x, y, f)

#create surface plot
persp(x, y, z, theta=-30, phi=25, expand=0.6, ticktype='detailed')


as.numeric(love[-c(1,2),])

p <- "  2  3  5  5  4  4  5  5"
as.numeric(sapply(strsplit(p, " "), "[["))
as.numeric(gsub("([0-9]+).*$", "\\1", p))

love <- read.table('love.dat')
means <- colMeans(love)
type <- rep(c('Wife','Husband'), each = 4)
question <- rep(c('Q1','Q2','Q3','Q4'), 2)
df <- data.frame(type, means, question)
library(ggplot2)
ggplot(data = df,
       aes(x = question, y = means, col = type, group = type)) +
  geom_point() +
  geom_line() +
  labs(x = 'Question No', y = 'Mean score', col = 'Type')

love <- as.matrix(love)
df <- data.frame(cbind(rbind(love[,1:4], love[,5:8]), 
            rep(c('Wife','Husband'), each = nrow(love))))
names(df) <- c('Q1','Q2','Q3','Q4', 'Spouse')
df$Q1 <- as.numeric(df$Q1)
df$Q2 <- as.numeric(df$Q2)
df$Q3 <- as.numeric(df$Q3)
df$Q4 <- as.numeric(df$Q4)
library('profileR')
mod <- pbg(data=df[,1:4], group=df[,5], original.names=TRUE)
print(mod) #prints average scores in the profile across two groups
summary(mod) #prints the results of three profile by group hypothesis tests

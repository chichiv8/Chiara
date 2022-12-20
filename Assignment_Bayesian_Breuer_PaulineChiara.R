library(tidyverse)
library(ggplot2)
library(dplyr)

### CASE: CRYPTO USERS IN A POOL OF COMMERCIAL BANKING CLIENTS 
### CLIENT INTERVIEW DATA WITH REPLACING THEM TO ESTIMATE THE PROBABILITY OF CRYPTO USAGE 
### k = USAGE OF CRYPTO 


ndraw <- 100000


# Defining and drawing from the prior distribution
n_app <- sample(20:250, ndraw, replace = TRUE)

# Defining the generative model
c_app <- function(n_app) {
  app <- rep(0:1, c(n_app - 20, 20))
  sum(sample(app, 20))
}

# Simulating the data
n_applied <- rep(NA, ndraw)
for(i in 1:ndraw) {
  n_applied[i] <- c_app(n_app[i])
}

# Filtering out those parameter values that didn't result in the
# data that we actually observed
post_app <- n_app[n_applied == 5]

hist(post_app, ylim=c (0,2000), breaks = 10, col = "pink")

### evt. hier noch das Hist ohne die ausgeklammerten 5 




# The posterior distribution showing the probability distribution for crypto users 
# (binning here in bins of 20 just make the graph easier to interpret)
barplot(table(cut(post_app, seq(0, 250, 20), horiz = TRUE)) / length(post_app), col = "lightblue")

### dont know if it makes sense to plot the bar horizon





n_draw <- 100000
n_app <- sample(20:250, n_draw, replace = TRUE)


# ### Crypto users who were already known  
c_app <- function(n_app) {
  app <- rep(0:1, c(n_app - 20, 20))
  prob_choose <- ifelse(app == 0, 1.0, 0.5)
  sum(sample(app, 20, prob = prob_choose))
}

n_applied <- rep(NA, n_draw)
for(i in 1:n_draw) {
  n_applied[i] <- c_app(n_app[i])
}

post_app <- n_app[n_applied == 5]


hist(post_app)




length(post_app)

barplot(table(cut(post_app, seq(0, 250, 20))) / length(post_app), col = "pink")




n_draw <- 100000

### Crypto Expert is analyzing the sample of total financial service users (bankspecific)

n_app <- rnbinom(n_draw, mu = 200 - 20, size = 4) + 20
hist(n_app)


c_app <- function(n_app) {
  app <- rep(0:1, c(n_app - 20, 20))
  prob_choose <- ifelse(app == 0, 1.0, 0.5)
  sum(sample(app, 20, prob = prob_choose))
}

n_applied <- rep(NA, n_draw)
for(i in 1:n_draw) {
  n_applied[i] <- c_app(n_app[i])
}

post_app <- n_app[n_applied == 5]




hist(post_app)

length(post_app)

barplot(table(cut(post_app, seq(0, 250, 20))) / length(post_app), col = "pink")
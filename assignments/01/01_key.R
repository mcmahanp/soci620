#####
# 1 #
#####
surv <- read.csv('https://mcmahanp.github.io/soci620/assignments/01/twizz_survey.csv')
post_grid <- read.csv('https://mcmahanp.github.io/soci620/assignments/01/twizz_posterior.csv')


#####
# 2 #
#####
# the sample size is the number of rows in the data frame
ssize <- nrow(surv)
# you can calculate the number of TRUE values in a logical vector using sum()
ntwiz <- sum(surv$pref_twiz)
print(ssize)
print(ntwiz)


#####
# 3 #
#####
# use the grid provided in post_grid
grid <- post_grid$grid
lik <- dbinom(ntwiz,size=ssize,p=grid)
plot(grid,lik,type='l')


#####
# 4 #
#####
study_prior <- post_grid$posterior/lik
plot(grid,study_prior,type='l')


#####
# 5 #
#####
# make a flat prior
flat_prior <- dunif(grid,0,1)
# calculate and normalize the posterior
posterior <- flat_prior*lik
posterior <- posterior/sum(posterior)
# get 1000 samples over the grid
samp <- sample(grid,size=1000,prob=posterior,replace=TRUE)
print(mean(samp))
print(mean(samp>0.3))

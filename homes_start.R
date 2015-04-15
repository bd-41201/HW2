##### ******** Mortgage and Home Sales Data ******** #####

## Read in the data
homes <- read.csv("homes2004.csv")

# conditional vs marginal value
par(mfrow=c(1,2)) # 1 row, 2 columns of plots
hist(homes$VALUE, col="grey", xlab="home value", main="")
plot(VALUE ~ factor(BATHS),
    col=rainbow(8), data=homes[homes$BATHS<8,],
    xlab="number of bathrooms", ylab="home value")

# create a var for downpayment being greater than 20%
homes$gt20dwn <-
	factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

# some quick plots.  Do more to build your intuition!
par(mfrow=c(1,2))
plot(VALUE ~ STATE, data=homes,
	col=rainbow(nlevels(homes$STATE)),
	ylim=c(0,10^6), cex.axis=.65)
plot(gt20dwn ~ FRSTHO, data=homes,
	col=c(1,3), xlab="Buyer's First Home?",
	ylab="Greater than 20% down")

## code hints

## Q1
# Additional plots to look at

# Are first homes near transportation?
# png('first_home_near_trans.png')
# plot(ETRANS~FRSTHO, data=homes,col=c(2,3),
#   + ylab="Is there a railway, airport, or 4 lane highway within 1/2 block?",xlab="Is this the buyers first home?")
# dev.off()

# First home buyers are slightly less likely to be near transportation.

# Is home value correlated with income?
# png('value_vs_hhincome.png')
# plot(log(VALUE) ~ log(ZINC2), data=homes,xlab="Household Income (Log Transformation)",ylab="Home Value (Log Transformation)")
# dev.off()

# Graphically, it appears that there is a relatively strong positive correlation between value and household income.
# The coefficients are significant:
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
# (Intercept)    10.279763   0.066859  153.75   <2e-16 ***
# log(ZINC2 + 1)  0.155967   0.006016   25.93   <2e-16 ***

#     Null deviance: 14308  on 15550  degrees of freedom
# Residual deviance: 13715  on 15549  degrees of freedom
# But the R2 is low ~.04

## Q2
# regress log(VALUE) on everything except AMMORT and LPRICE
pricey <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
# extract pvalues
pvals <- summary(pricey)$coef[-1,4]
# example: those variable insignificant at alpha=0.2
names(pvals)[pvals>.2]
# you'll want to replace .2 with your FDR cutoff
# you can use the `-AMMORT' type syntax to drop variables

## Q3:
# - don't forget family="binomial"!
# - use +A*B in forumula to add A interacting with B

## Q4
# this is your training sample
gt100 <- which(homes$VALUE>1e5)
# ybar and null deviance
source("../Utility Scripts/deviance.R")

ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")





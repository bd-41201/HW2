##### ******** Mortgage and Home Sales Data ******** #####
source("../Utility Scripts/deviance.R")
source("../Utility Scripts/fdr.R")

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

# Does the presence of factories affect value?
# png('home_near_factory.png')
# plot(log(VALUE+1)~ECOM2,data=homes,xlab="Factory or other industrial within 1/2 block?",ylab="Home Value (Log Transformation)")
# dev.off()

# Yes, the close proximity of a factory appears to have a negative impact on home value.

# Impact of number of bedrooms on value
# png('value_vs_bedrooms.png')
# plot(VALUE ~ factor(BEDRMS), data=homes[homes$BEDRMS<8,],col=rainbow(8),xlab="Number of Bedrooms",ylab="Home Value")
# dev.off()

## Q2
# regress log(VALUE) on everything except AMMORT and LPRICE
pricey <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
# extract pvalues
pvals <- summary(pricey)$coef[-1,4]
# Calculate the R2 for the full model
full_model_R2 <- 1 - pricey$deviance/pricey$null.deviance
# ~> [1] 0.3057124

## Use fdr_cut function to determine the p value cutoff
# source('../Utility Scripts/fdr.r')
crit_pval <- fdr_cut(pvals,.1)
# ~> [1] 0.07928016
# example: those variable insignificant at alpha=0.2
# names(pvals)[pvals>.2]
# you'll want to replace .2 with your FDR cutoff
# you can use the `-AMMORT' type syntax to drop variables

# list of variables to drop in subsequent regression
names(pvals)[pvals>crit_pval]
# ~> [1] "ECOM1Y"  "EGREENY" "ELOW1Y"  "ETRANSY" "ODORAY"  "PER"     "ZADULT"

# Run the regression again with smaller variable set.
pricey.2 <- glm(log(VALUE) ~ .-AMMORT-LPRICE-ECOM1-EGREEN-ELOW1-ETRANS-ODORA-PER-ZADULT, data=homes)

# Calculate the R2 for the new model
new_model_R2 <- 1 - pricey.2$deviance/pricey.2$null.deviance
# ~> [1] 0.3053729
# which is about the same as above...

## Q3:
# - don't forget family="binomial"!
# - use +A*B in forumula to add A interacting with B

pricey.gt20dwn <- glm(gt20dwn ~ .-AMMORT-LPRICE, data=homes, family="binomial")

# Interpret effects for 1st home buyers and # of bathrooms.
# For # of bathrooms
# b<-as.numeric(pricey.gt20dwn$coef["BATHS"])
# ~> [1] 0.2445396
# exp(b)
# ~> [1] 1.277033
# More BATHs multiplies the odds of greater than 20% down payment by 1.277

# For 1st home buyers
# f <- as.numeric(pricey.gt20dwn$coef["FRSTHOY"])
# ~> [1] -0.3699814
# exp(f)
# ~> [1] 0.6907472
# Being a first time home buyer multiplies the odds of greater than 20% down payment by 0.6907

# Add and describe the interaction for first time home buyers and baths.
pricey.gt20dwn.int <- glm(gt20dwn ~ .-AMMORT-LPRICE+BATHS*FRSTHO, data=homes, family="binomial")

# For first home buyers
# b.firsthome <- as.numeric(pricey.gt20dwn.int$coef["FRSTHOY"])
# ~> [1] -0.02136922
# exp(b.firsthome)
# ~> [1] 0.9788575
# Being a first home buyer multiplies the odds of greater than 20% down payment by 0.9788

# For # of bathrooms
# b.numbath <- as.numeric(pricey.gt20dwn.int$coef["BATHS"])
# ~> [1] 0.2994036
# exp(b.numbath)
# ~> [1] 1.349054
# For non first time home buyers an additional bathroom multiplies the odds of greater than 20% down payment by 1.3491

# With interaction
# b.numbath.int <- as.numeric(pricey.gt20dwn.int$coef["BATHS:FRSTHOY"])
# ~> [1] -0.2020316
# b.bath.frstho <- b.numbath + b.numbath.int
# ~> [1] 0.09737208
# exp(b.bath.frstho)
# ~> [1] 1.10227
# For first time home buyers an additional bathroom multiplies the odds of greater than 20% down payment by 1.1023

## Q4
# this is your training sample
gt100 <- which(homes$VALUE>1e5)
# ybar and null deviance

ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")

# Train the model from q3 on the sample
q3.train <- glm(gt20dwn ~ .-AMMORT-LPRICE+BATHS*FRSTHO,data=homes[gt100,],family="binomial")

# Calculate the R2 for the in sample observations
q4.predict.gt100 <- predict(q3.train,homes[gt100,],type="response")
q4.gt100.R2 <- R2(y=homes$gt20dwn[gt100],pred=q4.predict.gt100,family="binomial")
# ~> [1] 0.1047392

# Calculate the R2 for the out of sample observations
q4.predict.lt100 <- predict(q3.train,homes[-gt100,],type="response")
q4.lt100.R2 <- R2(y=homes$gt20dwn[-gt100],pred=q4.predict.lt100,family="binomial")
# ~> [1] 0.03520417

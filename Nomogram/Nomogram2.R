######################
 # Detailed Example 1 #
  ######################
# So can repeat random number sequence
set.seed(17)  
 n <- 1203
sex <- factor(sample(c('female','male'), n, rep=TRUE))
age    <- rnorm(n, 28, 7)
hand.breath <- rnorm(n, 3.44, 0.34)
thigh.circumference    <- rnorm(n, 15, 2)

# Use two population models, one with a systolic
# blood pressure effect and one without

L    <- ifelse(sex=='female', .1*(pmin(age,28)-28), .005*(age-28)^2)
  
L.HB <- L + .4*(pmax(hand.breath,3.44)-3.44)

L.TC <- L + .4*(pmax(thigh.circumference,15)-15)

bw    <- ifelse(runif(n) <= plogis(L),    1, 0)
  
bw.HB <- ifelse(runif(n) <= plogis(L.HB), 1, 0)

bw.TC <- ifelse(runif(n) <= plogis(L.TC), 1, 0)

# Use summary.formula in the Hmisc package to summarize the
# data one predictor at a time
 
s <- summary(bw.HB ~ age + sex + L.HB) 
s2 <- summary(bw.TC ~ age + sex + L.TC) 

options(digits=3)
    
print(s)
  
plot(s2)

plsmo(age, L,  group=sex, method='raw', add=TRUE, prefix='True', trim=0)
    
title('Lowess-smoothed Estimates with True Regression Functions')
dd <- datadist(age, sex, L.HB)
options(datadist='dd')
    
# can also do: dd <- datadist(dd, newvar)
f <- lrm(bw ~ rcs(age,5)*sex, x=TRUE, y=TRUE)

f

# x=TRUE, y=TRUE for pentrace
fpred <- Function(f)

fpred

fpred(age=20, sex=levels(sex))
anova(f)

p <- Predict(f, age, sex, conf.int=FALSE)

ggplot(p, rdata=data.frame(age, sex)) +
  +   geom_line(aes(x=age, y=L, color=sex), linetype='dotted',
                +             data=data.frame(age, L, sex))

# Specifying rdata to plot.Predict results in sex-specific
# rug plots for age using the Hmisc histSpikeg function, which uses
# ggplot geom_segment.  True regression functions are drawn as
# as dotted lines
f.bp <- lrm(dz.bp ~ rcs(age,5)*sex + rcs(sys.bp,5))

p <- Predict(f.bp, age, sys.bp, np=75)

bplot(p)  # same as lfun=levelplot
bplot(p, lfun=contourplot)
bplot(p, lfun=wireframe)

cat('Doing 25 bootstrap repetitions to validate model\n')
validate(f, B=25)   # in practice use 300+

cat('Doing 25 bootstrap reps to check model calibration\n')
cal <- calibrate(f, B=25)   # use 300+
plot(cal)

title('Calibration of Unpenalized Model')

p <- pentrace(f, penalty=c(.009,.009903,.02,.2,.5,1))

f <- update(f, penalty=p$penalty)

f

specs(f,long=TRUE)
lrm(formula = dz ~ rcs(age, 5) * sex, x = TRUE, y = TRUE, penalty = p$penalty)

edf <- effective.df(f)

p <- Predict(f, age, sex, conf.int=FALSE)

# Plot penalized spline fit + true regression functions
ggplot(p, rdata=llist(age, sex)) +
  + geom_line(aes(x=age, y=L, color=sex), linetype='dotted',
                + data=data.frame(age, L, sex))

options(digits=3)

s <- summary(f)
s
plot(s)

s <- summary(f, sex='male')
plot(s)
fpred <- Function(f)
fpred
fpred(age=30, sex=levels(sex))
sascode(fpred)
cat('Doing 40 bootstrap reps to validate penalized model\n')
validate(f, B=40)
cat('Doing 40 bootstrap reps to check penalized model calibration\n')
cal <- calibrate(f, B=40)
plot(cal)

title('Calibration of Penalized Model')

nom <- nomogram(f.bp, fun=plogis,
                  +   funlabel='Prob(dz)',
                  +   fun.at=c(.15,.2,.3,.4,.5,.6,.7,.8,.9,.95,.975))

plot(nom, fun.side=c(1,3,1,3,1,3,1,3,1,3,1))
options(datadist=NULL)


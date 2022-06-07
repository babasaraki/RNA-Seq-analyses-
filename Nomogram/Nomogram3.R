library(rms)
# define sample size
n <- 1203   
# so can reproduce the results
set.seed(17) 
age            <- rnorm(n, 28, 7)
hand.breath <- rnorm(n, 3.44, 0.34)
thigh.circumference    <- rnorm(n, 15, 2)
sex            <- factor(sample(c('female','male'), n,rep=TRUE))


# Specify population model for log odds that Y=1
L <- .4*(sex=='male') + .045*(age-28) +
  (log(thigh.circumference - 10)-5.2)*(-2*(sex=='female') + 2*(sex=='male'))
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)

ddist <- datadist(age, hand.breath, thigh.circumference, sex)
options(datadist='ddist')
print(ddist)
f <- lrm(y ~ lsp(age,28)+sex*rcs(thigh.circumference,4)+hand.breath)
nom <- nomogram(f, fun=function(x)1/(1+exp(-x)), 
                fun.lp.at = c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Birgth Weight")
#Instead of fun.at, could have specified fun.lp.at=logit of
#sequence above - faster and slightly more accurate
plot(nom, xfrac=.45)
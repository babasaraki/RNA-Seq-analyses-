# NOT RUN {
# define sample size
n <- 1203  
# so can reproduce the results
set.seed(17) 
d <- data.frame(age = rnorm(n, 28, 7),
                hand.breath = rnorm(n, 3.44, 0.34),
                thigh.circumference = rnorm(n, 15, 2),
                sex = factor(sample(c('female','male'), n,TRUE)))

# Specify population model for log odds that Y=1
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
d <- upData(d,
            L = .4*(sex=='male') + .045*(age-28) +
              (log(hand.breath - 2)-5.2)*(-2*(sex=='female') + 2*(sex=='male')),
            y = ifelse(runif(n) < plogis(L), 1, 0))

ddist <- datadist(d); options(datadist='ddist')

f <- lrm(y ~ lsp(age,28) + sex * rcs(hand.breath, 4) + thigh.circumference,
         data=d)

f <- glm(y ~ lsp(age,28) + sex * rcs(hand.breath, 4) + thigh.circumference,
         data=d)
nom <- nomogram(f, fun=function(x)1/(1+exp(-x)),  
                fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Birth Weight")

#Instead of fun.at, could have specified fun.lp.at=logit of
#sequence above - faster and slightly more accurate
plot(nom, xfrac=.45)
print(nom)
nom <- nomogram(f, age=seq(10,90,by=10))
plot(nom, xfrac=.45)
g <- lrm(y ~ sex + rcs(age, 3) * rcs(hand.breath, 3), data=d)
nom <- nomogram(g, interact=list(age=c(5,10,15,20)), 
                conf.int=c(.7,.9,.95))
plot(nom, col.conf=c(1,.5,.2), naxes=7)
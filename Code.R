### Task 1
library(tidyverse)
library(patchwork)

test = tibble(x = seq(-.25, 1.25, length.out = 1000)) # initialize tibble

beta_creator = function(a,b) {
  test = tibble(x = seq(-.25, 1.25, length.out = 1000)) |>
    mutate(beta.pdf = dbeta(x, a, b))
  
  return(test)
}

summary = tibble()

beta_grapher = function(a, b) {
  name = paste("Beta(", a, ", ", b, ")", sep ="")
  
  test = test |>
    mutate(beta.pdf = dbeta(x, a, b)) # create the beta distribution
  
  p1 <- ggplot(data = test) +
    geom_line(aes(x = x, y = beta.pdf)) +
    ggtitle(name)
  return(p1) 
}

beta_summarizer = function(a, b) {
  
  test = test |>
    summarize(
      name = "Formula",
      a = a,
      b = b,
      mean = a/(a+b),
      variance = (a*b)/(((a+b)^2)*(a+b+1)),
      skewness = (2*(b-a)*(sqrt(a+b+1)))/((a+b+2)*(sqrt(a*b))),
      kurtosis = (6*((a-b)^2)*(a+b+1)-(a*b)*(a+b+2))/((a*b)*(a+b+2)*(a+b+3))
    )
  
  return(rbind(summary, test))
}

summary = beta_summarizer(2, 5)
summary = beta_summarizer(5, 5)
summary = beta_summarizer(5, 2)
summary = beta_summarizer(0.5, 0.5)

graph1 = beta_grapher(2, 5)
graph2 = beta_grapher(5, 5)
graph3 = beta_grapher(5, 2)
graph4 = beta_grapher(0.5, 0.5)

graph1 + graph2 + graph3 + graph4 # put all graphs in one

## Task 2

beta.moment = function(a, b, k, centered) {
  
  integrand = function(x) {(x^k)*dbeta(x, a, b)}
  
  E = integrate(integrand, 0, 1)$value
  
  if(centered == F){
    return(E)
  } else {
    integrand = function(x) {x*dbeta(x, a, b)}
    mean = integrate(integrand, 0, 1)$value
# Calculate mean to use in next integrand
    
    integrand = function(x) {((x-mean)^k)*dbeta(x, a, b)}
    Ec = round(integrate(integrand, 0, 1)$value, 6) 
### Rounded to 6 decimal places to places so that R does not change the whole
### column to exponential notation
    
    return(Ec)
  }
}

(test2 = beta.moment(5, 5, 1, T))

beta_summarizer2 = function(a, b) {
## function to return mean, variance, skewness, and kurtosis using the derived
## formulas
  
  test = test |>
    summarize(
      name = "Derived",
      a = a,
      b = b,
      mean = beta.moment(a, b, 1, F),
      variance = beta.moment(a, b, 2, T),
      skewness = beta.moment(a, b, 3, T)/(beta.moment(a, b, 2, T)^(3/2)),
      kurtosis = (beta.moment(a, b, 4, T)/(beta.moment(a, b, 2, T)^2)) -3
    )
  
  return(rbind(summary, test))
}

summary = beta_summarizer2(2, 5)
summary = beta_summarizer2(5, 5)
summary = beta_summarizer2(5, 2)
summary = beta_summarizer2(0.5, 0.5)

### Task 3



n = 500 
a = 2
b = 5

histogrammer = function(a, b){
  set.seed(7272) 
  beta.sample = rbeta(n = 500, shape1 = a, shape2 = b)    

  name = paste("Beta(", a, ", ", b, ")", sep ="")
  print(name)

  g1 = ggplot(data = tibble(x = beta.sample)) +
      geom_histogram(aes(x = x, y = after_stat(density), color = "Histogram"), fill = "maroon") +
      xlim(min(beta.sample), max(beta.sample)) +
      geom_density(aes(x = x, color = "Estimated Density")) +
      geom_hline(yintercept = 0) +
      geom_line(data= beta_creator(a, b), aes(x=x, y=beta.pdf, color="True Density")) +  
      theme_bw()+                                                        
      xlab("x")+                                                          
      ylab("Density")+                                                   
      scale_color_manual("", values = c("True Density" = "black",
                                        "Estimated Density" = "blue",
                                        "Histogram" = "maroon"))+   
      ggtitle(name)+
      theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"), legend)

return(g1)
} # Just copied graph from HW6 and added it to a function with parameters a and b

histo1 = histogrammer(2, 5)
histo2 = histogrammer(5, 5)
histo3 = histogrammer(5, 2)
histo4 = histogrammer(0.5, 0.5)

histo1 + histo2 + histo3 + histo4 # Add all graphs together

## Task 4

library(cumstats)

set.seed(7272) 
beta.sample = rbeta(n = 500, shape1 = 2, shape2 = 5)   

a=2
b=5

graph1 = ggplot()+
  geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cummean(beta.sample))) +
  geom_hline(yintercept = a/(a+b)) + 
  xlab("n") +
  ylab("cumulative mean")

graph2 = ggplot()+
  geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cumvar(beta.sample))) +
  geom_hline(yintercept = (a*b)/(((a+b)^2)*(a+b+1))) + 
  xlab("n") +
  ylab("cumulative variance")

graph3 = ggplot()+
  geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cumskew(beta.sample))) +
  geom_hline(yintercept = (2*(b-a)*(sqrt(a+b+1)))/((a+b+2)*(sqrt(a*b)))) + 
  xlab("n") +
  ylab("cumulative skewness")

graph4 = ggplot()+
  geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cumkurt(beta.sample)-3)) +
  geom_hline(yintercept = (6*((a-b)^2)*(a+b+1)-(a*b)*(a+b+2))/((a*b)*(a+b+2)*(a+b+3))) + 
  xlab("n") +
  ylab("cumulative kurtosis")


for (i in 2:50) {
  set.seed(7272+i)
  beta.sample = rbeta(n = 500, shape1 = 2, shape2 = 5)
  graph1 = graph1 +
    geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cummean(beta.sample)), color = i)
  graph2 = graph2 +
    geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cumvar(beta.sample)), color = i)
  graph3 = graph3 +
    geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cumskew(beta.sample)), color = i)
  graph4 = graph4 +
    geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cumkurt(beta.sample)-3), color = i)
}
(lines = graph1+graph2+graph3+graph4)

## Task 5

mean = 0
variance = 0
skewness = 0
kurtosis = 0

for (i in 1:1000) {
  set.seed(7272+i)
  beta.sample = rbeta(n = 500, shape1 = 2, shape2 = 5)
  mean[i] = mean(beta.sample)
  variance[i] = var(beta.sample)
  skewness[i] = skewness(beta.sample)
  kurtosis[i] = kurtosis(beta.sample) - 3
}

## put all plots onto one graph

ggplot(data = tibble(mean)) + 
  geom_histogram(aes(x = mean, y = after_stat(density))) +
  geom_density(aes(x = mean, y = after_stat(density))) +
ggplot(data = tibble(variance)) +
  geom_histogram(aes(x = variance, y = after_stat(density))) +
  geom_density(aes(x = variance, y = after_stat(density))) + 
ggplot(data = tibble(skewness)) +
  geom_histogram(aes(x = skewness, y = after_stat(density))) +
  geom_density(aes(x = skewness, y = after_stat(density))) + 
ggplot(data = tibble(kurtosis)) +
  geom_histogram(aes(x = kurtosis, y = after_stat(density))) +
  geom_density(aes(x = kurtosis, y = after_stat(density))) 
  

### TASK 6


death.dat = read_csv("Death-Data/API_SP.DYN.CDRT.IN_DS2_en_csv_v2_76451.csv")

death.dat = death.dat |>
  select("Country Name", "2022")|>
  rename("death.prop" = "2022") |>
  mutate("death.prop" = 
           get("death.prop")/1000) |>
  filter(!is.na(death.prop))


### Method of Moments Estimator function

beta.MOM = function(data, par) {
  a = par[1]
  b = par[2]
  
  Ex1 = a/(a+b)
  Ex2 = ((a+1)*a)/((a+b+1)*(a+b))
  
  m1 = mean(data, na.rm = T)
  m2 = mean(data^2, na.rm = T)
  
  return(c(Ex1 - m1, Ex2 - m2))
}

(moms<- nleqslv(x = c(1, 1),
                fn = beta.MOM,
                data=death.dat$death.prop))

### MOM estimators

(alpha.hat.mom = moms$x[1])
(beta.hat.mom = moms$x[2])

### MLE estimator function

beta.MLE <- function(data, par, neg=F){
  a = par[1]
  b = par[2]
  
  loglik <- sum(log(dbeta(x=data, shape1 = a, shape2 = b)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}

(pars = optim(par = c(1, 1), 
              fn = beta.MLE,
              data=death.dat$death.prop,
              neg=T))

### MLE Estimators

(alpha.hat.mle = pars$par[1])
(beta.hat.mle = pars$par[2])

### Plots

MOM = tibble(x = seq(-.005, 0.025, length.out = 1000)) |>
  mutate(beta.pdf = dbeta(x, alpha.hat.mom, beta.hat.mom))

MLE = tibble(x = seq(-.005, 0.025, length.out = 1000)) |>
  mutate(beta.pdf = dbeta(x, alpha.hat.mle, beta.hat.mle))

ggplot()+
  geom_histogram(data = death.dat, aes(x = death.prop,
                                       y = after_stat(density)),
                 binwidth = 0.001, color = "lightgrey", fill = "maroon") +
  geom_line(data = MOM,
            aes(x = x, y = beta.pdf, color = "MOM Estimate"))+
  geom_line(data = MLE,
            aes(x = x, y = beta.pdf, color = "MLE Estimate"))+
  geom_hline(yintercept = 0) +
  scale_color_manual("", values = c("MOM Estimate" = "black",
                                    "MLE Estimate" = "darkgrey"))


a.mom = 1:1000
b.mom = 1:1000

a.mle = 1:1000
b.mle = 1:1000

for (i in 1:1000) {
  set.seed(7272+i)
  
  sample = rbeta(n = 266, shape1 = 8, shape2 = 950) 
  
  mom.pars = nleqslv(x = c(1, 1),
                     fn = beta.MOM,
                     data=sample)
  
  a.mom[i] = mom.pars$x[1]
  b.mom[i] = mom.pars$x[2]
  
  mle.pars = optim(par = c(1, 1), 
               fn = beta.MLE,
               data = sample,
               neg=T)
  a.mle[i] = mle.pars$par[1]
  b.mle[i] = mle.pars$par[2]
}

plotter = function(x, title) {
  plot = ggplot(data = tibble(x = x))+
    geom_density(aes(x = x, y = after_stat(density))) + 
    geom_hline(yintercept = 0) +
    labs(title = title)
  return(plot)
}

a.mom.plot = plotter(a.mom, "a.mom")
b.mom.plot = plotter(b.mom, "b.mom")
a.mle.plot = plotter(a.mle, "a.mle")
b.mle.plot = plotter(b.mle, "b.mle")

estimators = tibble(iteration = 1:1000, a.theta = 8, b.theta = 950,
                    a.theta.hat.mom = a.mom, b.theta.hat.mom = b.mom,
                    a.theta.hat.mle = a.mle, b.theta.hat.mle = b.mle)

(total.plots = a.mom.plot + b.mom.plot + a.mle.plot + b.mle.plot)


a.theta = 8
b.theta = 950


errors = tibble(estimator = c("a.mom", "a.mle", "b.mom", "b.mle"),
                mean = c(mean(a.mom), mean(a.mle), mean(b.mom), mean(b.mle)),
                bias = c(mean(a.mom) - a.theta,
                         mean(a.mle) - a.theta,
                         mean(b.mom) - b.theta,
                         mean(b.mle) - b.theta),
                precision = c(1/var(a.mom), 1/var(a.mle),
                              1/var(b.mom), 1/var(b.mle)),
                MSE = c(var(a.mom)+(mean(a.mom) - a.theta)^2,
                        var(a.mle)+(mean(a.mle) - a.theta)^2,
                        var(b.mom)+(mean(b.mom) - b.theta)^2, 
                        var(b.mle)+(mean(b.mle) - b.theta)^2))



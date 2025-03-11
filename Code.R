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

graph = ggplot()+
  geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cummean(beta.sample))) +
  geom_hline(yintercept = mean(beta.sample)) + 
  xlab("n") +
  ylab("cumulative mean")



for (i in 2:50) {
  set.seed(7272+i)
  beta.sample = rbeta(n = 500, shape1 = 2, shape2 = 5)
  graph = graph +
    geom_line(data = tibble(beta.sample), aes(x = 1:500, y = cummean(beta.sample)), color = i) 
}
(graph)

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
  

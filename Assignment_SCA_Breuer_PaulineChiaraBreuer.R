


## Simulating Data Set 
## iq, x1, x2, x3, x4 as independent variables, c1 and c2 as control
## variables and error for linear models


set.seed(123)

simdata_iq <- rnorm(1000, 100, 15) 
x1 <- rnorm(1000, 7, 1.2)
x2 <- rnorm (1000, 4, 2)
x3 <- rnorm (1000, 5, 1.5)
x4 <- rnorm(1000, 6, 2)

c1 <- seq(from=18, to=59, along.with=simdata_iq)

error=rnorm(100,0,16)


# Generate the dependent variable (b0 = 90, b1 = 7, b2 = 4, b3 = 5)

y1 = 90 + (7*x1) + (4*x2) + (5*x3) + error

# create the model 

m1 = lm(y1 ~ x1 + x2 + x3)

summary(m1)

plot(m1)



# create model with simdata_iq as dependent variable 

m2 = lm(simdata_iq ~ x1 + x2 + x3 + x4)

summary(m2)

plot(m2)




## Specification Curve Analysis 

library(specr)

## bind data for analysis 

data_sca <- cbind(simdata_iq, x1, x2, x3, x4, c1)

head(data_sca, 10)

summary(data_sca, 10)

setup_specs(y = c("simdata_iq"),               #  one dependent variable, simulated iq
            x = c("x1", "x2", "x3", "x4"),         # Analytical choices, all parameters included
            model = c("lm"),           #  type of model (linear model)
            controls = c("c1"))       # simulated age ranged from 18 to 59


lm_gauss <- function(formula, data) {
  glm(formula = formula, 
      data = data, 
      family = gaussian(link = "identity"))
}

### convert matrix in data frame using base R
df <- as.data.frame(data_sca)


results <- run_specs(df = df, 
                     y = c("simdata_iq"), 
                     x = c("x1", "x2", "x3", "x4"), 
                     model = c("lm", "lm_gauss"), 
                     controls = c("c1"))

results

plot_decisiontree(results, 
                  legend = TRUE)

summarise_specs(results)



summarise_specs(results, 
                subsets,
                var = p.value)


### Plot the SCA 

plot_specs(results, labels = "Estimated fluid Intelligence", "Parameters", desc = FALSE)






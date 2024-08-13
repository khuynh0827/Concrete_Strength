library(tidyverse)
library(car)
library(leaps)

concrete <- read_csv("C:/STATS/concrete_data.csv")
attach(concrete)
par(mfrow=c(3, 3))

summary(concrete)

hist(Strength)
hist(Cement)
hist(`Blast Furnace Slag`)
hist(`Fly Ash`)
hist(Superplasticizer)
hist(Water)
hist(`Coarse Aggregate`)
hist(`Fine Aggregate`)
hist(Age)

model <- lm(Strength~Cement+`Blast Furnace Slag`+`Fly Ash`+Superplasticizer +Water+`Coarse Aggregate`+`Fine Aggregate`+Age, data = concrete)
summary(model)
pairs(concrete)
anova(model)
par(mfrow=c(2, 2))
plot(model)

#model_general
stand_residuals <- rstandard(model)
leverage <- hatvalues(model)
cook <- cooks.distance(model)
df <- data.frame("Standardized Residuals" = stand_residuals, "Leverages" = leverage, "Cook's Distance" = cook)
df

# outliers
which((abs(stand_residuals) >2))

# leverage points
n <- 1030
p <- 8
which(leverage > 2*(p + 1)/ n)

# cook's distance
which(cook > 4/(n-2))

which(leverage > 2*(p + 1)/ n & (abs(stand_residuals) >2))

invResPlot(model)

summary(powerTransform(cbind(Strength, `Blast Furnace Slag`+1e-100, `Fly Ash`+1e-100, Superplasticizer+1e-100, Cement, Water, `Coarse Aggregate`, `Fine Aggregate`, Age)~1))

tStrength <- Strength^0.5
tCement <- Cement^0.5
tBlast <- log(`Blast Furnace Slag`+1e-100)
tFly <- log(`Fly Ash`+1e-100)
tSuper <- log(Superplasticizer+1e-100)
tWater <- Water
tCoarse <- `Coarse Aggregate`
tFine <- `Fine Aggregate`^2
tAge <- log(Age)
tmodel <- lm(tStrength~tCement+tBlast+tFly+tSuper+tWater+tCoarse+tFine+tAge)

plot(tmodel)
summary(tmodel)
anova(tmodel)
vif(tmodel)


#model_transformed
stand_residuals <- rstandard(tmodel)
leverage <- hatvalues(tmodel)
cook <- cooks.distance(tmodel)
df <- data.frame("Standardized Residuals" = stand_residuals, "Leverages" = leverage, "Cook's Distance" = cook)
df

# outliers
which((abs(stand_residuals) >2))

# leverage points
n <- 1030
p <- 8
which(leverage > 2*(p + 1)/ n)

# cook's distance
which(cook > 4/(n-2))

which(leverage > 2*(p + 1)/ n & (abs(stand_residuals) >2))



backAIC <- step(tmodel, direction = "backward", data = concrete)

# suggested model: I(Strength^0.5) ~ I(Cement^0.5) + log(`Blast Furnace Slag`) + 
# log(`Fly Ash`) + Water + log(Superplasticizer) + `Coarse Aggregate` + I(`Fine Aggregate`^2) + log(Age)

mint <- lm((Strength)^0.5 ~ 1, data = concrete)
forwardAIC <- step(mint, scope = list(lower = ~ 1, upper = ~tCement+tBlast+tFly+tSuper+tWater+tCoarse+tFine+tAge, direction = "forward", data = concrete))
# same suggested model

#suggests 7 predictors
backBIC <- step(tmodel, direction = "backward", data = concrete, k = log(n))

#suggests 8 predictors
forwardBIC <- step(mint, scope = list(lower = ~ 1, upper = ~tCement+tBlast+tFly+tSuper+tWater+tCoarse+tFine+tAge, direction = "forward", data = concrete, k = log(n)))

X <- cbind(tCement, tBlast, tFly, tSuper, tWater, tCoarse, tFine, tAge)
method1 <- regsubsets(as.matrix(X), concrete$Strength^0.5)
summary(method1)
# for p = 1, Age 
om1 <- lm(tStrength ~ tAge)
p <- 1
n <- nrow(concrete)
#adjusted R2
Rad1 <- summary(om1)$adj.r.squared
#AIC
AIC1 <- extractAIC(om1)[2]
#AIC corrected 
AICc1 <- AIC1 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC1 <- extractAIC(om1, k = log(n))[2]

# for p = 2, Cement + Age
om2 <- lm(tStrength ~ tCement + tAge)
p <- 2
n <- nrow(concrete)
#adjusted R2
Rad2 <- summary(om2)$adj.r.squared
#AIC
AIC2 <- extractAIC(om2)[2]
#AIC corrected 
AICc2 <- AIC2 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC2 <- extractAIC(om2, k = log(n))[2]

# for p = 3, concrete, Superplasticizer, Age
om3 <- lm(tStrength ~ tCement + tSuper + tAge)
p <- 3
n <- nrow(concrete)
#adjusted R2
Rad3 <- summary(om3)$adj.r.squared
#AIC
AIC3 <- extractAIC(om3)[2]
#AIC corrected 
AICc3 <- AIC3 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC3 <- extractAIC(om3, k = log(n))[2]

# for p = 4, concrete, `Blast Furnace Slag`, Water, Age
om4 <- lm(tStrength ~ tCement + tBlast + tWater + tAge)
p <- 4
n <- nrow(concrete)
#adjusted R2
Rad4 <- summary(om4)$adj.r.squared
#AIC
AIC4 <- extractAIC(om4)[2]
#AIC corrected 
AICc4 <- AIC4 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC4 <- extractAIC(om4, k = log(n))[2]

# for p = 5, concrete, `Blast Furnace Slag`, Water, Superplasticizer, Age
om5 <- lm(tStrength ~ tCement + tBlast + tWater + tSuper + tAge)
p <- 5
n <- nrow(concrete)
#adjusted R2
Rad5 <- summary(om5)$adj.r.squared
#AIC
AIC5 <- extractAIC(om5)[2]
#AIC corrected 
AICc5 <- AIC5 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC5 <- extractAIC(om5, k = log(n))[2]

# for p = 6, concrete, `Blast Furnace Slag`, Water, Superplasticizer, `Fine Aggregate`, Age
om6 <- lm(tStrength ~ tCement + tBlast + tWater + tSuper + tFine + tAge)
p <- 6
n <- nrow(concrete)
#adjusted R2
Rad6 <- summary(om6)$adj.r.squared
#AIC
AIC6 <- extractAIC(om6)[2]
#AIC corrected 
AICc6 <- AIC6 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC6 <- extractAIC(om6, k = log(n))[2]

# for p = 7, concrete, `Blast Furnace Slag`, Water, Superplasticizer, `Coarse Aggregate`, `Fine Aggregate`, Age
om7 <- lm(tStrength ~ tCement + tBlast + tWater + tSuper + tCoarse + tFine + tAge)
p <- 7
n <- nrow(concrete)
#adjusted R2
Rad7 <- summary(om7)$adj.r.squared
#AIC
AIC7 <- extractAIC(om7)[2]
#AIC corrected 
AICc7 <- AIC7 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC7 <- extractAIC(om7, k = log(n))[2]


# for p = 8, concrete, `Blast Furnace Slag`, `Fly Ash`, Water, Superplasticizer, `Coarse Aggregate`, `Fine Aggregate`, Age
om8 <- lm(tStrength ~ tCement + tBlast + tFly + tWater + tSuper + tCoarse + tFine + tAge)
p <- 8
n <- nrow(concrete)
#adjusted R2
Rad8 <- summary(om8)$adj.r.squared
#AIC
AIC8 <- extractAIC(om8)[2]
#AIC corrected 
AICc8 <- AIC7 + 2 * (p + 2) * (p + 3) / (n - p -1)
#BIC
BIC8 <- extractAIC(om8, k = log(n))[2]



table <- data.frame(size = 1:8,
                    Radj2 = c(Rad1, Rad2, Rad3, Rad4, Rad5, Rad6, Rad7, Rad8),
                    AIC = c(AIC1, AIC2, AIC3, AIC4, AIC5, AIC6, AIC7, AIC8),
                    AICc = c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6, AICc7, AICc8),
                    BIC = c(BIC1, BIC2, BIC3, BIC4, BIC5, BIC6, BIC7, BIC8))

tmodel2 <- lm(tStrength~tCement+tBlast+tSuper+tWater+tCoarse+tFine+tAge)
summary(tmodel2)
#full model is appropriate
anova(tmodel2, tmodel)

#scatterplot for model
par(mfrow = c(2,4))
stand_residuals <- rstandard(model)
plot(Cement, stand_residuals)
plot(Water, stand_residuals)
plot(`Coarse Aggregate`, stand_residuals)
plot(`Fine Aggregate`, stand_residuals)
plot(Age, stand_residuals)
plot(`Blast Furnace Slag`, stand_residuals)
plot(`Fly Ash`, stand_residuals)
plot(Superplasticizer, stand_residuals)

#scatterplot for model_tf
par(mfrow = c(2,4))
stand_residuals <- rstandard(tmodel)
plot(tCement, stand_residuals)
plot(tWater, stand_residuals)
plot(tCoarse, stand_residuals)
plot(tFine, stand_residuals)
plot(tAge, stand_residuals)
plot(tBlast, stand_residuals)
plot(tFly, stand_residuals)
plot(tSuper, stand_residuals)

# setwd("/Users/ellacharles/Downloads/sta302")
dat <- readRDS("dat_final.rds")

# Original model (triglycerides as response; the other outcomes are excluded)
model <- lm(triglyceride ~ . - id - hdl - ldl - dbp - sbp, data = dat)

# Normality check: Q-Q plot and histogram of residuals
qqnorm(rstandard(model), main = "Normal Q-Q Plot (Original Triglycerides)",
       pch = 16, cex = 0.6)
qqline(rstandard(model), lwd = 2)

# Check Box-Cox lambda value
bc <- MASS::boxcox(model, lambda = seq(-2, 2, 0.05))

lambda_hat <- bc$x[which.max(bc$y)]
lambda_hat

# Apply -1/5 transformation.
dat$trig_t <- -(dat$triglyceride^(-1/5))

model_t <- lm(trig_t ~ . - id - triglyceride - hdl - ldl - dbp - sbp, data = dat)

qqnorm(rstandard(model_t), main = "Q-Q Plot (Triglycerides Transformed)",
       pch = 16, cex = 0.6)
qqline(rstandard(model_t), lwd = 2)

## Check residual plots for Independence, Heteroskedasticity and Linearity.
res  <- resid(model_t)
sres <- rstandard(model_t)
fit  <- fitted(model_t)

# ---------- 1. LINEARITY ----------
# Residuals vs fitted
dev.off()
plot(fit, res, pch = 16, cex = 0.6, col = "steelblue",
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)
lines(lowess(fit, res), col = "darkgreen", lwd = 2)


# ---------- 2. CONSTANT VARIANCE ----------
plot(fit, sqrt(abs(sres)), pch = 16, cex = 0.6, col = "steelblue",
     xlab = "Fitted values", ylab = "sqrt(|Standardized residuals|)",
     main = "Scale-Location")
lines(lowess(fit, sqrt(abs(sres))), col = "darkgreen", lwd = 2)

lmtest::bptest(model_t)

# ---------- 3. INDEPENDENCE ----------
plot(res, pch = 16, cex = 0.6, col = "steelblue",
     xlab = "Observation order", ylab = "Residuals",
     main = "Residuals vs Observation Order")
abline(h = 0, col = "red", lty = 2)

lmtest::dwtest(model_t)


# ---------- 4. INFLUENCE AND MULTICOLLINEARITY ----------
plot(model_t, which = 4, col = "steelblue")   # Cook's distance
plot(model_t, which = 5, col = "steelblue")   # residuals vs leverage
car::vif(model_t)

# Early model
summary(model_t)
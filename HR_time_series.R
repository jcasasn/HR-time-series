library(forecast)
library(astsa)
library(kableExtra)
library(tidyverse)

# Generate time series object from CSV
batting = read_csv("mlb-batting-totals-by-year.csv")
hr = arrange(batting, Year) %>%
  subset(Year >= 1900 & Year <= 2015, select = HR) %>%
  ts(start = 1900)

# Plot time series
plot.ts(hr,
        main = "Time Series of Total Home Runs in the MLB per Year",
        ylab = "Total Home Runs per season",
        xlab = "Year")

# Take the log and then first difference, plot
hr = diff(log(hr))
plot.ts(hr,
        main = "Time Series of Log Home Runs after First Difference",
        ylab = "Total Home Runs per season",
        xlab = "Year")

# Ljung-Box test
Box.test(hr, lag = 10, type = "Ljung-Box")

# Plot ACF, PACF of ts
acf2(hr)

# Get auto model suggestion
auto.arima(hr)

# Estimate parameters
mle1 = arima(hr, order = c(1, 0, 1), method = "ML")
mle2 = arima(hr, order = c(1, 0, 0), method = "ML")

results = data.frame(
  "Model" = c("ARIMA(1, 0, 1)", "ARIMA(5, 0, 0)"),
  "phi" = c(round(mle1$coef[["ar1"]], 4), round(mle2$coef[["ar1"]], 4)),
  "theta" = c(round(mle1$coef[["ma1"]], 4), ""),
  "sigma" = c(round(mle1$sigma2, 4), round(mle2$sigma2, 4))
)
results %>% kable(align = 'c') %>% kable_styling()

# Plot residual diagnostics
model1 = sarima(hr, 1, 0, 1)
model2 = sarima(hr, 1, 0, 0)

# Calculate AIC, AICc, BIC
data.frame(
  "Model" = c("ARIMA(1, 0, 1)", "ARIMA(1, 0, 0)"),
  "AIC" = c(model1$AIC, model2$AIC),
  "AICc" = c(model1$AICc, model2$AICc),
  "BIC" = c(model1$BIC, model2$BIC)
) %>%
  kable(align = 'c') %>% kable_styling()

# Forecast and C.I.
hr_pred = sarima.for(hr, n.ahead = 4, 1, 0, 1)
data.frame(
  "Year" = c(2016:2019),
  "Pred." = round(hr_pred$pred, 4),
  "Low" = hr_pred_low,
  "High" = hr_pred_high
) %>%
  kable(align = 'c') %>% kable_styling()

# Plot forecasts against actual values
hr_act = arrange(batting, Year) %>%
  subset(Year >= 2015 & Year <= 2019, select = HR) %>%
  ts(start = 2015)
hr_act = diff(log(hr_act))

plot.ts(
  hr_act,
  lwd = 2,
  main = "Comparison between actual and predicted Home Runs for the 2016-2019 seasons",
  ylab = "Total Home Runs per season",
  xlab = "Year",
  xaxt = "n"
)
lines(2016:2019,
      hr_pred$pred,
      col = "red",
      lty = 2,
      lwd = 3)
legend(
  2016,
  0.00,
  c("Actual values", "Predicted values"),
  col = c("black", "red"),
  lty = c(1, 2),
  lwd = c(2, 3)
)
axis(side = 1, at = c(2016:2019))

# Spectral analysis, find first three dominant frequencies
hr.per = mvspec(hr, log = "no")

P3 = hr.per$details[order(hr.per$details[, 3], decreasing = TRUE),]

hr.u1 = 2 * P3[1, 3] / qchisq(.025, 2)
hr.l1 = 2 * P3[1, 3] / qchisq(.975, 2)
hr.u2 = 2 * P3[2, 3] / qchisq(.025, 2)
hr.l2 = 2 * P3[2, 3] / qchisq(.975, 2)
hr.u3 = 2 * P3[3, 3] / qchisq(.025, 2)
hr.l3 = 2 * P3[3, 3] / qchisq(.975, 2)

spec_int = data.frame(
  "Dominant Freq." = c(P3[1, 1], P3[2, 1], P3[3, 1]),
  "Spec" = c(P3[1, 3], P3[2, 3], P3[3, 3]),
  "Lower" = c(hr.l1, hr.l2, hr.l3),
  "Upper" = c(hr.u1, hr.u2, hr.u3)
)

spec_int %>% kable(align = 'c') %>% kable_styling()
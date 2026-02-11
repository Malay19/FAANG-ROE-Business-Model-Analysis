library(dplyr)
library(ggplot2)
faang <- read.csv("FAANG Financials (1).xlsx - Sheet1.csv")

str(faang)
summary(faang)

faang <- faang %>%
  mutate(PostCOVID = ifelse(FY >= 2020, 1, 0))

faang <- faang %>%
  mutate(
    BusinessModel = case_when(
      Ticket == "AAPL" ~ "High Margin / Low Turnover",
      Ticket == "AMZN" ~ "Low Margin / High Turnover",
      Ticket %in% c("META", "GOOG") ~ "Advertising",
      Ticket == "MSFT" ~ "Enterprise SaaS"
    )
  )

faang <- faang %>%
  group_by(Ticket) %>%
  arrange(FY) %>%
  mutate(RevenueGrowth = (Revenues - lag(Revenues)) / lag(Revenues)) %>%
  ungroup()

roe_stats <- faang %>%
  group_by(Ticket) %>%
  summarise(
    Mean_ROE = mean(ROE, na.rm = TRUE),
    SD_ROE   = sd(ROE, na.rm = TRUE),
    CV_ROE   = SD_ROE / Mean_ROE
  )

ggplot(faang, aes(
  x = AssetTurnover,
  y = NetProfitMargin,
  color = Ticket,
  shape = factor(PostCOVID)
)) +
  geom_point(size = 3) +
  labs(
    title = "Strategic Positioning: Asset Turnover vs Net Profit Margin",
    x = "Asset Turnover",
    y = "Net Profit Margin",
    shape = "Post-COVID"
  ) +
  theme_minimal()

ggplot(faang, aes(x = FY, y = ROE, color = Ticket, group = Ticket)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  labs(
    title = "Return on Equity Over Time by Company",
    x = "Fiscal Year",
    y = "ROE"
  ) +
  theme_minimal()


ggplot(faang, aes(x = AssetTurnover, y = ROE, color = Ticket)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Ticket) +
  labs(
    title = "Asset Turnover vs ROE by Company",
    x = "Asset Turnover",
    y = "ROE"
  ) +
  theme_minimal()

risk_return <- faang %>%
  group_by(Ticket) %>%
  summarise(
    avg_ROE = mean(ROE, na.rm = TRUE),
    sd_ROE  = sd(ROE, na.rm = TRUE),
    avg_revenue_bil = mean(Revenues, na.rm = TRUE) / 1e9
  )

ggplot(risk_return,
       aes(x = avg_ROE, y = sd_ROE, size = avg_revenue_bil, color = Ticket)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Risk–Return Profile by Company",
    x = "Average ROE",
    y = "ROE Volatility (Standard Deviation)",
    size = "Avg Revenue ($B)"
  ) +
  theme_minimal()

model_1 <- lm(
  ROE ~ NetProfitMargin + AssetTurnover + EquitytoAssetRatio,
  data = faang
)

summary(model_1)

model_2 <- lm(
  ROE ~ NetProfitMargin + AssetTurnover + EquitytoAssetRatio + Ticket,
  data = faang
)

summary(model_2)


model_3 <- lm(
  ROE ~ NetProfitMargin + AssetTurnover + EquitytoAssetRatio +
    NetProfitMargin:Ticket + AssetTurnover:Ticket,
  data = faang
)

summary(model_3)

model_4 <- lm(
  ROE ~ NetProfitMargin + AssetTurnover + EquitytoAssetRatio +
    PostCOVID +
    NetProfitMargin:PostCOVID +
    AssetTurnover:PostCOVID +
    Ticket,
  data = faang
)

summary(model_4)


faang <- faang %>%
  group_by(Ticket) %>%
  arrange(FY) %>%
  mutate(ROE_lag = lag(ROE)) %>%
  ungroup()


faang %>% 
  filter(is.na(ROE_lag)) %>% 
  select(Ticket, FY, ROE, ROE_lag)

model_5 <- lm(
  ROE ~ ROE_lag + NetProfitMargin + AssetTurnover + EquitytoAssetRatio + Ticket,
  data = faang
)

summary(model_5)

model_comparison <- data.frame(
  Model = c("Model 1: Baseline DuPont",
            "Model 2: Company Fixed Effects", 
            "Model 3: Interaction Effects",
            "Model 4: Post-COVID Effects",
            "Model 5: Lagged ROE"),
  R_squared = c(
    summary(model_1)$r.squared,
    summary(model_2)$r.squared,
    summary(model_3)$r.squared,
    summary(model_4)$r.squared,
    summary(model_5)$r.squared
  ),
  Adj_R_squared = c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared,
    summary(model_3)$adj.r.squared,
    summary(model_4)$adj.r.squared,
    summary(model_5)$adj.r.squared
  ),
  Residual_SE = c(
    summary(model_1)$sigma,
    summary(model_2)$sigma,
    summary(model_3)$sigma,
    summary(model_4)$sigma,
    summary(model_5)$sigma
  )
)

print(model_comparison)

faang_model3 <- faang %>%
  filter(!is.na(NetProfitMargin) & !is.na(AssetTurnover) & !is.na(EquitytoAssetRatio))

residuals_m3 <- residuals(model_3)
fitted_m3 <- fitted(model_3)


plot(fitted_m3, residuals_m3,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values (Model 3)",
     pch = 19,
     col = "steelblue")
abline(h = 0, lty = 2, col = "red")


qqnorm(residuals_m3, main = "Q-Q Plot (Model 3)", pch = 19, col = "steelblue")
qqline(residuals_m3, col = "red", lty = 2)

hist(residuals_m3, 
     breaks = 15,
     col = "lightblue",
     border = "black",
     main = "Distribution of Residuals (Model 3)",
     xlab = "Residuals")

cat("\nResidual Summary Statistics (Model 3):\n")
summary(residuals_m3)
cat("\nStandard Deviation of Residuals:", sd(residuals_m3), "\n")

outlier_threshold <- 2 * sd(residuals_m3)
outliers <- faang_model3 %>%
  mutate(residual = residuals_m3,
         fitted = fitted_m3) %>%
  filter(abs(residual) > outlier_threshold) %>%
  select(Ticket, FY, ROE, fitted, residual)

cat("\nPotential Outliers (|residual| > 2 SD):\n")
print(outliers)
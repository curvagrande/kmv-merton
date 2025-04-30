# Load necessary libraries
library(dplyr)
library(zoo)
library(readr)
library(lubridate)
library(tidyr)


####MERTON KMV MODEL
# Step 1: Load the file
new_stock_prices <- read_csv("apm466 stock pricesupdate.csv")

# Step 2: Print column names to identify the exact name
print(colnames(new_stock_prices))

# Step 3: Suppose we find the name is something like "Close (USD)" or has spaces
# We'll rename it safely
colnames(new_stock_prices) <- trimws(colnames(new_stock_prices))

# If the name is "Close (USD)", rename it:
colnames(new_stock_prices)[colnames(new_stock_prices) == "Close (USD)"] <- "Close"

# If it's "Close (USD)" with a non-breaking space, use:
colnames(new_stock_prices)[grepl("Close", colnames(new_stock_prices))] <- "Close"

# Step 4: Now convert the Close column to numeric
new_stock_prices <- new_stock_prices %>%
  mutate(Close = as.numeric(gsub(",", "", Close)))
new_stock_prices <- new_stock_prices %>%
  select(Date, Close, Volume)
new_stock_prices <- na.omit(new_stock_prices)


# Step 5: Check result
str(new_stock_prices)


library(dplyr)
library(zoo)
library(lubridate)

# Remove any rows where 'Close' contains "Dividend"
new_stock_prices <- new_stock_prices %>%
  filter(!grepl("Dividend", Close))

# Convert Date and Close columns to proper types
new_stock_prices <- new_stock_prices %>%
  mutate(
    Date = as.Date(Date, format = "%d-%b-%y"),
    Close = as.numeric(gsub(",", "", Close))
  ) %>%
  filter(!is.na(Date) & !is.na(Close))

# Sort by date
new_stock_prices <- new_stock_prices %>% arrange(Date)

# Compute log returns
new_stock_prices <- new_stock_prices %>%
  mutate(LogReturn = log(Close / lag(Close)))

# Compute 52-week rolling volatility (since this is weekly data)
new_stock_prices <- new_stock_prices %>%
  mutate(
    RollingVolatility = rollapply(LogReturn, width = 52, FUN = sd, fill = NA, align = "right") * sqrt(52)
  )


# Drop NA values from RollingVolatility
new_volatility_data <- new_stock_prices %>%
  filter(!is.na(RollingVolatility)) %>%
  select(Date, Close, RollingVolatility)

# View the resulting volatility data (or write it out)
print(head(new_volatility_data, 10))
# Or write to CSV if needed
write_csv(new_volatility_data, "kinross_volatility_data.csv")

######
# Load the risk-free rate file
risk_free <- read_csv("apm466 riskfreerate.csv")

# Clean date
risk_free <- risk_free %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date)

# If needed, pick the 1-year or 5-year rate (depending on what you assume for the Merton model)
# Example: Assume we use the 5-year column

risk_free <- risk_free %>%
  select(Date, `5 YR`) %>%
  rename(RiskFreeRate = `5 YR`)


balance_sheet <- read_csv("apm466balancesheet.csv")

# Clean the balance sheet
balance_sheet <- balance_sheet %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"),
         Year = year(Date)) %>%
  select(Year, `Total Debt`)
balance_sheet <- na.omit(balance_sheet)

write_csv(balance_sheet, "balancesheet_data.csv")
write_csv(risk_free, "riskfree_data.csv")
write_csv(new_stock_prices, "newstonkprices.csv")

####################



library(dplyr)
library(readr)
library(lubridate)
library(fuzzyjoin)

# Step 1: Load cleaned CSVs
volatility_data <- read_csv("kinross_volatility_data.csv")
riskfree_data <- read_csv("riskfree_data.csv")
balance_sheet <- read_csv("balancesheet_data.csv")

# Step 2: Prepare volatility data (add Year)
volatility_data <- volatility_data %>%
  mutate(Date = as.Date(Date),
         Year = year(Date)) %>%
  arrange(Date)

# Step 3: Prepare risk-free rate data (already has Date and Rate)
riskfree_data <- riskfree_data %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

merged_data <- fuzzyjoin::difference_left_join(
  volatility_data, riskfree_data,
  by = "Date",
  max_dist = as.difftime(60, units = "days")  # increase from 30 to 60
)
balance_sheet <- read_csv("balancesheet_data.csv")

# Make sure Year column exists
balance_sheet <- balance_sheet %>%
  mutate(Year = as.numeric(Year)) %>%
  select(Year, `Total Debt`)

# Then merge into the new data
new_merged_data <- merged_data %>%
  mutate(Year = lubridate::year(as.Date(Date.x))) %>%
  left_join(balance_sheet, by = "Year")

# Drop rows with missing risk-free or debt values
new_merged_data_clean <- new_merged_data %>%
  drop_na(RiskFreeRate, `Total Debt`)

write_csv(new_merged_data_clean, "new_merged_data_clean.csv")

###############
###from here
# Load required libraries
library(dplyr)
library(readr)
library(lubridate)
library(stats)

# Step 1: Load merged and cleaned data
data <- read_csv("new_merged_data_clean.csv")

# Step 2: Rename and rescale total debt (from thousands to full value)
data <- data %>%
  rename(TotalDebt = `Total Debt`,
         Date = `Date.x`) %>%
  mutate(TotalDebt = TotalDebt * 1000)

# Step 3: Use 20% of total debt as short-term default barrier
data <- data %>%
  mutate(EffectiveDebt = TotalDebt * 0.20)

# Step 4: Filter valid rows
data <- data %>%
  filter(Close > 0,
         RollingVolatility > 0,
         !is.na(RiskFreeRate),
         RiskFreeRate > -0.01)

# Step 5: Estimate asset value and asset volatility (no iteration)
data <- data %>%
  mutate(
    EstimatedAssetValue = Close + EffectiveDebt,
    EstimatedAssetVolatility = RollingVolatility * Close / EstimatedAssetValue,
    r = RiskFreeRate / 100
  )

# Step 6: Compute d2 and default probability
T <- 1
data <- data %>%
  mutate(
    d2 = (log(EstimatedAssetValue / EffectiveDebt) +
            (r - 0.5 * EstimatedAssetVolatility^2) * T) /
      (EstimatedAssetVolatility * sqrt(T)),
    ApproxDefaultProbability = pnorm(-d2)
  )

# Step 7: Save results
write_csv(data, "kinross_merton_approximation_r.csv")

library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)

# Load the data
data <- read_csv("kinross_merton_approximation_r.csv")

# Ensure date format
data <- data %>% mutate(Date = as.Date(Date))

# Plot default probability
ggplot(data, aes(x = Date, y = ApproxDefaultProbability)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Kinross Gold – Approximate Default Probability Over Time",
    x = "Date",
    y = "Default Probability"
  ) +
  theme_minimal()

library(kableExtra)

datakbl <- data |>
  select(c("Date", "Close", "EffectiveDebt", "EstimatedAssetValue", "EstimatedAssetVolatility", "ApproxDefaultProbability"))
head(datakbl) |>
  kable(
    col.names = c("Date", "Close", "EffectiveDebt", "EstimatedAssetValue", "EstimatedAssetVolatility", "ApproxDefaultProbability"),
    align = c("l", "r", "r", "r", "r", "r"),
    digits = 10, booktabs = TRUE, linesep = ""
  )
##to here

###############


#######credit metrics


#########################

# Define rating states
ratings <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")

# Transition matrix based on your image
transition_matrix <- matrix(c(
  0.9081, 0.0833, 0.0068, 0.0006, 0.0012, 0.0000, 0.0000, 0.0000,
  0.0070, 0.9065, 0.0779, 0.0064, 0.0006, 0.0014, 0.0002, 0.0000,
  0.0009, 0.0227, 0.9105, 0.0552, 0.0074, 0.0026, 0.0001, 0.0006,
  0.0002, 0.0033, 0.0595, 0.8693, 0.0530, 0.0117, 0.0012, 0.0018,
  0.0003, 0.0014, 0.0067, 0.0773, 0.8053, 0.0884, 0.0100, 0.0106,
  0.0000, 0.0011, 0.0024, 0.0043, 0.0648, 0.8346, 0.0407, 0.0520,
  0.0022, 0.0000, 0.0022, 0.0130, 0.0238, 0.1124, 0.6486, 0.1979,
  0, 0, 0, 0, 0, 0, 0, 1
), nrow = 8, byrow = TRUE)

rownames(transition_matrix) <- ratings
colnames(transition_matrix) <- ratings

# Matrix power (using %^% from 'expm' package)
library(expm)
transition_matrix_5yr <- transition_matrix %^% 5

face_value <- 500000000
coupon_rate <- 0.045
maturity_years <- 2  # From now to July 2027
recovery_rate <- 0.40
initial_rating <- "BBB"

# Define example spreads by rating in basis points (could be from Bloomberg or textbook)
credit_spreads <- c(
  AAA = 0.005, AA = 0.007, A = 0.01, BBB = 0.015,
  BB = 0.03, B = 0.05, CCC = 0.08, D = NA  # D handled via recovery
)

# Risk-free rate: we'll assume 3% flat for example
rf <- 0.03

# Value of bond under each terminal rating
rating_values <- sapply(ratings, function(r) {
  if (r == "D") {
    return(face_value * recovery_rate)
  } else {
    ytm <- rf + credit_spreads[r]
    cfs <- rep(face_value * coupon_rate, maturity_years)
    pv_coupon <- sum(cfs / (1 + ytm)^(1:maturity_years))
    pv_principal <- face_value / (1 + ytm)^maturity_years
    return(pv_coupon + pv_principal)
  }
})
names(rating_values) <- ratings

# Get 5-year transition probabilities from BBB to each rating
bbb_row <- transition_matrix_5yr["BBB", ]

# Expected value
expected_value <- sum(bbb_row * rating_values)

# Variance of portfolio value
value_variance <- sum(bbb_row * (rating_values - expected_value)^2)

# Show results
expected_value
sqrt(value_variance)  # standard deviation


library(ggplot2)

df <- data.frame(
  Rating = ratings,
  Probability = bbb_row,
  Value = rating_values
)

ggplot(df, aes(x = reorder(Rating, -Value), y = Probability, fill = Rating)) +
  geom_col() +
  labs(title = "5-Year Credit State Distribution from BBB",
       y = "Probability", x = "Final Rating") +
  theme_minimal()


####################

library(expm)
library(ggplot2)
library(dplyr)

# Define 1-year transition matrix
ratings <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
transition_matrix <- matrix(c(
  0.9081, 0.0833, 0.0068, 0.0006, 0.0012, 0.0000, 0.0000, 0.0000,
  0.0070, 0.9065, 0.0779, 0.0064, 0.0006, 0.0014, 0.0002, 0.0000,
  0.0009, 0.0227, 0.9105, 0.0552, 0.0074, 0.0026, 0.0001, 0.0006,
  0.0002, 0.0033, 0.0595, 0.8693, 0.0530, 0.0117, 0.0012, 0.0018,
  0.0003, 0.0014, 0.0067, 0.0773, 0.8053, 0.0884, 0.0100, 0.0106,
  0.0000, 0.0011, 0.0024, 0.0043, 0.0648, 0.8346, 0.0407, 0.0520,
  0.0022, 0.0000, 0.0022, 0.0130, 0.0238, 0.1124, 0.6486, 0.1979,
  0, 0, 0, 0, 0, 0, 0, 1
), nrow = 8, byrow = TRUE)
rownames(transition_matrix) <- ratings
colnames(transition_matrix) <- ratings

# Step 1: CreditMetrics cumulative default probabilities (BBB → D)
credit_pd <- numeric(5)
for (t in 1:5) {
  pt_matrix <- transition_matrix %^% t
  credit_pd[t] <- pt_matrix["BBB", "D"]
}

PD_KMV_1yr <- data %>%
  arrange(desc(Date)) %>%
  slice(1) %>%
  pull(ApproxDefaultProbability)
lambda <- -log(1 - PD_KMV_1yr)
kmv_pd <- 1 - exp(-lambda * (1:5))


# Step 3: Create data frame for plotting
df <- data.frame(
  Year = rep(1:5, 2),
  PD = c(kmv_pd, credit_pd),
  Model = rep(c("KMV", "CreditMetrics"), each = 5)
)

# Step 4: Plot
ggplot(df, aes(x = Year, y = PD, color = Model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Default Probability Over Time (KMV vs CreditMetrics)",
       x = "Year", y = "Cumulative Probability of Default") +
  theme_minimal() +
  theme(legend.position = "top")



#####table for params

library(dplyr)
library(kableExtra)

# Load final KMV data
data <- read_csv("kinross_merton_approximation_r.csv") %>%
  arrange(desc(Date)) %>%
  slice(1)

# Extract values
S <- data$Close
sigma_S <- data$RollingVolatility
r <- data$r
T <- 1
K <- data$EffectiveDebt
V <- data$EstimatedAssetValue
sigma_A <- data$EstimatedAssetVolatility

# Compute d1 and delta
d1 <- (log(V / K) + (r + 0.5 * sigma_A^2) * T) / (sigma_A * sqrt(T))
Delta <- pnorm(d1)

# Compute fixed point
fixed_point <- (sigma_S * S) / (V * Delta)

# Create parameter table
merton_params <- data.frame(
  Amount = c("Interest rate", "Underlying price", "Time to maturity", "Exercise price",
             "Stock volatility", "Asset volatility", "Option price", "Delta", "Fixed point"),
  Symbol = c("r", "V", "T", "K", "σS", "σA", "S", "Δ", "σS S / (V Δ)"),
  Value = c(r, V, T, K, sigma_S, sigma_A, S, Delta, fixed_point)
)

# Format and display
merton_params %>%
  mutate(Value = round(Value, 4)) %>%
  kable("html", booktabs = TRUE, caption = "Merton Model Iteration Parameters") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)


#########works till here dont touch


##########################


####new tbl experiment

library(dplyr)
library(readr)
library(lubridate)

# Load your merged data
data_1 <- read_csv("new_merged_data_clean.csv") %>%
  rename(TotalDebt = `Total Debt`, Date = `Date.x`) %>%
  mutate(TotalDebt = TotalDebt * 1000,
         EffectiveDebt = TotalDebt * 0.20)

# Estimate total equity (assume 1 billion shares outstanding)
shares_outstanding <- 1e9  # <-- Adjust if you know exact number
data_1 <- data_1 %>%
  filter(Close > 0, RollingVolatility > 0, RiskFreeRate > -0.01) %>%
  mutate(
    EquityValue = Close * shares_outstanding,
    EstimatedAssetValue = EquityValue + EffectiveDebt,
    EstimatedAssetVolatility = RollingVolatility * EquityValue / EstimatedAssetValue,
    r = RiskFreeRate / 100
  )

# Recalculate d2 and default probability
T <- 1
data_1 <- data_1 %>%
  mutate(
    d2 = (log(EstimatedAssetValue / EffectiveDebt) +
            (r - 0.5 * EstimatedAssetVolatility^2) * T) /
      (EstimatedAssetVolatility * sqrt(T)),
    ApproxDefaultProbability = pnorm(-d2)
  )

# Save corrected results
write_csv(data_1, "kinross_merton_approximation_scaled.csv")

library(readr)
library(dplyr)
library(expm)
library(ggplot2)

# Step 1: Load KMV-scaled output
data_1 <- read_csv("kinross_merton_approximation_scaled.csv")

# Step 2: Extract latest KMV 1-year PD
PD_KMV_1yr_1 <- data_1 %>%
  arrange(desc(Date)) %>%
  slice(1) %>%
  pull(ApproxDefaultProbability)

# Step 3: Compute KMV cumulative PD over 5 years (using constant hazard model)
lambda_1 <- -log(1 - PD_KMV_1yr_1)
kmv_pd_1 <- 1 - exp(-lambda_1 * (1:5))  # years 1 to 5

# Step 4: Define CreditMetrics 1-year transition matrix
ratings <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
transition_matrix <- matrix(c(
  0.9081, 0.0833, 0.0068, 0.0006, 0.0012, 0.0000, 0.0000, 0.0000,
  0.0070, 0.9065, 0.0779, 0.0064, 0.0006, 0.0014, 0.0002, 0.0000,
  0.0009, 0.0227, 0.9105, 0.0552, 0.0074, 0.0026, 0.0001, 0.0006,
  0.0002, 0.0033, 0.0595, 0.8693, 0.0530, 0.0117, 0.0012, 0.0018,
  0.0003, 0.0014, 0.0067, 0.0773, 0.8053, 0.0884, 0.0100, 0.0106,
  0.0000, 0.0011, 0.0024, 0.0043, 0.0648, 0.8346, 0.0407, 0.0520,
  0.0022, 0.0000, 0.0022, 0.0130, 0.0238, 0.1124, 0.6486, 0.1979,
  0, 0, 0, 0, 0, 0, 0, 1
), nrow = 8, byrow = TRUE)
rownames(transition_matrix) <- ratings
colnames(transition_matrix) <- ratings

# Step 5: CreditMetrics cumulative PDs (BBB → D) for 1 to 5 years
credit_pd_1 <- numeric(5)
for (t in 1:5) {
  pt <- transition_matrix %^% t
  credit_pd_1[t] <- pt["BBB", "D"]
}

# Step 6: Build and plot combined data
df <- data.frame(
  Year = rep(1:5, 2),
  PD = c(kmv_pd_1, credit_pd_1),
  Model = rep(c("KMV", "CreditMetrics"), each = 5)
)

ggplot(df, aes(x = Year, y = PD, color = Model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Default Probability Over Time (KMV vs CreditMetrics)",
    x = "Year",
    y = "Cumulative Default Probability"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



library(dplyr)
library(kableExtra)

# Load final KMV data
data_1 <- read_csv("kinross_merton_approximation_scaled.csv") %>%
  arrange(desc(Date)) %>%
  slice(1)

# Extract values
S <- data_1$Close
sigma_S <- data_1$RollingVolatility
r <- data_1$r
T <- 1
K <- data_1$EffectiveDebt
V <- data_1$EstimatedAssetValue
sigma_A <- data_1$EstimatedAssetVolatility

# Compute d1 and delta
d1 <- (log(V / K) + (r + 0.5 * sigma_A^2) * T) / (sigma_A * sqrt(T))
Delta <- pnorm(d1)

# Compute fixed point
fixed_point <- (sigma_S * S) / (V * Delta)

# Create parameter table
merton_params <- data.frame(
  Amount = c("Interest rate", "Underlying price", "Time to maturity", "Exercise price",
             "Stock volatility", "Asset volatility", "Option price", "Delta", "Fixed point"),
  Symbol = c("r", "V", "T", "K", "σS", "σA", "S", "Δ", "σS S / (V Δ)"),
  Value = c(r, V, T, K, sigma_S, sigma_A, S, Delta, fixed_point)
)

# Format and display
merton_params %>%
  mutate(Value = round(Value, 4)) %>%
  kable("html", booktabs = TRUE, caption = "Merton Model Iteration Parameters") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)

################################-----------------------------------------------------
#| echo: FALSE
#| message: FALSE
#| warning: false
#| results: 'hide'
#| include: false
# Load necessary libraries
library(dplyr)
library(zoo)
library(readr)
library(lubridate)
library(tidyr)


####MERTON KMV MODEL
# Step 1: Load the file
new_stock_prices <- read_csv("~/apm466a2/apm466 stock pricesupdate.csv")

# Step 2: Print column names to identify the exact name
print(colnames(new_stock_prices))

# Step 3: Suppose we find the name is something like "Close (USD)" or has spaces
# We'll rename it safely
colnames(new_stock_prices) <- trimws(colnames(new_stock_prices))

# If the name is "Close (USD)", rename it:
colnames(new_stock_prices)[colnames(new_stock_prices) == "Close (USD)"] <- "Close"

# If it's "Close (USD)" with a non-breaking space, use:
colnames(new_stock_prices)[grepl("Close", colnames(new_stock_prices))] <- "Close"

# Step 4: Now convert the Close column to numeric
new_stock_prices <- new_stock_prices %>%
  mutate(Close = as.numeric(gsub(",", "", Close)))
new_stock_prices <- new_stock_prices %>%
  select(Date, Close, Volume)
new_stock_prices <- na.omit(new_stock_prices)


# Step 5: Check result
str(new_stock_prices)


library(dplyr)
library(zoo)
library(lubridate)

# Remove any rows where 'Close' contains "Dividend"
new_stock_prices <- new_stock_prices %>%
  filter(!grepl("Dividend", Close))

# Convert Date and Close columns to proper types
new_stock_prices <- new_stock_prices %>%
  mutate(
    Date = as.Date(Date, format = "%d-%b-%y"),
    Close = as.numeric(gsub(",", "", Close))
  ) %>%
  filter(!is.na(Date) & !is.na(Close))

# Sort by date
new_stock_prices <- new_stock_prices %>% arrange(Date)

# Compute log returns
new_stock_prices <- new_stock_prices %>%
  mutate(LogReturn = log(Close / lag(Close)))

# Compute 52-week rolling volatility (since this is weekly data)
new_stock_prices <- new_stock_prices %>%
  mutate(
    RollingVolatility = rollapply(LogReturn, width = 52, FUN = sd, fill = NA, align = "right") * sqrt(52)
  )


# Drop NA values from RollingVolatility
new_volatility_data <- new_stock_prices %>%
  filter(!is.na(RollingVolatility)) %>%
  select(Date, Close, RollingVolatility)

# View the resulting volatility data (or write it out)
print(head(new_volatility_data, 10))
# Or write to CSV if needed
write_csv(new_volatility_data, "~/apm466a2/kinross_volatility_data.csv")

######
# Load the risk-free rate file
risk_free <- read_csv("~/apm466a2/apm466 riskfreerate.csv")

# Clean date
risk_free <- risk_free %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date)

# If needed, pick the 1-year or 5-year rate (depending on what you assume for the Merton model)
# Example: Assume we use the 5-year column

risk_free <- risk_free %>%
  select(Date, `5 YR`) %>%
  rename(RiskFreeRate = `5 YR`)


balance_sheet <- read_csv("~/apm466a2/apm466balancesheet.csv")

# Clean the balance sheet
balance_sheet <- balance_sheet %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"),
         Year = year(Date)) %>%
  select(Year, `Total Debt`)
balance_sheet <- na.omit(balance_sheet)

write_csv(balance_sheet, "~/apm466a2/balancesheet_data.csv")
write_csv(risk_free, "~/apm466a2/riskfree_data.csv")
write_csv(new_stock_prices, "~/apm466a2/newstonkprices.csv")

####################



library(dplyr)
library(readr)
library(lubridate)
library(fuzzyjoin)

# Step 1: Load cleaned CSVs
volatility_data <- read_csv("~/apm466a2/kinross_volatility_data.csv")
riskfree_data <- read_csv("~/apm466a2/riskfree_data.csv")
balance_sheet <- read_csv("~/apm466a2/balancesheet_data.csv")

# Step 2: Prepare volatility data (add Year)
volatility_data <- volatility_data %>%
  mutate(Date = as.Date(Date),
         Year = year(Date)) %>%
  arrange(Date)

# Step 3: Prepare risk-free rate data (already has Date and Rate)
riskfree_data <- riskfree_data %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

merged_data <- fuzzyjoin::difference_left_join(
  volatility_data, riskfree_data,
  by = "Date",
  max_dist = as.difftime(60, units = "days")  # increase from 30 to 60
)
balance_sheet <- read_csv("~/apm466a2/balancesheet_data.csv")

# Make sure Year column exists
balance_sheet <- balance_sheet %>%
  mutate(Year = as.numeric(Year)) %>%
  select(Year, `Total Debt`)

# Then merge into the new data
new_merged_data <- merged_data %>%
  mutate(Year = lubridate::year(as.Date(Date.x))) %>%
  left_join(balance_sheet, by = "Year")

# Drop rows with missing risk-free or debt values
new_merged_data_clean <- new_merged_data %>%
  drop_na(RiskFreeRate, `Total Debt`)

write_csv(new_merged_data_clean, "~/apm466a2/new_merged_data_clean.csv")

###############


#######credit metrics


#########################

# Define rating states
ratings <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")

# Transition matrix based on your image
transition_matrix <- matrix(c(
  0.9081, 0.0833, 0.0068, 0.0006, 0.0012, 0.0000, 0.0000, 0.0000,
  0.0070, 0.9065, 0.0779, 0.0064, 0.0006, 0.0014, 0.0002, 0.0000,
  0.0009, 0.0227, 0.9105, 0.0552, 0.0074, 0.0026, 0.0001, 0.0006,
  0.0002, 0.0033, 0.0595, 0.8693, 0.0530, 0.0117, 0.0012, 0.0018,
  0.0003, 0.0014, 0.0067, 0.0773, 0.8053, 0.0884, 0.0100, 0.0106,
  0.0000, 0.0011, 0.0024, 0.0043, 0.0648, 0.8346, 0.0407, 0.0520,
  0.0022, 0.0000, 0.0022, 0.0130, 0.0238, 0.1124, 0.6486, 0.1979,
  0, 0, 0, 0, 0, 0, 0, 1
), nrow = 8, byrow = TRUE)

rownames(transition_matrix) <- ratings
colnames(transition_matrix) <- ratings

# Matrix power (using %^% from 'expm' package)
library(expm)
transition_matrix_5yr <- transition_matrix %^% 5

face_value <- 500000000
coupon_rate <- 0.045
maturity_years <- 2  # From now to July 2027
recovery_rate <- 0.40
initial_rating <- "BBB"

# Define example spreads by rating in basis points (could be from Bloomberg or textbook)
credit_spreads <- c(
  AAA = 0.005, AA = 0.007, A = 0.01, BBB = 0.015,
  BB = 0.03, B = 0.05, CCC = 0.08, D = NA  # D handled via recovery
)

# Risk-free rate: we'll assume 3% flat for example
rf <- 0.03

# Value of bond under each terminal rating
rating_values <- sapply(ratings, function(r) {
  if (r == "D") {
    return(face_value * recovery_rate)
  } else {
    ytm <- rf + credit_spreads[r]
    cfs <- rep(face_value * coupon_rate, maturity_years)
    pv_coupon <- sum(cfs / (1 + ytm)^(1:maturity_years))
    pv_principal <- face_value / (1 + ytm)^maturity_years
    return(pv_coupon + pv_principal)
  }
})
names(rating_values) <- ratings

# Get 5-year transition probabilities from BBB to each rating
bbb_row <- transition_matrix_5yr["BBB", ]

# Expected value
expected_value <- sum(bbb_row * rating_values)

# Variance of portfolio value
value_variance <- sum(bbb_row * (rating_values - expected_value)^2)

# Show results
expected_value
sqrt(value_variance)  # standard deviation


library(ggplot2)

df <- data.frame(
  Rating = ratings,
  Probability = bbb_row,
  Value = rating_values
)

ggplot(df, aes(x = reorder(Rating, -Value), y = Probability, fill = Rating)) +
  geom_col() +
  labs(title = "5-Year Credit State Distribution from BBB",
       y = "Probability", x = "Final Rating") +
  theme_minimal()


####################

library(expm)
library(ggplot2)
library(dplyr)

# Define 1-year transition matrix
ratings <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
transition_matrix <- matrix(c(
  0.9081, 0.0833, 0.0068, 0.0006, 0.0012, 0.0000, 0.0000, 0.0000,
  0.0070, 0.9065, 0.0779, 0.0064, 0.0006, 0.0014, 0.0002, 0.0000,
  0.0009, 0.0227, 0.9105, 0.0552, 0.0074, 0.0026, 0.0001, 0.0006,
  0.0002, 0.0033, 0.0595, 0.8693, 0.0530, 0.0117, 0.0012, 0.0018,
  0.0003, 0.0014, 0.0067, 0.0773, 0.8053, 0.0884, 0.0100, 0.0106,
  0.0000, 0.0011, 0.0024, 0.0043, 0.0648, 0.8346, 0.0407, 0.0520,
  0.0022, 0.0000, 0.0022, 0.0130, 0.0238, 0.1124, 0.6486, 0.1979,
  0, 0, 0, 0, 0, 0, 0, 1
), nrow = 8, byrow = TRUE)
rownames(transition_matrix) <- ratings
colnames(transition_matrix) <- ratings

# Step 1: CreditMetrics cumulative default probabilities (BBB → D)
credit_pd <- numeric(5)
for (t in 1:5) {
  pt_matrix <- transition_matrix %^% t
  credit_pd[t] <- pt_matrix["BBB", "D"]
}

PD_KMV_1yr <- data_1 %>%
  arrange(desc(Date)) %>%
  slice(1) %>%
  pull(ApproxDefaultProbability)
lambda <- -log(1 - PD_KMV_1yr)
kmv_pd <- 1 - exp(-lambda * (1:5))


# Step 3: Create data frame for plotting
df <- data.frame(
  Year = rep(1:5, 2),
  PD = c(kmv_pd, credit_pd),
  Model = rep(c("KMV", "CreditMetrics"), each = 5)
)

# Step 4: Plot
ggplot(df, aes(x = Year, y = PD, color = Model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Default Probability Over Time (KMV vs CreditMetrics)",
       x = "Year", y = "Cumulative Probability of Default") +
  theme_minimal() +
  theme(legend.position = "top")


#########works till here dont touch


##########################


library(dplyr)
library(readr)
library(lubridate)

# Load your merged data
data_1 <- read_csv("~/apm466a2/new_merged_data_clean.csv") %>%
  rename(TotalDebt = `Total Debt`, Date = `Date.x`) %>%
  mutate(TotalDebt = TotalDebt * 1000,
         EffectiveDebt = TotalDebt * 0.20)

# Estimate total equity (assume 1 billion shares outstanding)
shares_outstanding <- 1e9  # <-- Adjust if you know exact number
data_1 <- data_1 %>%
  filter(Close > 0, RollingVolatility > 0, RiskFreeRate > -0.01) %>%
  mutate(
    EquityValue = Close * shares_outstanding,
    EstimatedAssetValue = EquityValue + EffectiveDebt,
    EstimatedAssetVolatility = RollingVolatility * EquityValue / EstimatedAssetValue,
    r = RiskFreeRate / 100
  )

# Recalculate d2 and default probability
T <- 1
data_1 <- data_1 %>%
  mutate(
    d2 = (log(EstimatedAssetValue / EffectiveDebt) +
            (r - 0.5 * EstimatedAssetVolatility^2) * T) /
      (EstimatedAssetVolatility * sqrt(T)),
    ApproxDefaultProbability = pnorm(-d2)
  )

# Save corrected results
write_csv(data_1, "~/apm466a2/kinross_merton_approximation_scaled.csv")

library(readr)
library(dplyr)
library(expm)
library(ggplot2)

# Step 1: Load KMV-scaled output
data_1 <- read_csv("~/apm466a2/kinross_merton_approximation_scaled.csv")

# Step 2: Extract latest KMV 1-year PD
PD_KMV_1yr_1 <- data_1 %>%
  arrange(desc(Date)) %>%
  slice(1) %>%
  pull(ApproxDefaultProbability)

# Step 3: Compute KMV cumulative PD over 5 years (using constant hazard model)
lambda_1 <- -log(1 - PD_KMV_1yr_1)
kmv_pd_1 <- 1 - exp(-lambda_1 * (1:5))  # years 1 to 5

# Step 4: Define CreditMetrics 1-year transition matrix
ratings <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
transition_matrix <- matrix(c(
  0.9081, 0.0833, 0.0068, 0.0006, 0.0012, 0.0000, 0.0000, 0.0000,
  0.0070, 0.9065, 0.0779, 0.0064, 0.0006, 0.0014, 0.0002, 0.0000,
  0.0009, 0.0227, 0.9105, 0.0552, 0.0074, 0.0026, 0.0001, 0.0006,
  0.0002, 0.0033, 0.0595, 0.8693, 0.0530, 0.0117, 0.0012, 0.0018,
  0.0003, 0.0014, 0.0067, 0.0773, 0.8053, 0.0884, 0.0100, 0.0106,
  0.0000, 0.0011, 0.0024, 0.0043, 0.0648, 0.8346, 0.0407, 0.0520,
  0.0022, 0.0000, 0.0022, 0.0130, 0.0238, 0.1124, 0.6486, 0.1979,
  0, 0, 0, 0, 0, 0, 0, 1
), nrow = 8, byrow = TRUE)
rownames(transition_matrix) <- ratings
colnames(transition_matrix) <- ratings

# Step 5: CreditMetrics cumulative PDs (BBB → D) for 1 to 5 years
credit_pd_1 <- numeric(5)
for (t in 1:5) {
  pt <- transition_matrix %^% t
  credit_pd_1[t] <- pt["BBB", "D"]
}

# Step 6: Build and plot combined data
df <- data.frame(
  Year = rep(1:5, 2),
  PD = c(kmv_pd_1, credit_pd_1),
  Model = rep(c("KMV", "CreditMetrics"), each = 5)
)

ggplot(df, aes(x = Year, y = PD, color = Model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Default Probability Over Time (KMV vs CreditMetrics)",
    x = "Year",
    y = "Cumulative Default Probability"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



library(dplyr)
library(kableExtra)

# Load final KMV data
dataa_1 <- read_csv("~/apm466a2/kinross_merton_approximation_scaled.csv") %>%
  arrange(desc(Date)) %>%
  slice(1)

# Extract values
S <- dataa_1$Close
sigma_S <- dataa_1$RollingVolatility
r <- dataa_1$r
T <- 1
K <- dataa_1$EffectiveDebt
V <- dataa_1$EstimatedAssetValue
sigma_A <- dataa_1$EstimatedAssetVolatility

# Compute d1 and delta
d1 <- (log(V / K) + (r + 0.5 * sigma_A^2) * T) / (sigma_A * sqrt(T))
Delta <- pnorm(d1)

# Compute fixed point
fixed_point <- (sigma_S * S) / (V * Delta)

# Create parameter table
merton_params <- data.frame(
  Amount = c("Interest rate", "Underlying price", "Time to maturity", "Exercise price",
             "Stock volatility", "Asset volatility", "Option price", "Delta", "Fixed point"),
  Symbol = c("r", "V", "T", "K", "σS", "σA", "S", "Δ", "σS S / (V Δ)"),
  Value = c(r, V, T, K, sigma_S, sigma_A, S, Delta, fixed_point)
)

# Format and display
merton_params %>%
  mutate(Value = round(Value, 4)) %>%
  kable("html", booktabs = TRUE, caption = "Merton Model Iteration Parameters") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)



library(kableExtra)

data_1 <- data_1 |>
  select(c("Date", "Close", "EffectiveDebt", "EstimatedAssetValue", "EstimatedAssetVolatility", "ApproxDefaultProbability"))
head(data_1, n=20) |>
  kable(
    col.names = c("Date", "Close", "EffectiveDebt", "EstimatedAssetValue", "EstimatedAssetVolatility", "ApproxDefaultProbability"),
    align = c("l", "r", "r", "r", "r", "r"),
    digits = 10, booktabs = TRUE, linesep = ""
  )



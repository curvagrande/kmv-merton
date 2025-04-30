import pandas as pd
import numpy as np
from scipy.stats import norm

# Step 1: Load merged and cleaned dataset
data = pd.read_csv("new_merged_data_clean.csv")  # Already includes: Date.x, Close, RollingVolatility, RiskFreeRate, Total Debt
data = data.rename(columns={"Date.x": "Date", "Total Debt": "TotalDebt"})

# Step 2: Rescale TotalDebt from thousands to full dollar amounts
data['TotalDebt'] = data['TotalDebt'] * 1000

# Step 3: Use only 20% of debt to represent short-term/default-relevant liabilities
data['EffectiveDebt'] = data['TotalDebt'] * 0.20

# Step 4: Filter for valid inputs
data = data[(data['Close'] > 0) & 
            (data['RollingVolatility'] > 0) & 
            (data['RiskFreeRate'].notna()) & 
            (data['RiskFreeRate'] > -0.01)]

# Step 5: Estimate firm value and asset volatility (no iteration)
data['EstimatedAssetValue'] = data['Close'] + data['EffectiveDebt']
data['EstimatedAssetVolatility'] = data['RollingVolatility'] * data['Close'] / data['EstimatedAssetValue']

# Step 6: Compute distance to default (d2) and default probability
T = 1
data['r'] = data['RiskFreeRate'] / 100  # Convert percentage to decimal

data['d2'] = (
    np.log(data['EstimatedAssetValue'] / data['EffectiveDebt']) +
    (data['r'] - 0.5 * data['EstimatedAssetVolatility'] ** 2) * T
) / (data['EstimatedAssetVolatility'] * np.sqrt(T))

data['ApproxDefaultProbability'] = norm.cdf(-data['d2'])

# Step 7: Save results
data.to_csv("kinross_merton_approximation.csv", index=False)

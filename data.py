from ucimlrepo import fetch_ucirepo
import pandas as pd
import numpy as np
"""
fetches dataset from uciml repo and saves to csv, so that we can use it in R
"""
  
# fetch dataset 
heart_disease = fetch_ucirepo(id=45) 
  
# data (as pandas dataframes) 
X = heart_disease.data.features 
y = heart_disease.data.targets 
  
# metadata 
# print(heart_disease.metadata) 
  
# variable information 
print(heart_disease.variables)

z = X.join(y)
# z.to_csv("data.csv")



# print(z.head())

for var in z.columns:
    print(var)
    print(f"No NAN:{z[var].isnull().sum()}")
    print(f"Mean of {var}: {np.mean(z[var])}")
    print(f"Min of {var}: {np.min(z[var])}")
    print(f"Max of {var}: {np.max(z[var])}")
    # remove outliers larger than 3 std
    data_clean = z[np.abs(z[var]-z[var].mean()) <= (2*z[var].std())]
    # remove NAN
    data_clean = data_clean.dropna()
    print(f"Shape of data_clean: {data_clean.shape}")
print("Done")


data_clean.to_csv("data_clean.csv")
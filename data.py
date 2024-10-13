from ucimlrepo import fetch_ucirepo
import pandas as pd

"""
fetches dataset from uciml repo and saves to csv, so that we can use it in R
"""
  
# fetch dataset 
heart_disease = fetch_ucirepo(id=45) 
  
# data (as pandas dataframes) 
X = heart_disease.data.features 
y = heart_disease.data.targets 
  
# metadata 
print(heart_disease.metadata) 
  
# variable information 
print(heart_disease.variables)

z = X.join(y)
z.to_csv("data.csv")

print("Done")
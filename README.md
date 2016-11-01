# House Prices: Advanced Regression Techniques

## Data Cleaning

* Discarded features with more than 50% of NAs
* Duplicates removed
* NA imputation: 
    * Categorical features associated with null quantities (example: GarageType; when there is no Garage) were imputed with "none"
    * Numerical features associated with null quantities (example: GarageYrBlt; when there is no Garage) were imputed with the minimum value
    * Other numerical features were imputed with the general mean or with the mean by neighbourhood.
* Features with near zero variance were excluded

## Feature Engineering

* Were identified all the ordinal categories and transformed to numeric.
* Nominal categories with few values and similar SalePrice were aggregated
* Were created dummy variables from the categorical variables
* Were discarded all the dummy variables with less than 1% of 1s.
* New Feactures: 
    * remodeled: 1/0
    * age: YrSold-YearBuilt or YrSold-YearRemodAdd in case of remodelation
    * garage_age: YrSold-GarageYrBlt
    * epoch: More older than 100/ More older than 50/ younger than 50
    * lot_side: LotArea/LotFrontage
    * space: X2ndFlrSF + X1stFlrSF
    * sec: X2ndFlrSF/X1stFlrSF
    * bath: FullBath+HalfBath
    * Bsmtbath: BsmtFullBath + BsmtHalfBath
    * totbath: Bsmtbath + bath
    * bathRoomRel: TotRmsAbvGrd- totbath
    * LotArea: log(data$LotArea+1)
    

* Were added quadratic and cubic components to features with a range wider than 500.


## Model Approach 

* Run data cleaning
* Run feature engineering
* Fit Lasso Model
* Identify outliers
* Run data cleaning without the identified outliers
* Run feature engineering with quadratic and cubic features
* Apply the model




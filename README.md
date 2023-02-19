# **FinQBoost** 
## Financial Portfolio Quintile Probability Forecaster <br/> <font color=438496>#2 winner of M6 Financial Forecasting Competition</font>
<br/>

![](./FinQBoost.png)

- **A boosting-based model that forecasts 4-weeks-ahead relative performance probability of the components of a portfolio as quintile probabilities.**

- [**This post**](https://medium.com/@miguelpmich/the-challenge-of-financial-forecasting-m6-edition-b26c20464f1b)
explains the main design aspects of the model and how it was used successfully in the M6. Tested monthly in real-time during the 1-year competition period its performance was very stable, beating the "naive" uniform forecast scores in all 12 monthly submissions. Its final mean score (RPS) was 0.15648, winning the 2nd global prize in the Forecasting Track. It was also the key to winning the 2nd prize in the main global duathlon Track of the M6 Competition.


- It's fast, it takes only **9 seconds** to both train and predict on a laptop CPU (Intel(R) Core(TM) i7-11800H).



## Usage
### Input data
- Publicly available Yahoo Finance data was used for the competition. As Yahoo does not allow the redistribution of their data, before running the script **adjusted EOD** price history must be downloaded from Yahoo Finance and saved in the data folder in `.csv` format.
- Prices must be in wide shape (one column for each product) with  date column first, dates in  "YYYY-MM-DD" format.
- All NAs in downloaded prices should be filled with "last-observation-carried-forward".
- At any given time Yahoo Finance data can occasionally include some issues that require manual fix. A test for the integrity and consistency of data after each download is highly advisable.
- Specific to the competition period:
    
    - DRE was acquired/delisted and for the last 4 months its price was held constant. The code automatically handles this but previous DRE prices have to be included. Retraining during last 4 months fixes full DRE train price history.
    - During the competition, VXX prices were overwritten with perfectly correlated VIXY prices due to the latter's much longer history.
    - During the first 3 months of the competition some serious errors in Yahoo Finance data required manual fix. The issue seems to be currently fixed for that period in Yahoo Finance but it will affect exact reproducibility of early templates.



### Make a forecast
To generate 4-weeks-ahead forecasts of the portfolio performance quintile probabilities:

- populate `data/` folder with the portfolio's adjusted price history `.csv` file.
    
    
- **run** `MakeForecast.R` **providing as argument the desired starting point to forecast from**. This date should be a Friday from available price history, it was always the last one during the real time competition. For example, to obtain submission for last month of the competition in January 2023:<br/><br/>`cd src; Rscript MakeForecast.R 2023-06-01`
  

Forecasts will be saved as `template.csv` in `outputs/` folder (only forecasts, all decisions = 0).

## Model degradation

Like many other models this model will degrade over time. Although it retrains each time with up-to-date data its feature selection will eventually stop being optimal some time down the road. It was optimal for the period it was used on during 2022 and the M6 portfolio selection.


## Reproducibility and R libraries

- For more precise reproducibility the `development_sessionInfo.txt` is attached, corresponding to the server where development and submissions for the competition were generated.

- For general use, recommended package versions are detailed in `recommended_sessionInfo.txt`, with more recent package versions and tested. According to this recommended setup the model relies on:

    - R version 4.2.2 Patched (2022-11-10 r83330)
    - data.table_1.14.4
    - xgboost_1.7.3.1
    - roll_1.1.6       
    - TTR_0.24.3
  


## DISCLAIMER

This model was developed for a competition and is being shared, at the organizers' request, for the purpose of advancing research. The model was not intended for making real-world investment decisions and should not be relied upon as an investment tool.
  
  

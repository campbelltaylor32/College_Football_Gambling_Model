# College Football Gambling Model

This project builds a machine learning model to predict whether the home team covers the spread in college football games.  

## Data Sources
- [`cfbfastR`](https://github.com/sportsdataverse/cfbfastR): play-by-play and team statistics  
- [collegefootballdata.com API](https://collegefootballdata.com/): betting lines, game outcomes, and additional team stats  

## Timeframe
- **Training Data:** 2015–2022 seasons  
- **Testing Data:** 2023–2024 seasons  

The final model achieves 57% precision in predicting home team covers.  

---

## Repository Structure

### Data Preparation & Feature Engineering
- Full_CFB_Game_Outcome_Historical.R  
  - Generates predictors from raw data  
  - Engineers moving averages & rolling statistics  
  - Cleans and formats gambling dataset  
  - Defines cover/push outcome logic  

- Merge_Predictors_CFB_Historical.R  
  - Combines predictors into a unified, ready-to-use dataset  

### Seasonal Updates
- 2025_Pred_Update.R 
  - Updates predictors weekly during the 2025 season  

- 2025_Game_Update.R  
  - Pulls and refreshes gambling spreads for the 2025 season  

### Modeling & Predictions
- CFB_Gambling_Model.ipynb  
  - Trains classification models  
  - Performs hyperparameter tuning  
  - Saves the final trained model  

- Week_Predictions.ipynb  
  - Applies the trained model to 2025 games  
  - Produces weekly cover predictions  

---

## Performance
- Task: Binary classification (home team covers vs. does not cover)  
- Final Precision: 57% on predicting home covers

---

## Updates
Weekly predictions for the 2025 season will be posted on my Twitter: camtaylor_4


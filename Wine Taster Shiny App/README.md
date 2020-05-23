# Wine Taster Shiny App

## Overview
A shiny app that predicts the quality of a wine based on the input parameters. The input parameters used in the app, include:
- Alcohol: The percent of alcohol in the wine.
- pH: Describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic).
- Citric acid: Found in small quantities, citric acid can add 'freshness' and flavor to wines.
- Residual Sugar: The amount of sugar remaining after fermentation stops, it's rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet

In this app, I use a Random Forest Regressor to predict the quality of the wine.

## How to Use the App
- Input the parameters in the input panel. You can change any or none of the parameters.
![Before](https://github.com/richardcsuwandi/r-projects/blob/master/Wine%20Taster%20Shiny%20App/images/before-tasting.png?raw=true)
- Click the 'Submit' button. The prediction will be shown in the 'Output' box.
![After](https://github.com/richardcsuwandi/r-projects/blob/master/Wine%20Taster%20Shiny%20App/images/after-tasting.png?raw=true)
View this app in action at [shinyapps.io](https://richardcsuwandi.shinyapps.io/good_wine)

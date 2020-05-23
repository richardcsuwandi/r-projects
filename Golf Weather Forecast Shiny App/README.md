# Golf Weather Forecast Shiny App

## Overview
A shiny app that predicts whether the user should play golf or not based on the input parameters. The input parameters used in the app, include:
- Outlook: The overall outlook of the weather, can either be sunny, overcast, or rainy.
- Temperature: The weather temperature, measured in degrees Fahrenheit.
- Humidity: The level of humidity in percent.
- Windy: Whether the weather is windy or not.

In this app, I use a Random Forest Regressor model to predict wthe user should play golf or not.

## How to Use the App
- Input the parameters in the input panel. You can change any or none of the parameters.
![Before](https://github.com/richardcsuwandi/r-projects/blob/master/Golf%20Weather%20Forecast%20Shiny%20App/images/before-calc.png?raw=true)
- Click the 'Submit' button. The prediction will be shown in the 'Output' box.
![After](https://github.com/richardcsuwandi/r-projects/blob/master/Golf%20Weather%20Forecast%20Shiny%20App/images/after-calc.png?raw=true)
View this app in action at [shinyapps.io](https://richardcsuwandi.shinyapps.io/play_golf)

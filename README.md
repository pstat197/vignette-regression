# vignette-regression

Vignette on implementing penalized regression models using life expectancy data; created as a class project for PSTAT197A in Fall 2022.

## Contributors
Aleksander Cichosz, Anni Li, Brian Che, Justin Vo, Noa Rapoport

## Vignette Abstract
This vignette focuses on disscussing the difference between three regularization  method: LASSO, Ridge and Elastic Net. A dataset from WTO, containing life expectancy and other 19 variables collecting around the world, is used to fit penalized regression models. 


## Repository Contents
-   `data` contains :

    -   `data preprocessing` R script file containing the preprocessing code
    
    -   `life_clean.csv` processed data
    
    -   `life-expectancy-raw` raw data from Kaggle
    
-   `scripts` contains :

    -   `vignette-script.R` an empty script file for you to copy and paste the example code 
    -   `drafts` a folder contains all the example code
    
-   `vignette-regression.Rproj`

-   `vignette.html`

-   `vignette.qmd`
## Reference List

One resource to look at are the sections on penalized regression approaches in Dr. Andrea Bellavia's book “Statistical methods for Environmental Mixtures," covering the three approaches we went over.

  https://bookdown.org/andreabellavia/mixtures/penalized-regression-approaches.html

For a programming implementation in R, the following link compares the three different model conceptually and goes over the code:

  https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r

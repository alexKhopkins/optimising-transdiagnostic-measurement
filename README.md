# Optimising the measurement of anxious-depressive, compulsivity and intrusive thought and social withdrawal transdiagnostic symptom dimensions

https://psyarxiv.com/q83sh

Alexandra Kathryn Hopkins, Claire Gillan, Jonathan Roiser, Toby Wise & Nura Sidarus 

psyarxiv, 2022

## Code

Analyses for this project are R markdown files and some use a python interface, using reticulate. All packages are outlined in the initial code setup but the python environment may require some manual installations for packages if using for the first time e.g. py_install("sklearn”). 

## Using the reduced items to predict your own factor scores

The exact items derived from the item reduction are found in the main paper and to calculate your own factor scores using these items, you can use the predictNewScores.Rmd script, which loads in the classifier and uses it to predict scores. Note that you will have to format the data in the same way the classifier expects, so please see the example data 'transDiagQs.csv' for the correct order of items and formatting. 


## Analysis scripts

**1. Exploratory factor analysis**

rtmatEFA.R

This script conducts an exploratory factor analysis on the whole dataset n = 4782 and the substudies independently. It saves the factor scores for the 3 factor model (Gillan et al. 2016, https://elifesciences.org/articles/11305) for the item reduction to use. 

**2. Item reduction**

fullReductionEFA.Rmd

This analysis trains a classifier to predict factor scores from the original item scores, similar to Wise & Dolan (2020) https://www.nature.com/articles/s41467-020-17977-w. This is done using multi-target regression (i.e. predicting scores on the 3 factors based on the individual questions). 

**3. External validation**

externalValidationRegressions.R

This script uses data from Rouault et al. (2018) https://www.sciencedirect.com/science/article/pii/S0006322318300295 and runs regression analyses examining relationships between the predicted factor scores for the 3 transdiagnostic factors and behavioural variables.

**4. Predicting new factor scores using reduced items**

predictNewScores.Rmd  

This provides a skeleton code for using the classifier in order to predict new factor scores for data using the reduced questionnaire items. 


If facing difficulties setting up the python environment in R, follow these steps:

* Go to https://jupyter.org/try-jupyter/lab/

* Upload predict_new_scores.ipynb (jupyter notebook in "predictNewScores" folder) and your input dataset (named transDiagQs.csv)

* Download output (predictedFactorScores.csv) from the data folder


If using your own data, note that the input dataset (transDiagQs.csv) for the classifier needs to follow the example provided in its structure (same columns), with zero-padding of items that are not used in the reduced item set, and scoring responses according to the respective questionnaire rules, i.e.:

 - reverse for the respective items from SDS, STAI, BIS, AES
 - subtract 1 for OCI-R & LSAS across all items (i.e. scores start at 0 rather than 1)
 - EAT has a specific scoring system for coding severity, such that rating 1 (always) should be coded as "3", rating 2 (usually) as "2", rating 3 (often) as "1", and ratings 4:6 (sometimes to never) as "0"


**Optimising the measurement of anxious-depressive, compulsivity and intrusive thought and social withdrawal transdiagnostic symptom dimensions
**
https://psyarxiv.com/q83sh``
Alexandra Kathryn Hopkins, Claire Gillan, Jonathan Roiser, Toby Wise & Nura Sidarus 
psyarxiv, 2022

**Code**
Analyses for this project are R markdown files and some use a python interface, using reticulate. All packages are outlined in the initial code setup but the python environment may require some manual installations for packages if using for the first time e.g. py_install("sklearn”). 

**Analysis scripts**
**1. Exploratory factor analysis**
rtmatEFA.R
This script conducts an exploratory factor analysis on the whole dataset n = 4782 and the substudies independently. It saves the factor scores for the 3 factor model (Gillan et al. 2016) for the item reduction to use. 

**2. Item reduction**
fullReductionEFA.Rmd
This analysis trains a classifier to predict factor scores from the original item scores. This is done using multi-target regression (i.e. predicting scores on the 3 factors based on the individual questions). 

**3. External validation**
externalValidationRegressions.R
This script uses data from Rouault et al. (2019) and runs regression analyses examining relationships between the predicted factor scores for the 3 transdiagnostic factors and behavioural variables.

**4. Predicting new factor scores using reduced items**
predictNewScores.Rmd 
This provides a skeleton code for using the classifier in order to predict new factor scores for data using the reduced questionnaire items. 

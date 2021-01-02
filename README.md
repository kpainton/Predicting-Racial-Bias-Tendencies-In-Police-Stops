# Predicting Racial Bias Tendencies in Police Stops 

The University of Washington Informatics School challenged each of it's students to focus it's projects on tackling structural inequalities. For My team and I's final project, we decided to investigate police stop tendencies in Washington State, our home state.  Our project focused on one primary goal: Can we predict the race of an individual from a traffic/pedestrian stop?

The Project utilized three major data sources:
- Washington State Traffic Stops (2009-2016) gathered from the Stanford Open Policing Project
- Washington County Voting Results collected from the Secretary of State Office for the general elections in 2016, 2012, and 2008
- Washington State County Race Demographics gathered from the US Census collected on 2015

Our statistical analysis was made up of two major components:
- Chi-Squared Independence Tests to determine relationships between categorical variables.  We used contingency tables from these tests to provide additional insight into the stop tendencies between different races. 
- ANOVA and TukeyPostHocTests which helped identify differences in mean values across different races groups among our numerical variables.  

The primary analysis for this project was to attempt to predict a driver's race based on the variables that we had collected.  We utilized an XGBoost framework, a boosting ensemble algorithm, for making our predictions.  In conclusion, the model ended up performing poorly with a roc_auc score of 0.62, indicating poor sensitivity and specificity.  

A full write up and power point presentation can be found in the pdf files provided.  

**Note: This project was completed as an assignment for a class and should not be used as a basis of forming any definitive conclusions.  The deliverable for this assignment was to submit a report with conclusions, however it's a project focused primarily on applying machine learning methods and statistical processes on a real dataset.**

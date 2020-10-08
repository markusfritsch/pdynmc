# pdynmc version 0.9.3.9000

Minor update of version 0.9.2 that adjusts minor bugs in estimation function and helper function for setting up instrument matrix

### pdynmc
* Default arguments and corresponding documentation
* Check functions
* Checks when setting up instrument matrix





# pdynmc version 0.9.2

Minor update of version 0.9.1 that adds additional functionality for setting up the instrument matrix and adjusts the instrument count displayed by the summary function.


### pdynmc
* Include additional option to limit expansion of the instrument set when deriving instrument from non-lagged dependent strictly exogenous covariates.


### print.summary.pdynmc
* Adjust displayed instrument count.





# pdynmc version 0.9.1

Minor update of version 0.9.0 that fixes a bug in the estimation function, adjusts matrix calculations to achieve minor speed
improvements, and robustifies general linear hypothesis testing functionality.


### pdynmc
* fix bug that appeared when deriving moment conditions from the explanatory variables besides the lagged
dependent variable (thanks to Massimo Giannini for pointing this out).
* adjust matrix calculations to achieve minor speed improvements
* adjust helper functions that allow limiting the lag range


### wald.fct
* Robustify wald.fct by using generalized inverse in inversion of covariance matrix.





# pdynmc version 0.9.0

Update of version 0.8.0 that includes visualizations for fitted model objects (coefficient-range plots for two-step and iterated estimation and plots of fitted values vs. residuals) and panel data structure


### functions for exploratory analysis of panel data added
* data.info: Returns information on structure of a balanced/unbalanced panel data set
* strucUPD.plot: Visualizes structure of unbalanced panel data


### generic functions added
* ninst: Returns the number of instruments of a fitted model
* optmIn: Returns input parameters used in numeric optimization of a fitted model
* wmat: Returns weighting matrix of a fitted model


### methods added
* case.names.pdynmc: Returns variable names of cross-sectional and longitudinal identifiers of a fitted model
* coef.pdynmc: Returns coefficient estimates of a fitted model
* dummy.coef.pdynmc: Returns time dummy coefficient estimates of a fitted model
* model.matrix.pdynmc: Returns instrument matrix of a fitted model
* ninst.pdynmc: Returns instrument count of a fitted model
* nobs.pdynmc: Returns number of cross-sectional and longitudinal observations of a fitted model
* optimIn.pdynmc: Returns input parameters used in numeric optimization of a fitted model
* plot.pdynmc: Plot methods for fitted model; returns plot of fitted values vs. residuals (default) or coefficient range across iterations of the estimation procedure (two-step or iterated estimation)
* print.pdynmc: Print fitted model object in console
* variable.names.pdynmc: Returns vector with names of explanatory variables of a fitted model

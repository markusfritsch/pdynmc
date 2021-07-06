# pdynmc version 0.9.5

Update of version 0.9.4 which adds further functionality and argument checks to estimation function. Additionally, the computation underlying non-robust two-step standard errors is adjusted (option accessible by changing argument "std.err" from its default to "std.err = unadjusted").

### pdynmc
* Allow lagged dependent variable to be instrumented
* Extend checks of function arguments
* Fix bug in computation of "unadjusted" two-step standard errors





# pdynmc version 0.9.4

Minor update of version 0.9.3 in which package DESCRIPTION, CITATION, and documentation is adjusted and two further package vignettes are added.

### new vignettes:
*pdynmc-intro
*pdynmc_introLong
*pdynmc-pres-in-a-nutshell





# pdynmc version 0.9.3

Minor update of version 0.9.2 that fixes minor bugs in estimation function and helper functions for setting up instrument and weighting matrix; additionally, coefficient path plots are added to the plot method and existing plot methods are adjusted.


### pdynmc
* Adjust default arguments and corresponding documentation
* Fix bug in setting up of weighting matrix
* Fix bug when imposing maximum lags from which instruments are derived
* Add check functions
* Adjust checks when setting up instrument matrix
* Residuals and fitted values are added to pdynmc object (besides the previously available internal residuals and fitted values).


### plot.pdynmc
* Plotting coefficient paths across GMM iterations (including approximate standard errors).
* Plot methods for creating fitted vs. residual plots and coefficient range plots are adjusted.





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

# pdynmc version 0.9.13.9006

Update of version 0.9.12 that adds an estimation function to compute the Anderson-Hsiao estimator for AR(1) panel data models, ... and fixes typos in documentation. Additionally, function NLIV was renamed as NLIV.T and NLIV.alt was renamed to NLIV.t.

### NLIV.T
* adjust code to allow for more general data structures
* allow for unbalanced panels

### NLIV.t
* adjust code to allow for more general data structures
* allow for unbalanced panels

### data.info
* adjust code to allow for more general data structures



# pdynmc version 0.9.12

Update of version 0.9.11 that includes some suggestions on code improvements of Kevin Tappe (github user tappek) and corrects computation of the onestep unadjusted standard errors and the serial correlation test statistic. Adjustments to the instrument counts were also made following pointers by Steve Bond. Additionally, the `sargan.fct` was added which computes the sargan test statistic for the overidentifying restrictions.

### pdynmc
* adjust computation of onestep unadjusted standard errors
* add checks of input arguments
* remove redundant parts of code
* adjust function documentation and correct typos

### mtest.fct
* correct bugs in computation of the test statistic

### wald.fct
* remove redundant parts of code

### sargan.fct
* added to package for computing sargan test statistic

### jtest.fct
* warning message for non-spherical error terms added

### plot.pdynmc
* adjust function documentation
* add further option for `fire`-plot

### Z_i.fct
* safeguard instrument count in internal helper function for the case when there are columns with zeros only



# pdynmc version 0.9.11

Update of version 0.9.10 that adds three estimation functions for the lag parameter of AR(1) panel data models. Additionally, the update allows for user-specified dummy matrix in estimation function. For this purpose, the internal helper function `corSparse` which was adopted from package 'qlcMatrix' in a previous function update was adjusted. Additionally, the argument checks of the estimation function were updated and an option to collapse the moment conditions was added.

### pdynmc
* argument checks of estimation function updated
* option for collapsing moment conditions added

### NLIV
* closed form estimation function for AR(1) panel data models
* based on original version of Ahn and Schmidt (1995) moment conditions

### NLIV.alt
* closed form estimation functions for AR(1) panel data models
* based on alternative formulation of Ahn and Schmidt (1995) moment conditions

### FDLS
* closed form estimation functions for AR(1) panel data models
* based on estimator proposed by Han and Phillips (2010)

### corSparse
* internal helper function updated for checking for collinearities in dummy part of instrument matrix



# pdynmc version 0.9.10

Update of version 0.9.9 that generalizes functionality of functions for exploratory analysis of panel data. The function `corSparse` from package 'qlcMatrix' was added as internal helper function, as the aforementioned package was scheduled to be moved from CRAN to the archive by 2023-11-29. Additionally, bug fixes are provided for the estimation function and the documentation of the package is adjusted according to the new CRAN recommendation.

### pdynmc
* adjust check-related bug when instrumenting endogenous covariates

### pDensTime.plot
* `...` argument added to function
* adjust scaling of abscissa for general time periods
* allow for user defined axis labels

### corSparse
* new internal helper function copied from package 'qlcMatrix', which is scheduled to be archived





# pdynmc version 0.9.9

Update of version 0.9.8 which adds new function for visualization of evolution of empirical density of a variable of interest over longitudinal dimension of a panel dataset. Additionally, typos in description of `cigDemand` dataset are adjusted and further information is added to summary of `pdynmc' objects.

### pdynmc
* Summary function adjusted to add clarification on type of estimation

### functions for exploratory analysis of panel data added
* pDensTime.plot: : Visualizes empirical density of column of panel dataset





# pdynmc version 0.9.8

Update of version 0.9.7 which adds functionality for excluding the lagged dependent variable from the right-hand-side of the equation. Additionally, the update adds the published version of the article as vignette, ensures correct rendering of the package documentation (thanks to Kevin Tappe), and corrects minor bugs in the estimation function (thanks to Github user Dazhwu). 


### pdynmc
* Add flexibility for excluding the lagged dependent variable from the right-hand-side of the model equation (by setting function argument `lagTerms.y = 0`).
* Adjust documentation to reflect the new feature.
* Adjust further controls part of instrument matrix when using instruments from further covariates (function arguments: `include.x`, `varname.reg.pre`, `varname.reg.ex`)
* Adjust internal helper functions (Wonestep.fct, sub.clForm.fct) correspondingly





# pdynmc version 0.9.7

Update of version 0.9.6 which updates the estimation function, the functions for visualizing the panel data structure, and adds two datasets to the package. The functionality for deriving instruments and estimating parameters: Covariates for which no parameters are estimated, but from which instruments are derived and covariates for which parameters for which parameters are estimated, but from which no instruments are derived.


### pdynmc
* Add flexibility for covariates for which no parameters are estimated, but which are used as instruments (function arguments: `include.x.instr`, `varname.reg.instr`)
* Add flexibility for covariates for which parameters are estimated, but which are not used as instruments (function arguments: `include.x.toInstr`, `varname.reg.toInstr`)
* Adjust code for further data structures
* Update function documentation
* Fix bug in function that creates instrument matrix


### data.info
* Adjust code for further data structures


### struc.UPD.plot
* Adjust code for further data structures



### datasets
* Add dataset of Arellano and Bond (1991) on employment by firms located in the UK
* Add dataset of Stock and Watson (2003) on cigarette consumption in the US





# pdynmc version 0.9.6

Minor update of version 0.9.5 which adds doubly corrected standard errors. Also, commits and suggestions of github user tappek are added. Additionally, the compatibility of the estimation function with further input data structures is improved and a bug in the estimation function when multiple instruments from non-lagged dependent endogenous covariates are derived is corrected.


### pdynmc
* Implement doubly corrected standard errors
* Adjust computation of unadjusted and corrected standard errors
* Ensure compatibility with `tibble` data frames
* Correct bug in deriving instruments from multiple endogenous covariates


### wald.fct
* Adjust structure of function according to S3 class htest.object


### jtest.fct
* Adjust structure of function according to S3 class htest.object
* Minor adjustment to avoid partial argument matching


### mtest.fct
* Adjust structure of function according to S3 class htest.object
* Set default for argument `t.order`
* Minor adjustment to avoid partial argument matching





# pdynmc version 0.9.5

Minor update of version 0.9.4 which adds further functionality and argument checks to estimation function. Additionally, the computation underlying non-robust two-step standard errors is adjusted (option accessible by changing argument "std.err" from its default to "std.err = unadjusted") and the functions for deriving instruments from further exogenous covariates were adjusted to comply with data requirements.


### pdynmc
* Allow lagged dependent variable to be instrumented
* Extend checks of function arguments
* Fix bug in computation of "unadjusted" two-step standard errors
* Ensure that instruments derived from further exogenous covariates comply with data requirements





# pdynmc version 0.9.4

Minor update of version 0.9.3 in which package DESCRIPTION, CITATION, and documentation is adjusted and two further package vignettes are added.


### new vignettes
* pdynmc-intro
* pdynmc_introLong
* pdynmc-pres-in-a-nutshell





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
* data.info: Returns information on structure of a balanced/unbalanced panel dataset
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

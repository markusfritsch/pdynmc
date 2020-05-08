## pdynmc version 0.9.0

HIGHLIGHTS: Update of version 0.8.0 that includes visualizations for fitted model objects (coefficient-range plots for two-step and iterated estimation and plots of fitted values vs. residuals) and panel data structure

## functions for exploratory analysis of panel data added
* data.info: Returns information on structure of a balanced/unbalanced panel data set
* strucUPD.plot: Visualizes structure of unbalanced panel data


## generic functions added
* ninst: Returns the number of instruments of a fitted model
* optmIn: Returns input parameters used in numeric optimization of a fitted model
* wmat: Returns weighting matrix of a fitted model


## methods added
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

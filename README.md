# pdynmc: Dynamic linear panel estimation based on linear and nonlinear moment conditions

Linear dynamic panel data modeling based on linear and nonlinear moment conditions 
as proposed by Holtz-Eakin, Newey, and Rosen (1988) https://doi.org/10.2307/1913103, 
Ahn and Schmidt (1995) https://doi.org/10.1016/0304-4076(94)01641-C, and 
Arellano and Bover (1995) https://doi.org/10.1016/0304-4076(94)01642-D.

Estimation of the model parameters relies on the Generalized Method of Moments (GMM),
numerical optimization (when nonlinear moment conditions are employed) and the
computation of closed form solutions (when estimation is based on linear moment 
conditions). One-step, two-step and iterated estimation is available.

For inference and specification testing, Windmeijer (2005) https://doi.org/10.1016/j.jeconom.2004.02.005 
corrected standard errors, serial correlation tests, tests for overidentification, 
and Wald tests are available.

Functions for visualizing panel data structures and modeling results obtained
from GMM estimation are also available. The plot methods include functions to plot
unbalanced panel structure, coefficient ranges and coefficient paths across
GMM iterations (the latter is implemented according to the plot shown in
Hansen and Lee, 2021 https://doi.org/10.3982/ECTA16274).

See also: https://cran.r-project.org/web/packages/pdynmc/index.html.
For further details on the implementation, see Fritsch, Pua, and Schnurbus (2021) https://journal.r-project.org/archive/2021/RJ-2021-035/index.html.


To install the latest development version of the package, please use:
```{r}
library(devtools)
install_github("markusfritsch/pdynmc")
```

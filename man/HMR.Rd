\name{HMR}
\Rdversion{0.3.1}
\alias{HMR}
\title{Flux estimation with static chamber data}
\description{HMR reads static chamber data from a semicolon- or comma-separated file, and then processes each data series
by recommending a specific data analysis and performing the user selected analysis. Finally, it exports the results to a
semicolon- or comma-separated file.}
\usage{HMR(filename, series = NA, dec = '.', sep = ';', JPG = FALSE, PS = FALSE,
PHMR = FALSE, npred = 500, LR.always = FALSE, FollowHMR = FALSE, ngrid = 1000)}
\arguments{
  \item{filename}{The name of a HMR data file. It is assumed that the data file folder has previously been set by the \code{setwd}
  command, e.g. \code{setwd('C:/My HMR applications')}, or through the 'File|Change dir...' menu. The required fixed format of HMR
  data files is described in the details section below.}
  \item{series}{Data series in the data file for which analysis is requested. The default \code{series=NA} requests analysis of all
  available data series, whereas for example \code{series=c('k0a','k0d')} only requests analysis of the series named \code{k0a} and
  \code{k0d}, cf. the sample data in the details section below.}
  \item{dec}{The decimal separator used in the data file. The options are \code{dec='.'} and \code{dec=','}, and the default is
  \code{dec='.'}.}
  \item{sep}{The column separator used in the data file. The options are \code{sep=';'} and \code{sep=','}, and the default is
  \code{sep=';'}.}
  \item{JPG}{A logical indicating whether plots of the model fits should be exported as JPG-files. A plot is exported for each
  analysed data series, and the files are named after the data series and located in the data file folder. The default is
  \code{JPG=FALSE}.}
  \item{PS}{A logical indicating whether plots of the model fits should be exported as postscript files. A plot is exported for each
  analysed data series, and the files are named after the data series and located in the data file folder. The default is
  \code{PS=FALSE}.}
  \item{PHMR}{A logical indicating whether model predicted values should be exported to a file for data series for which non-linear
  modeling is selected. The export file is column separated like the data file and it is located in the data file folder and named after
  the data file preceded by 'PHMR - '. The default is \code{PHMR=FALSE}.}
  \item{npred}{If \code{PHMR=TRUE} (see above), \code{npred} model predicted values are exported for each data series. The default is
  \code{npred=500}.}
  \item{LR.always}{A logical indicating whether data series should be analysed by linear regression in addition to the user selected
  analysis. The default is \code{LR.always=FALSE}.}
  \item{FollowHMR}{A logical indicating whether to select the data analysis recommended by HMR in all cases. This cancels all plotting.
  The default is \code{FollowHMR=FALSE}.}
  \item{ngrid}{The number of grid-points in the initial grid search of the minimum of the criterion function. It must be at least 100,
  and the default is \code{ngrid=1000}.} 
}
\details{HMR data files must be semicolon- or comma-separated files organised in five columns containing data series names, chamber
volumes, chamber cross-sectional areas, observation time-points, and the observed chamber concentrations. Semicolon- and comma-separated
files may be edited by text editors or spreadsheet software. The following 45 lines constitute an example of a HMR data file containing
five data series of static chamber data:

\preformatted{
Series;V;A;Time;Concentration
 k0a; 140.6250; 0.5625;   0;  13.98
 k0a; 140.6250; 0.5625;  10;  14.65
 k0a; 140.6250; 0.5625;  20;  15.15
 k0a; 140.6250; 0.5625;  30;  15.85
 k0a; 140.6250; 0.5625;  40;  16.29
 k0a; 140.6250; 0.5625;  50;  17.17
 k0a; 140.6250; 0.5625;  60;  17.45
 k0a; 140.6250; 0.5625;  70;  17.85
 k0a; 140.6250; 0.5625;  80;  17.79
 k0a; 140.6250; 0.5625;  90;  17.89
 k0a; 140.6250; 0.5625; 100;  17.63
 k0a; 140.6250; 0.5625; 110;  17.82
 k0d; 140.6250; 0.5625;   0;  15.60
 k0d; 140.6250; 0.5625;  10;  15.62
 k0d; 140.6250; 0.5625;  20;  16.53
 k0d; 140.6250; 0.5625;  30;  16.90
 k0d; 140.6250; 0.5625;  40;  17.40
 k0d; 140.6250; 0.5625;  50;  17.69
 k0d; 140.6250; 0.5625;  60;  18.64
 k0d; 140.6250; 0.5625;  70;  18.36
 k0d; 140.6250; 0.5625;  80;  19.14
 k0d; 140.6250; 0.5625; 110;  18.83
 k0d; 140.6250; 0.5625; 120;  19.27
F1T3;   2.0101; 0.0201;   0;  15.00
F1T3;   2.0101; 0.0201;  26;  39.34
F1T3;   2.0101; 0.0201;  49;  91.74
F1T3;   2.0101; 0.0201;  87; 121.66
F1T3;   2.0101; 0.0201; 117; 130.33
F1T3;   2.0101; 0.0201; 147; 132.04
F1T3;   2.0101; 0.0201; 182; 147.97
F1T3;   2.0101; 0.0201; 213; 149.78
F1T3;   2.0101; 0.0201; 262; 152.37
F2T2;   2.0101; 0.0201;   0;  10.87
F2T2;   2.0101; 0.0201;  20;  19.49
F2T2;   2.0101; 0.0201;  54;  24.99
F2T2;   2.0101; 0.0201;  85;  27.24
F2T2;   2.0101; 0.0201; 119;  33.13
F2T2;   2.0101; 0.0201; 155;  30.14
F2V2;   2.0101; 0.0201;   0;   9.94
F2V2;   2.0101; 0.0201;  28;  31.64
F2V2;   2.0101; 0.0201;  60;  48.88
F2V2;   2.0101; 0.0201;  91;  58.08
F2V2;   2.0101; 0.0201; 123;  76.16
F2V2;   2.0101; 0.0201; 162; 106.83
}

Apart from the (required) headline the five columns contain:

\itemize{
  \item{Column 1:}{Text labels that identify the data series. The labels must be 100\% identical within data series and different between
  data series. In the sample data above, the first column identifies five data series named \code{k0a}, \code{k0d}, \code{F1T3}, \code{F2T2},
  and \code{F2V2}.}
  \item{Column 2:}{The chamber volume, \eqn{V}. In the sample data above, \eqn{V=140.625} [\eqn{L}] for data series \code{k0a} and \code{k0d},
  and \eqn{V=2.0101} [\eqn{L}] for data series \code{F1T3}, \code{F2T2}, and \code{F2V2}.}
  \item{Column 3}{The chamber cross-sectional area, \eqn{A}. In the sample data above, \eqn{A=0.5625} [\eqn{m^2}] for data series \code{k0a}
  and \code{k0d}, and \eqn{A=0.0201} [\eqn{m^2}] for data series \code{F1T3}, \code{F2T2}, and \code{F2V2}.}
  \item{Column 4:}{The non-negative measurement time-points in increasing order. At least three observation time-points per data series is
  required. In the sample data above, the time-points are provided in minutes and cover, approximately, two-four hour periods per data series.}
  \item{Column 5:}{The chamber concentrations associated with the time-points in the fourth column. In the sample data above, this column
  contains concentrations of nitrous oxide [\eqn{\mu g/L}].}
}

Missing values (\code{NA}'s) are not allowed in HMR data files.

There are no requirements regarding the units of input data. The chosen units do, however, determine the unit of the estimated flux, which
has the same unit as \eqn{(VC)/ (At)}, where \eqn{t} and \eqn{C} denote, respectively, time and concentration. Some examples:

\itemize{
\item{\eqn{V} in [\eqn{L}], \eqn{A} in [\eqn{m^2}], \eqn{t} in [\eqn{h}], \eqn{C} in [\eqn{\mu g/L}]:
The flux unit is [\eqn{\mu g/m^2/h}].}
\item{\eqn{V} in [\eqn{L}], \eqn{A} in [\eqn{m^2}], \eqn{t} in [\eqn{min.}], \eqn{C} in [\eqn{ppmv=\mu L/L}]:
The flux unit is [\eqn{\mu L/m^2/min.}].}
\item{\eqn{V} in [\eqn{m^3}], \eqn{A} in [\eqn{km^2}], \eqn{t} in [\eqn{s}], \eqn{C} in [\eqn{kg/m^3}]:
The flux unit is [\eqn{kg/km^2/s}].}
}

The HMR function analyses the data series sequentially, and starts each analysis by fitting the non-linear function
(Hutchinson and Mosier, 1981)
\deqn{C_t=\varphi+f_0 e^{-\kappa t}}
using a single-parameter (\eqn{\kappa}) criterion (concentrated least squares; Seber and Wild, 1989). In this equation, \eqn{f_0}
denotes the initial flux, \eqn{\varphi} denotes the new chamber equilibrium concentration, and \eqn{\kappa>0} is an adaption rate
that depends on soil, gas, and chamber characteristics. The HMR function then recommends a particular analysis, and the user is
requested to confirm it or to select another data analysis. The possible choices are:

\itemize{
\item{HMR: Non-linear regression.}
\item{LR: Linear regression.}
\item{No flux: Abort analysis and claim zero flux.}
}

The HMR function recommends the first choice if possible, but for linear concentration data or noisy data representing no clear trend the
non-linear function will be overparameterised, and the HMR function will recommend linear regression or no flux. For linear concentration
data the criterion function will not possess a unique optimum but rather continue to improve as \eqn{\kappa} tends to zero, and in such
cases the HMR function will recommend data analysed by linear regression. For noisy concentration data with no clear trend the criterion
function will also not possess a unique optimum but rather continue to improve as \eqn{\kappa} tends to infinity, and in such cases the
HMR function will recommend data analysed by linear regression, or, if assuming a linear trend seems inappropriate, to abort analysis and
claim no flux.

To assist the user in selecting a data analysis the HMR function displays plots -- organised in a 2x2 matrix -- of the criterion function
and the various models fits. The upper left plot displays the criterion function over the range of numerically feasible values of
\eqn{\kappa}. Green parts of the curve represent values of \eqn{\kappa} for which the corresponding estimated values of \eqn{\varphi} and
\eqn{C_0} are valid (positive), whereas red parts of the curve represent values of \eqn{\kappa} for which this is not the case. The optimal
value of \eqn{\kappa} (minimises the criterion function over the valid values of \eqn{\kappa}) is indicated by a blue square. The upper
right plot is a zoom into the upper left plot. The lower left plot displays the model fit of the various possible models, and the HMR
recommendation for data analysis is displayed in the headline. The lower right panel contains four 'buttons' representing the four possible
user selections (select by left mouse button click). Pressing the cancel button terminates the HMR function without completing the requested
analyses.
}
\value{
A data frame with one row per analysed data series and variables:
  \item{Series}{Name of the data series.}
  \item{f0}{The estimated flux.}
  \item{f0.se}{The standard error of the estimated flux.}
  \item{f0.p}{The p-value for the null hypothesis of zero flux.}
  \item{f0.lo95}{The lower end-point of the 95\%-confidence interval for the flux.}
  \item{f0.up95}{The upper end-point of the 95\%-confidence interval for the flux.}
  \item{Method}{The method used for estimating the flux.}
  \item{Warning}{A character string that contains a warning message in case of estimation problems, e.g. if linear regression estimated a negative predeployment concentration.}
  \item{LR.f0}{The flux estimated by linear regression. (Only present if \code{LR.always=TRUE}.)}
  \item{LR.f0.se}{The standard error of the flux estimated by linear regression. (Only present if \code{LR.always=TRUE}.)}
  \item{LR.f0.p}{The p-value for the null hypothesis of zero flux calculated by linear regression. (Only present if \code{LR.always=TRUE}.)}
  \item{LR.f0.lo95}{The lower end-point of the 95\%-confidence interval for the flux calculated by linear regression. (Only present if \code{LR.always=TRUE}.)}
  \item{LR.f0.up95}{The upper end-point of the 95\%-confidence interval for the flux calculated by linear regression. (Only present if \code{LR.always=TRUE}.)}
  \item{LR.Warning}{A character string that contains a warning message if linear regression estimated a negative predeployment concentration. (Only present if
  \code{LR.always=TRUE}.)}
The data frame is also exported to a file named after the data file preceded by 'HMR - ' and located in the data file folder. The exported file
uses the same column separator as the data file.
}
\author{Asger R. Pedersen, Ph.D. in statistics, Senior scientist at Aarhus University, Denmark}
\references{
Hutchinson, G.L. and Mosier, A.R. (1981). Improved soil cover method for field measurement of nitrous oxide fluxes.
\emph{Soil Science Society of America Journal}, \bold{45}, p. 311-316.

Seber, G.A.F. and Wild, C.J. (1989). \emph{Nonlinear regression}. Wiley, New York.
}
\examples{
\dontrun{
# Suppose the sample data above are located on a Windows machine
# in the file 'C:\My HMR applications\N2O.csv'.

# Start by setting the data file folder:
setwd('C:/My HMR applications')

# Notice that R uses '/' in folder declarations, whereas
# Windows uses '\'.

# Analyse all data series:
HMR(filename='N2O.csv')

# Produces the following output:
  Series         f0      f0.se       f0.p    f0.lo95    f0.up95 Method Warning
1    k0a 2.5968E+01 1.4721E+00 7.2967E-09 2.2688E+01 2.9248E+01    HMR    None
2    k0d 1.8720E+01 1.3961E+00 2.9756E-07 1.5561E+01 2.1878E+01    HMR    None
3   F1T3 2.1265E+02 1.2176E+01 4.9647E-07 1.8386E+02 2.4144E+02    HMR    None
4   F2T2 4.5093E+01 4.4410E+00 5.2977E-04 3.2762E+01 5.7423E+01    HMR    None
5   F2V2 5.5853E+01 3.5905E+00 9.9703E-05 4.5884E+01 6.5822E+01     LR    None

# The output is also exported to the semicolon-separated file
# 'C:\My HMR applications\HMR - N2O.csv'.

# Analyse data series 'k0a' and 'k0d' and export model predicted
# chamber concentrations:
HMR(filename=datafile,series=c('k0a','k0d'),PHMR=TRUE)

# The model predicted concentrations are exported to the file
# 'C:\My HMR applications\PHMR - N2O.csv'.

# Analyse all data series with both linear regression and the
# user selected analysis:
HMR(filename=datafile,LR.always=TRUE)

# Produces the following (slightly edited) output:
  Series         f0      f0.se       f0.p    f0.lo95    f0.up95 Method Warning...
1    k0a 2.5968E+01 1.4721E+00 7.2967E-09 2.2688E+01 2.9248E+01    HMR    None...
2    k0d 1.8720E+01 1.3961E+00 2.9756E-07 1.5561E+01 2.1878E+01    HMR    None...
3   F1T3 2.1265E+02 1.2176E+01 4.9647E-07 1.8386E+02 2.4144E+02    HMR    None...
4   F2T2 4.5093E+01 4.4410E+00 5.2977E-04 3.2762E+01 5.7423E+01    HMR    None...
5   F2V2 5.5853E+01 3.5905E+00 9.9703E-05 4.5884E+01 6.5822E+01     LR    None...

...      LR.f0   LR.f0.se    LR.f0.p LR.f0.lo95 LR.f0.up95 LR.Warning
... 8.9948E+00 1.1729E+00 1.7021E-05 6.3813E+00 1.1608E+01       None
... 8.0045E+00 1.0199E+00 2.5776E-05 5.6974E+00 1.0312E+01       None
... 5.0291E+01 9.9206E+00 1.4481E-03 2.6833E+01 7.3750E+01       None
... 1.2259E+01 2.9792E+00 1.4675E-02 3.9872E+00 2.0531E+01       None
... 5.5853E+01 3.5905E+00 9.9703E-05 4.5884E+01 6.5822E+01       None

# The output is also exported to the semicolon-separated file
# 'C:\My HMR applications\HMR - N2O.csv'. Hence, several analyses
# of the same data file overwrites the output file, so to save a
# particular output file it has to be renamed before the next
# application of HMR to the same data file.
}
}
\keyword{htest}
\keyword{regression}
\keyword{nonlinear}

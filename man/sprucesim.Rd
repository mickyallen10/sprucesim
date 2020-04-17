\name{sprucesim}
\alias{sprucesim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Stand Level Forest Growth and Yield Simulator}
\description{Simulates stand level forest growth and yield for Norway spruce forests, includining thinning treatments.
}
\usage{
sprucesim(stand.df, functions, n.periods = 5, period.length, print.comments = F, domsi = F, thin = F, num.thins = NULL, thin.bph = T, thinbyperc = F, min.bph.thin1 = NULL, min.bph.thin2 = NULL, min.hd.thin1 = NULL, min.hd.thin2 = NULL, bph.after.thin1 = NULL, bph.after.thin2 = NULL, bph.tq1 = NULL, bph.tq2 = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stand.df}{A data frame of stand level values.
}
  \item{functions}{List of functions for simulating forest growth and yeild must be specified. Functions needed are: fn.domht (Dominant height growth function), fn.survival (survival function), fn.basalarea (basal area growth function), fn.qmd (quadratic mean diameter calculation), fn.vol (volume estimation function), fn.treerem (function for calculating the number of trees per hectare after thinning).
}
  \item{n.periods}{
Integer value of the number of periods simulated.
}
  \item{period.length}{
Length of each growth period. Defaults to 5 (the recomended period length).
}
  \item{print.comments}{
%%     ~~Describe \code{print.comments} here~~
}
  \item{domsi}{
Logical value. If dominant stand height is to be predicted from Site Index. Defaults to FALSE.
}
  \item{thin}{
Logical value. Specify if thinning should be implemented.
}
  \item{num.thins}{
Integer value. Specify the number of thins that will be implemented. Currently only accepts values of 1 or 2.
}
  \item{thin.bph}{
Logical value. Specify if the timing of thinning should determined by a set basal area.
}
  \item{thinbyperc}{
Logical value. Specify how thinning should be performed. If False, then a minimum residual basal area after thinning (min.bph.thin1 or min.bph.thin2) must be specified. If True, then the percent of basal area remaining after thinning must be specified (bph.tq1 or bph.tq2).
}
  \item{min.bph.thin1}{
If num.thins == (1 | 2) & thinbyperc == F, then specify the residual basal area per hectare (m2) following thinning.
}
  \item{min.bph.thin2}{
If num.thins == 2 & thinbyperc == F, then specify the residual basal area per hectare (m2) following the second thinning.
}
  \item{min.hd.thin1}{
If thin.bph == F and num.thins == (1 | 2), then specify the dominant height (m) at which the stand should be thinned.
}
  \item{min.hd.thin2}{
If thin.bph == F and num.thins == 2, then specify the dominant height (m) at the second thinning.
}
  \item{bph.after.thin1}{
if thinbyperc == F and num.thins == (1 | 2), then specify the residual basal area after the first thinning.
}
  \item{bph.after.thin2}{
If thinbyperc == F and num.thins == 2, then specify the residual basal area after the second thinning.
}
  \item{bph.tq1}{
If thinbyperc == T and num.thins in (1 | 2), then specify the percentage residual basal area after first thin (value between 0 and 1).
}
  \item{bph.tq2}{
If thinbyperc == T and num.thins == 2, then specify the percentage residual basal area after second thin (value between 0 and 1).
}
  \item{\dots}{
other values that may be passed.
}
}
\details{
The data.frame stand.df should include columns for: plot.id (an ID for the stand/plots to be simulated), age (stand age from planting in years), tph (the number of trees per hectare), bph (basal are per hectare in m2), qmd (stand quadratic mean diameter in cm), hd (stand dominant height in m), si (site index in m), vol (stand volume per hectare in m3), and thinid = (0 for unthinned and 1 for thinned).
}
\note{In addition, if initial stands/plots are thinned, columns for thinning values should be provided. These variables are: tpha, bpha, qmda, vola (trees per hectare, basal area per hectare, quadratic mean diameter, and volume per hectare "after" thinning); tphb, bphb, qmdb, volb (trees per hectare, basal area per hectare, quatratic mean diameter, and volume per hectare "before thinning"; bphr and volr (basal area per hectare and volume per hectare "removed" in thinning); tage (stand age at thinning), hd_thin (dominant stand height at thinning))}

\value{ Returns a LIST named "fl" which has the following components for each simulation period:

    \item{plot.id}{<unique plot.id supplied in stand.df}
    \item{age}{stand age (years)}
    \item{tph}{number of trees per hectare}
    \item{tpha}{number of trees per hectare after thinning}
    \item{tphb}{number of trees per hectare before thinning}
    \item{bph}{basal area per hectare (m2)}
    \item{bpha}{basal area per hectare (m2) after thinning}
    \item{bphb}{basal area per hectare (m2) before thinning}
    \item{bphr}{basal area per hectare (m2) removed in thinning}
    \item{qmd}{quadratic mean diameter (cm)}
    \item{qmda}{quadratic mean diameter (cm) after thinning}
    \item{qmdb}{quadratic mean diameter (cm) before thinnig}
    \item{hd}{dominant stand height (m)}
    \item{tage}{stand age at time of thinning}
    \item{hd_thin}{Dominant stand height (m) at time of thinning}
    \item{si}{site index (m) at base age 40 years}
    \item{thinid}{ID for if a stand is thinned (1) or not thinned (0)}
    \item{vol}{stand volume per hectare (m3)}
    \item{vola}{stand volume per hectare (m3) before thinning}
    \item{volb}{stand volume per hectare (m3) after thinning}
    \item{volr}{stand volume per hectare (m3) removed in thinning}

}

\author{
Micky G Allen II <micky10@vt.edu>
}
\examples{

#Simulate G&Y without thinning
sprucesim(stand.df = SpruceStands,
         period.length=5,
n.periods = 20,
functions = list(
fn.basalarea = basal_area,
fn.domht = dom_height,
fn.survival = survival,
fn.qmd = qmd,
fn.treerem = tree_reduction,
fn.vol = volume),

thin=FALSE,
num.thins = NULL,

thin.bph = TRUE,
thinbyperc=FALSE,

min.bph.thin1 = NULL,
min.bph.thin2 = NULL,

min.hd.thin1 = NULL,
min.hd.thin2 = NULL,

bph.after.thin1 = NULL,
bph.after.thin2 = NULL,

bph.tq1 = NULL,
bph.tq2 = NULL)


#Thin 1 time removing 25% basal area
#Thin when stand basal area reaches 24 m2
sprucesim(stand.df = SpruceStands,
                 period.length=5,
                 n.periods = 20,
                 functions = list(
                   fn.basalarea = basal_area,
                   fn.domht = dom_height,
                   fn.survival = survival,
                   fn.qmd = qmd,
                   fn.treerem = tree_reduction,
                   fn.vol = volume),

                 thin=TRUE,
                 num.thins = 1,

                 thin.bph = TRUE,
                 thinbyperc=TRUE,

                 min.bph.thin1 = 24,
                 min.bph.thin2 = NULL,

                 min.hd.thin1 = NULL,
                 min.hd.thin2 = NULL,

                 bph.after.thin1 = NULL,
                 bph.after.thin2 = NULL,

                 bph.tq1 = 0.8,
                 bph.tq2 = NULL)

#Thin 1 time removing 25% basal area
#Thin when stand dominant height is 12m
sprucesim(stand.df = SpruceStands,
         period.length=5,
         n.periods = 20,
         functions = list(
           fn.basalarea = basal_area,
           fn.domht = dom_height,
           fn.survival = survival,
           fn.qmd = qmd,
           fn.treerem = tree_reduction,
           fn.vol = volume),

         thin=TRUE,
         num.thins = 1,

         thin.bph = FALSE,
         thinbyperc=TRUE,

         min.bph.thin1 = NULL,
         min.bph.thin2 = NULL,

         min.hd.thin1 = 12,
         min.hd.thin2 = NULL,

         bph.after.thin1 = NULL,
         bph.after.thin2 = NULL,

         bph.tq1 = 0.8,
         bph.tq2 = NULL)
         }

* Encoding: UTF-8.
* Fix Variable formatting & levels.
ALTER TYPE diener_mean (F4.2).
FORMATS audit_total phq9_total (F2.0).
FORMATS Alc30D (F2.0).
VARIABLE LEVEL diener_mean audit_total phq9_total GPA Alc30D (SCALE).
Execute.

* Add Variable Lables.
VARIABLE LABELS
diener_mean 'Flourishing Mean'
audit_total 'AUDIT Total Score'
phq9_total 'PHQ9 Total Score'
GPA 'Grade Point Average'
Alc30D 'Past Month Alcohol Use in Days'.
Execute.

* Fix Nominal Gender_Male level, value label and variable label. 
VARIABLE LEVEL Gender_Male (NOMINAL).
VALUE LABELS Gender_Male 0 'Female' 1 'Male'.
VARIABLE LABELS Gender_Male 'Dichotomous Gender (M/F)'.
Execute.

* Compute dichotomous abstainer variable. 
COMPUTE abstainer = 0. 
Execute.

IF (Alc30D = 0) abstainer = 1.
IF (Alc30D > 0) abstainer = -1.
Execute.

VARIABLE LEVEL abstainer (NOMINAL).
VALUE LABELS abstainer 1 'abstainer' -1 'drinker'.
VARIABLE LABELS abstainer 'Dichotomous abstinance from Alcohol'.


* Generate Descriptive Stats.
DESCRIPTIVES VARIABLES = diener_mean audit_total phq9_total Alc30D
  /STATISTICS=MEAN STDDEV MIN MAX VARIANCE.
Execute.



* Generate Histograms.
GRAPH
  /HISTOGRAM(NORMAL)= phq9_total
  /TITLE = 'Histogram of PHQ-9 Total Scores'.
GRAPH
  /HISTOGRAM(NORMAL)=audit_total
  /TITLE = 'Histogram of AUDIT Total Scores'.
GRAPH
  /HISTOGRAM(NORMAL)=diener_mean
  /TITLE = 'Histogram of Flourishing Mean'.
GRAPH
  /HISTOGRAM(NORMAL)=GPA
  /TITLE = 'Histogram of GPA'.
GRAPH
  /HISTOGRAM(NORMAL)=Alc30D
  /TITLE = 'Histogram of Past Months Alcohol Use'.
EXECUTE.


* Nominal Predictor & Nominal Moderator.

 DATA LIST FREE/ 
   Gender_M   abstaine   phq9_tot   . 
BEGIN DATA. 
      .0000    -1.0000     9.7879 
     1.0000    -1.0000     7.0455 
      .0000     1.0000     7.0000 
     1.0000     1.0000     5.8333 
END DATA. 
GRAPH/SCATTERPLOT= 
 Gender_M WITH     phq9_tot BY       abstaine .



* 2. continuous (mean-centered) predictor and nominal moderator.

*AGGREGATE 
*  /OUTFILE=* MODE=ADDVARIABLES 
*  /BREAK= 
*  /AVG_diener_mean 'Average of diener_mean'=MEAN(diener_mean).
*Execute.

*COMPUTE diener_mean_mc = diener_mean - AVG_diener_mean.
*Execute.

DATA LIST FREE/ 
   diener_m   Gender_M   phq9_tot   . 
BEGIN DATA. 
     -.8241      .0000    12.4516 
      .1759      .0000     9.0033 
     1.1609      .0000     5.6068 
     -.8241     1.0000     6.9839 
      .1759     1.0000     6.3328 
     1.1609     1.0000     5.6914 
END DATA. 
GRAPH/SCATTERPLOT= 
 diener_m WITH     phq9_tot BY       Gender_M .


*  3 - moderation with continuous predictor and continuous moderator (both mean-centered).

*AGGREGATE 
 * /OUTFILE=* MODE=ADDVARIABLES 
  */BREAK= 
 * /AVG_Alc30D 'Average of Alc30D'=MEAN(Alc30D).
* Execute.

*COMPUTE Alc30D_mc = Alc30D - AVG_Alc30D.
*Execute.


DATA LIST FREE/ 
   diener_m   Alc30D_m   phq9_tot   . 
BEGIN DATA. 
     -.8241    -3.1358     8.0262 
      .1759    -3.1358     7.8003 
     1.1609    -3.1358     7.5777 
     -.8241    -1.1358     9.1976 
      .1759    -1.1358     7.7481 
     1.1609    -1.1358     6.3203 
     -.8241     3.7442    12.0558 
      .1759     3.7442     7.6207 
     1.1609     3.7442     3.2521 
END DATA. 
GRAPH/SCATTERPLOT= 
 diener_m WITH     phq9_tot BY       Alc30D_m .




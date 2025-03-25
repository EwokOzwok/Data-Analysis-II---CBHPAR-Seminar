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



AGGREGATE 
  /OUTFILE=* MODE=ADDVARIABLES 
  /BREAK= 
  /AVG_diener_mean 'Average of diener_mean'=MEAN(diener_mean).
Execute.

COMPUTE diener_mean_mc = diener_mean - AVG_diener_mean.
Execute.


AGGREGATE 
  /OUTFILE=* MODE=ADDVARIABLES 
  /BREAK= 
  /AVG_Alc30D 'Average of Alc30D'=MEAN(Alc30D).
Execute.

COMPUTE Alc30D_mc = Alc30D - AVG_Alc30D.
Execute.

AGGREGATE 
  /OUTFILE=* MODE=ADDVARIABLES 
  /BREAK= 
  /AVG_audit_total 'Average of audit_total'=MEAN(audit_total).
Execute.

COMPUTE AUDIT_mc = audit_total - AVG_audit_total.
Execute.

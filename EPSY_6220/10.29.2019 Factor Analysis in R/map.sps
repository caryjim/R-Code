
set printback=none width=80  seed = 1953125   mxloops=9000.


*  Velicer's Minimum Average Partial (MAP) Test.

*  To run this program you need to first specify the data
   for analysis and then RUN, all at once, the commands
   from the MATRIX statement to the END MATRIX statement.

*  There are 4 possible methods of specifying the data
   that will be processed by this program:

*  Method 1: You can manually enter (type in) a correlation matrix

*  Method 2: Have the program read the active data file

*  Method 3: Have the program read an SPSS data file on your computer

*  Method 4: Have the program read a correlation
             matrix that was created by an SPSS procedure,
             as in the following examples:

*            correlation  var1 to var9 / matrix out ('C:\corrmat').

*            factor var = var1 to var9 / matrix out (cor = 'C:\corrmat').

*            You must then use the same MATRIX OUT filename 
             (e.g., 'C:\corrmat') in the MGET command within the
             program itself.  The above commands are now merely
             comments and will not run unless the "*"s & spaces in
             the first columns are removed.


matrix.

*  Specify the data for the analyses:

*  Method 1: You can manually enter a correlation matrix
   (i.e., type the numbers in yourself) directly into the program,
   as in the example below for Harman's data.  Simply use the
   command COMPUTE CR = to enter and name the data, as in the example.
* Harman's data (1967, p 80).
compute cr = {
 1.000,  .846,  .805,  .859,  .473,  .398,  .301,  .382;
  .846, 1.000,  .881,  .826,  .376,  .326,  .277,  .415;
  .805,  .881, 1.000,  .801,  .380,  .319,  .237,  .345;
  .859,  .826,  .801, 1.000,  .436,  .329,  .327,  .365;
  .473,  .376,  .380,  .436, 1.000,  .762,  .730,  .629;
  .398,  .326,  .319,  .329,  .762, 1.000,  .583,  .577;
  .301,  .277,  .237,  .327,  .730,  .583, 1.000,  .539;
  .382,  .415,  .345,  .365,  .629,  .577,  .539, 1.000  }.


*  Method 2: Have the program read the active SPSS raw data file,
   where the rows are cases & the columns are variables;
   First, remove the * from the first column (only the
   first column) to activate the following GET command;
   Then enter the names of your variables in the place
   of "var1 to var9" on the GET command.
*GET cr / FILE = * / missing=omit / VAR = Question_01 to Question_12.


*  Method 3: Have  the program read an SPSS raw data file on your computer,
   where the rows are cases & the columns are variables;
   First, remove the * from the first column (only the
   first column) to activate the following GET command;
   Then enter the name (&, if necessary, the path to) an existing 
   SPSS data file on your computer after FILE = on the
   GET command below;
   Then enter the names of your variables in the place
   of "var1 to var9" on the GET command.
*GET cr / FILE = 'C:\data.sav' / missing=omit / VAR = var1 to var9.


*  Method 4: You can have the program read a correlation
   matrix that was created by an SPSS procedure, as described above;
   First, remove the * from the first column (only the
   first column) to activate the following MGET command;
   Then enter, after FILE = on the MGET command below,
   the name (&, if necessary, the path to) an existing SPSS 
   correlation matrix file on your computer
   (suggestion: if you make the correlation matrix the active data file
   in SPSS, then you can simply enter * instead of the path & file name).
*MGET /type= corr / FILE = 'C:\corrmat' .


****************** End of user specifications. ******************

* computes the correlation matrix, if necessary.
do if ( nrow(cr) ne ncol(cr) or ( nrow(cr) eq ncol(cr) and trace(cr) / ncol(cr) ne 1) ).
compute x = cr.
compute ncases = nrow(x).
compute nvars  = ncol(x).
compute ones = make(ncases,1,1).
compute xi1 = ( 1 / ncases) * t(ones).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(x) -  ((t(csum(x))*csum(x)) /ncases) ).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute cr = (d * vcv * d).
end if.

* MAP test computations.
call eigen (cr,eigvect,eigval).
compute loadings = eigvect * sqrt(mdiag(eigval)).
compute nvars = ncol(cr).
compute fm = make(nrow(cr),2,-9999).
compute fm(1,2) = (mssq(cr)-nvars)/(nvars*(nvars-1)).
compute fm4 = fm.
compute fm4(1,2) = (msum(cr &**4)-nvars)/(nvars*(nvars-1)).
loop #m = 1 to nvars - 1.
compute biga = loadings(:,1:#m).
compute partcov = cr - (biga * t(biga)).
compute d = mdiag( 1 / (sqrt(diag(partcov))) ).
compute pr = d * partcov * d.
compute fm(#m+1,2) = (mssq(pr)-nvars)/(nvars*(nvars-1)).
compute fm4(#m+1,2) = (msum(pr &**4)-nvars)/(nvars*(nvars-1)).
end loop.

* identifying the smallest fm value & its location (= # factors).
compute minfm = fm(1,2).
compute nfacts = 0.
compute minfm4 = fm4(1,2).
compute nfacts4 = 0.
loop #s = 1 to nrow(fm).
compute fm(#s,1) = #s -1.
compute fm4(#s,1) = #s - 1.
do if ( fm(#s,2) < minfm ).
compute minfm = fm(#s,2).
compute nfacts = #s - 1.
end if.
do if ( fm4(#s,2) < minfm4 ).
compute minfm4 = fm4(#s,2).
compute nfacts4 = #s - 1.
end if.
end loop.

print /title="Velicer's Minimum Average Partial (MAP) Test:".
print eigval  /title="Eigenvalues" /format "f12.4".
print { fm, fm4(:,2) } /title="Average Partial Correlations"
 /clabels= " " "squared"   "power4" /format "f14.4".
print minfm /title="The smallest average squared partial correlation is"/format "f12.4".
print minfm4/title="The smallest average 4rth power partial correlation is"/format "f12.4".
print nfacts  /title="The Number of Components According to the Original (1976) MAP Test is".
print nfacts4 /title="The Number of Components According to the Revised (2000) MAP Test is".

end matrix.



* References.
 
* the original MAP test:
  Velicer, W. F. (1976). Determining the number of components 
  from the matrix of partial correlations. Psychometrika, 41, 321-327.
 
* the revised (2000) MAP test i.e., with the partial correlations
  raised to the 4rth power (rather than squared):
  Velicer, W. F., Eaton, C. A., and Fava, J. L. (2000). Construct
  explication through factor or component analysis: A review and 
  evaluation of alternative procedures for determining the number 
  of factors or components. Pp. 41-71 in R. D. Goffin and 
  E. Helmes, eds., Problems and solutions in human assessment
  Boston: Kluwer.
 
* the present programs:
  O'Connor, B. P. (2000). SPSS and SAS programs for determining 
  the number of components using parallel analysis and Velicer's 
  MAP test. Behavior Research Methods, Instrumentation, and
  Computers, 32, 396-402.




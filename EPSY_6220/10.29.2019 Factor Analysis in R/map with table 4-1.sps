* Encoding: UTF-8.
set printback=none width=80  seed = 1953125   mxloops=9000.
*  Velicer's Minimum Average Partial (MAP) Test.
matrix.
compute cr = {
1,00.64,0.66,0.62,0.64,0.63,0.17,0.13,0.15,0.16,0.13;
0.64,1,00.65,0.62,0.66,0.67,0.12,0.17,0.13,0.15,0.17;
0.66,0.65,1,00.65,0.62,0.63,0.15,0.15,0.12,0.15,0.16;
0.62,0.62,0.65,1,00.64,0.66,0.13,0.17,0.14,0.16,0.15;
0.64,0.66,0.62,0.64,1,00.65,0.12,0.16,0.15,0.17,0.15;
0.63,0.67,0.63,0.66,0.65,1,00.15,0.18,0.12,0.15,0.11;
0.17,0.12,0.15,0.13,0.12,0.15,1,00.55,0.57,0.51,0.60;
0.13,0.17,0.15,0.17,0.16,0.18,0.55,1,00.52,0.57,0.53;
0.15,0.13,0.12,0.14,0.15,0.12,0.57,0.52,1,00.59,0.55;
0.16,0.15,0.15,0.16,0.17,0.15,0.51,0.57,0.59,1,00.58;
0.13,0.17,0.16,0.15,0.15,0.11,0.6,0.53,0.55,0.58,1.00 }.

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
call eigen(cr,eigvect,eigval).
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

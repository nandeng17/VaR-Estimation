# Value-at-Risk Estimation in R
This code will do the following tasks：


0) Compute the mean vector and covariance matrix through (i)standard method (ii)EWMA method. 


1) Use the empirical distribution of historical log returns and the full loss operator to estimate VaR.


2) Assume the log returns are normally distributed, and using the full loss operator to estimate VaR. Note: to obtain the VaR run a simulation sampling the normal r.v. N times, where N = 10, 000.


3) Assume the log returns are normally distributed, and using the first order approximation to the loss operator to estimate VaR。 Note: there is no need to run a simulation.


4) Schocked the histrocial return data and compute the stressed VaR, as well as economic capital charges.


5) Back-testing the VaR results in realized data and compare model effectivness.



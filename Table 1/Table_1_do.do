***37 bin results for full MPA system***
keep if abs(x)<=37
bys x post: egen yhat = sum(y)
drop y
rename yhat y
drop gpz
duplicates drop
sum
**** Pre-MPA results
preserve
keep if abs(x)<=37
keep if post==0
generate treated = x >= 0
* Generate polynomial terms of the running variable
generate x2 = x^2
generate x3 = x^3
* Generate interaction terms with the treatment indicator
generate x_treated = x*treated
generate x2_treated = x2*treated
generate x3_treated = x3*treated
* Run the regression with the specified model (linear and third-order polynomial) - Col.1 & 2 of Table 2
newey y x treated x_treated , lag(1) force
newey y x x2 x3 treated x_treated x2_treated x3_treated, lag(1) force
restore

**** Post-MPA results
preserve
keep if post==1
sum
generate treated = x >= 0
* Generate polynomial terms of the running variable
generate x2 = x^2
generate x3 = x^3
* Generate interaction terms with the treatment indicator
generate x_treated = x*treated
generate x2_treated = x2*treated
generate x3_treated = x3*treated
* Run the regression with the specified model (linear and third-order polynomial) - Col.3 & 4 of Table 2
newey y x treated x_treated , lag(1) force
newey y x x2 x3 treated x_treated x2_treated x3_treated, lag(1) force
restore
preserve

**** RD-DID 
keep if abs(x)<=37
bys x post: egen yhat = sum(y)
drop y
rename yhat y
duplicates drop
sum
generate treated = x >= 0
* Generate polynomial terms of the running variable
generate x2 = x^2
generate x3 = x^3
* Generate interaction terms with the treatment indicator
generate x_treated = x*treated
generate x2_treated = x2*treated
generate x3_treated = x3*treated
*Sort data
xtset post x
* thirdorder polynomia
*Sort data
xtset post x
* linear est -Col.5 Table 2
newey y i.treated##i.post##(c.x), lag(1) force
* thirdorder polynomia - Col.6 Table 2
newey y i.treated##i.post##(c.x c.x2 c.x3), lag(1) force
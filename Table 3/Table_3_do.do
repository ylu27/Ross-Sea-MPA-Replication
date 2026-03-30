
keep if abs(x)<=37
bys x post: egen yhat = sum(y)
drop y
rename yhat y
drop gpz
duplicates drop
sum
sum if post==0
sum if post==1
sum if x<0
sum if post==0 & x<0
sum if post==1 & x<0
sum if x>0
sum if post==0 & x>0
sum if post==1 & x>0
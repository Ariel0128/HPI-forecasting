proc import out=houseprice
	datafile="/home/u47247130/sasuser.v94/HousePrice.csv"
	replace;
run;

proc contents data= houseprice;
run;

proc sgplot data=houseprice;
	series x=date y= hpi;
	series x=date y= hpi_annual_change / y2axis;
	y2axis min= -10 max= 30;
	title "House Price Index";
run;

/* answer q2*/
proc reg data=houseprice;
	model hpi = cpi gdp_annual_change 
	current_account_balance interest_rate employment unemployment_rate / clb;
	output out = reg_p p = forecast ;
run;
proc reg data=houseprice;
	model hpi = cpi  employment / clb;
run;

proc corr data=houseprice;
run;

proc import out=train
	datafile="/home/u47247130/sasuser.v94/train.csv"
	replace;
run;
proc import out=test
	datafile="/home/u47247130/sasuser.v94/test.csv"
	replace;
run;

proc esm data=train out = winters print = forecasts 
	plot = (forecasts modelforecasts);
   id Date interval = quarter ; 
   forecast hpi / method=linear;
run;

data winters_p;
	set winters;
	rename hpi= forecast;
	if date > "01jun15"d;
run;
	

data winters_err;
   set test;
   set winters_p;
   err_winters = hpi-forecast;
   e2_winters = err_winters**2;   
   rerr_winters = sqrt(e2_winters);
   pcterr_winters = 100*abs(err_winters)/abs(hpi);
run;

proc means n mean data = winters_err;
	var e2_winters rerr_winters pcterr_winters;
	label e2_winters = "Mean Squared Error";
	label rerr_winters = "Root Mean Square Deviation";
	label pcterr_winters = "Mean Absolute Percentage Error";
run; quit;
	

proc import out=train_reg
	datafile="/home/u47247130/sasuser.v94/train_reg.csv"
	replace;
run;

proc reg data=train_reg;
	model hpi = cpi gdp_annual_change 
	current_account_balance interest_rate employment unemployment_rate;
	output out = reg_p p = forecast ;
run;
data out_reg;
   SET reg_p;
   if date > "01jun15"d;
   keep forecast;
run;
/** evaluate forecast accuracy **/
data reg_err;
   set test;
   set out_reg;
   err_reg = hpi-forecast;
   e2_reg = err_reg**2;
   rerr_reg = sqrt(e2_reg);
   pcterr_reg = 100*abs(err_reg)/abs(hpi);
run;

proc means n mean data = reg_err;
var e2_reg rerr_reg pcterr_reg;
label e2_reg = "Mean Squared Error";
label rerr_reg = "Root Mean Square Deviation";
label pcterr_reg = "Mean Absolute Percentage Error";
run; quit;

proc sgplot data=reg_err;
	series x=date y= hpi;
	series x=date y= forecast / y2axis;
	title "House Price Index";
run;

proc arima data = train ;
identify var = hpi(1);
estimate p = 1;  /* ARIMA(1,1,0) */
run;
quit;

proc arima data=train;
	identify var = hpi stationarity = (adf = 1);
	identify var = hpi(1) stationarity = (adf = 1);
	identify var = hpi(2) stationarity = (adf = 1);
	identify var = hpi(3) stationarity = (adf = 1);
run;quit;

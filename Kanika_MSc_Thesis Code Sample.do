***********************************************************
****FROM CASH TO CLASSROOMS: DIRECT AND INDIRECT EFFECTS***
******OF A CONDITIONAL CASH TRANSFER PROGRAM ON GIRLS'*****
**********EDUCATION IN WEST BENGAL, INDIA******************
**********BY: KANIKA PHOGAT, BOCCONI UNIVERSITY************
***********************************************************


/* This code is part of my research evaluating the causal impact of a Conditional Cash Transfer (CCT) program, the Kanyashree Prakalpa (henceforth, KP), on girls' education outcomes in West Bengal, India. The analysis uses Difference-in-Differences (DD) and Triple Differences (DDD) estimation strategies to evaluate the program's direct effects on eligible girls and its spillover effects on their younger ineligible siblings.

The code includes three main parts:

1. Data cleaning and merging: Prepares data for analysis by standardizing variable names acoss different years and creating new variables needed for the main analysis.

2. Main analysis: Checks for parallel trends assumptions and then implements DD and DDD estimation strategies to get causal ITT (intent to treat) effect of the CCT program.

3. Robustness checks: Conducts synthetic control analysis to validate findings from DD.

The data for this analysis was sourced from the ASER (Annual Status of Education Report) survey, conducted by Pratham, India. 
*/


clear

set more off
set matsize 5000

cd "/Users/Kanika/Desktop/THESIS"
global desktop "/users/Kanika/desktop" 
global mainpath "$desktop/Thesis"
global data "$mainpath/data"
global graphs "$mainpath/graphs"
global output "$mainpath/tables"

/* These globals allow me to avoid writing out directories explicitly each time I read in or export a file. Also, it makes it easier for other people to replicate my analysis in their computer as they will only need to change the globals before running the do file once they have access to the datasets. 
*/



***********************************************************
********Part I: CLEANING AND MERGING DATASETS**************
***********************************************************

///What you will find in Part I: 

*I) It appends yearly datasets and renames some variables.
*II) It creates outcome and control variables to prepare for main analysis. Please note that variable names and their values were heterogenous throughout the years. 

*Note: Original datasets covering all of India were provided in Excel. I first extracted the states I needed and addressed the inconsistencies within each year's data like number values stored as strings for the same variable in different years. This process was faster and easier in excel. After preparing and standardizing the data for each year in Excel, I imported and merged the clean datasets into Stata. 


use "$data/WB+JH+OD+CH 2008.dta", clear

append using ///
    "WB+JH+OD+CH 2009.dta" ///
    "WB+JH+OD+CH 2010.dta" ///
    "WB+JH+OD+CH 2011.dta" ///
    "WB+JH+OD+CH 2012.dta" ///
    "WB+JH+OD+CH 2013.dta" ///
    "WB+JH+OD+CH 2014.dta" ///
    "WB+JH+OD+CH 2016.dta" ///
    "WB+JH+OD+CH 2018.dta" ///
    "WB+JH+OD+CH 2022.dta"

***Enrollment:

* Generating separate variables for school type for 2018 and 2022 due to inconsistent variable names and values for these years.

gen school_govt1 = (school_type == 1)///
      if year == 2018 | year == 2022 

gen school_private1 = (school_type == 2) ///
      if year == 2018 | year == 2022

gen school_madarsa1 = (school_type == 3) ///
      if year == 2018 | year == 2022

gen school_other1 = (school_type == 4) ///
      if year == 2018 | year == 2022


gen enroll = .

* Child is enrolled if attending any type of educational instritution
	  
replace enroll = 1 if school_govt == 1 | school_private == 1 | school_madarsa == 1 | school_other == 1 | school_govt1 == 1 | school_private1 == 1 | school_madarsa1 == 1 | school_other1 == 1

* Child is not enrolled if she is out of school either as a 'never enrolled' or as a 'dropped out'

replace enroll = 0 if oos_never_enr == 1 | oos_dropout == 1 


***Reading and Math levels on an ordinal scale from 0 to 4:

gen reading_level = .

* For years with separate dummy variables for reading levels. Please note only the years that I have data for are included in the loops.

foreach year in 2008 2009 2010 2011 2012 {
	
    replace reading_level = 0 ///
	if read_nothing == 1 & year == `year'
		  
    replace reading_level = 1 ///
	if read_letter == 1 & year == `year'
		  
    replace reading_level = 2 ///
	if read_word == 1 & year == `year'
		  
    replace reading_level = 3 ///
	if read_level_1 == 1 & year == `year'
		  
    replace reading_level = 4 ///
	if read_level_2 == 1 & year == `year'
}

* For years with coded variables for reading levels

foreach year in 2013 2014 2016 2018 2022 {
    
	replace reading_level = read_code - 1 if year == `year'

}


* For years with separate dummy variables for math levels

foreach year in 2008 2009 2010 2011 2012 {
    
	replace math_level = 0 ///
	if math_nothing == 1 & year == `year'
	
    replace math_level = 1 ///
	 if math_num_1_9 == 1 & year == `year'
	 
    replace math_level = 2 ///
	if math_num_10_99 == 1 & year == `year'
	
    replace math_level = 3 ///
	if math_subtraction == 1 & year == `year'
	
    replace math_level = 4 ///
	if math_division == 1 & year == `year'
}

* For years with coded variables for math levels

foreach year in 2013 2014 2016 2018 2022 {
	
    replace math_level = math_code - 1 if year == `year'
	
}

tab reading_level
tab math_level

***Private Tutoring

gen child_tuition=.

replace child_tuition = 1 if tuition == 1
replace child_tuition = 0 if tuition == 2

replace child_tuition = 1 if tuition_yes == 1 
replace child_tuition = 0 if tuition_no == 1

***Treated State

gen WB = 0
replace  WB = 1 if state_code==19

***Variable for post-intervention years

gen post = 0
replace post = 1 if year>2013

*** Variable for female gender

gen female = 0
replace female = 1 if child_gender == 2 


***Interaction Terms for DD and DDD regressions

gen WB_post = WB*post
gen WB_post_fem = WB*post*female


*** Control Variables. Please note that this code sample includes only selected controls for brevity. Full analysis includes additional variables.

ren child_age age //child's age

ren total_member hh_size //household size

** Whether house is cemented

gen hh_cement = .

replace hh_cement = 0 if hhtype_katcha == 1 | hhtype_semi_katcha == 1
replace hh_cement = 1 if hhtype_pucca == 1

replace hh_cement = 0 if hh_type == 1 | hh_type == 2
replace hh_cement = 1 if hh_type == 3

** Household with electricity

gen hh_electricity = .

replace hh_electricity = 0 if hh_electricity_conn_no == 1
replace hh_electricity = 1 if hh_electricity_conn_yes == 1

replace hh_electricity = 0 if hh_electricity_conn == 2
replace hh_electricity = 1 if hh_electricity_conn == 1

** If child's mother has some education

gen mother_schooling= .

replace mother_schooling = 1 if mother_gone_to_school_yes == 1
replace mother_schooling = 0 if mother_gone_to_school_no == 1

replace mother_schooling = 1 if mother_gone_to_school == 1
replace mother_schooling = 0 if mother_gone_to_school == 2

** If child's village has a secondary school

gen vlg_sec = .

replace vlg_sec = 1 if vlg_govt_secondary_school_1_10 == 1
replace vlg_sec = 0 if vlg_govt_secondary_school_1_10 == 2

replace vlg_sec = 1 if vlg_govt_secondary_school == 1
replace vlg_sec = 0 if vlg_govt_secondary_school == 2


***********************************************************
********Part II: MAIN ANALYSIS AND REPORTING **************
***********************************************************

///What you will find in Part II: 

*I) Summary statistics and tables and graphs for Parallel trends assumption
*II) Regression analysis and output tables using DD and DDD estimation strategies 


*TABLE 1: SUMMARY STATISTICS

/* This table summarizes some key covariates for West Bengal (treated states) and Jharkhand, Odisha and Chhattisgrah (Control States)
*/

preserve 

keep if age>=13 & age<=16 & female ==1

matrix Table_1 = J(16, 7, .)

local i = 1

foreach var of varlist female age hh_electricity hh_toilet_combined hh_phone vlg_pvt_school vlg_pvt_clinic vlg_banking vlg_road_pucca vlg_sec hh_size mother_schooling vlg_mid hh_cement {
    
    * Means, SD, and N for WB
    qui sum `var' if WB == 1 & year<2014
    matrix Table_1[`i', 1] = r(mean)
    matrix Table_1[`i', 2] = r(sd)
    matrix Table_1[`i', 3] = r(N)

    * Means, SD, and N for Non-WB (Control States)
    qui sum `var' if WB == 0 & year<2014
    matrix Table_1[`i', 4] = r(mean)
    matrix Table_1[`i', 5] = r(sd)
    matrix Table_1[`i', 6] = r(N)

    * Difference in means between WB and Non-WB
    qui reg `var' WB if year<2014, vce(robust)
    matrix Table_1[`i', 7] = _b[WB]

    local i = `i' + 1
}

** Saving row and column names

matrix rownames Table_1 = "Female" "Age" "Electricity" "Toilet" "Phone" "Private School" "Private Clinic" "Banking" "Paved Road" "Sec School" "Household Size" "Mother's Schooling" "Middle School" "Cemented House"
matrix colnames Table_1 = "Mean(WB)" "SD(WB)" "N(WB)" "Mean(Non-WB)" "SD(Non-WB)" "N(Non-WB)" "Difference"

** Saving output in excel 

putexcel set "summary_stats.xls", replace

putexcel A1 = "Variable", bold
putexcel B1 = "Mean(WB)", bold
putexcel C1 = "SD(WB)", bold
putexcel D1 = "N(WB)", bold
putexcel E1 = "Mean(Non-WB)", bold
putexcel F1 = "SD(Non-WB)", bold
putexcel G1 = "N(Non-WB)", bold
putexcel H1 = "Difference", bold
putexcel A2 = matrix(Table_1), names nformat(number_d2)

restore 


*GRAPH 1: PARALLEL TRENDS FOR ENROLLMENT

/*
This section plots the mean enrollment trends for four groups. I am doing DD analyis where I compare 13–16-year-old girls in West Bengal (treated states) and 13–16-year-old girls in the control states before and after the KP implementation.. I am also doing DDD analysis where I compare Group A (13–16-year-old girls) and Group B (13–16-year-old boys) in the treatment state (West Bengal) to Group A (13–16-year- old girls) and Group B (13–16-year-old boys) in the control states before and after the KP implementation.

Four plots are generated to visualize enrollment trends for:
- WB Girls vs WB Boys
- WB Girls vs Control Girls
- Control Girls vs Control Boys
- WB Boys vs Control Boys


*/
preserve

keep if age >= 13 & age <= 16

* Creating different groups for plotting

gen WB_girls = (WB == 1 & female == 1)
gen WB_boys = (WB == 1 & female == 0)
gen control_girls = (WB == 0 & female == 1)
gen control_boys = (WB == 0 & female == 0)

* Calculating mean enrollment by group and year

collapse (mean) enroll, by(year WB_girls WB_boys control_girls control_boys)

* Create enrollment rate by group for plotting

gen enroll_rate = .
replace enroll_rate = enroll if WB_girls == 1
replace enroll_rate = enroll if WB_boys == 1
replace enroll_rate = enroll if control_girls == 1
replace enroll_rate = enroll if control_boys == 1

* Labelling respective groups

gen group_label = ""
replace group_label = "WB Girls" if WB_girls == 1
replace group_label = "WB Boys" if WB_boys == 1
replace group_label = "Control Girls" if control_girls == 1
replace group_label = "Control Boys" if control_boys == 1

* Plotting parallel trends for each pair

twoway (line enroll_rate year if group_label == "WB Girls", ///
lcolor(blue) lpattern(solid) lwidth(medium)) ///
(line enroll_rate year if group_label == "WB Boys", ///
lcolor(blue) lpattern(dash) lwidth(medium)), ///
legend(order(1 "WB Girls" 2 "WB Boys")) ///
xlabel(2008(4)2022) ///
ylabel(, angle(horizontal)) ///
xline(2014) xtitle("Year") ///
ytitle("Mean Enrollment Rate") yscale(range(0.8 1)) /// 
graph save plot1, replace

twoway (line enroll_rate year if group_label == "WB Girls", ///
lcolor(blue) lpattern(solid) lwidth(medium)) ///
(line enroll_rate year if group_label == "Control Girls", ///
lcolor(red) lpattern(solid) lwidth(medium)), ///
legend(order(1 "WB Girls" 2 "Control Girls")) ///
xlabel(2008(4)2022) ///
ylabel(, angle(horizontal)) ///
xline(2014) xtitle("Year") ///
ytitle("Mean Enrollment Rate") ///
yscale(range(0.8 1))  ///
graph save plot2, replace

twoway (line enroll_rate year if group_label == "Control Girls", ///
lcolor(red) lpattern(solid) lwidth(medium)) ///
(line enroll_rate year if group_label == "Control Boys", ///
lcolor(red) lpattern(dash) lwidth(medium)), ///
legend(order(1 "Control Girls" 2 "Control Boys")) ///
xlabel(2008(4)2022) ///
ylabel(, angle(horizontal)) ///
xline(2014) xtitle("Year") ///
ytitle("Mean Enrollment Rate") ///
yscale(range(0.8 1)) ///
graph save plot3, replace

twoway (line enroll_rate year if group_label == "WB Boys", ///
lcolor(blue) lpattern(dash) lwidth(medium)) ///
(line enroll_rate year if group_label == "Control Boys", ///
lcolor(red) lpattern(dash) lwidth(medium)), ///
legend(order(1 "WB Boys" 2 "Control Boys")) ///
xlabel(2008(4)2022) ///
ylabel(, angle(horizontal)) ///
xline(2014) xtitle("Year") ///
ytitle("Mean Enrollment Rate") ///
yscale(range(0.8 1)) ///
graph save plot4, replace

graph combine plot1.gph plot2.gph plot3.gph plot4.gph, rows(2) title("Parallel Trends Assumptions Check for Enrollment")

restore

/*
Now, to reinforce my findings from the graphical comparsion of mean enrollemnts of differenrt groups, I conduct additional tests. 
Firstly, I  conduct regression analysis to check the validity of the parallel trend assumption for both DD and DDD specifications. I test for parallel trends in boys' and girls' enrollment as well as learning outcomes in the six years prior to the program (2008–2013) using the ASER data. 
Secondly, I conduct the regression analysis with individual year dummies and conduct a joint F test for each of the interaction coefficients.
*/

* TABLE 2 & 3: PARALLEL TRENDS REGRESSION

* Diff-in-diff:

preserve
keep if year<2014 //pre-intervention years
gen n_year = year-2008  // 2008 is the first year I have data for 
gen WB_year = WB*n_year // state-year interaction term


reg  enroll WB_year WB n_year i.year age mother_schooling ///
[pw=hh_multiplier] if age>=13 & age<=16 & female==1, robust cluster(state_code) 

outreg2 using "parallel_trends_dd.xls", excel replace ///
    title("Table 2: Parallel Trends Assumption DD") ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_year) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes noas
	
** NOW MANUALLY ADDING P VALUE FROM WILD BOOTSTRAP PROCEDURE WITH SE CLUSTERED AT THE STATE LEVEL AND SUB-CLUSTERED AT THE DISTRICT LEVEL BECAUSE THIS IS WHAT I USE TO CONDUCT INFERENCE IN MY ANALYSIS.

boottest  WB_year, cluster(state_code)  bootcluster(district_code) jackknife reps(1000000) 

* Triple Diff-in-diff:

preserve
keep if year<2014 
gen n_year = year-2008 
gen WB_year = WB*n_year
gen fem_year = female*n_year
gen WB_year_fem = WB*female*n_year // state-gender-year interaction term

reg enroll WB_year_fem WB_year fem_year WB_fem n_year WB ///
female i.year age  mother_schooling [pw=hh_multiplier] ///
if age>=13 & age<=16,robust cluster (state_code) 

outreg2 using "parallel_trends_ddd.xls", excel append ///
    title("Table 3: Parallel Trends Assumption DDD") ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_year_fem) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes noas

** NOW MANUALLY ADDING P VALUE FROM WILD BOOTSTRAP PROCEDURE INTO REG TABLE DIRECTLY

boottest  WB_year_fem, cluster(state_code) bootcluster(district_code) jackknife reps(1000000)


* TABLE 3 & 4: PARALLEL TRENDS REGRESSION WITH INDIVIDUAL YEAR DUMMIES AND CONDUCTING A JOINT F TEST FOR EACH OF THE INTERACTION COEFFICIENTS

// Generating individual year dummies

tabulate year, generate(year_dummy)

// Generating double interactions for each year that is available 

gen WB_year2008 = WB * year_dummy1
gen WB_year2009 = WB * year_dummy2
gen WB_year2010 = WB * year_dummy3
gen WB_year2011 = WB * year_dummy4
gen WB_year2012 = WB * year_dummy5
gen WB_year2013 = WB * year_dummy6
gen WB_year2014 = WB * year_dummy7
gen WB_year2016 = WB * year_dummy8
gen WB_year2018 = WB * year_dummy9
gen WB_year2022 = WB * year_dummy10

gen female_year2008 = female * year_dummy1
gen female_year2009 = female * year_dummy2
gen female_year2010 = female * year_dummy3
gen female_year2011 = female * year_dummy4
gen female_year2012 = female * year_dummy5
gen female_year2013 = female * year_dummy6
gen female_year2014 = female * year_dummy7
gen female_year2016 = female * year_dummy8
gen female_year2018 = female * year_dummy9
gen female_year2022 = female * year_dummy10

// Generating triple interactions for each year

gen WB_year2008_female = WB * year_dummy1 * female
gen WB_year2009_female = WB * year_dummy2 * female
gen WB_year2010_female = WB * year_dummy3 * female
gen WB_year2011_female = WB * year_dummy4 * female
gen WB_year2012_female = WB * year_dummy5 * female
gen WB_year2013_female = WB * year_dummy6 * female
gen WB_year2014_female = WB * year_dummy7 * female
gen WB_year2016_female = WB * year_dummy8 * female
gen WB_year2018_female = WB * year_dummy9 * female
gen WB_year2022_female = WB * year_dummy10 * female

* Diff-in-diff:

reg enroll WB year_dummy1 year_dummy2 year_dummy3 year_dummy4 ///
year_dummy5 year_dummy6 year_dummy7 year_dummy8 year_dummy9 ///
year_dummy10 WB_year2008 WB_year2009 WB_year2010 WB_year2011 ///
WB_year2012 WB_year2013 WB_year2014 WB_year2016 WB_year2018 ///
WB_year2022 age mother_schooling [pw=hh_multiplier] if age>=13 & age<=16 & female==1, robust cluster(state_code)

outreg2 using "parallel_trends_dd1.xls", excel replace ///
    title("Table 3: Year-Wise Parallel trends for Eligible Girls using DD specification") ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_year2008 WB_year2009 WB_year2010 ///
	WB_year2011 WB_year2012 WB_year2013) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes	noas	

***Joint F test for each interaction term for pre-years and then manually adding it to the reg table

boottest WB_year2008 WB_year2009 WB_year2010 WB_year2011 WB_year2012 WB_year2013, ///
cluster(state_code) bootcluster(district_code) jackknife reps(1000000)


* Triple Diff-in-diff:

reg enroll WB female year_dummy1 year_dummy2 year_dummy3 ///
year_dummy4 year_dummy5 year_dummy6 year_dummy7 year_dummy8 ///
year_dummy9 year_dummy10 WB_year2008 WB_year2009 WB_year2010 ///
WB_year2011 WB_year2012 WB_year2013 WB_year2014 WB_year2016 ///
WB_year2018 WB_year2022 female_year2008 female_year2009 ///
female_year2010 female_year2011 female_year2012 ///
female_year2013 female_year2014 female_year2016 ///
female_year2018 female_year2022 WB_year2008_female ///
WB_year2009_female WB_year2010_female WB_year2011_female ///
WB_year2012_female WB_year2013_female WB_year2014_female ///
WB_year2016_female WB_year2018_female WB_year2022_female ///
age mother_schooling [pw=hh_multiplier] if age>=13 & age<=16, robust cluster(state_code)


outreg2 using "parallel_trends_ddd1.xls", excel replace ///
    title("Table 4: Year-Wise Parallel trends for Eligible Girls using DDD specification") ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_year2008_female WB_year2009_female ///
	WB_year2010_female WB_year2011_female ///
	WB_year2012_female WB_year2013_female) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes	noas	

***Joint F test for each interaction term for pre-years

boottest WB_year2008_female WB_year2009_female WB_year2010_female WB_year2011_female WB_year2012_female WB_year2013_female, cluster(state_code) bootcluster(district_code) jackknife 

/*
Now, I  evaluate the effect of the KP program on enrollment among eligible girls using Difference-in-Differences (DD) estimation. Three models are estimated:
1. Without controls or trends.
2. With time trends only.
3. With both time trends and control variables.
*/

* TABLE 5: DD RESULTS FOR ENROLLMENT 

* No controls + no trends

reg enroll WB_post WB post female [pw=hh_multiplier] ///
if age>=13 & age<=16 & female==1, robust cluster(state_code)

boottest WB_post, cluster(state_code) bootcluster(district_code) jackknife reps(1000000) seed (12345)   

outreg2 using "enrollment_DD.xls", excel replace ///
    title("Table 5: Difference-in-differences (DD) Estimate of the Impact of KP program on Enrollment of Eligible Girls") ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_post WB post ) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes noas

* Only trends; no controls

reg enroll WB_post WB post i.year state_code#year ///
[pw=hh_multiplier] if age>=13 & age<=16 & female==1, robust cluster(state_code)

boottest WB_post, cluster(state_code) bootcluster(district_code) jackknife reps(1000000) seed(12345) 

outreg2 using "enrollment_DD.xls", excel append ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_post WB post ) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes noas

* Both linear trends and controls

reg enroll WB_post WB post i.year state_code#year ///
age hh_electricity hh_toilet_combined hh_phone ///
vlg_pvt_school vlg_pvt_clinic vlg_banking ///
vlg_road_pucca vlg_sec hh_size mother_schooling ///
vlg_mid hh_cement [pw=hh_multiplier]if age>=13 & age<=16 ///
& female==1, robust cluster(state_code)

boottest WB_post, cluster(state_code) bootcluster(district_code) jackknife reps(1000000) seed(12345)

outreg2 using "enrollment_DD.xls", excel append ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_post WB post ) ///
	bdec(4) sdec(3) rdec(3) ///
	nonotes noas

	
/*
Now, I  evaluate the effect of the KP program on enrollment among eligible girls using Difference-in Difference-in Differences (DD) estimation. Three models are estimated:
1. Without controls or trends.
2. With time trends only.
3. With both time trends and control variables.
*/
	
	
* TABLE 6: DDD RESULTS FOR ENROLLMENT 

* No controls + no trends

reg enroll WB_post_fem WB post female WB_post WB_fem post_fem ///
[pw=hh_multiplier] if age>=13 & age<=16, robust cluster(state_code)

boottest WB_post_fem, cluster(state_code) bootcluster(district_code) jackknife reps(1000000)

outreg2 using "enrollment_DDD.xls", excel replace ///
    title("Table 6: Triple-difference (DDD) Estimate of the Impact of KP program on Enrollment of Eligible Girls") ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_post_fem WB post female) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes noas

* Only trends; no controls

reg enroll WB_post_fem WB post female WB_post WB_fem post_fem ///
i.year state_code#year female#year [pw=hh_multiplier]///
if age>=13 & age<=16, robust cluster(state_code)

boottest WB_post_fem, cluster(state_code) bootcluster(district_code) jackknife reps(1000000) 

outreg2 using "enrollment_DDD.xls", excel append ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_post_fem WB post female) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes noas

* Both linear trends and controls

reg enroll WB_post_fem WB post female WB_post WB_fem post_fem ///
i.year state_code#year female#year age hh_electricity ///
hh_toilet_combined hh_phone vlg_pvt_school vlg_pvt_clinic ///
vlg_banking vlg_road_pucca vlg_sec hh_size mother_schooling ///
vlg_mid hh_cement [pw=hh_multiplier] if age>=13 & age<=16, robust cluster(state_code)

boottest WB_post_fem, cluster(state_code) bootcluster(district_code) jackknife reps(1000000)

outreg2 using "enrollment_DDD.xls", excel append ///
    addstat("Observations", e(N), "R-squared", e(r2)) ///
    keep(WB_post_fem WB post female) ///
	bdec(4) sdec(3) rdec(3) ///
    nonotes noas
	

***********************************************************
**Part III: ROBUSTNESS CHECK - SYNTHETIC CONTROL METHOD****
***********************************************************

/*
This section implements the Synthetic Control Method (SCM) to validate the findings from the DD analysis. The goal here is to construct a synthetic version of West Bengal from untreated states in the donor pool. This synthetic unit will then serve as a control group.
*/

///What you will find in Part III: 

*I) Loading the cleaned and merged data from excel and declaring a balanced panel dataset
*II) Implementing SCM using synth2 


*Note:This part involved a separate process where I included nearly all states from the yearly Excel files to create the donor pool needed for constructing a synthetic West Bengal (the treated state). I then cleaned these Excel file with a  similar process used for the original analysis to ensure consistency in variables across states and years.

clear all 

use "$data/SCM merged file.dta", clear

* Restricting to  relevant sample for analysis

keep if female==1 
keep if age>=13 & age<=16

* Collapsing the data to ensure unique state code and year combinations

collapse (mean) enroll reading_level math_level child_tuition ///
age hh_electricity hh_toilet_combined hh_phone vlg_pvt_school ///
vlg_pvt_clinic vlg_banking vlg_road_pucca vlg_sec hh_size ///
mother_schooling vlg_mid hh_cement, by(state_code year)

* Now declaring the panel data structure

xtset state_code year

* Now using Synth2 command for checking covariate balance in pre-treatment periods and plotting the enrollment trends for West Bengal and Synthetic West Bengal. I use all the original control variables and some lagged outcome variables to construct synthetic West Bengal. Treated unit or tru is 19 which is the state code for West Bengal and trp is treatment period as post intervention period starts from 2014 onwards. 

synth2 enroll hh_electricity hh_toilet_combined hh_phone ///
vlg_pvt_school vlg_pvt_clinic vlg_banking ///
vlg_road_pucca vlg_sec hh_size mother_schooling hh_cement ///
enroll(2011) enroll(2012) enroll(2013) , tru(19) trp(2014)


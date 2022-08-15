//Clean the combined data_2013-2018


************************************************
************ data cleaning part ***********
************************************************

use "xxx\CHARLS\Combined data_2013~2018.dta"//users need to COMPLETE the address

destring communityID, replace
destring iyear, replace
destring imonth, replace

rename ba000_w2_3 gender
rename ba004_w3_1 byear
rename ba004_w3_2 bmonth
rename ba004_w3_3 bday
rename bd001_w2_4 edu
label variable edu "Highest level of education"
rename bb001_w3 address
label variable address "Current address"
rename bb001_w3_1 address_type1
label variable address_type1 "Current address type1"
rename bb001_w3_2 address_type
label variable address_type "Current address type2"
rename be001 marriage
rename da049 sleep
rename dc028 satisfaction
rename ga001 income_indicator
rename ga002 income_lastyr
label variable income_lastyr "Salary income last year excl pension income"
replace income_lastyr = (ga002_bracket_min + ga002_bracket_max)/2 if income_lastyr ==. & ga002_bracket_min !=. & ga002_bracket_max !=.
drop ga002_bracket_min
drop ga002_bracket_max
rename fb011 retired
label variable retired "Retired or internal retired"
rename fb012 reced
label variable reced "receded"
rename fm003 retire_workunit
label variable retire_workunit "Workunit type"
rename fm011 retire_type
label variable retire_type "Retirement type"
rename fm012 worker_cadre
label variable worker_cadre "Retired as worker or cadre"
rename fm026 int_retire_reason
rename fm052 work_retired
label variable work_retired "Work or not after retirement"
rename fn002_w3 pension
label variable pension "Receiving/participating Pension of Government, insitutions and firms"
rename fn002_w2s1 rec_govpension
label variable rec_govpension "receiving pension of the government"
rename fn002_w2s2 rec_instpension
label variable rec_instpension "receiving pension of institutions"
rename fn002_w2s3 rec_firmpension
label variable rec_firmpension "receiving pension of the firms"
rename fn030_w2 sup_pension
label variable sup_pension "Receiving/participating supplement pension"
rename fn002_w2s4 rec_sup_pension
label variable rec_sup_pension "Receiving supplement pension"
label variable datay "Database year"

//Generate social activity
gen social = 1 if da056s12 ==.
replace social = 0 if da056s12 == 12 | da056_s12 == 12
replace social =. if (datay == 2013 | datay == 2015) & da056s1 ==. & da056s2 ==. & da056s3 ==. & da056s4 ==. & da056s5 ==. & da056s6 ==. & da056s7 ==. & da056s8 ==. & da056s9 ==. & da056s10 ==. & da056s11 ==. & da056s12 ==.
replace social =. if datay == 2018 & da056_s1 ==. & da056_s2 ==. & da056_s3 ==. & da056_s4 ==. & da056_s5 ==. & da056_s6 ==. & da056_s7 ==. & da056_s8 ==. & da056_s9 ==. & da056_s10 ==. & da056_s11 ==. & da056_s12 ==.
label variable social "Social Activity (0=never; 1=at least once)"
drop da056s1 da056s2 da056s3 da056s4 da056s5 da056s6 da056s7 da056s8 da056s9 da056s10 da056s11 da056s12 da056_s1 da056_s2 da056_s3 da056_s4 da056_s5 da056_s6 da056_s7 da056_s8 da056_s9 da056_s10 da056_s11 da056_s12 

//Generate retirement income
gen income_pension_month = fn005_w2_1_
replace income_pension_month = fn005_w2_2_ if income_pension ==.
replace income_pension_month = fn005_w2_3_ if income_pension ==.
replace income_pension_month = fn005_w2_4_ if income_pension ==.
gen income_pension = income_pension_month * 12
label variable income_pension "Pension Income this year"
drop income_pension_month fn005_w2_1_ fn005_w2_2_ fn005_w2_3_ fn005_w2_4_

//Generate subjective health status
gen health = da001
replace health = da002 if health ==.
label variable health "Subjective Health"
drop da001 da002

//Generate retirement dummy
replace retired = 1 if zf15 == 1 //Retired if reported retirement in the last interview
replace retired = 1 if zf26 == 1 //Retired if reported finishing retirement procedure in the last interview
replace retired = 1 if fm037_w2 == 1 //Retired if reported internal retirement but not formal retirement in the last interview, but reported retirement in current interview
replace reced = 1 if zf16 == 1 //Receded if reported recede in the last interview
replace work_retired = 1 if work_retired ==. & zf27 == 1 //Work after retirement if reported work after retirement and this question was skipped in current interview
replace pension = 2 if fn006_w2 == 1 | fn006_w2 == 2 | fn006_w2 == 3 //Pension applied but not received if reported that they would have pension in the future (no matter they applied it or not currently)
drop zf16
drop zf26
drop zf27
drop fm037_w2
drop fn006_w2

//Generate age
gen age1 = iyear - byear
label variable age1 "Age(year-year)"
gen age2 = iyear -  byear + (imonth - bmonth) / 12 if bmonth > 0
replace age2 = iyear -  byear + (imonth - 1) / 12 if age2 ==.
label variable age2 "Age(with month)"
gen age3 = iyear - byear - 1 + floor((imonth + 12 - bmonth) / 3)*0.25 if bmonth > 0
replace age3 = iyear - byear - 1 + floor((imonth + 11) / 3)*0.25 if age3 ==.
label variable age3 "Age(with quarter)"


//Generate depression score
gen dc9 = 0 if dc009 == 1
replace dc9 = 1 if dc009 == 2
replace dc9 = 2 if dc009 == 3
replace dc9 = 3 if dc009 == 4
gen dc10 = 0 if dc010 == 1
replace dc10 = 1 if dc010 == 2
replace dc10 = 2 if dc010 == 3
replace dc10 = 3 if dc010 == 4
gen dc11 = 0 if dc011 == 1
replace dc11 = 1 if dc011 == 2
replace dc11 = 2 if dc011 == 3
replace dc11 = 3 if dc011 == 4
gen dc12 = 0 if dc012 == 1
replace dc12 = 1 if dc012 == 2
replace dc12 = 2 if dc012 == 3
replace dc12 = 3 if dc012 == 4
gen dc13 = 0 if dc013 == 4 //positive question
replace dc13 = 1 if dc013 == 3
replace dc13 = 2 if dc013 == 2
replace dc13 = 3 if dc013 == 1
gen dc14 = 0 if dc014 == 1
replace dc14 = 1 if dc014 == 2
replace dc14 = 2 if dc014 == 3
replace dc14 = 3 if dc014 == 4
gen dc15 = 0 if dc015 == 1
replace dc15 = 1 if dc015 == 2
replace dc15 = 2 if dc015 == 3
replace dc15 = 3 if dc015 == 4
gen dc16 = 0 if dc016 == 4 //positive question
replace dc16 = 1 if dc016 == 3
replace dc16 = 2 if dc016 == 2
replace dc16 = 3 if dc016 == 1
gen dc17 = 0 if dc017 == 1
replace dc17 = 1 if dc017 == 2
replace dc17 = 2 if dc017 == 3
replace dc17 = 3 if dc017 == 4
gen dc18 = 0 if dc018 == 1
replace dc18 = 1 if dc018 == 2
replace dc18 = 2 if dc018 == 3
replace dc18 = 3 if dc018 == 4
gen depresum = dc9 + dc10 + dc11 + dc12 + dc13 + dc14 + dc15 + dc16 + dc17 + dc18
gen depre_10 = 1 if depresum >= 10
replace depre_10 = 0 if depresum < 10
gen depre_16 = 1 if depresum >= 16
replace depre_16 = 0 if depresum < 16
label variable depresum "Sum of depression"
label variable depre_10 "Depression dummy: 1 if >10, 0 if <10"
label variable depre_16 "Depression dummy: 1 if >16, 0 if <16"
drop dc9 dc10 dc11 dc12 dc13 dc14 dc15 dc16 dc17 dc18

//Clean the interface
drop iyear imonth byear bmonth bday dc009 dc010 dc011 dc012 dc013 dc014 dc015 dc016 dc017 dc018 
order ID communityID depresum depre_10 depre_16 age1 age2 age3 gender retired reced retire_workunit retire_type worker_cadre int_retire_reason work_retired pension rec_govpension rec_instpension rec_firmpension sup_pension rec_sup_pension edu marriage health sleep satisfaction income_indicator income_lastyr income_pension social address address_type1 address_type datay


grstyle init
grstyle color background white

replace retired=0 if retired==2
label variable age2 "age "

gen health_a = 1 if  health ==3 |  health ==2 |  health ==1
replace health_a = 0 if health_a==.

destring ID, replace

sort ID datay, stable
gen edu_adj=.
by ID: replace edu_adj=edu[_n-1] if datay==2015 & edu==12
by ID: replace edu_adj=edu[_n-1] if datay==2018 & edu==12
by ID: replace edu_adj=edu_adj[_n-1] if datay==2018 & edu==12 & edu_adj[_n-1]!=.
by ID: replace edu_adj=edu[_n+1] if datay==2015 & edu==12 & edu_adj[_n-1]==.
replace edu_adj=edu if edu_adj==.


// transform into continuous education level
gen edu_adj_c=.
replace edu_adj_c=16 if edu_adj==9
replace edu_adj_c=14 if edu_adj==8
replace edu_adj_c=13 if edu_adj==7
replace edu_adj_c=12 if edu_adj==6
replace edu_adj_c=9 if edu_adj==5
replace edu_adj_c=6 if edu_adj==4
replace edu_adj_c=5 if edu_adj==3
replace edu_adj_c=3 if edu_adj==2
replace edu_adj_c=0 if edu_adj==1

///depression
gen log_dep =  log(depresum+1)

//marriage
replace marriage=0 if marriage!=1 & marriage!=.

//time. caution: interview time is not data releasing time 
gen time_2013 =1 if datay==2013
replace time_2013 = 0 if time_2013 ==.

gen time_2015 =1 if datay==2015
replace time_2015 = 0 if time_2015 ==.

gen time_2018 =1 if datay==2018
replace time_2018 = 0 if time_2018 ==.

gen distance = age2 - 60 if gender==1
replace distance = age2 - 50 if gender==2

gen cutoff=1 if age2>=60 & gender==1
replace cutoff=0 if age2<60 & gender==1

replace cutoff=1 if age2>=50 & gender==2
replace cutoff=0 if age2<50 & gender==2

gen dic_cutoff = distance*cutoff

gen distance_sqr = distance*distance 
gen distance_sqr_cut = distance_sqr*cutoff

order ID communityID depresum depre_10 log_dep age1 age2 age3 gender retired edu edu_adj edu_adj_c, first


/// summary statictics 
gen gender_a = gender-1
summarize depresum age2 gender_a retired edu_adj_c marriage if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)

************************************************
************ empirical analysis part ***********
************************************************


**mental health and age
binscatter2 log_dep age2 if gender==1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3), nquantiles(100) xtitle(age) ytitle(Male: Log Depression Score) xlabel(40(5)90,)
graph save "E:\figure\log depression score1", replace
graph export "E:\figure\log depression score1.png", replace

binscatter2 log_dep age2 if gender==2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3), nquantiles(100) xtitle(age) ytitle(Female: Log Depression Score) xlabel(40(5)90,)
graph save "E:\figure\log depression score2", replace
graph export "E:\figure\log depression score2.png", replace

binscatter2 log_dep age2 if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3), nquantiles(100) xtitle(age) ytitle(Pooling: Log Depression Score) xlabel(40(5)90,)
graph save "E:\figure\log depression score0", replace
graph export "E:\figure\log depression score0.png", replace

graph combine "E:\figure\log depression score1" "E:\figure\log depression score2"  "E:\figure\log depression score0", xsize(10) ysize(8) ycommon xcommon col(2) 
graph export "E:\figure\log depression score.png", replace


**measurement error

kdensity log_dep  if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) , xtitle(log depression score) title(Panel A: Pooling)
graph save "E:\figure\measurement error0", replace
graph export "E:\figure\measurement error0.png", replace

kdensity log_dep  if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3), xtitle(log depression score) title(Panel B: Male)
graph save "E:\figure\measurement error1", replace
graph export "E:\figure\measurement error1.png", replace


 kdensity log_dep  if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) , xtitle(log depression score) title(Panel C: Female)
graph save "E:\figure\measurement error2", replace
graph export "E:\figure\measurement error2.png", replace

graph combine "E:\figure\measurement error0" "E:\figure\measurement error1"  "E:\figure\measurement error2", xsize(18) ysize(5) ycommon xcommon col(3) 
graph export "E:\figure\measurement error.png", replace
 
 
***********assumption test***************

**Assumption 1: discontinuity of treatment assignment besides cutoff


** graph evidence

binscatter2 retired  distance if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3), nquantiles(80) rd(0 ) line(none) xtitle(Distance to cutoff) ytitle(Retire Rate: Pooling) xlabel(-20(5)40,) title(Panel A: Pooling)
graph save "E:\figure\discontinuity of treatment assignment", replace
graph export "E:\figure\discontinuity of treatment assignment.png", replace


binscatter2 retired  age2 if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3), nquantiles(80) rd(55 60 ) line(none) xtitle(Age) ytitle(Retire Rate: Male) xlabel(40(5)90,) title(Panel B: Male)
graph save "E:\figure\discontinuity of treatment assignment1", replace
graph export "E:\figure\discontinuity of treatment assignment1.png", replace

binscatter retired  age2 if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) , nquantiles(80) rd(50 55) line(none) xtitle(Age) ytitle(Retire Rate: Female) xlabel(40(5)90, ) title(Panel C: Female)
graph save "E:\figure\discontinuity of treatment assignment2", replace
graph export "E:\figure\discontinuity of treatment assignment2.png", replace

graph combine "E:\figure\discontinuity of treatment assignment" "E:\figure\discontinuity of treatment assignment1"  "E:\figure\discontinuity of treatment assignment2", xsize(18) ysize(6) ycommon  col(3)
graph export "E:\figure\discontinuity of treatment assignment.png", replace

***first stage

reg retired cutoff distance dic_cutoff if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-5 & distance<=5, r
outreg2 using " E:\result\first.tex", addstat(F statistic, e(F)) addtext(Bandwidth, 5) nocons keep(cutoff) replace

reg retired cutoff distance dic_cutoff if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2, r
outreg2 using " E:\result\first.tex", addstat(F statistic, e(F)) addtext(Bandwidth, 2) nocons keep(cutoff) append

reg retired cutoff distance dic_cutoff if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=55 & age2<=65, r
outreg2 using " E:\result\first.tex", addstat(F statistic, e(F)) addtext(Bandwidth, 5) nocons keep(cutoff) append

reg retired cutoff distance dic_cutoff if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=51 & age2<=62, r
outreg2 using " E:\result\first.tex", addstat(F statistic, e(F)) addtext(Bandwidth, 2) nocons keep(cutoff) append

reg retired cutoff distance dic_cutoff if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=47 & age2<=53, r
outreg2 using " E:\result\first.tex",addstat(F statistic, e(F)) addtext(Bandwidth, 3) nocons keep(cutoff) append

reg retired cutoff distance dic_cutoff if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=49 & age2<=51, r
outreg2 using " E:\result\first.tex", addstat(F statistic, e(F)) addtext(Bandwidth, 1) nocons keep(cutoff) append


****Assumption 3: manipulation of running variable besides cutoff

**pooling
rddensity distance if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance <= 5  & distance >= -5, c(0)

hist  distance if  (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance <= 5  & distance >= -5 ,width(0.02)  xline(0, lcolor(red)) xtitle(Distance to cutoff) title(Pooling) percent 

graph save "E:\figure\manipulation of running variable0", replace
graph export "E:\figure\manipulation of running variable0.png", replace 


**male
rddensity age2 if gender ==1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=55 & age2<=65, c(60) 

hist age2 if gender ==1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=55 & age2<=65,width(0.02)  xline(60, lcolor(red)) xtitle(Age) title(Male)  percent

graph save "E:\figure\manipulation of running variable1", replace
graph export "E:\figure\manipulation of running variable1.png", replace 


** female
rddensity age2 if gender ==2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=40 & age2<=60, c(50) 

hist age2 if gender ==2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=45 & age2<=55,width(0.02)  xline(50, lcolor(red)) xtitle(Age) title(Female) percent
 
graph save "E:\figure\manipulation of running variable2", replace
graph export "E:\figure\manipulation of running variable2.png", replace 

graph combine "E:\figure\manipulation of running variable0"  "E:\figure\manipulation of running variable1" "E:\figure\manipulation of running variable2", xsize(18) ysize(6) ycommon col(3) 
graph export "E:\figure\manipulation of running variable.png", replace


***** Assumption 2: continuity of covariales

//education

**pooling
rdplot edu_adj_c distance if  (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)& distance <= 2  & distance >= -2 & log_dep!=0, c(0) p(1) nbins(10 10) ci(90) xtitle(Age) graph_options(title(Education years: Pooling) legend(off))  

graph save "E:\figure\balance_edu", replace
graph export "E:\figure\balance_edu.png", replace

reg edu_adj_c cutoff distance dic_cutoff if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0, vce(cluster ID)

outreg2 using " E:\result\balance.tex", addtext(Bandwidth, 2) nocons keep(cutoff) replace

**male

rdplot edu_adj_c age2 if gender ==1 & age2>=58 & age2<=62 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & log_dep!=0, c(60) p(1) xtitle(Age) graph_options(title(Education years: Male) legend(off)) p(1) nbins(10 10) ci(90)

graph save "E:\figure\balance_edu_m", replace
graph export "E:\figure\balance_edu_m.png", replace

mean edu_adj_c  if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=57 & age2<=62 & log_dep!=0

reg edu_adj_c cutoff distance dic_cutoff if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=58 & age2<=62 & log_dep!=0, vce(cluster ID)

outreg2 using " E:\result\balance.tex",addtext(Bandwidth, 2) nocons keep(cutoff) append

**famale

rdplot edu_adj_c age2  if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)& age2>=48 & age2<=52 & log_dep!=0, c(50) p(1) nbins(10 10) ci(90) xtitle(Age) graph_options(title(Education years: Female) legend(off))  

graph save "E:\figure\balance_edu_f", replace
graph export "E:\figure\balance_edu_f.png", replace

reg edu_adj_c cutoff distance dic_cutoff if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=48 & age2<=52 & log_dep!=0, vce(cluster ID)

outreg2 using " E:\result\balance.tex",addtext(Bandwidth, 2) nocons keep(cutoff) append


//marriage

**pooling
rdplot marriage distance if  (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)& distance <= 2  & distance >= -2 & log_dep!=0, c(0) p(1) nbins(10 10) ci(90) xtitle(Distance to cutoff) graph_options(title(Marriage status: Pooling) legend(off))  

graph save "E:\figure\balance_mar", replace
graph export "E:\figure\balance_mar.png", replace

reg marriage cutoff distance dic_cutoff if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0, vce(cluster ID)

outreg2 using " E:\result\balance.tex",addtext(Bandwidth, 2) nocons keep(cutoff) append

**male
rdplot marriage age2 if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)& age2>=58 & age2<=62 & log_dep!=0, c(60) p(1) nbins(10 10) ci(90) xtitle(Age) graph_options(title(Marriage status: Male) legend(off))  

graph save "E:\figure\balance_mar_m", replace
graph export "E:\figure\balance_mar_m.png", replace

reg marriage cutoff distance dic_cutoff if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=58 & age2<=62 & log_dep!=0, vce(cluster ID)

outreg2 using " E:\result\balance.tex", addtext(Bandwidth, 2) nocons keep(cutoff) append


**famale

rdplot marriage age2 if gender ==2 & age2>=48 & age2<=52 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)& log_dep!=0, c(50) p(1) nbins(10 10) ci(90)  xtitle(Age) graph_options(title(Marriage status: Female) legend(off) )  

graph save "E:\figure\balance_mar_f", replace
graph export "E:\figure\balance_mar_f.png", replace
 
reg marriage cutoff distance dic_cutoff if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & age2>=48 & age2<=52 & log_dep!=0, vce(cluster ID)
outreg2 using " E:\result\balance.tex",addtext(Bandwidth, 2) nocons keep(cutoff) append


graph combine "E:\figure\balance_edu" "E:\figure\balance_mar" "E:\figure\balance_edu_m"  "E:\figure\balance_mar_m" "E:\figure\balance_edu_f"  "E:\figure\balance_mar_f" , xsize(6) ysize(8)  col(2) 
graph export "E:\figure\balance_all.png", replace


***************** results *******************

********* sample: pooling male and female

****OLS

reg log_dep retired if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)&log_dep!=0, vce(cluster ID)
outreg2 using "E:\result\pooling.tex", nocons keep(retired) replace


*****RD

**plot
rdplot log_dep distance if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , c(0) fuzzy(retired)  vce(cluster ID) p(1) nbins(10 10) ci(90) xtitle(Cutoff) graph_options(title(Pooling) legend(off))  

graph save "E:\figure\result_pooling", replace
graph export "E:\figure\result_pooling.png", replace 

mean depresum if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0

***bw 2
ivregress 2sls log_dep distance dic_cutoff i.datay (retired = cutoff) if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\pooling.tex", addtext(Bandwidth, 2, Adding controls, No) nocons keep(retired) append


**change bandwidth to 1
ivregress 2sls log_dep  distance dic_cutoff i.datay (retired = cutoff) if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-1 & distance<=1 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\pooling.tex", addtext(Bandwidth, 1, Adding controls, No)  nocons keep(retired) append


**add controls
ivregress 2sls log_dep distance dic_cutoff i.datay edu_adj_c marriage (retired = cutoff) if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\pooling.tex", addtext(Bandwidth, 2, Adding controls, Yes)  nocons keep(retired) append

ivregress 2sls log_dep  distance dic_cutoff i.datay edu_adj_c marriage (retired = cutoff) if (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-1 & distance<=1 & log_dep!=0 , vce(cluster ID)
outreg2 using " E:\result\pooling.tex", addtext(Bandwidth, 1, Adding controls, Yes )  nocons keep(retired) append



********* male subsample


****OLS

reg log_dep retired if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)&log_dep!=0, vce(cluster ID)
outreg2 using "E:\result\baseline_male.tex", nocons keep(retired) replace


*****RD

**plot male
rdplot log_dep distance if gender == 1 &(retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , c(0) fuzzy(retired)  vce(cluster ID) p(1) nbins(10 10) ci(90) xtitle(Age) graph_options(title(Male) legend(off))  

graph save "E:\figure\result_m", replace
graph export "E:\figure\result_m.png", replace 

mean depresum if gender == 1 &(retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0

***bw 2
ivregress 2sls log_dep distance dic_cutoff i.datay (retired = cutoff) if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\baseline_male.tex", addtext(Bandwidth, 2, Adding controls, No) nocons keep(retired) append


**change bandwidth to 1
ivregress 2sls log_dep  distance dic_cutoff i.datay (retired = cutoff) if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-1 & distance<=1 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\baseline_male.tex", addtext(Bandwidth, 1, Adding controls, No)  nocons keep(retired) append


**add controls
ivregress 2sls log_dep distance dic_cutoff i.datay edu_adj_c marriage (retired = cutoff) if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\baseline_male.tex", addtext(Bandwidth, 2, Adding controls, Yes)  nocons keep(retired) append

ivregress 2sls log_dep  distance dic_cutoff i.datay edu_adj_c marriage (retired = cutoff) if gender == 1 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-1 & distance<=1 & log_dep!=0 , vce(cluster ID)
outreg2 using " E:\result\baseline_male.tex", addtext(Bandwidth, 1, Adding controls, Yes )  nocons keep(retired) append



********* famle subsample

**plot female
rdplot log_dep distance if gender == 2 &(retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , c(0) fuzzy(retired)  vce(cluster ID) p(1) nbins(10 10) ci(90) xtitle(Age) graph_options(title(Female) legend(off))  

graph save "E:\figure\result_f", replace
graph export "E:\figure\result_f.png", replace 

graph combine "E:\figure\result_pooling"  "E:\figure\result_m" "E:\figure\result_f"  , xsize(18) ysize(6)  col(3) 
graph export "E:\figure\result_all.png", replace

**ols

reg log_dep retired if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3)&log_dep!=0, vce(cluster ID)
outreg2 using "E:\result\baseline_female.tex", nocons keep(retired) replace

mean depresum if gender == 2 &(retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0


***bw 2
ivregress 2sls log_dep distance dic_cutoff i.datay (retired = cutoff) if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\baseline_female.tex", addtext(Bandwidth, 2, Adding controls, No) nocons keep(retired) append


**change bandwidth to 1
ivregress 2sls log_dep  distance dic_cutoff i.datay (retired = cutoff) if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-1 & distance<=1 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\baseline_female.tex", addtext(Bandwidth, 1, Adding controls, No)  nocons keep(retired) append


**add controls
ivregress 2sls log_dep distance dic_cutoff i.datay edu_adj_c marriage (retired = cutoff) if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-2 & distance<=2 & log_dep!=0 , vce(cluster ID) 
outreg2 using " E:\result\baseline_female.tex", addtext(Bandwidth, 2, Adding controls, Yes)  nocons keep(retired) append

ivregress 2sls log_dep  distance dic_cutoff i.datay edu_adj_c marriage (retired = cutoff) if gender == 2 & (retired == 1 | pension !=3 | rec_govpension == 1 | rec_instpension == 2 | rec_firmpension == 3) & distance>=-1 & distance<=1 & log_dep!=0 , vce(cluster ID)
outreg2 using " E:\result\baseline_female.tex", addtext(Bandwidth, 1, Adding controls, Yes )  nocons keep(retired) append

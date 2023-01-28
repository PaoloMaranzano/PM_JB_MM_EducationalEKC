*************************************************************************************************
********** The role of education and income inequalities in EKC: evidences from Europe **********
*************************************************************************************************

***** Upload dataset *****
import excel "C:\Users\paulm\OneDrive\Documenti\Ricerca\Educational EKC (with Joao Bento)\Data_November_2020\Panel_November2020_19502016.xlsx", firstrow


***** Data transformation *****
** CO2 and GDP
generate y = ln(Emiss_CO2/(pop*1000000))
generate x = ln(rgdp/pop)
generate x2 = x^2
generate CO2pop = (Emiss_CO2*1000000)/(pop*1000000)
generate rgdppop = rgdp/pop
** Education and Human Capital
generate lhc = ln(hc)
generate yr_sch2 = yr_sch^2
generate lschool = ln(yr_sch)
generate lschool2 = lschool^2
** Trade openess
generate open = abs(csh_m) + abs(csh_x)
generate lopen = ln(open)
** Production factors
generate lavh = ln(avh)
generate lrtfp = ln(rtfp)
generate lrk_lavh = ln(rk/avh)
generate lrk = ln(rk)
** Income inequality
generate gini_disp_corr = gini_disp / gini_disp_se
generate lgini_disp_corr = ln(gini_disp_corr)
generate lgini_disp = ln(gini_disp)
** Temporal trend
generate time = Year
** Energy
generate renewpc = Energy_prod_renew/pop
generate nonrenewpc = Energy_prod_fossil/pop
generate lrenew = ln(Energy_prod_renew)
generate lnonrenew = ln(Energy_prod_fossil)
generate lrenewpc = ln(renewpc)
generate lnonrenewpc = ln(nonrenewpc)
generate lrenew_nrenew = ln(Energy_prod_renew/Energy_prod_fossil)
generate renew_tot = Energy_prod_renew / Energy_prod_tot
generate lrenew_tot = ln(renew_tot)
replace lrenew_tot = 0 if lrenew_tot == .
** European union
generate EU = 1 if Country=="Sweden" | Country=="Finland" | Country=="Denmark" | Country=="Germany" | Country=="Belgium" | Country=="Austria" | Country=="Ireland" | Country=="Spain" | Country=="Italy" | Country=="France" | Country=="Greece" | Country=="Netherlands" | Country=="United Kingdom" | Country=="Portugal"
replace EU = 0 if EU==.
** 2008-2012 Crisis
generate D08 = 1 if Year >=2008 & Year <= 2012
replace D08 = 0 if D08!=1



***** Panel settings *****
xtset CountryID Year, yearly






***** Country subsets *****
** Full panel: 1950-2016
generate subset1 = 1 if Country=="Sweden" | Country=="Norway" | Country=="Finland" | Country=="Denmark" | Country=="Germany" | Country=="Belgium" | Country=="Austria" | Country=="Ireland" | Country=="Spain" | Country=="Italy" | Country=="France" | Country=="Greece" | Country=="Netherlands" | Country=="United Kingdom" | Country=="Portugal" | Country=="Turkey" | Country=="Switzerland"
replace subset1 = 0 if subset1 != 1

** Tabulate
tabstat y x lschool lgini_disp renew_tot open rtfp lavh lrk, statistics(count) by(Country)
tabstat y x lschool lgini_disp renew_tot open rtfp if subset1==1 | Country=="Poland" | Country=="Hungary", statistics( count ) by(Country)
tabstat y x lschool lgini_disp renew_tot open rtfp if subset1==1, statistics( count ) by(Country)

** Clustering (done with R code)
generate cluster1 = "g1" if Country=="Sweden" | Country=="Norway" | Country=="Finland" | Country=="Denmark" | Country=="Germany" | Country=="Belgium" | Country=="Austria" | Country=="France" | Country=="Netherlands" | Country=="Switzerland"
replace cluster1 = "g2" if Country=="Ireland" | Country=="Spain" | Country=="Italy" | Country=="Greece" | Country=="United Kingdom" | Country=="Portugal" | Country=="Turkey"
tabulate cluster1, generate(b)





***** Graphical analysis *****
** Environmental Kuznets Curve
twoway (scatter y x) (qfit y x, lcolor(red) lwidth(vthick)) if subset1 == 1, legend(off) title(Aggregate EKC) subtitle(Full panel 1950-2016) xtitle(log(GDP per capita)) ytitle(log(CO2 per capita))  name(g1)
twoway (scatter y x) if subset1 == 1 & b1==1, title(EKC for European countries) subtitle(High income inequality group 1950-2016) xtitle(log(GDP per capita)) ytitle(log(CO2 per capita)) name(g2)
twoway (scatter y x) if subset1 == 1 & b2==1, title(EKC for European countries) subtitle(Low income inequality group 1950-2016) xtitle(log(GDP per capita)) ytitle(log(CO2 per capita)) name(g3)
graph combine g1 g2 g3
** Educational Kuznets Curve
twoway (scatter y lschool) if subset1 == 1, title(Educational Kuznets Curve) subtitle(Full panel 1950-2016) xtitle(log(GDP per capita)) ytitle(log(CO2 per capita)) name(g4)
twoway (scatter y lschool) if subset1 == 1 & b1==1, title(Educational Kuznets Curve) subtitle(High income inequality group 1950-2016) xtitle(log(GDP per capita)) ytitle(log(CO2 per capita)) name(g5)
twoway (scatter y lschool) if subset1 == 1 & b2==1, title(Educational Kuznets Curve) subtitle(Low income inequality group 1950-2016) xtitle(log(GDP per capita)) ytitle(log(CO2 per capita)) name(g6)
graph combine g4 g5 g6
** EduEKC for low-income inequality countries
twoway (scatter CO2pop yr_sch) (qfit CO2pop yr_sch, lcolor(red) lwidth(thick)) if subset1==1 & b1==1, by(Country) by(, title(Educational Kuznets Curve) subtitle(Low income inequality countries (1950-2015))) ///
xtitle(Years of schooling) ytitle(CO2 per capita (tons)) legend(order(1 "Observed" 2 "Quadratic fit")) name(p1)
twoway (scatter CO2pop rgdppop) (qfit CO2pop rgdppop, lcolor(red) lwidth(thick)) if subset1==1 & b1==1, by(Country) by(, title(Environmental Kuznets Curve) subtitle(Low income inequality countries (1950-2015))) ///
xtitle(GDP per capita (\$)) ytitle(CO2 per capita (tons)) legend(order(1 "Observed" 2 "Quadratic fit")) name(p2)
graph combine p1 p2
graph export "C:\Users\paulm\Desktop\EduEKC_low.png", as(png) name("p1")
graph export "C:\Users\paulm\Desktop\EnvEKC_low.png", as(png) name("p2")
graph export "C:\Users\paulm\Desktop\EduEnvEKC_low.png", as(png) name("Graph")
twoway (scatter CO2pop yr_sch) (qfit CO2pop yr_sch, lcolor(red) lwidth(thick)) if subset1==1 & b2==1, by(Country) by(, title(Educational Kuznets Curve) subtitle(High income inequality countries (1950-2015))) ///
xtitle(Years of schooling) ytitle(CO2 per capita (tons)) legend(order(1 "Observed" 2 "Quadratic fit")) name(p3)
twoway (scatter CO2pop rgdppop) (qfit CO2pop rgdppop, lcolor(red) lwidth(thick)) if subset1==1 & b2==1, by(Country) by(, title(Environmental Kuznets Curve) subtitle(High income inequality countries (1950-2015))) ///
xtitle(GDP per capita (\$)) ytitle(CO2 per capita (tons)) legend(order(1 "Observed" 2 "Quadratic fit")) name(p4)
graph combine p3 p4
graph export "C:\Users\paulm\Desktop\EduEKC_high.png", as(png) name("p3")
graph export "C:\Users\paulm\Desktop\EnvEKC_high.png", as(png) name("p4")
graph export "C:\Users\paulm\Desktop\EduEnvEKC_high.png", as(png) name("Graph")







***** Panel unit-root tests *****
** 1st generation: Im-Pesaran-Shin
xtunitroot ips y if subset1==1, trend
xtunitroot ips d.y if subset1==1, trend
xtunitroot ips x if subset1==1, trend
xtunitroot ips d.x if subset1==1, trend
xtunitroot ips lschool if subset1==1, trend
xtunitroot ips d.lschool if subset1==1, trend
xtunitroot ips renew_tot if subset1==1, trend
xtunitroot ips d.renew_tot if subset1==1, trend
xtunitroot ips lopen if subset1==1, trend
xtunitroot ips d.lopen if subset1==1, trend
xtunitroot ips lopen if subset1==1
xtunitroot ips d.lopen if subset1==1

** 2st generation: Levin-Li-Chu
xtunitroot llc y if subset1==1 & Year>1950, trend
xtunitroot llc d.y if subset1==1 & Year>1951, trend
xtunitroot llc x if subset1==1 & Year>1950, trend
xtunitroot llc d.x if subset1==1 & Year>1951, trend
xtunitroot llc lschool if subset1==1 & Year>1950, trend
xtunitroot llc d.lschool if subset1==1 & Year>1951, trend
xtunitroot llc lrenew_tot if subset1==1 & Year>1950, trend
xtunitroot llc d.lrenew_tot if subset1==1 & Year>1951, trend
xtunitroot llc lopen if subset1==1 & Year>1950, trend
xtunitroot llc d.lopen if subset1==1 & Year>1951, trend

** 2nd generation: CIPS test by Pesaran (2007)
xtcips y if subset1==1 & Year>1950, trend maxlags(5) bglags(5)
xtcips d.y if subset1==1 & Year>1951, trend maxlags(5) bglags(5)
xtcips x if subset1==1 & Year>1950, trend maxlags(5) bglags(5)
xtcips d.x if subset1==1 & Year>1951, trend maxlags(5) bglags(5)
xtcips lschool if subset1==1 & Year>1950, trend maxlags(5) bglags(5)
xtcips d.lschool if subset1==1 & Year>1951, trend maxlags(5) bglags(5)
xtcips lrenew_tot if subset1==1 & Year>1950, trend maxlags(5) bglags(5)
xtcips d.lrenew_tot if subset1==1 & Year>1951, trend maxlags(5) bglags(5)
xtcips lopen if subset1==1 & Year>1950, trend maxlags(5) bglags(5)
xtcips d.lopen if subset1==1 & Year>1951, trend maxlags(5) bglags(5)


***** Cross-sectional dependency tests *****
** Breusch-Pagan LM test for cross-sectional correlation in fixed effects model (large T and small N)
xtreg y x x2 lschool lschool2 lopen renew_tot time if subset1==1, fe
xttest2
** Pesaran test for cross-sectional independence in panel-data models (small T and large N)
xtreg y x x2 lschool lschool2 lopen lrenew_tot time if subset1==1, fe
xtcsd, pesaran
xtreg y x x2 lschool lschool2 lopen lrenew_tot if subset1==1, re
xtcsd, pesaran



***** Cointegration tests *****
** Pedroni
xtpedroni y x lschool lrenew_tot lopen if subset1==1, nopdols
xtpedroni y x lrenew_tot lopen if subset1==1, nopdols
xtpedroni y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, nopdols
xtcointtest pedroni y x D08 lschool lopen lrenew_tot if subset1==1, trend ar(panelspecific)
xtcointtest pedroni y x D08 lschool lopen lrenew_tot if subset1==1, trend ar(same)
* group-mean stats (between-dimension stats)
xtcointtest pedroni y x lschool lopen lrenew_tot if subset1==1, ar(panelspecific) trend demean lags(aic 10)
* panel stats (within-dimension stats)
xtcointtest pedroni y x lschool lopen lrenew_tot if subset1==1, ar(same) trend demean lags(aic 10)

** Westerlund
xtwest y x lschool lrenew_tot lopen if subset1==1, lags(1) constant bootstrap(100)
xtwest y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) constant bootstrap(100)
xtcointtest westerlund y x D08 lschool lopen lrenew_tot if subset1==1, somepanels trend
xtcointtest westerlund y x D08 lschool lopen lrenew_tot if subset1==1, allpanels trend
xtcointtest westerlund y x lschool lopen lrenew_tot if subset1==1, somepanels trend demean
xtcointtest westerlund y x lschool lopen lrenew_tot if subset1==1, allpanels trend demean

** Kao
xtcointtest kao y x lschool lopen lrenew if subset1==1
xtcointtest kao y x lschool lopen lrenew if subset1==1, demean



***** Endogeneity test *****
** Davidson-Mackinnon for panels
xtivreg y (x x2 lschool lschool2 lrenew_tot lopen = l.x l.x2 l.lschool l.lschool2 l.lrenew_tot l.lopen) if subset1==1, fe
dmexogxt x
dmexogxt x2
dmexogxt lschool
dmexogxt lschool2
dmexogxt lopen
dmexogxt lrenew_tot



***** Descriptive tables *****
tabstat CO2pop rgdppop yr_sch renew_tot open if subset1==1, stats(mean) by(Country)
tabstat CO2pop rgdppop yr_sch renew_tot open if subset1==1, stats(mean sd)

***** Correlation by group
corr y x lschool if subset1 == 1 & b1==1
corr y x lschool if subset1 == 1 & b2==1
corr y x lschool lgini_disp if subset1 == 1 & b1==1 
corr y x lschool lgini_disp if subset1 == 1 & b2==1

twoway (scatter x lschool) (lfit x lschool) if subset1==1 & b1==1, title(Years of schooling VS GDP per capita) subtitle(High income inequality group) ///
xtitle(log(Years of schooling)) ytitle(log(Real GDP per capita)) legend(order(1 "Observed" 2 "Linear fit")) name(g10)
twoway (scatter x lschool) (lfit x lschool) if subset1==1 & b2==1, title(Years of schooling VS GDP per capita) subtitle(Low income inequality group) ///
xtitle(log(Years of schooling)) ytitle(log(Real GDP per capita)) legend(order(1 "Observed" 2 "Linear fit")) name(g11)
graph combine g10 g11




***** Panel data analysis *****
** Full panel
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1, fe
estimates store ivfe
scalar TP_ivfe_gdp = exp(- _b[x] / (2*_b[x2]))
scalar TP_ivfe_sch = exp(- _b[lschool] / (2*_b[lschool2]))
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1, re
estimates store ivre
scalar TP_ivre_gdp = exp(- _b[x] / (2*_b[x2]))
scalar TP_ivre_sch = exp(- _b[lschool] / (2*_b[lschool2]))
scalar list TP_ivfe_sch TP_ivre_sch
scalar list TP_ivfe_gdp TP_ivre_gdp

** Hausman test for FE vs RE
hausman ivfe ivre

** High income inequality countries (b2)
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b2==1, fe
estimates store ivfe_high
scalar TP_ivfe_gdp_high = exp(- _b[x] / (2*_b[x2]))
scalar TP_ivfe_sch_high = exp(- _b[lschool] / (2*_b[lschool2]))
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b2==1, re
estimates store ivre_high
scalar TP_ivre_gdp_high = exp(- _b[x] / (2*_b[x2]))
scalar TP_ivre_sch_high = exp(- _b[lschool] / (2*_b[lschool2]))
scalar list TP_ivfe_sch_high TP_ivre_sch_high
scalar list TP_ivfe_gdp_high TP_ivre_gdp_high

** Low income inequality countries (b1)
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b1==1, fe
estimates store ivfe_low
scalar TP_ivfe_gdp_low = exp(- _b[x] / (2*_b[x2]))
scalar TP_ivfe_sch_low = exp(- _b[lschool] / (2*_b[lschool2]))
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b1==1, re
estimates store ivre_low
scalar TP_ivre_gdp_low = exp(- _b[x] / (2*_b[x2]))
scalar TP_ivre_sch_low = exp(- _b[lschool] / (2*_b[lschool2]))
scalar list TP_ivfe_sch_low TP_ivre_sch_low
scalar list TP_ivfe_gdp_low TP_ivre_gdp_low

** Turning points comparison
scalar list TP_ivfe_sch TP_ivre_sch TP_ivfe_sch_high TP_ivre_sch_high TP_ivfe_sch_low TP_ivre_sch_low
scalar list TP_ivfe_gdp TP_ivre_gdp TP_ivfe_gdp_high TP_ivre_gdp_high TP_ivfe_gdp_low TP_ivre_gdp_low

** Estimations
estimates table ivfe ivre, stats(N r2 r2_a) style(oneline) star(0.01 0.05 0.1) b(%06.3fc)
estimates table ivfe_low ivre_low ivfe_high ivre_high, stats(N r2 r2_a) style(oneline) star(0.01 0.05 0.1) b(%06.3fc)



***** Edu KC, Env KC and EduEnv KC for full panel
xtivreg y x x2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1, fe
display exp(-_b[x]/(2*_b[x2]))
estimates store full_gdp
scalar TP_full_gdp = exp(- _b[x] / (2*_b[x2]))
xtivreg y lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1, fe
display exp(-_b[lschool]/(2*_b[lschool2]))
estimates store full_edu
scalar TP_full_edu = exp(- _b[lschool] / (2*_b[lschool2]))
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1, fe
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estimates store full_edu_gdp
estimates table full_gdp full_edu full_edu_gdp, stats(N r2 r2_a) style(oneline) star(0.01 0.05 0.1) b(%06.3fc)


***** Edu KC, Env KC and EduEnv KC for high income-inequality countries
xtivreg y x x2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b2==1 & Year <= 2015, fe
display exp(-_b[x]/(2*_b[x2]))
estimates store high_gdp
scalar TP_high_gdp = exp(- _b[x] / (2*_b[x2]))
xtivreg y lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b2==1 & Year <= 2015, fe
display exp(-_b[lschool]/(2*_b[lschool2]))
estimates store high_edu
scalar TP_high_edu = exp(- _b[lschool] / (2*_b[lschool2]))
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b2==1 & Year <= 2015, fe
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estimates store high_edu_gdp
estimates table high_gdp high_edu high_edu_gdp, stats(N r2 r2_a) style(oneline) star(0.01 0.05 0.1) b(%06.3fc)


***** Edu KC, Env KC and EduEnv KC for low income-inequality countries
xtivreg y x x2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b1==1 & Year <= 2014, fe
display exp(-_b[x]/(2*_b[x2]))
estimates store low_gdp
scalar TP_low_gdp = exp(- _b[x] / (2*_b[x2]))
xtivreg y lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b1==1 & Year <= 2014, fe
display exp(-_b[lschool]/(2*_b[lschool2]))
estimates store low_edu
scalar TP_low_edu = exp(- _b[lschool] / (2*_b[lschool2]))
xtivreg y x x2 lschool lschool2 lopen (lrenew_tot = L.lrenew_tot) if subset1==1 & b1==1 & Year <= 2014, fe
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estimates store low_edu_gdp
estimates table low_gdp low_edu low_edu_gdp, stats(N r2 r2_a) style(oneline) star(0.01 0.05 0.1) b(%06.3fc)

* graph drop g13 g14
generate EduKC_low = 12.947 * lschool - 2.833 * lschool2 - 26.382
generate EKC_low = 7.499*x - 0.349*x2 - 51.886
twoway (scatter y lschool) (scatter EduKC_low lschool) if subset1==1 & b1==1, title(Educational Kuznets Curve) subtitle(Low income inequality countries (1950-2015)) ///
xtitle(log(Years of schooling)) ytitle(log(CO2 per capita)) legend(order(1 "Observed" 2 "Estimated quadratic fit")) name(g13)
twoway (scatter y x) (scatter EKC_low x) if subset1==1 & b1==1, title(Environmental Kuznets Curve) subtitle(Low income inequality countries (1950-2015)) ///
xtitle(log(GDP per capita)) ytitle(log(CO2 per capita)) legend(order(1 "Observed" 2 "Estimated quadratic fit")) name(g14)
graph combine g13 g14

graph drop g13 g14
drop EKC_low EduKC_low
generate EduKC_low = exp(12.947 * lschool - 2.833 * lschool2 - 26.382)*1000000
generate EKC_low = exp(7.499*x - 0.349*x2 - 51.886)*1000000
generate EduKC_low = exp(9.412 * lschool - 1.976 * lschool2 - 16.167)*1000
generate EKC_low = exp(5.383*x - 0.237*x2 - 35.406)*1000
twoway (scatter CO2pop yr_sch) (scatter EduKC_low yr_sch, mcolor(red)) (scatter CO2pop yr_sch if Country == "Germany", mcolor(green)) (scatter CO2pop yr_sch if Country == "France") (scatter CO2pop yr_sch if Country == "Norway", mcolor(yellow)) ///
if subset1==1 & b1==1, title(Educational Kuznets Curve) subtitle(Low income inequality countries (1950-2015)) ///
xtitle(Years of schooling) ytitle(CO2 per capita (tonns)) legend(order(1 "Observed" 2 "Estimated quadratic fit" 3 "Germany" 4 "France" 5 "Norway")) name(g13)
twoway (scatter CO2pop rgdppop) (scatter EKC_low rgdppop, mcolor(red)) (scatter CO2pop rgdppop if Country == "Germany", mcolor(green)) (scatter CO2pop rgdppop if Country == "France") (scatter CO2pop rgdppop if Country == "Norway", mcolor(yellow)) ///
if subset1==1 & b1==1, title(Environmental Kuznets Curve) subtitle(Low income inequality countries (1950-2015)) ///
xtitle(GDP per capita (\$)) ytitle(CO2 per capita (tonns)) legend(order(1 "Observed" 2 "Estimated quadratic fit" 3 "Germany" 4 "France" 5 "Norway")) name(g14)
graph combine g13 g14





***** Dynamic panel data analysis
** EduKC & EnvKC with log(openess) + log(share_renewable)
xtdpdsys y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
** EduKC & EnvKC with log(openess) + log(share_renewable) endogenous
xtabond y x x2 lschool lschool2 lopen if subset1==1, lags(1) endog(lrenew_tot) artests(2)
xtabond y x x2 lschool lschool2 lopen if subset1==1 & b1==1, lags(1) endog(lrenew_tot) artests(2)
xtabond y x x2 lschool lschool2 lopen if subset1==1 & b2==1, lags(1) endog(lrenew_tot) artests(2)
** EduKC & EnvKC with log(openess)
xtabond y x x2 lschool lschool2 lopen if subset1==1, lags(1) artests(2)
xtabond y x x2 lschool lschool2 lopen if subset1==1 & b1==1, lags(1) artests(2)
xtabond y x x2 lschool lschool2 lopen if subset1==1 & b2==1, lags(1) artests(2)
** EduKC & EnvKC with log(openess) + share_renewable
xtabond y x x2 lschool lschool2 lopen renew_tot if subset1==1, lags(1) artests(2)
xtabond y x x2 lschool lschool2 lopen renew_tot if subset1==1 & b1==1, lags(1) artests(2)
xtabond y x x2 lschool lschool2 lopen renew_tot if subset1==1 & b2==1, lags(1) artests(2)
** EduKC & EnvKC with log(openess) + log(GDP) endogenous
xtabond y lschool lschool2 lopen renew_tot if subset1==1, lags(1) endog(x x2) artests(2)
xtabond y lschool lschool2 lopen renew_tot if subset1==1 & b1==1, lags(1) endog(x x2) artests(2)
xtabond y lschool lschool2 lopen renew_tot if subset1==1 & b2==1, lags(1) endog(x x2) artests(2)


***** Panel cointegration methods: pmg estimators
xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) pmg replace technique(bfgs)
xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1 & b1==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) pmg replace technique(bfgs)
xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1 & b2==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) pmg replace technique(bfgs)

xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) dfe replace
xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1 & b1==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) dfe replace
xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1 & b2==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) dfe replace

* xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1, ec(ec_mg) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) mg replace
* xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1 & b1==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) dfe replace
* xtpmg d.y d.x d.x2 d.lschool d.lschool2 d.lopen d.lrenew_tot if subset1==1 & b2==1, ec(ec) lr(l.y x x2 lschool lschool2 lopen lrenew_tot) dfe replace




************************************************************
********** ARELLANO-BOND difference-GMM estimator **********
************************************************************

***** Dynamic panel data analysis with energy exogenous
** Full sample
*
xtabond y lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtabond y x x2 lrenew_tot lopen if subset1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2) vce(robust)
estat abond
** Low income-inequality
xtabond y lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtabond y x x2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y x x2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust)
estat abond
** High income-inequality
xtabond y lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtabond y x x2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond



***** Dynamic panel data analysis with energy endogenous
** Full sample
xtabond y lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot) vce(robust)
estat abond
*
xtabond y x x2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtabond y x x2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot) vce(robust)
estat abond
*
xtabond y x x2 lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y x x2 lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot) vce(robust)
estat abond

** Low income-inequality
xtabond y lschool lschool2 lopen if subset1==1 & b1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y lschool lschool2 lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond
*
xtabond y x x2 lopen if subset1==1 & b1==1, lags(1) artests(2) endog(lrenew_tot) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtabond y x x2 lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond
*
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust)
estat abond

** High income-inequality
xtabond y lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtabond y x x2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond
*
xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtabond y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond



********************************************************
********** ARELLANO-BOND system-GMM estimator **********
********************************************************

***** Dynamic panel data analysis with energy exogenous
** Full sample
xtdpdsys y lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtdpdsys y x x2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtdpdsys y x x2 lrenew_tot lopen if subset1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtdpdsys y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y x x2 lschool lschool2 lrenew_tot lopen if subset1==1, lags(1) artests(2) vce(robust)
estat abond
** Low income-inequality
xtdpdsys y lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtdpdsys y x x2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtdpdsys y x x2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust)
estat abond
*
xtdpdsys y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust)
estat abond
** High income-inequality
xtdpdsys y lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond
*
xtdpdsys y x x2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtdpdsys y x x2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond
*
xtdpdsys y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y x x2 lschool lschool2 lrenew_tot lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust)
estat abond



***** Dynamic panel data analysis with energy endogenous
** Full sample
xtdpdsys y lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot) vce(robust)
estat abond
*
xtdpdsys y x x2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtdpdsys y x x2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot) vce(robust)
estat abond
*
xtdpdsys y x x2 lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y x x2 lschool lschool2 lopen if subset1==1, lags(1) artests(2) endog(lrenew_tot) vce(robust)
estat abond
** Low income-inequality
xtdpdsys y lschool lschool2 lopen if subset1==1 & b1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y lschool lschool2 lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond
*
xtdpdsys y x x2 lopen if subset1==1 & b1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtdpdsys y x x2 lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond
*
xtdpdsys y x x2 lschool lschool2 lopen if subset1==1 & b1==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y x x2 lopen if subset1==1 & b1==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond
** High income-inequality
xtdpdsys y lschool lschool2 lopen if subset1==1 & b2==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y lschool lschool2 lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond
*
xtdpdsys y x x2 lopen if subset1==1 & b2==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
estat sargan
quietly xtdpdsys y x x2 lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond
*
xtdpdsys y x x2 lschool lschool2 lopen if subset1==1 & b2==1, lags(1) artests(2) endog(lrenew_tot)
display exp(-_b[x]/(2*_b[x2]))
display exp(-_b[lschool]/(2*_b[lschool2]))
estat sargan
quietly xtdpdsys y x x2 lschool lschool2 lopen if subset1==1 & b2==1, lags(1) artests(2) vce(robust) endog(lrenew_tot)
estat abond

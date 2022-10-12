/*
 STATA:  EXPORTING RESULTS
 */
 
 
version 17

use "DATA", replace
 
 
// GRAPHS/FIGURES - png2rtf includes PNG graphs in RTF output
histogram msas_phys_num_present_b, percent title("Number of Symptoms Present") 
gr export symp.png, height(325) width(450) replace
png2rtf using "H:\Stats\Exporting Results\Figures.rtf", g(symp.png) title(Figure 1: Number of Symptoms) replace

graph box ejection_fraction_unit_10 bun_units_10 , medtype(marker)
gr export labs.png, height(325) width(450) replace
png2rtf using "H:\Stats\Exporting Results\Figures.rtf", g(labs.png) append

twoway (scatter mean_life_expectancy_10 age_consent_01, sort)
gr export mortality.png, height(325) width(500) replace
png2rtf using "H:\Stats\Exporting Results\Figures.rtf", g(mortality.png) append 




// TABLES - asdoc - An easy way of exporting tables  
// NOTE: fails to append document created above
// NOTE: can be used with survey weights asdoc syv:
global labs ejection_fraction_unit_10 bun_units_10 creatinine_units_10 sodium_units_10 bilirubin_units_10 
global sym  i.pain_present_b i.lack_of_energy_present_b i.lack_of_appetite_present_b i.dry_mouth_present_b i.sob_present_b

// descriptives
asdoc sum $labs, label save(H:\Stats\Exporting Results\Tables.rtf) title(Table 1: Lab Results) replace 
 
// crosstabs 
asdoc tabulate pain_present_b nausea_present_b, nokey column save(H:\Stats\Exporting Results\Tables.rtf) title(Table 2: Pain & Nausea) append
 
// correlations 
asdoc pwcorr $labs, sig save(H:\Stats\Exporting Results\Tables.rtf) title(Table 3: Correlations of Lab Results) append
 
// t-tests
asdoc ttest ejection_fraction_unit_10, by(sex_01) save(H:\Stats\Exporting Results\Tables.rtf) title(Table 4: Lab by Sex) append
 
// regression tables  
asdoc reg mean_life_expectancy_10 $labs, title(Table 5: Regression results for Mortality by Labs) save(H:\Stats\Exporting Results\Tables.rtf) append 
 
// VIF - multicolliinearity 
asdoc vif, label save(H:\Stats\Exporting Results\Tables.rtf) title(Table 6: Multicollinearity of Labs) append	



// TABLE OF REGRESSION RESULTS  
// NOTE: fails to append document created above
eststo clear 
eststo:  glm mean_life_expectancy_10 $labs, fam(gamma) link(log)
eststo:  glm mean_life_expectancy_10 $sym,  fam(gamma) link(log)
eststo:  glm mean_life_expectancy_10 $labs $sym, fam(gamma) link(log)

esttab using "H:\Stats\Exporting Results\GLM Models.rtf", replace label nonumbers  ///
       cells(b(fmt(3)) p(fmt(3) par)) title("Table 7: GLM Models for Life Expectency") ///
       gaps lines collabels(none) mtitles("Model 1 Coef" "Model 2 Coef" "Model 3 Coef" ) ///
       addnotes("GLM: gamma family with log link.")
eststo clear



// TABLE BASED ON SCALERS
// helpful commands:
return list     // list scalers
ereturn list    // list scalers
matlist table_y // preview table during construction


* Create table to compare Males & Female Subjects
gen all    = 1 if !missing(sex_01)
gen male   = 1 if sex_01 == 1
gen female = 1 if sex_01 == 2


mat table_y=J(12,7,.)
local r=1
local c=1

foreach group in all male fem { 	
preserve
keep if `group' == 1

proportion `group'	     
     mat table_y[`r',`c']=  e(N) 
	 local r=`r' + 1 

foreach x in racial_white_01 racial_black_01 racial_asian_01 ethnicity_01 {
    proportion `x'
         mat table_y[`r',`c']   = e(freq)[1,2] 	
         mat table_y[`r',`c'+1] = e(b)[1,2] * 100	 
         local r=`r' + 1  
         } 			 
sum age_consent_01, det 
         mat table_y[`r',`c']  = r(N)  	
         mat table_y[`r',`c'+1]= r(mean) 	
         local r=`r' + 1 
         mat table_y[`r',`c'+1]= r(sd)	
         local r=`r' + 1  	
         mat table_y[`r',`c'+1]= r(p50)
         local r=`r' + 1  	
         mat table_y[`r',`c'+1]= r(p25)
         local r=`r' + 1  	
         mat table_y[`r',`c'+1]= r(p75)
         local r=`r' + 1  
         mat table_y[`r',`c'+1]= r(min)
         local r=`r' + 1  
         mat table_y[`r',`c'+1]= r(max)
         local r=`r' + 2  		   

restore
local r= 1 
local c=`c' + 2 
}
* add column for p values 
local r= 2
foreach x in racial_white_01 racial_black_01 racial_asian_01 ethnicity_01 {
    tab2 sex_01  `x' , chi
         mat table_y[`r',`c'] = r(p)	
         local r=`r' + 1  
    } 			  
ttest age_consent_01, by(sex_01)
      mat table_y[`r',`c'] = r(p) 		
         local r=`r' + 3		
ranksum age_consent_01, by(sex_01)
      mat table_y[`r',`c'] = r(p) 						 

frmttable using "H:\Stats\Exporting Results\Profile table.rtf", replace         ///
   statmat(table_y)                                                             ///
   title("Table 1: Profile by Sex")                                             ///
   rtitle("N"\"White"\"Black"\"Asian"\"Hispanic/Latino"\"Age, mean"\"  SD"\     ///
          "  Median"\"  IRQ 25%"\"  IRQ 75%"\"  Min"\"  Max")                   /// 
   ctitle(" " "Total N" "Col%" "Male N" "Col%" "Female N" "Col%" "p-value")     ///		  
   note("p-value based on chi-square, group t-test, or two-sample Wilcoxon rank-sum (Mann–Whitney) test" ///
        "Race and ethnicity categories are not mutually exclusive")             ///                       
   sdec(0,2,0,2,0,2,3) basefont(arial fs10) varlabels 
   	



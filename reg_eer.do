clear
cd "~/Desktop/jotarepos/voting_eer/"
insheet using "data_to_stata.csv", clear

foreach var of varlist * {
cap replace `var' = "" if `var'=="NA"
}

destring playervote playerlama_star playervote_cond playerchange_vote playerbetteryeslag playertlag playervotelag grouppolicylag playerbiddelta total_yeslag best_respond playertlag playerbiddelta, replace force

encode sessioncode, gen(sn)
g player_unique = sn*100+playerid_in_group

drop if sessioncode=="49mqxl65" & subsessionround_number==9
g held_one = 0 
g held_none = 0

replace held_one = 1 if playertlag==0
replace held_none = 1 if playertlag<0

g bloque= floor(subsessionround_number/8)
g periodo = subsessionround_number-bloque*8
replace periodo = 8 if periodo<1

g positive_change = 0 
replace positive_change = 1 if playerbiddelta>0

g high_treatment = 0 
replace high_treatment=1 if groupuniforme==0

g high_treatment_none = held_none*high_treatment 
g high_treatment_one = held_one*high_treatment 

g cost35 = 0 
replace cost35= 1 if groupcosto==35

g cost35_none = held_none*cost35
g cost35_one = held_one*cost35

g pivo_pass = 0
*replace pivo_pass = 1 if total_yeslag==4
replace pivo_pass = 1 if total_yeslag==5
replace pivo_pass=. if periodo==1

g pivo_pass_none = held_none*pivo_pass 

g pivo_reject = 0 
replace pivo_reject = 1 if total_yeslag==6
*replace pivo_reject = 1 if total_yeslag==7
replace pivo_reject=. if periodo==1

g pivo_reject_none = held_none*pivo_reject 

g trend_none = total_yeslag*held_none
g trend_one = total_yeslag*held_one

g total_yeslag2 = total_yeslag*total_yeslag

g tarde = 0 
replace tarde =1 if mod(marca, 2) == 0

g bfv= playerbid-100

replace bfv=playerbid-80 if groupcosto==20 & grouppolicy==1
replace bfv=playerbid-65 if groupcosto==35 & grouppolicy==1


**Spearman coefficients: assortative discussion***
preserve
keep if groupcosto<60 & mod(marca, 2) == 0
bysort groupuniforme: spearman playerbid playerlama
bysort groupuniforme grouppolicy: spearman playerbid playerlama
bysort groupuniforme grouppolicy: spearman  playert playerlama
restore


***Table 4 in the paper Bid adjustment vs. pivotality***
preserve
keep if groupcosto<60 & mod(marca, 2) == 0 & held_one==0
xtset sn

xtreg playerbiddelta held_none pivo_pass pivo_pass_none, fe
estimates store m1

xtreg playerbiddelta held_none pivo_pass pivo_pass_none if playerbetteryeslag==1 & grouppolicylag==0, fe 
estimates store m2

xtreg playerbiddelta held_none pivo_reject pivo_reject_none if playerbetteryeslag==0 & grouppolicylag==1, fe 
estimates store m3

esttab m1 m2 m3 using "table_4.tex", replace ///
    b(2) p(3) brackets label nostar ///                          // 2 decimals for coefs, 3 for p-values in brackets
    varwidth(25) modelwidth(12) ///
    mgroups("All" "Policy rejected" "Policy approved", pattern(1 1 1) prefix(\multicolumn{1}{c}{) suffix(})) ///
    mtitles("" "$\lambda \geq \lambda^*(2)$" "$\lambda < \lambda^*(2)$") ///
    order(_cons held_none pivo_pass pivo_reject pivo_pass_none pivo_reject_none) /// // Force constant to the top
    coeflabels(_cons "Constant" ///
               held_none "Held None" ///
               pivo_pass "Pivotal" ///
               pivo_reject "Pivotal" ///
               pivo_pass_none "Held None $\times$ Pivotal" ///
               pivo_reject_none "Held None $\times$ Pivotal") ///
    stats(N r2_o, labels("Obs." "$R^2$") fmt(0 2)) ///   // Overall R2 to match your output
    booktabs gap ///                                     // Adds the [1em] vertical layout spacing
restore
*fragments     


g held_none_tarde = held_none*tarde


g alasjustas_lag = pivo_pass+pivo_reject
g alasjustas=0
replace alasjustas=1 if grouppolicy==1 & total_yes==6
replace alasjustas=1 if grouppolicy==0 & total_yes==5

g uniforme_tarde = groupuniforme*tarde
g policy_justas = alasjustas*grouppolicy
g cost35_policy = grouppolicy*cost35
g cost35_policy_justas = grouppolicy*cost35*alasjustas
g cost35_justas = cost35*alasjustas
g justas_tarde = alasjustas*tarde
g policy_tarde= grouppolicy*tarde
g policy_justas_tarde= alasjustas*grouppolicy*tarde
g pivo_pass_tarde= pivo_pass*tarde
g pivo_reject_tarde= pivo_reject*tarde

g justaslag_tarde = alasjustas_lag*tarde


g grandes= 0 
replace grandes=1 if playerlama>.5

g pivo_pass_grande= pivo_pass*grandes
g pivo_reject_grande= pivo_reject*grandes
g grandes_tarde= grandes*tarde
g grandes_justas_tarde= grandes_tarde*alasjustas_lag 
g grandes_justas= grandes*alasjustas_lag 


**TABLE B3
preserve 
keep if groupcosto<60 

reg bfv groupuniforme tarde uniforme_tarde, vce(robust)
estimates store ma1

reg bfv alasjustas_lag tarde justaslag_tarde, vce(robust)
estimates store ma2

reg bfv grandes alasjustas_lag tarde justaslag_tarde grandes_tarde grandes_justas grandes_justas_tarde, vce(robust)
estimates store ma3

esttab ma1 ma2 ma3 using "table_b3.tex", replace ///
    b(2) p(3) brackets label nostar ///                  // 2 decimals for coefs, 3 for p-values in brackets, no stars
    varwidth(35) modelwidth(12) ///
    order(_cons groupuniforme tarde uniforme_tarde alasjustas_lag justaslag_tarde grandes grandes_tarde grandes_justas grandes_justas_tarde) /// // Forces precise vertical row ordering
    coeflabels(_cons "Constant" ///
               groupuniforme "Dispersed" ///
               tarde "Late" ///
               uniforme_tarde "Late $\times$ Dispersed" ///
               alasjustas_lag "Pivotal" ///
               justaslag_tarde "Late $\times$ Pivotal" ///
               grandes "Large $\lambda$" ///
               grandes_tarde "Late $\times$ Large $\lambda$" ///
               grandes_justas "Pivotal $\times$ Large $\lambda$" ///
               grandes_justas_tarde "Late $\times$ Pivotal $\times$ Large $\lambda$") ///
    stats(N r2, labels("Obs." "$R^2$") fmt(0 2)) ///     // Standard R^2 for OLS regression
    booktabs gap                                         // Injects the [1em] structural vertical row spacing
restore





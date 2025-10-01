*** Purpose: Prepare Chp 5 examples
*** Author:  S Bauldry
*** Date:    Oct 1, 2025

*** Load prepared NSDUH data
cd ~/desktop
import delimited isem-nsduh-data.csv, delim(",")


*** Estimates and CIs for empirical example
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth) (mar fem blk hsp -> PD) ///
    (PD mar fem blk hsp -> drk)
estat teffects

sem (Anx -> k6nrv k6rst) (Dep -> k6dep k6hop k6eff k6wth)
estat gof, stats(all)


*** Figure 5.8 contour plot
clear all

set obs 10000
gen obs_id = _n
gen gamma = 1 + mod(obs_id - 1, 100) * (3 - 1) / 99
gen delta = 0.01 + int((obs_id - 1) / 100) * (0.10 - 0.01) / 99
gen B = gamma * delta


twoway (contour B gamma delta, ccuts(0 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.30)), ///
       xtitle("gamma") ytitle("delta")


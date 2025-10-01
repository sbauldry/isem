*** Purpose: Prepare Chp 4 examples
*** Author:  S Bauldry
*** Date:    Oct 1, 2025

*** Load prepared NSDUH data
cd ~/desktop
import delimited isem-nsduh-data.csv, delim(",")


*** Table 4.1
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth)
estat gof, stats(all)

sem (Anx -> k6nrv k6rst) (Dep -> k6dep k6hop k6eff k6wth)
estat gof, stats(all)


*** Table 4.2
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth)
estat eqgof

sem (Anx -> k6nrv k6rst) (Dep -> k6dep k6hop k6eff k6wth)
estat eqgof
	

*** Table 4.3
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth), cov(e.k6nrv*e.k6rst)
estat gof, stats(all)
est sto m1u

sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth)
estat gof, stats(all)
est sto m1r

lrtest m1u m1r

sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth) (mar fem blk hsp -> PD) ///
    (PD mar fem blk hsp -> drk)
estat gof, stats(all)
est sto m2u

sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth) (mar fem blk hsp -> PD) ///
    (PD -> drk)
estat gof, stats(all)
est sto m2r

lrtest m2u m2r


*** Table 4.4
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth)
estat mindices

sem (Anx -> k6nrv k6rst) (Dep -> k6dep k6hop k6eff k6wth)
estat mindices

sem (Anx -> k6nrv k6rst k6eff) (Dep -> k6dep k6hop k6eff k6wth)
estat gof, stats(all)
estat eqgof

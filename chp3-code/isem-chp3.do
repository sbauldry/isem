*** Purpose: Prepare Chp 3 examples
*** Author:  S Bauldry
*** Date:    Jan 27, 2025

*** Load prepared NSDUH data
cd ~/desktop
import delimited isem-chp3-data.csv, delim(",")


*** Table 3.1
*** measurement component of model
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth)
estat eqgof


*** Table 3.2
*** standard MLE
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth) ///
    (fem blk hsp mar -> PD)                     ///
	(PD fem blk hsp mar -> drk)
estat eqgof
estat teffects, compact
	
*** robust MLE
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth) ///
    (fem blk hsp mar -> PD)                     ///
	(PD fem blk hsp mar -> drk), vce(robust)
estat eqgof

*** missing MLE
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth) ///
    (fem blk hsp mar -> PD)                     ///
	(PD fem blk hsp mar -> drk), method(mlmv)
estat eqgof




*** Table 3.3
*** miiv estimator
sem (PD -> k6nrv k6rst k6dep k6hop k6eff k6wth) ///
    (fem blk hsp mar -> PD) (PD fem blk hsp mar -> drk)

ivregress 2sls k6rst (k6nrv = drk k6dep k6hop k6eff k6wth fem blk hsp mar)
ivregress 2sls k6dep (k6nrv = k6dep k6hop k6eff k6wth)

ivregress 2sls k6nrv fem blk hsp mar
ivregress 2sls drk fem blk hsp mar k6nrv


*** MIIV estimator
sem (nice <- Like _cons@0) (Like -> plst like), means(Like)

ivregress 2sls plst (nice = like)
ivregress 2sls like (nice = plst)


sem (lead <- Lead _cons@0) (Lead -> intl infl strg comp) ///
    (nice <- Like _cons@0) (Like -> plst like), means(Lead Like)

ivregress 2sls intl (lead = infl strg comp)
ivregress 2sls infl (lead = intl strg comp)
ivregress 2sls strg (lead = infl intl comp)
ivregress 2sls comp (lead = infl intl strg)

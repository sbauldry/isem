*** Purpose: prepare examples for chapter 6
*** Author:  S Bauldry
*** Date:    Mar 10, 2025

*** read prepared NSDUH data
import delimited ~/desktop/isem-nsduh-data.csv, delim(",")


*** user written command for polychoric correlations
polychoric k6nrv k6hop k6rst k6dep k6eff k6wth
cor k6nrv k6hop k6rst k6dep k6eff k6wth

*** fit measurement models
gsem (PD -> k6nrv k6hop k6rst k6dep k6eff k6wth, ologit)
gsem (Anx -> k6nrv k6hop, ologit) (Dep -> k6dep k6rst k6eff k6wth, ologit)


*** fit structural model
gsem (PD -> k6nrv k6hop k6rst k6dep k6eff k6wth, ologit) ///
     (mar fem blk hsp -> PD) ///
	 (mar fem blk hsp PD -> drk), vce(robust)

gsem (PD -> k6nrv k6hop k6rst k6dep k6eff k6wth, ologit) ///
     (mar fem blk hsp -> PD) ///
	 (mar fem blk hsp PD -> drk, nbreg), vce(robust)


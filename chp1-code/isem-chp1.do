*** Purpose: Prepare chapter 1 descriptives
*** Author:  S Bauldry
*** Date:    Sep 18, 2025

*** load prepared NSDUH data and drop missing cases
cd ~/desktop
import delimited using isem-nsduh-data, clear delim(",")
drop if mi(k6nrv, k6hop, k6rst, k6dep, k6eff, k6wth)

*** Table 1 - descriptive statistics
sum drk mar k6nrv k6rst k6dep k6hop k6eff k6wth fem hsp blk

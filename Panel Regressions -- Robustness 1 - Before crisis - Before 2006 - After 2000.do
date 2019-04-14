global DIR_DATA `"[PATH TO DATA]"'

cd `"${DIR_DATA}"'

global FILE_NAME `"Regression Dataset.dta"'

global DATE: display %tdCCYY-NN-DD date(c(current_date), "DMY")

cap mkdir `"${PATH_OUTPUT_DIR}"'

di `"${OUTFILE}"'

use using `"${FILE_NAME}"',  clear

drop if iso3=="GRC" | iso3 == "ARG" | iso3 == "SAU" | iso3 == "MYS"

gen banks_negative = pchgy_banks_lcu < 0

egen id = group(iso3)
egen t = group(date)

gen year = year(date)

drop if year>=2007 | year <= 1999

global LI_MAPDATA new old
global LI_REGION AE EME all /* "AE | EME" */
global LI_METHOD OLS GMM_D GMM_S
global LI_MEASURE quantity price all
global LI_SS S1 S2 S3 S4
global MAXLAG 2
global IVSTYLE /* ivstyle(i.t, mz equation(level)) */

est clear
xtset id t
foreach x of global LI_MAPDATA {
  foreach y of global LI_REGION {
    foreach z of global LI_MEASURE {
      foreach m of global LI_METHOD {
        foreach s of global LI_SS {
          di `"MaP dataset: `x' || Region: `y' || Measure: `z' || Method: `m' || Spec: ${`s'}"'
          global YVAR pchgy_nonbanks_lcu
          global MaP `"`z'_`x'"'
          global S1  filr bankcrisis c.pchgy_banks_lcu i.banks_negative##c.${MaP}##c.pchgy_banks_lcu
          global S2 l.( filr bankcrisis c.pchgy_banks_lcu i.banks_negative##c.${MaP}##c.pchgy_banks_lcu)
          global S3  filr bankcrisis c.pchgy_banks_lcu c.${MaP}
          global S4 l.( filr bankcrisis c.pchgy_banks_lcu c.${MaP})
          global XVAR ${`s'}
          if "`y'"=="all"{
            local IF (region_imf == "AE") | (region_imf == "EME")
          }
          else {
            local IF (region_imf == "`y'")
          }

          if "`m'" == "OLS" {
            eststo `s'_`m'_`x'_`y'_`z': xtreg l(0/1).${YVAR} (${XVAR}) i.t if `IF', fe vce(cluster id)
          }
          else if "`m'" == "GMM_S" {
            eststo `s'_`m'_`x'_`y'_`z': xtabond2  l(0/1).${YVAR} (${XVAR}) i.t if `IF', gmmstyle(l.(${YVAR}) l.(${XVAR}), laglimits(1 ${MAXLAG})) ${IVSTYLE} mata pca  artests(3) robust
          }
          else if "`m'" == "GMM_D" {
            eststo `s'_`m'_`x'_`y'_`z': xtabond2  l(0/1).${YVAR} (${XVAR}) i.t if `IF', gmmstyle(l.(${YVAR}) l.(${XVAR}), orthog laglimits(1 ${MAXLAG})) ${IVSTYLE} mata pca noleveleq robust /* Difference GMM */
          }
        }
      }
    }
  }
}


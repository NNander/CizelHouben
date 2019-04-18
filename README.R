# README File for all code

# The main code runs from the scripts Event Study--XXXX. First the propensity 
# mstching scripts are runned followed by the Event Study-- Detrending.R script. 
# Last step is the Event Study-- CEGR Methodology.

################################# Codes included and what they do (briefly)

# batch_preparation_events_all_borrower.R	 --> Not called in any function or script, produces l.events farme 
# batch_preparation_events_all.R   --> Not called in any function or script
# batch_preparation_events_new_borrower.R --> Not called in any function or script
# batch_preparation_events_new.R --> Not called in any function or script
# batch_preparation_events_old_borrower.R --> Not called in any function or script
# batch_preparation_events_old.R --> Not called in any function or script
# batch_preparation_yxdata_BISsubset.R --> Not called in any function or script
# batch_preparation_yxdata.R	--> Called in Event Study CEGR methods, seem to produce l.xydata
# cerutti.RData	--> follows form ?
# classifications.R	--> 			
# country_classification.R --> used in all scripts
# data_processor.R  --> used by country_classification.R, MISSING DATA
# detrend.R --> used in detrending scripts
# es_cegr_method.R --> used in Event Study -- CEGR Methodology.R and propensity matching scripts
# es_detrended_method_annual.R ?
# es_detrended_method.R ?
# es_regression_old_v2.R ?
# es_regression_old_v3.R ? 
# es_regression_old.R ? 
# Event Study -- CEGR Methodology.R --> script
# Event Study -- Detrending.R --> script
# Event Study -- Propensity Matching -- Stage 1.R --> script
# Event Study -- Propensity Matching.R --> script
# event_dataset_preparation.R --> used in batch preperation codes
# execute_matching.R --> used in Event Study -- Propensity Matching 
# extract_estimates.R		--> used by es_cegr_method.R
# extract_stats.R			--> used by reporrt.lrm
# get_MaP_events_old.R  ?
# loaders.R --> used in the scripts, loads data that seems to be missing
# prepare_MaP_events_Cerutti2015_March_borrower.R --> Used by batch prepeation files
# prepare_MaP_events_Cerutti2015_March_Robustness-1.R --> not used
# prepare_MaP_events_Cerutti2015_March_Robustness-2.R --> not used
# prepare_MaP_events_Cerutti2015_March.R	 --> not used
# prepare_ydata.oldnew.levels.R --> used by Event Study -- Detrending
# report_felm.R  --> not used
# save_stata.R --> not used
# simulate_events.R --> not used
# stars.R --> used by es_detrended_method
# Table summarizing MaP events.R	--> runs, produces unbused output (graphs)
# utils.R --> unused
# wald_test.R	-->	Used in Event study
# zzz.R --> unused

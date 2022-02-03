# Meta-Analysis

This repository is used to carry out a generalized meta-analysis for any outcome.

This is used for establishing dose-repose `DR` relationship between `Physical-Activity` and disease outcomes/causes. At the moment, we are dealing with:

* All-cause cancer
* All-cause cvd
* All-cause dementia
* All-cause mortality
* Alzheimer's disease
* Bladder cancer
* Breast cancer
* Colon cancer
* Coronary heart disease
* Depression
* Elevated depressive symptoms
* Endometrial cancer
* Esophageal cancer
* Gastric cardia cancer
* Head and neck cancer
* Heart failure
* Kidney cancer
* Liver cancer
* Lung cancer
* Major depression
* Myeloid leukemia
* Myeloma
* Parkinson's disease
* Prostate cancer
* Rectum cancer
* Stroke
* Vascular dementia

The main script which runs the analysis, and saves the outputs (in `CSV` files and figures) is stored at: [script/master_script.R](script/master_script.R)


## Physical activity and risk of depression

We have written a script which reads the output from `meta-analyis` for:

* Depression
* Elevated depressive symptoms
* Major depression

And plots them in [script/plot-depression-outcomes](script/plot-depression-outcomes.R)

## Interactive visualization

We have developed an interactive application to show individual `dose-response` curve for all the outcomes. You may see it here: https://shiny.mrc-epid.cam.ac.uk/meta-analyses-physical-activity/

The code for this app sits in a separate repo at: https://github.com/meta-analyses/meta-analysis-shiny
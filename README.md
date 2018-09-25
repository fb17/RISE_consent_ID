# RISE_consent_ID
RISE consent and house numbering in Indonesia

Household Consent ID – Coding overview

Daily QC
This code was run by Zainal each day and emailed to Jane, Matthew, Daniel, and me.

RMarkdown report: RISE_consent_daiyQC_ID.Rmd
Uses:
•	consentid-dailyqc.R
o	consentid-corrections.R
o	consentid-clean.R

Daily Summary report (supervisor report)
This was intended to match the CFW supervisor’s daily summary sheet. Just a series of tables.
	
RMarkdown report: RISE_consent_summary_ID.Rmd
Uses: 
•	consentid-summary report.R
o	consentid-corrections.R
o	consentid-clean.R
 

Weekly Report – don’t use
This ended up being a sporadic update report to Matthew and leadership. I only used this a bit and it will have errors in it… replaced by final report******

RMarkdown report: RISE_consent_summary_ID.Rmd
Used:
•	consentid-summary.R

Timings
This report was created to help Jane see how much time was being taken in parts of the survey.

RMarkdown report: RISE_consent_timings.Rmd
Uses:
•	consentid-summary report.R
o	consentid-corrections.R
o	consentid-clean.R

Final Report
This was the final report shared with leadership. It evolved from the weekly report.

RMarkdown report: RISE_consent_final_report.Rmd (R Markdown file)
Uses:
•	consentid-summary report.R
o	consentid-corrections.R
o	consentid-clean.R

House numbers to match with Icha’s maps
•	consentid-summary report.R
•	spreadsheet saved as: "S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/3. analysis/RISE_consent_2018_maps.csv")

Consent list for Obj 2a
•	consentid-summary report.R
•	spreadsheet saved as: " S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/4. reports/consent_enviro.csv")

Consent lists for Obj 3 and 4
•	consentid-summary report.R
•	spreadsheet saved as:
o	S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/4. reports/hhd_consent_items.csv"
o	S:/R-MNHS-SPHPM-EPM-IDEpi/Current/RISE/4. Surveys/2.Consent and House ID/2. ID/2. Data/4. reports/child_consent_items.csv"

NOTE: 
1) change path to save consent lists as needed
2) for child consent - change date for age calculation based on start date of child sampling


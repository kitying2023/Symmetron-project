# Identifying Outliers in Randomized Controlled Trials Using ClinicalTrials.gov data

The primary objective of this data challenge is to explore the feasibility of using data visualization tools and statistical methods to identify randomized controlled trials (RCTs) that are outliers in terms of study design and patient baseline characteristics. This study uses data from `ClinicalTrials.gov` and focuses on two sets of RCTs: 

1. PARP (Poly ADP‐Ribose Polymerase) inhibitors for locally advanced or metastatic breast cancer
2. Exercise therapy for treating acute non-specific low back pain

This repo contrains:

1. selecttrial.ipynb contains python code in a Juptyter notebook that allows the user to extract a list of 
  NCT_ID numbers through the AACT API based on search terms.
2. breastcancer and back pain folders contain code for extracting data from AACT using list of NCT_ID numbers
   and creating csv files for individual variables. They also include finalized plots for variables
3. rplot contains R code for producing funnel and forest plots.
4. exampleplots contain examples used in our report.
5. TBC...

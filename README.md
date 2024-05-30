# Project

Objective: Analyse data from total spend data for 2010 – 2023

Provide an overall summary

Identify anomalies

Extract from e-mail:

“In advance of the interview, you will need to analyse data on MPs expenses and prepare a presentation, assuming the audience is an internal audit team who have requested you analyse this data and share your findings.

The audit team has requested you analyse the “Total spend” data from the IPSA website, providing an overall summary as well as drawing their attention to anything of note. You may find this guide to MP’s expenses useful.

You will spend 10 minutes presenting your work at the start of the interview, followed by 5 minutes for questions from the interview panel. You will need to send your slides (or equivalent) and source files (e.g. workbook or code) to GIAARecruitment@giaa.gov.uk by 9am on Thursday 30 May.” Understanding budget discrepancies (A guide to MPs’ staffing and business costs | IPSA (theipsa.org.uk))

There may be discrepancies between the total spent by an MP against their budget, as published in the “Total Spend” data for the financial year, and the total amount spent under the same budget, as calculated from the published individual claims for the MP.

These discrepancies occur for the following reasons:

While the total spend against each budget for each MP represents how much they spent under that budget in each financial year, not all individual claims that make up that total can be published. For example, some security or disability-assistance costs under the Office budget, Accommodation budget, or under “one-off costs”, will be counted in the published total spend, but will not be released on the individual claims data.

Some accruals and prepayments made around the end of a financial year may also have been published as individual entries in the former or previous financial year.

Packages required for code

install.packages(c("dplyr", "readr", "purrr", "janitor", "stringr", "tidyr", "testthat", "tibble", "forecast", "ggplot2", "reshape2", "corrplot", "viridis", "cowplot", "shiny"))

Code steps (number denotes script)

1. Import and construct a list for the years of data and continue until all are read

a. Initialise variables for start of the year and create list

b. Function to contruct url and to input start and end year placeholders

c. While loop constructed to continue indefinitely until it encouters an error (no more data sets left)

i. Inside the while loop, the url is constructed which takes the start and end year as inputs

ii. After data is imported, it creates a label for that year and adds a new column for each year

iii. A message is printed to indicate a successful import

iv. The start and end year of the loops are incremented by 1

2. Data Cleansing

a. Replace symbols in financial variables (£ ,””) and if variable values have \n in it remove all values after and including the \n from the values

b. Convert character variables to double if they contain predominantly numerical values and round numerical values to nearest pound

c. Rename variables –

i. Office maximum budget available = Office budget

ii. Staffing maximum budget available = Staffing budget

iii. Accommodation maximum budget available = Accommodation budget

iv. Travel and subsistence spend = Travel and subsistence (uncapped)

v. Other costs spend = Other costs (uncapped)

vi. Winding-up spend = Wind-up spend

vii. Wind-up maximum budget available = Wind-up budget

viii. Winding-up budget = Wind-up budget

d. Include start-up spend if it doesn’t exist on the datasets

e. Replace missing values with 0’s for these variables

i. Start-up spend

ii. Wind-up spend

iii. Accommodation budget

iv. Accommodation spend

f. Derive variables:

i. Derive ‘Subtotal of office running costs’ for 2022/23 (office spend + staff spend).

ii. Derive ‘Subtotal of other parliamentary costs’ for 2022/23 (Accommodation spend + Travel and subsistence spend)

iii. Derive ‘Overall total spend for this financial year’ for 2022/23 (Office spend + Staffing spend + Accommodation spend + Travel and subsistence (uncapped) + Other costs (uncapped))

g. Derive ‘Office budget’ for year 2011/12 - all values are 0

h. Replace negative values with positive values for variables with total or spend in their names

i. Drop these variables:

i. Drop all variables detailing ‘reason’ for budget

ii. Travel and subsistence maximum budget available

iii. Other costs maximum budget available

j. Clean column names using Janitor package (issue with naming conventions)

k. Bind all years to be in one dataset

l. Replaces instances of missing values for numerical values with 0

m. Final dataset (before data visualisations) named “processed_data” and final list of variables (data set presents them in ‘cleaned’ format):

i. MP’s name

ii. Constituency

iii. Office budget

iv. Office spend

v. Remaining office budget

vi. Staffing budget

vii. Staffing spend

viii. Remaining staffing budget

ix. Subtotal of office running costs

x. Accommodation budget

xi. Accommodation spend

xii. Remaining accommodation budget

xiii. Travel and subsistence (uncapped)

xiv. Other costs (uncapped)

xv. Subtotal of other parliamentary costs

xvi. Overall total spend for this financial year

xvii. pID

xviii. Year

3. Test processed data against original imported data (in case processing caused unexpected changes)

a. Set options to display values with non-scientific notation

b. List variables to be selected

c. Clean column names and remove symbols (£, etc.)

d. Output unprocessed and processed data sets by year and select only specific variables

e. List the years of interest

f. Define the columns to select

g. Function to compare totals and then subtracting totals to find absolute difference

i. List the years

ii. Constructs the name of the two data sets (processed and unprocessed

iii. Checks both exist

iv. Selects the columns for both data sets and combines into a single table using p_id as the unique identifier

v. Rounds values

vi. Calculates absolute different between processed and unprocessed with the same names (except the .x and .y)

vii. Stores the data in a list using the year as the key

viii. Skips years if p_id or a particular year is missing

4. Data visualisations for totals

a. Rename, select, and reorder variables for data visuals

b. Rename year

c. Summarise total spend by year

d. Prepare data for forecasting

e. Fit ARIMA model

f. Forecast for the next 3-years

g. Prepare forecasted data for plotting

h. Combine the past and projected data

i. Calculate percentage change

j. Create bar plot for total data, superimpose line, and place percentage change on top of the bars

k. Similar steps for visuals 2 & 3 but without forecast

l. Again, similar steps for remaining visuals but stacked bar chart for budget vs spend

5. Data visualisations for averages

a. Select relevant columns

b. Summary statistics calculation

c. Convert to long format

d. Create bar plots for each category

e. Create box plots for each category

6. Data visualisations for outliers

a. Select data

b. Convert data to long format

c. Identify outliers (standard IQR method)

d. Create plots

7.

a. Select column

b. Calculate years of service

c. Merge years of service

d. Summarise counts for different combinations

8. Benford’s law analysis

a. Calculate Benford’s law probabilities

b. Filter 0’s

c. Check assumptions

d. Plots for Benford’s law

FINAL LIST OF VARIABLES (after processing code – step 3):

1. MP’s name

2. Constituency

3. Office budget

4. Office spend

5. Remaining office budget

6. Staffing budget

7. Staffing spend

8. Remaining staffing budget

9. Wind-up Spend

10. Remaining wind-up budget

11. Subtotal of office running costs

12. Accommodation budget

13. Accommodation spend

14. Remaining accommodation budget

15. Travel and subsistence (uncapped)

16. Other costs (uncapped)

17. Subtotal of other parliamentary costs

18. Overall total spend for this financial year

19. pID

20. Year

21. Start-up maximum budget available

22. Start-up spend

23. Remaining start up budget

24. Winding up budget

Additional Documents: A guide to MPs’ staffing and business costs | IPSA (theipsa.org.uk) - Further Information related to task (minimum and maximum claim amount, transaction dates, negative values, staffing options, overspending and underspending, budget discrepancies, how claims are checked, thematic reviews, accommodation costs). Annual budgets, costs and claims | IPSA (theipsa.org.uk) - Total Spend Data Source. IPSA – The Scheme of MPs’ Staffing and Business Costs 2024-25 (ctfassets.net) - What they can and can’t claim (page 12 – repayment of money to IPSA

RAP: Quality assurance of code for analysis and research — Quality Assurance of Code for Analysis and Research (best-practice-and-impact.github.io) - Best Practice for RAP.

Shiny orgsurveyr/R at master · ukgovdatascience/orgsurveyr (github.com) - RAP and R Shiny Example

Previous work on MP’s expenses: MP_expenses_main_report.pdf (publishing.service.gov.uk) PSPE-WP9-12.pdf (lse.ac.uk)

Auditing Analytics: About the author | Audit Analytics with R (jonlin.ca) - Analytics for Auditing (standard cleaning data, data visualisation for auditing, anomalies, testing, machine learning) (GitHub - jonlinca/auditanalytics: Source for book Audit Analytics with R)

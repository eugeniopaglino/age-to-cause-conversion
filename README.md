Immigrant Mortality before and during the COVID-19 Pandemic in the
United States
================
Eugenio Paglino and Irma T. Elo

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Immigrant Mortality during the COVID-19 Pandemic

In this project we investigate patterns of mortality among immigrants in
the United States during the first year of the COVID-19 pandemic. We
used death record files for 2017-2020 from the National Center for
Health Statistics (NCHS) under a data user agreement. We classified
deaths by sex, age, bridged race/ethnicity, nativity, and cause of
death. US-born deaths include those that occurred to individuals who
were born and residing in the 50 US states. Foreign-born deaths were
those that occurred to foreign-born individuals who reside in the 50 US
states.

We obtained population counts by single years of age, bridged
race/ethnicity, and sex for 2017-2020 from the CDC WONDER portal. We
used the 2017-2019 American Community Survey (ACS) 1-year files to
estimate the proportion foreign born by 10-year age groups, sex, and
race and ethnicity. We then applied these proportions to the population
counts to estimate foreign-born and US-born populations by 10-year age
groups, sex, and race/ethnicity. Because the 2020 ACS data collection
was affected by the COVID-19 pandemic, we applied the 2019 proportions
to the 2020 population data. Appendix Tables 1a and 1b present the
number of deaths and populations by nativity, race/ethnicity, and sex
for broad age groups in 2017-2019 and 2020.

We included nine exhaustive and mutually exclusive cause-of-death
categories based on the underlying cause of death. These are respiratory
diseases, circulatory diseases, cancers, Alzheimer’s disease and
dementia, diabetes, COVID-19, external causes, and all other causes of
death combined. We chose these cause-of-death categories because
mortality from them has been linked to the COVID-19 pandemic. Because of
large native-born and foreign-born differences in external cause
mortality, we included these as a separate category.

We then calculated age and sex-specific death rates for 10-year age
groups by nativity and race/ethnicity. Age-standardized death rates
(ASDR) were then computed for all causes combined, COVID-19, and all
causes other than COVID-19 at ages 25+, 25-64, and 65+ and by the more
detailed causes of death at ages 25+ for the pre-pandemic period
2017-2019 and for 2020. We pooled three pre-pandemic years to adjust for
year-to-year fluctuations in the death rates. We age-standardized the
rates using the average age-structure of the US population in 2017-2020.

Based on these data, we examined the contribution to COVID-19 and all
other causes of death, other than COVID-19, to the change in all-cause
mortality between 2017-2019 and 2020 by sex, nativity, and
race/ethnicity at ages 25+, 25-64 and 65+. We further decomposed the
cause-of-death contributions by the more detailed cause-of-death
categories to US-born and foreign-born difference in ASDRs at ages 25+
by race/ethnicity and sex in 2017-2019 and 2020 and their contributions
to the change in these differences between 2017-2019 and 2020.

In this repository we stored all the codes needed to reproduce the
figures and tables in the paper as well as supplementary material. Here
is the repository structure.

    ## C:/Users/eugen/Dropbox/Upenn/GitHub/pandemic-immigrant-mortality
    ## ├── data
    ## │   ├── input
    ## │   │   ├── ACS20171Year
    ## │   │   │   ├── ACS20171Year.dat
    ## │   │   │   └── ACS20171Year.xml
    ## │   │   ├── ACS20181Year
    ## │   │   │   ├── ACS20181Year.dat
    ## │   │   │   └── ACS20181Year.xml
    ## │   │   ├── ACS20191Year
    ## │   │   │   ├── ACS20191Year.dat
    ## │   │   │   └── ACS20191Year.xml
    ## │   │   ├── ACS20201Year
    ## │   │   │   ├── ACS2020_PUMS_README.pdf
    ## │   │   │   ├── psam_pusa.csv
    ## │   │   │   ├── psam_pusb.csv
    ## │   │   │   └── pums_estimates_20.csv
    ## │   │   ├── ACSDescStats
    ## │   │   │   ├── ACS17to19.dat
    ## │   │   │   └── ACS17to19.xml
    ## │   │   ├── CDCComorbidities
    ## │   │   │   └── contributingConditions.csv
    ## │   │   ├── CDCPopEstimates
    ## │   │   │   ├── CDCPopEstimates2017-2020.txt
    ## │   │   │   └── CDCPopEstimates2021.txt
    ## │   │   ├── CDCPopEstimatesSingleRace
    ## │   │   │   ├── SingleRacePop2017-2020.txt
    ## │   │   │   └── SingleRacePop2021.txt
    ## │   │   ├── ICD10CMCodes
    ## │   │   │   ├── ICD10CMCodesAll.txt
    ## │   │   │   └── ICD10CMCodesDetailed.txt
    ## │   │   ├── mort2017
    ## │   │   │   ├── 2014 Mortality History File Contents.doc
    ## │   │   │   ├── Mort2005ToPresentAllCntyLayout_NAPHSIS.doc
    ## │   │   │   ├── MULT2017.USPSAllCnty.zip
    ## │   │   │   ├── MULT2017.USPSAllCnty.zip.exe
    ## │   │   │   └── MULT2017USAllCnty.txt
    ## │   │   ├── mort2018
    ## │   │   │   ├── Mort2005ToPresentAllCntyLayout_NAPHSIS.doc
    ## │   │   │   ├── Mort2018PS.AllCnty.txt
    ## │   │   │   ├── MULT2018.USPSAllCnty.zip.exe
    ## │   │   │   └── MULT2018USAllCnty.txt
    ## │   │   ├── mort2019
    ## │   │   │   ├── Mort2005ToPresentAllCntyLayout_NAPHSIS.doc
    ## │   │   │   ├── MULT2019.USPSAllCnty.zip.exe
    ## │   │   │   ├── MULT2019PS.AllCnty.txt
    ## │   │   │   └── MULT2019USAllCnty.txt
    ## │   │   ├── mort2020
    ## │   │   │   ├── Mort_2005ToPresentAllCntyLayout_NAPHSIS.doc
    ## │   │   │   ├── MULT2020.PSAllCnty.txt
    ## │   │   │   ├── MULT2020.USPSAllCnty.zip
    ## │   │   │   ├── MULT2020.USPSAllCnty.zip.exe
    ## │   │   │   └── MULT2020USAllCnty.txt
    ## │   │   ├── mort2021
    ## │   │   │   ├── Mort_2021 Historical File Documentation.pdf
    ## │   │   │   ├── MULT2021PSAllCnty.txt
    ## │   │   │   └── MULT2021USAllCnty.txt
    ## │   │   ├── mortPop
    ## │   │   │   ├── countypop1990to2020_formatted.dta
    ## │   │   │   └── Documentation - mortpop files.docx
    ## │   │   └── utilities
    ## │   │       ├── FIPSmetroregion4cat.csv
    ## │   │       ├── stateBEARegionCrosswalk.rds
    ## │   │       └── states.csv
    ## │   └── output
    ## │       ├── ageStdRates.csv
    ## │       ├── ageStdRatesAbove65.csv
    ## │       ├── ageStdRatesBelow65.csv
    ## │       ├── ageStdRatesSens.csv
    ## │       ├── comorbTableUCD.csv
    ## │       ├── comorbTableUCDComp.csv
    ## │       ├── exampleData.csv
    ## │       ├── finalTable.csv
    ## │       ├── finalTable.feather
    ## │       ├── mortTable.feather
    ## │       ├── popTable.feather
    ## │       ├── proportionsData.feather
    ## │       ├── relativeComorbTable.csv
    ## │       ├── standardPop.csv
    ## │       ├── summaryTableDataFemales.csv
    ## │       └── summaryTableDataMales.csv
    ## ├── figures
    ## │   ├── absDiffTrendsSimplePlot.svg
    ## │   ├── absDiffTrendsSimplePlotSens.svg
    ## │   ├── comorbTableUCDForeign.png
    ## │   ├── comorbTableUCDForeignComp.png
    ## │   ├── comorbTableUCDUS.png
    ## │   ├── comorbTableUCDUSComp.png
    ## │   ├── contributionsTableFemales.png
    ## │   ├── contributionsTableMales.png
    ## │   ├── ICDCodesTable.png
    ## │   ├── popTable.png
    ## │   ├── ratioTrendsPlot.svg
    ## │   ├── ratioTrendsPlotSens.svg
    ## │   ├── ratioTrendsTable.png
    ## │   ├── relativeComorbTableForeign.png
    ## │   ├── relativeComorbTableUS.png
    ## │   ├── relDiffTrendsHBarPlot.svg
    ## │   ├── relDiffTrendsHPlot.svg
    ## │   ├── relDiffTrendsSimpleHPlot.svg
    ## │   ├── relDiffTrendsSimpleHPlotSens.svg
    ## │   ├── sampleTableFemales.png
    ## │   ├── sampleTableMales.png
    ## │   ├── summaryTableFemales.png
    ## │   ├── summaryTableMales.png
    ## │   ├── USBornForeignDiffFemalesColorSafePlot.svg
    ## │   ├── USBornForeignDiffFemalesColorSafePlotSens.svg
    ## │   ├── USBornForeignDiffMalesColorSafePlot.svg
    ## │   └── USBornForeignDiffMalesColorSafePlotSens.svg
    ## ├── R
    ## │   ├── contributionsPlots.Rmd
    ## │   ├── createComorbTable.Rmd
    ## │   ├── createComorbTableComp.Rmd
    ## │   ├── createFinalTable.Rmd
    ## │   ├── createMortTable.Rmd
    ## │   ├── createPopTableCDC.Rmd
    ## │   ├── createPropTable.Rmd
    ## │   ├── mortalityDifferentialsPlots.Rmd
    ## │   ├── mortalityRatiosPlots.Rmd
    ## │   ├── relativeComorbTable.Rmd
    ## │   └── sampleAndSummaryTables.Rmd
    ## ├── README.md
    ## └── README.Rmd

The `data/output` folder contains all the datasets produced in the
project. The most important ones are the `ageStdRates*.csv` files that
store the age standardized crude death rates by sex, race/ethnicity,
nativity, cause of death, and period together with uncertainty measures
for each rate. The structure of these files is described below. The
remaining files are produced in the project as intermediate steps. The
`figures` folder contains all figures and tables of the project.
Finally, the `R` folder contains all the code used to clean and analyse
the data. This repository does not contain the individual level death
records because they are not publicly available.

The table below details the structure of the `ageStdRates*.csv` files.

<table class="table" style="font-size: 12px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
output/ageStdRates
</caption>
<thead>
<tr>
<th style="text-align:left;">
Name
</th>
<th style="text-align:left;">
Class
</th>
<th style="text-align:left;">
Label
</th>
<th style="text-align:left;">
Values
</th>
<th style="text-align:left;">
Missing
</th>
<th style="text-align:left;">
Summary
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
period
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Years or year for which the mortality measure was computed.
</td>
<td style="text-align:left;">
‘2017-2019’ ‘2020’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
sex
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Sex
</td>
<td style="text-align:left;">
‘Female’ ‘Male’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
raceEthnicity
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Race/Ethnicity
</td>
<td style="text-align:left;">
‘Asian’ ‘Black’ ‘Hispanic’ ‘White’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 4
</td>
</tr>
<tr>
<td style="text-align:left;">
foreign
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Whether the mortality measure was computed for the Foreign-Born or the
US-Born population.
</td>
<td style="text-align:left;">
‘Foreign-Born’ ‘Total’ ‘US-Born’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 3
</td>
</tr>
<tr>
<td style="text-align:left;">
majorCauses
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Group of the underlying cause of death reported on the death
certificates.
</td>
<td style="text-align:left;">
‘All other causes’ ‘Alzheimer’s disease and dementia’ ‘Circulatory
diseases’ ‘COVID-19’ ‘Diabetes’ and 4 more
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 9
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Ages over which the mortality measure was computed, 100 is used as an
upper bound.
</td>
<td style="text-align:left;">
‘25-100’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 1
</td>
</tr>
<tr>
<td style="text-align:left;">
deaths
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Number of deaths.
</td>
<td style="text-align:left;">
Num: 0 to 3275911
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 106751.167, sd: 344136.165, nuniq: 407
</td>
</tr>
<tr>
<td style="text-align:left;">
pop
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Mid-period population.
</td>
<td style="text-align:left;">
Num: 1416388.584 to 224932209
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 37059951.417, sd: 56394352.809, nuniq: 48
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: 0 to 2419.774
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 268.98, sd: 383.012, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
logASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Logarithm of the Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: 3.071 to 7.791
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 5.056, sd: 1.044, nuniq: 408
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR.SD
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Standard deviation of the ASDR computed using the formula in Chiang
(1984) “The Life Table and Its Application” (pp. 105-106).
</td>
<td style="text-align:left;">
Num: 0 to 6.456
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 0.811, sd: 0.825, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
coefOfVar
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Coefficient of variation for the ASDR computed as ASDR.SD/ASDR.
</td>
<td style="text-align:left;">
Num: 0.027 to 3.753
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 0.575, sd: 0.558, nuniq: 408
</td>
</tr>
</tbody>
</table>
<table class="table" style="font-size: 12px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
output/ageStdRatesAbove65
</caption>
<thead>
<tr>
<th style="text-align:left;">
Name
</th>
<th style="text-align:left;">
Class
</th>
<th style="text-align:left;">
Label
</th>
<th style="text-align:left;">
Values
</th>
<th style="text-align:left;">
Missing
</th>
<th style="text-align:left;">
Summary
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
period
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Years or year for which the mortality measure was computed.
</td>
<td style="text-align:left;">
‘2017-2019’ ‘2020’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
sex
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Sex
</td>
<td style="text-align:left;">
‘Female’ ‘Male’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
raceEthnicity
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Race/Ethnicity
</td>
<td style="text-align:left;">
‘Asian’ ‘Black’ ‘Hispanic’ ‘White’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 4
</td>
</tr>
<tr>
<td style="text-align:left;">
foreign
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Whether the mortality measure was computed for the Foreign-Born or the
US-Born population.
</td>
<td style="text-align:left;">
‘Foreign-Born’ ‘Total’ ‘US-Born’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 3
</td>
</tr>
<tr>
<td style="text-align:left;">
majorCauses
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Group of the underlying cause of death reported on the death
certificates.
</td>
<td style="text-align:left;">
‘All other causes’ ‘Alzheimer’s disease and dementia’ ‘Circulatory
diseases’ ‘COVID-19’ ‘Diabetes’ and 4 more
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 9
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Ages over which the mortality measure was computed, 100 is used as an
upper bound.
</td>
<td style="text-align:left;">
‘65-100’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 1
</td>
</tr>
<tr>
<td style="text-align:left;">
deaths
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Number of deaths.
</td>
<td style="text-align:left;">
Num: 0 to 2670630
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 80830.824, sd: 272034.284, nuniq: 405
</td>
</tr>
<tr>
<td style="text-align:left;">
pop
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Mid-period population.
</td>
<td style="text-align:left;">
Num: 158719.795 to 66442571
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 8821142.375, sd: 16086779.765, nuniq: 48
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: 0 to 6786.988
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 853.186, sd: 1194.322, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
logASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Logarithm of the Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: 3.774 to 8.823
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 6.172, sd: 1.112, nuniq: 408
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR.SD
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Standard deviation of the ASDR computed using the formula in Chiang
(1984) “The Life Table and Its Application” (pp. 105-106).
</td>
<td style="text-align:left;">
Num: 0 to 5.182
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 0.677, sd: 0.697, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
coefOfVar
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Coefficient of variation for the ASDR computed as ASDR.SD/ASDR.
</td>
<td style="text-align:left;">
Num: 0.006 to 1.094
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 0.162, sd: 0.17, nuniq: 408
</td>
</tr>
</tbody>
</table>
<table class="table" style="font-size: 12px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
output/ageStdRatesBelow65
</caption>
<thead>
<tr>
<th style="text-align:left;">
Name
</th>
<th style="text-align:left;">
Class
</th>
<th style="text-align:left;">
Label
</th>
<th style="text-align:left;">
Values
</th>
<th style="text-align:left;">
Missing
</th>
<th style="text-align:left;">
Summary
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
period
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Years or year for which the mortality measure was computed.
</td>
<td style="text-align:left;">
‘2017-2019’ ‘2020’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
sex
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Sex
</td>
<td style="text-align:left;">
‘Female’ ‘Male’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
raceEthnicity
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Race/Ethnicity
</td>
<td style="text-align:left;">
‘Asian’ ‘Black’ ‘Hispanic’ ‘White’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 4
</td>
</tr>
<tr>
<td style="text-align:left;">
foreign
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Whether the mortality measure was computed for the Foreign-Born or the
US-Born population.
</td>
<td style="text-align:left;">
‘Foreign-Born’ ‘Total’ ‘US-Born’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 3
</td>
</tr>
<tr>
<td style="text-align:left;">
majorCauses
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Group of the underlying cause of death reported on the death
certificates.
</td>
<td style="text-align:left;">
‘All other causes’ ‘Alzheimer’s disease and dementia’ ‘Circulatory
diseases’ ‘COVID-19’ ‘Diabetes’ and 4 more
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 9
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Ages over which the mortality measure was computed, 100 is used as an
upper bound.
</td>
<td style="text-align:left;">
‘25-65’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 1
</td>
</tr>
<tr>
<td style="text-align:left;">
deaths
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Number of deaths.
</td>
<td style="text-align:left;">
Num: 0 to 850264
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 25920.343, sd: 77380.811, nuniq: 402
</td>
</tr>
<tr>
<td style="text-align:left;">
pop
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Mid-period population.
</td>
<td style="text-align:left;">
Num: 1225192.498 to 158489638
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 28238809.042, sd: 40529995.045, nuniq: 48
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: 0 to 1055.559
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 86.487, sd: 136.983, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
logASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Logarithm of the Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: -0.434 to 6.962
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 3.57, sd: 1.582, nuniq: 408
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR.SD
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Standard deviation of the ASDR computed using the formula in Chiang
(1984) “The Life Table and Its Application” (pp. 105-106).
</td>
<td style="text-align:left;">
Num: 0 to 3.856
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 0.426, sd: 0.46, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
coefOfVar
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Coefficient of variation for the ASDR computed as ASDR.SD/ASDR.
</td>
<td style="text-align:left;">
Num: 0.055 to 17.436
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 1.505, sd: 2.128, nuniq: 408
</td>
</tr>
</tbody>
</table>
<table class="table" style="font-size: 12px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
output/ageStdRatesSens
</caption>
<thead>
<tr>
<th style="text-align:left;">
Name
</th>
<th style="text-align:left;">
Class
</th>
<th style="text-align:left;">
Label
</th>
<th style="text-align:left;">
Values
</th>
<th style="text-align:left;">
Missing
</th>
<th style="text-align:left;">
Summary
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
period
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Years or year for which the mortality measure was computed.
</td>
<td style="text-align:left;">
‘2017-2019’ ‘2020’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
sex
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Sex
</td>
<td style="text-align:left;">
‘Female’ ‘Male’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 2
</td>
</tr>
<tr>
<td style="text-align:left;">
raceEthnicity
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Race/Ethnicity
</td>
<td style="text-align:left;">
‘Asian’ ‘Black’ ‘Hispanic’ ‘White’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 4
</td>
</tr>
<tr>
<td style="text-align:left;">
foreign
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Whether the mortality measure was computed for the Foreign-Born or the
US-Born population.
</td>
<td style="text-align:left;">
‘Foreign-Born’ ‘Total’ ‘US-Born’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 3
</td>
</tr>
<tr>
<td style="text-align:left;">
majorCauses
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Group of the underlying cause of death reported on the death
certificates.
</td>
<td style="text-align:left;">
‘All other causes’ ‘Alzheimer’s disease and dementia’ ‘Circulatory
diseases’ ‘COVID-19’ ‘Diabetes’ and 4 more
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 9
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
Ages over which the mortality measure was computed, 100 is used as an
upper bound.
</td>
<td style="text-align:left;">
‘25-100’
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
nuniq: 1
</td>
</tr>
<tr>
<td style="text-align:left;">
deaths
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Number of deaths.
</td>
<td style="text-align:left;">
Num: 0 to 3275911
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 106751.167, sd: 344136.165, nuniq: 407
</td>
</tr>
<tr>
<td style="text-align:left;">
pop
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Mid-period population.
</td>
<td style="text-align:left;">
Num: 1402366.041 to 224932209
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 36992066.792, sd: 56413881.045, nuniq: 48
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: 0 to 2487.108
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 273.558, sd: 389.976, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
logASDR
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Logarithm of the Age-Standardized Crude Death Rate.
</td>
<td style="text-align:left;">
Num: 3.076 to 7.819
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 5.073, sd: 1.044, nuniq: 408
</td>
</tr>
<tr>
<td style="text-align:left;">
ASDR.SD
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Standard deviation of the ASDR computed using the formula in Chiang
(1984) “The Life Table and Its Application” (pp. 105-106).
</td>
<td style="text-align:left;">
Num: 0 to 6.675
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
mean: 0.828, sd: 0.849, nuniq: 409
</td>
</tr>
<tr>
<td style="text-align:left;">
coefOfVar
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Coefficient of variation for the ASDR computed as ASDR.SD/ASDR.
</td>
<td style="text-align:left;">
Num: 0.027 to 3.741
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
mean: 0.575, sd: 0.557, nuniq: 408
</td>
</tr>
</tbody>
</table>

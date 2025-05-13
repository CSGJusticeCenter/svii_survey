# SVII Survey (2024)

## Project Overview 

In 2018, The Council of State Governments (CSG) Justice Center sent out an annual survey to corrections departments in all 50 states to collect data on the impact of supervision violations on prison admissions and populations. The study found that in 2017, probation and parole violations made up as much as 45 percent of state prison admissions nationwide, with wide variation across states. With continued partnership with the Correctional Leaders Association and support from Arnold Ventures, the CSG Justice Center will be collecting and reporting these numbers annually through 2025. 

In September 2022, the CSG Justice Center sent the [Supervision Violations and Their Impact on Incarceration](https://projects.csgjusticecenter.org/supervision-violations-impact-on-incarceration/methodology/) survey (created in Google Sheets) to the departments of corrections of all 50 states. 

Respondents were asked to provide aggregate counts of the number of people admitted to or in prison on a given day across numerous categories for calendar years 2018-2023. Respondents were given the option to provide the data according to fiscal year if that is the standard reporting method for the state and indicate if so.  


The categories requested include: 

* *Total admissions/population*: The number of people admitted to or in prison.  
* *Total violation admissions/population*: The number of people admitted to or in prison because of a violation of supervision conditions.  
* *Total probation violation admissions/population*: The number of people admitted to or in prison because of a violation of probation. This includes people admitted to or in prison because of new offense violations and technical violations of probation.  
* *New offense probation violation admissions/population*: The number of people admitted to or in prison because of a new offense violation of probation.  
* *Technical probation violation admissions/population*: The number of people admitted to or in prison because of a technical violation of probation.  
* *Total parole violation admissions/population*: The number of people admitted to or in prison because of a violation of probation. This includes people admitted to or in prison because of new offense violations and technical violations of parole.  
* *New offense parole violation admissions/population*: The number of people admitted to or in prison because of a new offense violation of parole.  
* *Technical parole violation admissions/population*: The number of people admitted to or in prison because of a technical violation of parole.  
* *Cost*: Average operating cost per person per day across all prison facilities.   

Where available, data provided in the survey was supplemented with publicly available data or data that we routinely collect for other purposes. We note on each [state’s page](https://projects.csgjusticecenter.org/supervision-violations-impact-on-incarceration/report/#statedashboard) when we used publicly available data.  

Additionally, in the most recent survey conducting, respondents were asked to provide demographic data on the categories listed above.  

### Caution on Comparing States: 

States define admissions and populations due to supervision violations differently:   

* Some states reported that they include quick dips and supervision sanctions in their admissions numbers, while others do not.  
* Some states reported that they classify new felony offenses as technical violations, while others do not.  
* States do not necessarily have the infrastructure or institutional memory to pull data in a consistent way over time.  
* When provided the opportunity, states revised data that they previously submitted in past years.  
* States may continue to revise their survey responses during future validation processes.  

For more information about [Supervision Violations and Their Impact on Incarceration](https://projects.csgjusticecenter.org/supervision-violations-impact-on-incarceration/), visit our website or [OSF project registration](https://osf.io/f74d6/).   

## Repository 

This repository contains code that does: 

1) Formats data for imputation.
2) Produces national estimates and state-by-state costs



### Repository Structure 

```
svii_survey
└── code
    └── survey
        ├── 03_clean.R       # Format data like 2022 survey data, replace data with BJS numbers
        ├── execute_report.R # create final results for national report - executes 05_multiple_imputation.R
        └── 05_multiple_imputation.R 
            ^Sources the following programs in this order: 03_clean.R, MImp1.R, MImp3.R, MImp3.R, Costs.R
```


### Instructions

In order to successfully produce national estimates and state-by-state costs, survey programs should be run in the following order: 

1) execute_report.R

    - execute_report.R accepts parameters (set to a default for the current survey year) as well as for setting output file name and format.




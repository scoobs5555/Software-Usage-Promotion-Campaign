
# Software Usage Promotion Campaign Uplift Modeling <!-- omit from toc -->


- [Project Overview](#project-overview)
- [Installation and Setup](#installation-and-setup)
  - [Codes and Resources Used](#codes-and-resources-used)
  - [Python Packages Used](#python-packages-used)
- [Data](#data)
  - [Source Data](#source-data)
  - [Data Acquisition](#data-acquisition)
  - [Data Preprocessing](#data-preprocessing)
- [Code structure](#code-structure)
- [Results and evaluation](#results-and-evaluation)
    - [Treatment 1 - Tech Support](#treatment-1---tech-support)
    - [Treatment 2 - Discount](#treatment-2---discount)
    - [Treatment 3 - Tech Support and Discount](#treatment-3---tech-support-and-discount)
- [Future work](#future-work)
- [Acknowledgments/References](#acknowledgmentsreferences)
- [License](#license)
# Project Overview

A startup that sells software would like to know whether its multiple outreach efforts were successful in attracting new customers or boosting consumption among existing customers. They would also like to distinguish the effects of several incentives on different kinds of customers. In other words, they would like to learn the heterogeneous treatment effect of each investment on customers' software usage.

# Installation and Setup


## Codes and Resources Used

- **Editor Used:**  Visual Studio Code V1.92.2
- **Python Version:** 3.11.2
- **R Version:** 4.2.3

## Python Packages Used

- **General Purpose:** 
- **Data Manipulation:** `Pandas`, `Numpy`
- **Data Visualization:** `Seaborn`, `Matplotlib`
- **Machine Learning:** 


# Data



## Source Data
The data for this project was downloaded from https://www.kaggle.com/datasets/hwwang98/software-usage-promotion-campaign-uplift-model/data

The data contains ~2,000 customers and is comprised of:

**Customer features:** details about the industry, size, revenue, and technology profile of each customer.
**Interventions:** information about which incentive was given to a customer.
**Outcome:** the amount of product the customer bought in the year after the incentives were given.

## Data Acquisition
N/A

## Data Preprocessing
Exploratory data analysis and pre-processing was done with python in "Software Usage Promotion Campaign.ipynb"

# Code structure
An updated csv file ("multi_attribution_update") was exported and analyzed with R in ("software usage causal analysis.Rmd)



```bash
├── data
│   ├── multi_attribution_sample.csv
│   ├── multi_attribution_update.csv
│   ├── sofware usage causal analysis.Rmd
├   ├── Software Usage Promotion Campaign.ipynb
├   ├── software-usage-causal-analysis.html
├   ├── sofware-usage-causal analysis.md
├   ├── sofware-usage-causal-analysis_files
        ├──figure-gfm
│           ├── unnamed-chunk-10-1.png
│           ├── unnamed-chunk-10-2.png
            ├── unnamed-chunk-10-3.png
            ├── unnamed-chunk-13-1.png
            ├── unnamed-chunk-13-2.png
            ├── unnamed-chunk-13-3.png
            ├── unnamed-chunk-14-1.png
            ├── unnamed-chunk-14-2.png
            ├── unnamed-chunk-14-3.png
            ├── unnamed-chunk-15-1.png
            ├── unnamed-chunk-15-2.png
            ├── unnamed-chunk-15-3.png
    ├── README.md
    └── .gitignore
```

# Results and evaluation
### Treatment 1 - Tech Support
Our model predicts that when tech support is provided, an increase in revenue of approximately $7,143 is expected with a standard error of about $208.

While holding all other variables constant, companies that have global offices create additional revenue of approximately $3,801.  Similarly, companies that are large consumers in their industry (major) and companies that are classified as "commercial" create additional revenue of approximately $1,485 and $1,252 respectively.

The number of computers a company has can also be used to predict additional revenue.  Every computer a company has can potentially generate an additional $47 in revenue. 

### Treatment 2 - Discount
Our model predicts that when a discount is provided, an increase in revenue of approximately $5,692 is expected with a standard error of about $250.

While holding all other variables constant, companies that have global offices create addition revenue of approximately $4,328.  Similarly, companies that are large consumers in their industry (major) and companies that are classified as "commercial" create additional revenue of approximately $2,547 and $585 respectively.

The number of computers a company has can also be used to predict additional revenue.  Every computer a company has can potentially generate an additional $49 in revenue. 

### Treatment 3 - Tech Support and Discount
Our model predicts that when tech support AND a discount is provided, an increase in revenue of approximately $8,485 is expected with a standard error of about $230.

While holding all other variables constant, companies that have global offices create addition revenue of approximately $4,108.  Similarly, companies that are large consumers in their industry (major) and companies that are classified as "commercial" create additional revenue of approximately $2,135 and $851 respectively.

The number of computers a company has can also be used to predict additional revenue.  Every computer a company has can potentially generate an additional $47 in revenue. 

While all 3 treatments are likely to created additional revenue, combining both tech support and a discount will likely create more.  However, the benefit of providing a discount on top of tech support is only around $1,342 and so the costs of providing tech support and a discount should be taken into account to ensure that revenue is being maximized.

The company's profile should also be taken into account as companies with global offices, who are large consumers in their industry, who are classified as "commercial" and have a large number of computers will also affect their revenue in a positive way.

# Future work
The costs of providing tech support and the amount of discount provided are not known.  These costs should be further investigated to ensure that revenue is being maximized. 

# Acknowledgments/References
--

# License

For this github repository, the License used is [MIT License](https://opensource.org/license/mit/).

Datasource - Haowen Wang. (2022). Software Usage Promotion Campaign Uplift Modeling [Data set]. Kaggle. https://doi.org/10.34740/KAGGLE/DS/2020436



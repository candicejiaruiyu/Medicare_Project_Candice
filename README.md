# Medicare_Project_Candice

## Abstract of the project:
Medicare Advantage plans offer a wide range of benefits ranging from financial (e.g., deductible) to ancillary (e.g., meals, transports). We use enrollment, performance, and benefits data for Connecticut 2019-2022 to identify the most relevant benefits that drive membership. We apply the Non-Negative Matrix Factorization algorithm to reduce dimensionality and summarize the benefit features present in the extracted data while obtaining a set of indices that summarize the structure of benefits. Then, we employ a linear mixed model and linear projection to quantify the correlation between extracted features and plan enrollment. We identify the correlation among the market share and inpatient copay days, vision exams, remote access technologies, health education, etc. The model also shows that brand has a statistically significant effect and indirect impact of star ratings on membership.


**Datasets_Collection_201909.R:** The code used for collect and manipulate the data. It includes row reduction, column break down, normalization, NA value manipulation and one-hot encoding. 
**Exploratory_Analysis_Enrollment.Rmd:** The code used to do the descriptive analysis for enrollment data. 
**NMF_Visualization.Rmd:** The code used to extract the data from NMF results. It also include interactive stacked bar chart using plotly. 


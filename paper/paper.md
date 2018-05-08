---
title: 'rdhs: an R package to interact with The Demographic and Health Surveys (DHS) Program data sets'
tags:
  - R
  - DHS
  - API
  - survey analysis
authors:
  - name: Oliver J Watson
    orcid: 0000-0003-2374-0741
    affiliation: "1"
  - name: Jeffrey W Eaton
    orcid: 0000-0001-7728-728X
    affiliation: "1"
affiliations:
  - name: 1.	MRC Centre for Global Infectious Disease Analysis, Department of Infectious Disease Epidemiology, Imperial College London
    index: 1
date: 8th May 2018
bibliography: paper.bib
---

# Summary

The Demographic and Health Surveys (DHS) Program has collected and disseminated population survey data from over 90 countries for over 30 years. In many countries, DHS provide the key data that mark progress towards targets such as the Sustainable Development Goals (SDGs) and inform health policy such as detailing trends in child mortality [@Silva2012] and characterising the distribution of malaria control interventions in Africa in order to map the burden of malaria since the year 2000 [@Bhatt2015]. Though standard health indicators are routinely published in survey final reports, much of the value of DHS is derived from the ability to download and analyse standardized microdata datasets for subgroup analysis, pooled multi-country analysis, and extended research studies. 

The analysis of the microdata datasets, however, requires a 'clean' dataset that contains all the desired information. One of the main challenges when interacting with the raw DHS datasets is isolating the required dataset variables across different countries. Since the DHS Program started, there have been 7 'phases' of questionnaires used between 1984 - 2018. The data from each phase then recoded to consistency and comparability across surveys. However, new questions are often included or ammended between different phases of the DHS program, which results in variable names sometimes changing between different phases. As well as this, there are a number of country specific records that are not part of model questionnaires. As such, it can become increasingly difficult to identify which variables to use within your final 'clean' dataset. 

The rdhs package was designed to facilitate the management and processing of DHS survey data. This occurs through both functioning as an API client, allowing access to all data provided within the DHS API, and helping to download the raw datasets from the DHS website and read them into conventional R data structures. In overview, the package provides a suite of tools for the following:

 + 1. Access standard survey indicators through the DHS Program API. This data includes the same summarised health indicators that is available through the DHS web application STATcompiler, as well as additional data endpoints that provide metadata for the conducted surveys and raw datasets.  
 + 2. Identify all survey datasets that include a particular topic or indicator relevant to a particular analysis. 
 + 3. Directly download survey datasets from the DHS website. 
 + 4. Load datasets and data dictionaries into R.
 + 5. Extract variables and pool harmonized datasets for multi-survey analysis. 

The functionality provided represents the output of conversations with numerous research groups globally, and serves to simplify commonly required analytical pipelines. The end result aims to increase the end user accessibility to the raw data and create a tool that supports reproducible global health research. Furthermore, the package is hoped to enable researches in lower middle income countries, which constitute the majority of countries that are surveyed as part of the DHS program, to analyse their data without the need for proprietary software. 


# References

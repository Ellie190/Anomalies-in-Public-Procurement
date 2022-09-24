# Anomalies-in-Public-Procurement
Data Mining to Identify Anomalies in Public Procurement Rating Parameters

## Project Overview
- Performed Descriptive Analysis on Public Procument Data
- Built an anomaly detection model using the `anomalize` package to detect anomalies in time-series data
- Built a R shiny dashboard with two main functionalities: (1) Descriptive Analysis and (2) anomaly detection on public procurement data

## Application Access
 https://ds-analytics.shinyapps.io/Anomalies-in-Public-Procurement/

## Context 
**Public Procument**
- Public procurement refers to the purchase by governments and state-owned enterprises of goods, services and works.
- The public procurement process is the sequence of activities starting with the assessment of needs through awards to contract management and final payment. <br>

**Bids and Tender**
- Bidding is an offer to set a price tag by an individual or business for a product or service or a demand that something be done. <br>
- A business tender is an offer to do work or supply goods at a fixed price. 
- The tender or bid process is designed to ensure that the work to be done is given out in a fair way

**Anomaly Detection**
- Anomaly detection is the identification of rare items, events or observations which deviate significantly from the majority of the data and do not conform to a well defined notion of normal behaviour. 
- Such examples may arouse suspicions of being generated by a different mechanism, or appear inconsistent with the remainder of that set of data. <br>

**Why Anomaly Detection**
- To detect suspicious activities within the public procument stage that deviate from the usual activities 
- To identify possible/potential fraudulent activities in public procument or in the rewarding of tenders for government goods. 
- For fraud detection in tenders awarding and public procurement stages <br>

**More Info on Tender Fraud:** https://www.purchasing-procurement-center.com/tender-fraud.html

## Resources 
**RStudio Version:** 2022.07.1 Build 554 <br>
**Libraries:** `tidyverse`, `plotly`, `highcharter`, `lubridate`, `xts`, `DT`, `anomalize`, `tibbletime`, `shiny`, `bs4Dash`, `shinycssloaders`, `waiter` <br>
**Public Procurement Dataset:** https://data.world/city-of-ny/9k82-ys7w <br>
**Dataset Info:** https://data.cityofnewyork.us/City-Government/Bid-Tabulations/9k82-ys7w

## Dashboard Snippets
![Image 1](https://github.com/Ellie190/Anomalies-in-Public-Procurement/blob/main/img/img1.png) <br><br>
![Image 2](https://github.com/Ellie190/Anomalies-in-Public-Procurement/blob/main/img/img2.png) <br><br>
![Image 3](https://github.com/Ellie190/Anomalies-in-Public-Procurement/blob/main/img/img3.png) <br><br>
![Image 4](https://github.com/Ellie190/Anomalies-in-Public-Procurement/blob/main/img/img4.png) <br><br>
![Image 5](https://github.com/Ellie190/Anomalies-in-Public-Procurement/blob/main/img/img5.png) <br><br>

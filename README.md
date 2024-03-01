
<!-- README.md is generated from README.Rmd. Please edit that file -->

# COVID-19 Epidemiology for Malaysia

#### Department of Social and Preventive Medicine, Faculty of Medicine, University Malaya

<!-- badges: start -->
<!-- badges: end -->

The COVID-19 pandemic has propagated the globe over with uneering ease.
It has been no different in Malaysia. This platform aims to track and
leverage on important parameters of disease propagation and control
across Malaysia in suplementing the decision-making process within the
country.

The COVID-19 Epidemiology in Malaysia platform collates data within
Malaysia from several different sources and publishes these finding to a
dashboard.

## The Dashboard

Dashboard is available on
<https://spm.um.edu.my/knowledge-centre/covid19-epid-live/>

## The Data

Data avaialable and utilised within the platform, include:

| Dataset                                             | Download                                                                                              | Updated           | Source                                                                                             | Codebook                                                                                                         |
|-----------------------------------------------------|-------------------------------------------------------------------------------------------------------|-------------------|----------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| COVID-19 transmission indicators in Malaysia        | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/transmission.csv)              | Daily             | [Ministry of Health, Malaysia GitHub](https://github.com/MoH-Malaysia/covid19-public)              | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_dynamic.csv)            |
| Cluster of COVID-19 cases in Malaysia               | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/cluster.csv)                   | Daily             | [Ministry of Health, Malaysia GitHub](https://github.com/MoH-Malaysia/covid19-public)              | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_dynamic.csv)            |
| Mobility in Malaysia                                | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/mobility.csv)                  | Daily             | [Google mobility reports](https://www.google.com/covid19/mobility/)                                | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_dynamic.csv)            |
| COVID-19 testing in Malaysia                        | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/testing.csv)                   | Daily             | [Ministry of Health, Malaysia GitHub](https://github.com/MoH-Malaysia/covid19-public)              | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_dynamic.csv)            |
| COVID-19-associated healthcare capacity in Malaysia | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/capacity.csv)                  | Daily             | [Ministry of Health, Malaysia GitHub](https://github.com/MoH-Malaysia/covid19-public)              | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_dynamic.csv)            |
| COVID-19 vaccinations in Malaysia                   | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/vaccination.csv)               | Daily             | [Common Integration Test Framework-Malaysia, GitHub](https://github.com/CITF-Malaysia/citf-public) | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_dynamic.csv)            |
| COVID-19 variants detected in Malaysia              | [GISAID Initiative](https://www.gisaid.org/)                                                          | Daily             | [GISAID Initiative](https://www.gisaid.org/)                                                       | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_dynamic.csv)            |
| Timeseries of cases by clusters                     | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/static/cluster_timeseries.csv) | No Longer updated | [Director General, Ministry of Health Malaysia, Daily Press Release](https://kpkesihatan.com/)     | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_static_cluster.csv)     |
| Linelist of mortalities based on Press release      | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/static/mortalities.csv)        | No Longer updated | [Director General, Ministry of Health Malaysia, Daily Press Release](https://kpkesihatan.com/)     | [CSV](https://raw.githubusercontent.com/spm-um/c19-epi-4msia/main/data/codebook/codebook_static_mortalities.csv) |

## A description of the data

#### COVID-19 transmission indicators in Malaysia

Data on national/ sub-national transmission indicators such as absolute
and per capita number of cases and deaths. Also include estimates of the
time-varying reproductive number.

#### Cluster of COVID-19 cases in Malaysia

Data on national type of clusters that have been detected. These are
categorised as import-, religious-, community-, high-risk-, education-,
detention-center- and workplace-linked clusters.

#### Mobility in Malaysia

Data on national/ sub-national mobility as measured by Google.

#### COVID-19 testing in Malaysia

Data on national/ sub-national testing statistics such as absolute and
per capita testing numbers. Also include the test positivity rate.

#### COVID-19-associated healthcare capacity in Malaysia

Data on national/ sub-national healthcare capacity in Malaysia. Include
the number of patients, capacity and utilisation rate at hospitals,
intensive care units, and low-risk centers as well as for ventilators.

#### COVID-19 vaccinations in Malaysia

Data on national/ sub-national vaccines that have been administered in
absolute counts and as a percentage of the total population.

#### Ratio of COVID-19 variants to cases in Malaysia

Data on ratio of cumulative number of all variants detected to
cumulative number of cases detected in Malaysia.

#### COVID-19 variants detected in Malaysia

Data on national distribution by type of variants. Data is provided by
the GISAID Initiative.

### CFR and features of cases and deaths

Linelist data on cases and deaths are now avaiable from the [Ministry of
Health, Malaysia GitHub](https://github.com/MoH-Malaysia/covid19-public)
without granular data on co-morbidities experienced by mortalities.

#### Timeseries of cases by clusters

Time-series data on all clusters detected between 1 January 2020 and 28
May 2021. Acquisition was stopped after reporting was halted by the MOH
on 28 May 2021.

#### Linelist of mortalities based on Press release

Linelist data on all mortalities detected between 17 March 2020 and 13
July 2021. Acquisition was stopped after reporting was halted by the MOH
on 13 July 2021. Data has detailed breakdown of comorbidities of each
COVID-19 mortality.

## Acknowledgements

This dashboard has been conceptualized, developed and maintained by a
team from the Department of Social and Preventive Medicine, lead by Prof
Dr Sanjay Rampal, assisted by Prof Dr Victor Hoe Chee Wai, Prof Dr Ng
Chiu Wan, Dr Vivek Jason Jayaraj and Dr Diane Chong Woei Quan.

## Terms of use

This data set is licensed under the Creative Commons Attribution 4.0
International (CC BY 4.0) by the Department of Social and Preventive
Medicine, Faculty of Medicine, University Malaya. Copyright University
Malaya 2021.

For publications that use the data, please cite the following
publication: " Jayaraj, V. J., Rampal, S., Ng, C.-W., & Chong, D. W. Q.
(2021). The Epidemiology of COVID-19 in Malaysia. The Lancet Regional
Health - Western Pacific, 17, 100295.
<https://doi.org/10.1016/J.LANWPC.2021.100295/ATTACHMENT/39AF55B7-8401-4A7F-A427-8DD9DF113F98/MMC1.DOCX>
"

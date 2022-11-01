options(stringAsFactors=F)
if(interactive()) setwd('~/d/sci/src/hydrocephalus')
library(tidyverse)
library(janitor)
library(openxlsx)

nusinersen = read.xlsx('data/nusinsersen.xlsx') %>% 
  clean_names() %>%
  mutate(year = as.integer(substr(latest_fda_received_date,8,12)))

hydrocephalus = read.xlsx('data/hydrocephalus.xlsx') %>% 
  clean_names() %>%
  mutate(year = as.integer(substr(latest_fda_received_date,8,12)))

summary_stats = read.xlsx('data/summary_stats.xlsx') %>% clean_names()

# search nusinersen reports for hydrocephalus
nusinersen %>% 
  filter(grepl('[Hh]ydrocephalus',reactions)) %>%
  mutate(hc_reaction = str_extract(';.+[Hh]ydrocephalus.+;', reactions)) %>%
  select(case_id, hc_reaction, serious, outcomes, sex, patient_age, year, reactions) -> n_query

# search hydrocephalus reports for nusinersen
hydrocephalus %>% 
  filter(grepl('[Ss]pinraza',suspect_product_names) | grepl('[Nn]usinersen|[Ss]pinraza',suspect_product_active_ingredients)) %>%
  mutate(hc_reaction = str_extract(';.+[Hh]ydrocephalus.+;', reactions)) %>%
  mutate(reactions = gsub(';','<br>',reactions)) %>%
  rename(country = country_where_event_occurred) %>%
  select(case_id, hc_reaction, serious, outcomes, sex, patient_age, event_date, latest_fda_received_date, reactions, year, concomitant_product_names, sender, country) -> h_query

# check that the exact same set of cases is returned by both methods
identical(n_query$case_id, h_query$case_id) # yes, same 16

# prepare table for markdown in blog post
h_query %>%
  mutate(adverse_events = gsub("Hydrocephalus","**Hydrocephalus**",gsub(';','<br>',reactions)),
         outcomes = gsub(';','<br>',outcomes)) %>%
  mutate(notes = case_when(grepl('Zolgensma',concomitant_product_names) ~ 'also received Zolgensma')) %>%
  mutate(sex = gsub('[a-z]*','',sex)) %>%
  mutate(age = gsub('Not Specified', '?', gsub('YR','y',gsub('MTH','mo',patient_age)))) %>%
  mutate(other_meds = gsub(';','<br>',concomitant_product_names)) %>%
  select(year, case_id, sex, age, adverse_events, outcomes, other_meds) -> for_markdown
write.table(for_markdown, 'output/for_markdown.txt', sep=' | ', na='', quote=F, col.names=T)

# calculate proportional reporting ratio, see p. 5 of https://www.ema.europa.eu/en/documents/regulatory-procedural-guideline/draft-guideline-use-statistical-signal-detection-methods-eudravigilance-data-analysis-system_en.pdf
a = nrow(h_query)
b = sum(nusinersen$year >= 2017 & !(nusinersen$case_id %in% h_query$case_id))
c = sum(hydrocephalus$year >= 2017 & !(hydrocephalus$case_id %in% h_query$case_id))
d = sum(summary_stats$total_reports[as.numeric(summary_stats$year) >= 2017], na.rm=T) - a - b - c
prr = (a/(a+b))/(c/(c+d))
s = sqrt(1/ a + 1/ c - 1/(a + b) - 1/(c + d))
l95 = prr / exp(1.96*s)
u95 = prr * exp(1.96*s)
prr
l95
u95


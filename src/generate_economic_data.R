require(tidyquant)
require(stringr)
require(ggplot2)

source(file="read_abs_data.R")

# Need to get GDP/cap, exports, labour force participation and FDI
## Importing excel data
labour <- read_abs_data(path="../data/6202001.xls", sheet=2)
gdp <- read_abs_data(path="../data/5206001_key_aggregates.xls", sheet=2)
trade <- read_abs_data(path="../data/536801.xls", sheet=2)
bop <- read_abs_data(path="../data/530201.xls", sheet=2)

## Extracting relevant series
participation <- labour %>% filter(series %in% c("Participation rate ;  Persons ;"))
gdp_per_capita <- gdp %>% filter(series %in% c("GDP per capita: Chain volume measures ;"))
exports <- trade %>% filter(series %in% c("Debits, Total goods and services ;"))
fdi <- bop %>% filter(series %in% c("Direct investment ;"))

## Resampling monthly series to quarterly
participation_resampled <- participation %>% tq_transmute(select=value, mutate_fun=apply.quarterly, FUN=mean) %>% mutate(series=participation$series[1])
exports_resampled <- exports %>% tq_transmute(select=value, mutate_fun=apply.quarterly, FUN=mean) %>% mutate(series=exports$series[1])

## Writing output
output <- bind_rows(participation_resampled, gdp_per_capita, exports_resampled, fdi)
write.csv(output, "../output/economic_data.csv")

## Miscellaneous 
#exports %>% mutate(qtr=as.yearqtr(date)) %>% sum(qtr)
gdp %>% distinct(series) %>% filter(str_detect(series, "exports"))
labour %>% distinct(series) %>% filter(str_detect(series, "(.+)Employment(.+)"))
labour %>% distinct(series) %>% filter(str_detect(series, "(.+)Unemployment(.+)"))
labour %>% distinct(series) %>% filter(series=="Employed total ;  Persons ;") 
labour %>% filter(str_detect(series, "Employment to population ratio "))
bop %>% distinct(series) %>% filter(str_detect(series, "Direct investment"))
bop %>% filter(str_detect(series, "Direct investment ;")) %>% filter(Date > "1980-01-01")

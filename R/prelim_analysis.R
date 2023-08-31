# Loading necessary packages
rm( list = ls() )
graphics.off()
gc( reset = T )
library(lubridate)
library(here)
library(ggrepel)
library(tidyverse)
library(data.table)
# Set seed for the Rmd
set.seed(42)

i_am('R/MainAnalysis.Rmd')
inDir <- here('data','input')
intermediateDir <- here('data','output')

input_dt = 
  arrow::read_feather( here( intermediateDir, 'final_input_clean.feather' ) ) %>%
  .[ !( cause_of_death %in% c( 'COVID-19', 'Total', 'HIV/AIDS' ) ) ]


logmx_dt =
  merge(
    input_dt %>% copy %>%
      .[ !( state %in% c( 'Vermont', 'North Dakota', 'Wyoming' ) ) ] %>%
      .[ , 
         .( nDx = sum( nDx ),
            Ex  = max( Ex ) ),
         .( country, state, year, sex, cause_of_death ) ] %>%
      .[ , logmx := log( nDx / Ex ) ],
    input_dt %>% copy %>%
      .[ !( state %in% c( 'Vermont', 'North Dakota', 'Wyoming' ) ) ] %>%
      #.[ , nDx := ifelse( nDx == 0, 0.1, nDx ) ] %>%
      .[ , 
         .( nDx = sum( nDx ),
            Ex  = max( Ex ) ),
         .( country, state, year, sex, age_group ) ] %>%
      .[ , logmx := log( nDx / Ex ) ] %>%
      data.table::dcast(
        country + state + year + sex ~ age_group,
        value.var = 'logmx'
      ),
    by = c( 'country', 'state', 'year', 'sex' )
  )



### Case 1 - US to US

selinput_dt = logmx_dt[ country == 'US', ]
states      = unique( selinput_dt$state )
randStates  <- sample( states, length( states )) 
testStates  <- randStates[1:floor(length(states)/4)]
trainStates <- randStates[-1:-floor(length(states)/4)]


trainData = selinput_dt[ state %in% trainStates, ]
testData = selinput_dt[ state %in% testStates, ]

# Construct data frame of state abbreviations + divisions, plus DC
linearModel <- 
  lm(
    logmx ~ 1 + `<1` + `1-4` + `5-14` + `15-24` +
      `25-34` + `35-44` + `45-54` + `55-64` + 
      `65-74` + `75-84` + `85+` +
      cause_of_death +
      cause_of_death:`<1` + cause_of_death:`1-4` + 
      cause_of_death:`5-14` + cause_of_death:`15-24` + 
      cause_of_death:`25-34` + cause_of_death:`35-44` + 
      cause_of_death:`45-54` + cause_of_death:`55-64` + 
      cause_of_death:`65-74` + cause_of_death:`75-84` + 
      cause_of_death:`85+`,
    data = trainData )

trainData <- 
  trainData %>% copy %>%
  .[ , prediction := predict( linearModel ) ]

trainData %>%
  ggplot() +
  geom_point(mapping=aes(x=logmx,y=prediction),alpha=0.3) +
  facet_wrap(~cause_of_death) +
  coord_equal(xlim = c(-10,-3),ylim=c(-10,-3))

trainData %>%
  mutate(predDeaths = exp(prediction)*Ex) %>%
  group_by(cause_of_death) %>%
  summarise(deaths=sum(nDx),
            predDeaths=sum(predDeaths)) %>%
  ungroup() %>%
  mutate(prop=deaths/sum(deaths),
         predProp=predDeaths/sum(predDeaths))


testData <- 
  testData %>% copy %>%
  .[ , prediction := predict( linearModel, testData ) ]

propData <- 
  testData %>%
  mutate(predDeaths = exp(prediction)*Ex) %>%
  group_by(cause_of_death) %>%
  summarise(deaths=sum(nDx),
            predDeaths=sum(predDeaths)) %>%
  ungroup() %>%
  mutate(prop=deaths/sum(deaths),
         predProp=predDeaths/sum(predDeaths))

propData

predVsObsProps <- 
  propData %>%
  ggplot() +
  geom_abline(intercept = 0,slope=1) +
  geom_point(mapping=aes(x=prop,y=predProp)) +
  geom_text_repel(mapping=aes(x=prop,y=predProp,label=cause_of_death),
                  min.segment.length=0) +
  coord_trans(x='log10',y='log10') +
  labs(y='Predicted Proportion of All Deaths',
       x='Observed Proportion of All Deaths') +
  theme_minimal()

ggsave(here('figures','predVsObsProps_US.png'),predVsObsProps,
       bg='white')



### Case 2 - US to BR

trainData =  logmx_dt[ country == 'US', ]
testData  =  logmx_dt[ country == 'BR', ]

# Construct data frame of state abbreviations + divisions, plus DC
linearModel <- 
  lm(
    logmx ~ 1 + `<1` + `1-4` + `5-14` + `15-24` +
      `25-34` + `35-44` + `45-54` + `55-64` + 
      `65-74` + `75-84` + `85+` +
      cause_of_death +
      cause_of_death:`<1` + cause_of_death:`1-4` + 
      cause_of_death:`5-14` + cause_of_death:`15-24` + 
      cause_of_death:`25-34` + cause_of_death:`35-44` + 
      cause_of_death:`45-54` + cause_of_death:`55-64` + 
      cause_of_death:`65-74` + cause_of_death:`75-84` + 
      cause_of_death:`85+`,
    data = trainData )

trainData <- 
  trainData %>% copy %>%
  .[ , prediction := predict( linearModel ) ]


testData <- 
  testData %>% copy %>%
  .[ , prediction := predict( linearModel, testData ) ]

propData <- 
  testData %>%
  mutate(predDeaths = exp(prediction)*Ex) %>%
  group_by(cause_of_death) %>%
  summarise(deaths=sum(nDx),
            predDeaths=sum(predDeaths)) %>%
  ungroup() %>%
  mutate(prop=deaths/sum(deaths),
         predProp=predDeaths/sum(predDeaths))

propData

predVsObsProps <- 
  propData %>%
  ggplot() +
  geom_abline(intercept = 0,slope=1) +
  geom_point(mapping=aes(x=prop,y=predProp)) +
  geom_text_repel(mapping=aes(x=prop,y=predProp,label=cause_of_death),
                  min.segment.length=0) +
  coord_trans(x='log10',y='log10') +
  labs(y='Predicted Proportion of All Deaths',
       x='Observed Proportion of All Deaths') +
  theme_minimal()

ggsave(here('figures','predVsObsProps_UStoBR.png'),predVsObsProps,
       bg='white')


### Case 3 - BR - SP to BR - AM

trainData =  logmx_dt[ country == 'BR' & state %in% c( 'SP', 'DF', 'PE' ) ]
testData  =  logmx_dt[ country == 'BR' & state %in% c( 'AL' ), ]

# Construct data frame of state abbreviations + divisions, plus DC
linearModel <- 
  lm(
    logmx ~ 1 + `<1` + `1-4` + `5-14` + `15-24` +
      `25-34` + `35-44` + `45-54` + `55-64` + 
      `65-74` + `75-84` + `85+` +
      cause_of_death +
      cause_of_death:`<1` + cause_of_death:`1-4` + 
      cause_of_death:`5-14` + cause_of_death:`15-24` + 
      cause_of_death:`25-34` + cause_of_death:`35-44` + 
      cause_of_death:`45-54` + cause_of_death:`55-64` + 
      cause_of_death:`65-74` + cause_of_death:`75-84` + 
      cause_of_death:`85+`,
    data = trainData )

trainData <- 
  trainData %>% copy %>%
  .[ , prediction := predict( linearModel ) ]


testData <- 
  testData %>% copy %>%
  .[ , prediction := predict( linearModel, testData ) ]

propData <- 
  testData %>%
  mutate(predDeaths = exp(prediction)*Ex) %>%
  group_by(cause_of_death) %>%
  summarise(deaths=sum(nDx),
            predDeaths=sum(predDeaths)) %>%
  ungroup() %>%
  mutate(prop=deaths/sum(deaths),
         predProp=predDeaths/sum(predDeaths))

propData 

predVsObsProps <- 
  propData %>%
  ggplot() +
  geom_abline(intercept = 0,slope=1) +
  geom_point(mapping=aes(x=prop,y=predProp)) +
  geom_text_repel(mapping=aes(x=prop,y=predProp,label=cause_of_death),
                  min.segment.length=0) +
  coord_trans(x='log10',y='log10') +
  labs(y='Predicted Proportion of All Deaths',
       x='Observed Proportion of All Deaths') +
  theme_minimal()

ggsave(here('figures','predVsObsProps_BR.png'),predVsObsProps,
       bg='white')









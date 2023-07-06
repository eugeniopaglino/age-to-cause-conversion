
require(data.table);require(tidyverse)

brsim_dir <- 'C://Users/zecos/OneDrive/josehcms/DATA/BRA/SIM'

# Idade do falecido em minutos, horas, dias, meses ou anos. 
#(Idade: composto de dois subcampos. 
# - O primeiro, de 1 dígito, indica a unidade da idade (se 1=minuto, se 2 = hora, se 3 = mês, se 4 = ano, se = 5 idade maior que 100 anos). 
# - O segundo, de dois dígitos, indica a quantidade de unidades: 
# Idade menor de 1 hora: subcampo varia de 01 e 59 (minutos); De 1 a 23 Horas:subcampo varia de 01 a 23 (horas); 
# De 24 horas e 29 dias: subcampo varia de 01 a 29 (dias); 
# De 1 a menos de 12 meses completos: subcampo varia de 01 a 11 (meses); 
# Anos - subcampo varia de 00 a 99; - 9 - ignorado)


sim_files <- 
  file.path( brsim_dir,
             grep( paste0( 2009:2019, collapse = '|'), list.files( brsim_dir ), value = T ) )

brsim_dt <- 
  lapply( sim_files, 
          function( i ) {
            
            sim_dt <- 
              fread( i ) %>%
              .[ TIPOBITO != 1 , 
                 .(
                   state          = substr( CODMUNRES, 1, 2 ),
                   year           = DTOBITO %% 10000,
                   sex            = ifelse( SEXO == 2, 'Female',
                                            ifelse( SEXO == 1, 'Male', NA ) ),
                   ageVar         = sprintf( '%03d', IDADE ),
                   ICD10Code      = CAUSABAS
                 ) ] %>%
              .[ , ageType := substr( ageVar, 1, 1 ) ] %>%
              .[ , ageNum := as.numeric( paste0( substr( ageVar, 2, 3 ) ) ) ] %>%
              .[ , age := case_when( ageType %in% 0:3 ~ 0,
                                     ageType == 4 ~ ageNum,
                                     ageType == 5 ~ ageNum + 100,
                                     ageType == 9 ~ NA ) ] %>%
              .[ , `:=` ( 
                ICDMainCode   = substr( ICD10Code, 1, 3 ),
                ICDLetter     = substr( ICD10Code, 1, 1 ),
                ICDNumber     = as.numeric( substr( ICD10Code, 2, 4 ) ),
                ICDMainNumber = as.numeric( substr( ICD10Code, 2, 3 ) )
              ) ] %>%
              .[ , causeOfDeath := factor(
                case_when(
                  ICD10Code %in% c('E244','G312','G621','G72.1','I426', 
                                   'K292','K852','K860','R780') ~ 22,
                  ICDMainCode %in% c('F10','K70','X45','X65','Y15') ~ 22,
                  ICDLetter == 'B' & between(ICDMainNumber,20,24) ~ 1,
                  ICDLetter == 'A' | (ICDLetter == 'B' & !between(ICDMainNumber,20,24)) ~ 2,
                  ICDMainCode == 'C22' ~ 3,
                  ICDMainCode %in% c('C33','C34') ~ 4,
                  ICDMainCode %in% c('C18','C19','C20','C21','C50','C53','C61') ~ 5,
                  ICDLetter == 'C' & !(ICDMainNumber %in% c(18:21,50,53,61)) ~ 6,
                  ICDLetter == 'D' & ICDMainNumber <= 48 ~ 6,
                  ICDLetter == 'F' & between(ICDMainNumber,1,99) ~ 7,
                  ICDLetter == 'G' & between(ICDMainNumber,0,99)  & ICDMainCode != 'G30' ~ 8,
                  ICDMainCode == 'G30' ~ 9,
                  ICDLetter == 'I' & between(ICDMainNumber,10,15) ~ 10,
                  ICDLetter == 'I' & !between(ICDMainNumber,10,15) ~ 11,
                  ICDLetter == 'J' & between(ICDMainNumber,9,18) ~ 12,
                  ICDLetter == 'J' & !between(ICDMainNumber,9,18) & ICDMainNumber != 99 ~ 13,
                  ICDLetter == 'K' & between(ICDMainNumber,0,92) ~ 14,
                  ICDLetter == 'E' & between(ICDMainNumber,10,14) ~ 15,
                  ICDLetter == 'E' & between(ICDMainNumber,0,88) & !between(ICDMainNumber,10,14) ~ 16,
                  ICDLetter == 'N' & between(ICDMainNumber,0,98) ~ 17,
                  ICDLetter == 'P' & between(ICDMainNumber,0,96) ~ 18,
                  ICDLetter == 'Q' & between(ICDMainNumber,0,99) ~ 18,
                  ICDLetter == 'X' & between(ICDMainNumber,86,99) ~ 19,
                  ICDLetter == 'Y' & (between(ICDMainNumber,0,9) | ICDMainCode == 'Y871') ~ 19,
                  ICDMainCode == 'Y870' | (ICDLetter == 'X' & between(ICDMainNumber,66,84)) ~ 20,
                  ICDLetter == 'X' & ICDMainNumber %in% c(40:44,60:64,85) ~ 21,
                  ICDLetter == 'Y' & between(ICDMainNumber,10,14) ~ 21,
                  ICDMainCode == 'Y85' | (ICDLetter == 'V' & between(ICDMainNumber,1,99)) ~ 23,
                  ICDLetter %in% c('V','W','X','Y') ~ 24,
                  ICDLetter == 'R' & between(ICDMainNumber,0,99) ~ 25,
                  ICD10Code == 'U071' ~ 26,
                  TRUE ~ 27),
                levels=1:28,
                labels=c('HIV/AIDS','Other infectious and\nparasitic diseases',
                         'Liver cancer','Lung cancer',
                         'Breast, prostate, colorectal,\nand cervical cancer',
                         'All other cancers','Mental disorders',
                         "Nervous system, excl. Alzheimer's","Alzheimer's disease",
                         'Hypertensive heart disease',
                         'Circulatory diseases,\nexcl. Hypertensive heart disease',
                         'Respiratory diseases\nexcl. Influenza and pneumonia',
                         'Influenza and pneumonia','Diseases of the digestive system',
                         'Diabetes',
                         'Endocrine, nutritional,\nand metabolic diseases excl. Diabetes',
                         'Diseases of the genitourinary system',
                         'Perinatal conditions\nand congenital anomalies','Homicide',
                         'Suicide','Drug overdose','Alcohol-induced','Transport accidents',
                         'Other external causes','Symptoms, signs,\nand ill-defined conditions',
                         'COVID-19',
                         'All other causes',
                         'Total' ) ) ] %>%
              .[ ,
                 .( deaths = .N ),
                 by = .( state, year, sex, age, causeOfDeath ) ]
            
          } ) %>%
  rbindlist()

library(here)
inDir <- here('data','input')
intermediateDir <- here('data','output')
brsim_dt %>% arrow::write_feather(here(intermediateDir,'mortDataCleanBR.feather'))

finalData <-  setDT( arrow::read_feather(here(intermediateDir,'finalData.feather')) )

finalData$age %>% unique

# put ages on the same US format
brsim_dt[ , agegrp := cut( age,
                           breaks = c( 0, 1, seq( 5, 85, 10 ), 200 ),
                           labels = c( finalData$age %>% unique ),
                           right = FALSE,
                           include.lowest = TRUE ) ]

mortDataBR <- 
  brsim_dt[ !is.na( sex ) & !is.na( age ) & year %in% 2009:2011, 
            .( deaths = sum( deaths ) / 3 ), 
            .( sex, age = agegrp, causeOfDeath ) ] %>%
  setorder( sex, age, causeOfDeath )


require(wpp2022)
data('pop1dt')

popDataBR <- 
  popAge1dt[ country_code == 76 ] %>%
  .[ , agegrp := cut( age,
                      breaks = c( 0, 1, seq( 5, 85, 10 ), 200 ),
                      labels = c( finalData$age %>% unique ),
                      right = FALSE,
                      include.lowest = TRUE ) ] %>%
  .[ , .( Male = sum( popM ) * 1000, Female = sum( popF ) * 1000 ), .( year, age = agegrp ) ] %>%
  data.table::melt(
    id.vars = c( 'year', 'age' ),
    measure.vars = c( 'Male', 'Female' ),
    value.name = 'pop',
    variable.name = 'sex'
  )

finalDataBR <- 
  mortDataBR %>%
  merge(
    popDataBR[ year == 2010, .( sex, age, pop ) ],
    by = c( 'sex', 'age' )
  )


### Model
ageSpecificRates <- finalData %>%
  group_by(state,year,sex,age) %>%
  summarise(deaths=sum(deaths),
            pop=max(pop),
            logm=log(deaths/pop)) %>%
  ungroup() %>%
  select(-c(deaths,pop)) %>%
  pivot_wider(values_from = logm, names_from = age,names_prefix = 'logm')

finalData <- finalData %>%
  filter(!(causeOfDeath %in% c('COVID-19','Total','HIV/AIDS')),
         !(state %in% c('Vermont','North Dakota','Wyoming'))) %>%
  group_by(state,year,sex, causeOfDeath) %>%
  summarise(deaths=sum(deaths),
            pop=max(pop),
            logm=log(deaths/pop)) %>%
  ungroup() %>%
  left_join(ageSpecificRates,by=c('state','year','sex'))

trainData <- finalData 

ageSpecificRatesBR <- finalDataBR %>%
  group_by(sex,age) %>%
  summarise(deaths=sum(deaths),
            pop=max(pop),
            logm=log(deaths/pop)) %>%
  ungroup() %>%
  select(-c(deaths,pop)) %>%
  pivot_wider(values_from = logm, names_from = age,names_prefix = 'logm')

finalDataBR <- finalDataBR %>%
  filter(!(causeOfDeath %in% c('COVID-19','Total','HIV/AIDS'))) %>%
  group_by(sex, causeOfDeath) %>%
  summarise(deaths=sum(deaths),
            pop=max(pop),
            logm=log(deaths/pop)) %>%
  ungroup() %>%
  left_join(ageSpecificRatesBR,by=c('sex'))

testData <- finalDataBR
# Construct data frame of state abbreviations + divisions, plus DC
linearModel <- lm(logm ~ 1 + `logm<1` + `logm1-4` + `logm5-14` + `logm15-24` + 
                    `logm25-34` + `logm35-44` + `logm45-54` + `logm55-64` + 
                    `logm65-74` + `logm75-84` + `logm85+` +
                    causeOfDeath +
                    causeOfDeath:`logm<1` + causeOfDeath:`logm1-4` + 
                    causeOfDeath:`logm5-14` + causeOfDeath:`logm15-24` + 
                    causeOfDeath:`logm25-34` + causeOfDeath:`logm35-44` + 
                    causeOfDeath:`logm45-54` + causeOfDeath:`logm55-64` + 
                    causeOfDeath:`logm65-74` + causeOfDeath:`logm75-84` + 
                    causeOfDeath:`logm85+`,
                  data = trainData)

trainData <- trainData %>%
  mutate(prediction = predict(linearModel))

trainData %>%
  ggplot() +
  geom_point(mapping=aes(x=logm,y=prediction),alpha=0.3) +
  facet_wrap(~causeOfDeath) +
  coord_equal(xlim = c(-10,-3),ylim=c(-10,-3))

trainData %>%
  mutate(predDeaths = exp(prediction)*pop) %>%
  group_by(causeOfDeath) %>%
  summarise(deaths=sum(deaths),
            predDeaths=sum(predDeaths)) %>%
  ungroup() %>%
  mutate(prop=deaths/sum(deaths),
         predProp=predDeaths/sum(predDeaths))

testData <- testData %>%
  mutate(prediction = predict(linearModel,testData))

testData %>%
  ggplot() +
  geom_point(mapping=aes(x=logm,y=prediction),alpha=0.3) +
  facet_wrap(~causeOfDeath) +
  coord_equal(xlim = c(-10,-3),ylim=c(-10,-3))

propData <- testData %>%
  mutate(predDeaths = exp(prediction)*pop) %>%
  group_by(causeOfDeath) %>%
  summarise(deaths=sum(deaths),
            predDeaths=sum(predDeaths)) %>%
  ungroup() %>%
  mutate(prop=deaths/sum(deaths),
         predProp=predDeaths/sum(predDeaths))

require(ggrepel)
predVsObsProps <- propData %>%
  ggplot() +
  geom_abline(intercept = 0,slope=1) +
  geom_point(mapping=aes(x=prop,y=predProp)) +
  geom_text_repel(mapping=aes(x=prop,y=predProp,label=causeOfDeath),
                  min.segment.length=0) +
  coord_trans(x='log10',y='log10') +
  labs(y='Predicted Proportion of All Deaths',
       x='Observed Proportion of All Deaths') +
  theme_minimal()

propData %>%
  as.data.table %>%
  .[ , diff := round( 100 * ( predProp - prop ), 2 ) ] %>%
  head( 30 ) %>%
  setorder( diff ) %>%
  .[ , .( causeOfDeath, obs = round( prop * 100, 1 ), pred = round( predProp * 100, 1 ), diff ) ]

propData %>%
  as.data.table %>% 
  .[ causeOfDeath %in% c( 'Drug overdose', 'Suicide', 'Other external causes', 'Transport accidents', 'Homicide' )]

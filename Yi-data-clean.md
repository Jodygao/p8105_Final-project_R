P8105 Fall 2023 Final Project Proposal
================
All Group Member Collaboration
2023-11-10

### Group Members

- Yuandi Gao (yg2907)
- Yi Li (yl5214)
- Yingting Zhang (yz4434)
- Ruohan Hong (rh3132)
- Shiqi Wu (sw3737)

\##Import data from NHANES

``` r
import_df= function(path){
  read_xpt(path)
}
obesity=
  import_df(path='data/P_BMX.XPT') |> 
  full_join((import_df(path='data/P_DEMO.XPT')), by='SEQN') |>
  full_join((import_df(path='data/P_PAQ.XPT')), by='SEQN')|>
  full_join ((import_df(path='data/P_DBQ.XPT')), by='SEQN') |> 
  select (SEQN,RIAGENDR,RIDAGEYR,DMDMARTZ,INDFMPIR,RIDRETH3,DMDEDUC2,PAD680,BMXBMI,DBD900,DBD910) |> 
  filter(
    !(DMDMARTZ %in% c('77','99','.') ),
    !(INDFMPIR == '.'),
    !(DMDEDUC2 %in% c('7','9','.') ),
    !(PAD680 %in% c('7777', '9999', '.')),
    !(BMXBMI =='.'),
    !(DBD900 %in% c('7777', '9999', '.')),
    !(DBD910 %in% c('7777', '9999', '.'))
  ) |> 
  rename(
    gender=RIAGENDR,
    age=RIDAGEYR,
    marital_status=DMDMARTZ,
    income_to_poverty=INDFMPIR,
    race=RIDRETH3,
    education=DMDEDUC2,
    sedentary_activity=PAD680,
    bmi=BMXBMI,
    freq_fast_food=DBD900,
    freq_frozen=DBD910
  ) |> 
  mutate(
    gender = case_match(
      gender,
      1~"Male",
      2~"Female"),
    gender = as.factor(gender),
  marital_status= case_match(
    marital_status,
      1~"Married",
      2~"Widowed/Divorced/Separated",
      3~"Never married"
  ),
    race= case_match(
    race,
      1~"Mexican American",
      2~"Other Hispanic",
      3~"White",
      4~"Black",
      5~"Asian",
      6~"Other"
     ),
      education= case_match(
    education,
      1~"Less than 9th grade",
      2~"9-11th grade",
      3~"High school graduate" ,
      4~"Some college or AA degree",
      6~"College graduate or above"
     ),
   bmi =case_when(
    bmi<30 ~ 'normal',
    bmi>=30 ~'obese'
  )
  )
```
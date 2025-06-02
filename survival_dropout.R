---
title: "Survival Student Dropout Models"
output: pdf_document
date: 
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Packages

```{r eval=FALSE, include=FALSE}
# packages
#install.packages(c("mlr3", "mlr3proba", "mlr3learners", "mlr3measures", "mlr3verse", "survival", "randomForestSRC", "devtools", "dplyr", "lubridate"))
library(devtools)
devtools::install_github("jakubkuzilek/oulad")
library(oulad)
library(survival)
library(dplyr)
library(lubridate)
library(tidyr)
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3measures)
library(mlr3verse) 
library(stringr)
```


# Dataset

```{r}
# survival data with demographics
surv_data <- oulad::student |>
  left_join(oulad::student_registration, 
            by = c("id_student", "code_module", "code_presentation")) |>
  filter(code_module       == "GGG") |>
  filter(str_detect(code_presentation, "J")) |>
  mutate(time  = ifelse(is.na(date_unregistration), 269, date_unregistration),
         event = ifelse(is.na(date_unregistration), 0, 1)) |>
  filter(time > 0)

# survival data with vle
vle_agg <- oulad::student_vle |>
  left_join(surv_data |> select(id_student, code_module, code_presentation, time),
            by = c("id_student", "code_module", "code_presentation")) |>
  filter(date == time) |>  
  group_by(id_student, code_module, code_presentation) |>
  summarise(prior_clicks = sum(sum_click), .groups = "drop")

# combining survival data with demographics and vle
surv_data <- surv_data |>
  left_join(vle_agg, by = c("id_student", "code_module", "code_presentation")) |>
  mutate(prior_clicks = replace_na(prior_clicks, 0))
```


# Preparing dataset

```{r}
surv_data_rf <- surv_data |>
  select(gender, region, highest_education, imd_band, age_band,
         disability, prior_clicks, time, event) |>
  mutate(
    age_band          = as.factor(age_band),
    gender            = as.factor(gender),
    highest_education = as.factor(highest_education),
    imd_band          = as.factor(imd_band),
    region            = as.factor(region)) |>
  na.omit()
```

# Cox PH assumption

```{r eval=FALSE, include=FALSE}
task <- TaskSurv$new(
  id      = "oulad_surv",
  backend = surv_data_rf,
  time    = "time",
  event   = "event")

learner <- lrn("surv.coxph")

learner$train(task)

cox.zph(learner$model)
```


# Setup

```{r eval=FALSE, include=FALSE}
task <- TaskSurv$new(
  id      = "oulad_surv",
  backend = surv_data_rf,
  time    = "time",
  event   = "event")

learners <- c(lrn("surv.coxph"), lrn("surv.rfsrc",
                                     ntree    = 476,
                                     mtry     = 2,
                                     nodesize = 15))

resampling <- rsmp("cv", folds = 5)
resampling$instantiate(task)

measures <- list(
  msr("surv.brier"),  
  msr("surv.cindex")                        
)
```


# Model performance

```{r eval=FALSE, include=FALSE}
bmr <- benchmark(
  benchmark_grid(
    tasks       = task,
    learners    = learners,
    resamplings = resampling
  )
)

bmr$aggregate(measures)
```


# Hyperparameter tuning

```{r echo=TRUE}
library(paradox)
library(mlr3tuning)

task <- TaskSurv$new(
  id      = "oulad_surv",
  backend = surv_data_rf,
  time    = "time",
  event   = "event")

measure <- msr("surv.cindex")

resampling <- rsmp("cv", folds = 5)
resampling$instantiate(task)

param_set <- ps(
  ntree     = p_int(lower = 100, upper = 500),
  mtry      = p_int(lower = 2,   upper = 6),
  nodesize  = p_int(lower = 5,   upper = 25)
)

tuner <- tnr("random_search")  
terminator <- trm("evals", n_evals = 20)  

learner <- lrn("surv.rfsrc")
at <- AutoTuner$new(
  learner      = learner,
  resampling   = resampling,
  measure      = measure,
  search_space = param_set,
  terminator   = terminator,
  tuner        = tuner
)

at$train(task)
at$learner$model 
at$archive$data   

```

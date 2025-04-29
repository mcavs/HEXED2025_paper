# packages
install.packages("devtools")
library(devtools)
devtools::install_github("jakubkuzilek/oulad")
install.packages(c("survival", "dplyr", "lubridate"))
library(survival)
library(dplyr)
library(lubridate)
library(tidyr)
install.packages(c("mlr3", "mlr3proba", "mlr3learners", "mlr3measures", "mlr3verse", "survival", "randomForestSRC"))
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3measures)
library(mlr3verse) 

# survival data with demographics
surv_data <- oulad::student |>
  left_join(oulad::student_registration, 
            by = c("id_student", "code_module", "code_presentation")) |>
  filter(code_module       == "AAA", 
         code_presentation == "2013J") |>
  mutate(time  = date_unregistration - date_registration,
         time  = ifelse(is.na(time), 269 - date_registration, time),
         event = ifelse(is.na(date_unregistration), 0, 1))

# survival data with vle
vle_agg <- oulad::student_vle |>
  left_join(surv_data |> select(id_student, code_module, code_presentation, time),
            by = c("id_student", "code_module", "code_presentation")) |>
  filter(date < time) |>  
  group_by(id_student, code_module, code_presentation) |>
  summarise(prior_clicks = sum(sum_click), .groups = "drop")

# combining survival data with demographics and vle
surv_data <- surv_data |>
  left_join(vle_agg, by = c("id_student", "code_module", "code_presentation")) |>
  mutate(prior_clicks = replace_na(prior_clicks, 0))

# Cox model training
cox_model <- coxph(Surv(time, event) ~ 
                     age_band + gender + highest_education + 
                     imd_band + prior_clicks,
                   data = surv_data)

# Random survival forest model training
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

task <- TaskSurv$new(
  id = "oulad_surv",
  backend = surv_data_rf,
  time = "time",
  event = "event")

learner_cox <- lrn("surv.coxph")
learner_rsf <- lrn("surv.rfsrc")


resampling <- rsmp("cv", folds = 10)
resampling$instantiate(task)

measures <- list(
  msr("surv.brier"),  
  msr("surv.cindex")                        
)

bmr <- benchmark(
  benchmark_grid(
    tasks       = task,
    learners    = learners,
    resamplings = resampling
  )
)

bmr$aggregate(measures)






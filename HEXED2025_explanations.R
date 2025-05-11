task <- TaskSurv$new(
  id      = "task",
  backend = surv_data_rf,        
  time    = "time",         
  event   = "event"      
)

set.seed(123)
split <- partition(task, ratio = 0.8)

set.seed(123)
cox_model <- lrn("surv.coxph", id = "coxph")
cox_model$train(task, row_ids = split$train)

rsf_model <- lrn("surv.rfsrc", id = "rsf", ntree = 336, mtry = 2, nodesize = 19)
rsf_model$train(task, row_ids = split$train)

library(survex)
cox_explainer <- explain(cox_model,
                         data = as.data.frame(task$data()),
                         y = task$truth(),
                         predict_function = predict)

rsf_explainer <- explain(rsf_model,
                         data = as.data.frame(task$data()),
                         y = task$truth(),
                         predict_function = predict)

cox_parts <- model_parts(cox_explainer)
rsf_parts <- model_parts(rsf_explainer)

library(ggplot2)
rsf_vip <- plot(rsf_parts) +
  labs(y = "Brier score loss",
       x = "") + 
  theme_bw(base_size = 20) + 
  geom_line(size = 2) + 
  theme(legend.title  = element_blank(),
        plot.subtitle = element_blank(),
        plot.tag      = element_blank(),
        plot.title    = element_blank(),
        strip.text    = element_blank(),
        legend.position = "none") 

cox_vip <- plot(cox_parts) +
  labs(y = "Brier score loss") + 
  theme_bw(base_size = 20) + 
  geom_line(size = 2) + 
  theme(legend.title  = element_blank(),
        plot.subtitle = element_blank(),
        plot.tag      = element_blank(),
        plot.title    = element_blank(),
        strip.text    = element_blank(),
        legend.position = "bottom")


plot_grid(rsf_vip, cox_vip, 
          labels = c("A", "B"),
          label_size = 20,
          ncol = 1,
          rel_heights = c(1, 1.3))



cox_profile <- model_profile(cox_explainer, variables = "imd_band")
rsf_profile <- model_profile(rsf_explainer)

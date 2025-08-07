library(dplyr)

obj3 <- preprocess_data(data = psxai::heart_preprocess,
                indeps = NULL,
                y = "Heart Disease",
                pred = "predictions") %>%
  ps_calc() %>%
  psxai::plot_forest()

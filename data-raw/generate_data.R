

# 01_read data ------------------------------------------------------------

library(dplyr)
heart_preprocess <- readr::read_delim("data-raw/heart.csv") %>%
  mutate_at("cp", ~case_when(. == 0 ~ "typical angina",
                             . == 1 ~ "atypical angina",
                             . == 2 ~ "non-anginal",
                             . == 3 ~ "asymptomatic")) %>%
  mutate_at("sex", ~case_when(. == 0 ~"Male",
                              . == 1 ~ "Female")) %>%
  mutate_at("restecg", ~case_when(. == 0 ~ "normal",
                                  . == 1 ~ "stt abnormality",
                                  . == 2 ~ " lv hypertrophy")) %>%
  mutate_at("target", ~case_when(. == 0 ~ "no",
                                 . == 1 ~ "yes")) %>%
  mutate_at("fbs", ~case_when(. == 0 ~ "no",
                              . == 1 ~ "yes")) %>%
  mutate_at("exang", ~case_when(. == 0 ~ "no",
                                . == 1 ~ "yes")) %>%
  dplyr::rename("Age" = "age",
                "Sex" = "sex",
                "Chest Pain Type" = "cp",
                "Resting blood pressure"  = "trestbps",
                "Cholestrol" = "chol",
                "Fasting blood sugar" = "fbs",
                "Resting Electrocardiographic results" = "restecg",
                "Maximum heart rate achieved" = "thalach",
                "Exercise-induced angina" = "exang",
                "ST depression" = "oldpeak",
                "Slope of the peak exercise ST segment" = "slope",
                "No. of major vessels colored by fluoroscopy" = "ca",
                "Thalassemia" = "thal",
                "Heart Disease" = "target") %>%
  mutate_at("Heart Disease", ~case_when(. == "yes" ~ 1,
                                        . == "no" ~0))

#predictions
a = glm(`Heart Disease` ~ Sex + Age + Cholestrol, data = data1, family = "binomial")
heart_preprocess$predictions <- a$fitted.values

usethis::use_data(heart_preprocess, overwrite = TRUE)




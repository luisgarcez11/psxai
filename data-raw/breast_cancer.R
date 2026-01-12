library(dplyr)
library(ggplot2)
library(psxai)
library(dplyr)
library(ggplot2)
library(psxai)

n <- names(read.csv("data-raw/data/adult.csv"))
predictions <- read.csv("data-raw/predicted_probs_breast_cancer.csv") %>%
  pull(1)
predictions <- c(0.31, predictions)

data2 <- read.csv("data-raw/data/adult.csv")

obj3 <- preprocess_data(data = data2 %>%
                          select(age, fnlwgt, education.num,sex,
                                 capital.gain, capital.loss, hours.per.week,
                                 income
                          ) %>%
                          mutate(predictions = predictions),
                        indeps = NULL,
                        y = "income",
                        pred = "predictions") %>%
  ps_calc() %>%
  psxai::plot_forest()

obj3$plot

png("data-raw/plot_results_psxai.png", width = 2300, height = 600,
    res = 200)
obj3$plot
dev.off()


# shap vs psxai --------------------------------------------------------------------

obj3$iptw_results %>%
  arrange(desc(abs(estimate))) %>%
  View()

shap_feature_imp <- read.csv(file = "data-raw/shap_feature_importance.csv") %>%
  left_join(obj3$iptw_results,
            by = c("col_name" = "variable"))

ggplot2::ggplot(data = shap_feature_imp) +
  geom_point(mapping = aes(x = feature_importance_vals, y = abs(estimate), colour  = col_name))

#TASK:
#test with other dataset. with only binary and continuous variables



#the main of PS is access feature improtannce without confouding

#introduction - explain the importance of explianing ML models,
#explain the main pitfalls od SHAP

#software - R package

#methodology
#explain the methodology

#results
#comparison with SHAP

#discussion/conclusions





import pandas as pd

import os
import shap
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import numpy
import csv
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score, confusion_matrix




deps = "original_heart_disease"
indeps = ["original_sex","original_fasting_blood_sugar"      ,
          "original_exercise_induced_angina"                             
, "original_age","original_resting_blood_pressure" ,"original_cholestrol"                                          
,"original_maximum_heart_rate_achieved" ,"original_st_depression"                                       
, "original_slope_of_the_peak_exercise_st_segment",
"original_no_of_major_vessels_colored_by_fluoroscopy"          
,"original_thalassemia"  ]


# Read the CSV file
data = pd.read_csv("data_to_shap.csv")
#X, y = shap.datasets.diabetes()
X = pd.DataFrame(data[indeps])  # only selected columns
# Rename columns
X.columns = X.columns.str.replace("original_", "", regex=True)
X.columns = X.columns.str.replace("_", " ", regex=True)
X = X.rename(columns={"chest pain type asymptomatic": "chest pain type (asymptomatic)", 
                  "chest pain type atypical angina": "chest pain type (atypical angina)", 
                  "chest pain type non anginal": "chest pain type (non anginal)", 
                  "chest pain type typical angina": "chest pain type (typical angina)",
                  "resting electrocardiographic results lv hypertrophy" : "resting electrocardiographic results (lv hypertrophy)",
                  "resting electrocardiographic results normal" : "resting electrocardiographic results (normal)",
                  "resting electrocardiographic results stt abnormality" : "resting electrocardiographic results (stt abnormality)"})

y = data[deps].to_numpy()

#scaler
scaler = StandardScaler()

# Fit and transform
X_scaled = scaler.fit_transform(X)

model = MLPClassifier(
    hidden_layer_sizes=5,  # two layers: 32 and 16 neurons
    activation='logistic',           # relu, tanh, or logistic
    solver='lbfgs',
    alpha=1e-5,             # optimizer: adam, lbfgs, or sgd
    max_iter=3000,
    random_state=42
)


# 5. Fit the model
model_fit = model.fit(X_scaled, y)


#6. write probabilities
a = numpy.asarray(model.predict_proba(X_scaled))
numpy.savetxt("predicted_probs.csv", a, delimiter=",")

#7. accuracy
y_pred = model.predict(X_scaled)
y_prob = a[:,1] # For AUC
roc_auc_score(y, y_prob)



# explain all the predictions in the test set
shap.initjs()

explainer = shap.PermutationExplainer(model.predict, X_scaled)
shap_values = explainer.shap_values(X_scaled)
shap.summary_plot(shap_values, X)
plt.savefig('plot_shap.png',bbox_inches="tight")
plt.close()

#shap importance
vals = numpy.abs(shap_values).mean(0)

shap_importance = pd.DataFrame(list(zip(X.columns, vals)),
                                  columns=['col_name','feature_importance_vals'])
shap_importance.sort_values(by=['feature_importance_vals'],
                               ascending=False, inplace=True)
shap_importance.head()
shap_importance.to_csv('shap_feature_importance.csv')

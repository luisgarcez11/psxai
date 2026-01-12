

import pandas as pd

import os
import shap
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import numpy
import csv
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score, confusion_matrix




deps = 'default.payment.next.month'
indeps = [ 'SEX', 'EDUCATION', 'MARRIAGE', 'AGE', 'PAY_0',
       'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6', 'BILL_AMT1', 'BILL_AMT2',
       'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1',
       'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6' ]

df['SEX'] = df['SEX'].replace('1', 'male')
df['SEX'] = df['SEX'].replace('2', 'female')

# Read the CSV file
data = pd.read_csv("data/UCI_Credit_Card.csv")
#X, y = shap.datasets.diabetes()
X = pd.DataFrame(data[indeps])  # only selected columns

y = data[deps].to_numpy()

#scaler
scaler = StandardScaler()

# Fit and transform
X_scaled = scaler.fit_transform(X)

model = MLPClassifier(
    hidden_layer_sizes=10,  # two layers: 32 and 16 neurons
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
numpy.savetxt("predicted_probs_breast_cancer.csv", a, delimiter=",")

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

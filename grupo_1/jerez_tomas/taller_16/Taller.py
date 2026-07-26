
import os
os.environ["TABPFN_TOKEN"] = ""

token = os.getenv("TABPFN_TOKEN")
print("Python ve el token:", token is not None and len(token) > 0)


import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report

from tabpfn import TabPFNClassifier

RUTA = r"C:\Users\Hespa\OneDrive\Escritorio\Documentos\GitHub\dm_2016325\grupo_1\jerez_tomas\taller_16\tic-tac-toe.data"
nombres = ["c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "clase"]
df = pd.read_csv(RUTA, header=None, names=nombres)
print("Shape:", df.shape)
print(df.head())
print(df["clase"].value_counts())


X = pd.get_dummies(df.drop(columns="clase"))
print("Shape X tras one-hot:", X.shape)   


le = LabelEncoder()
y = le.fit_transform(df["clase"])
mapa = {"positive": "Gana X", "negative": "No gana X"}
class_names = [mapa[c] for c in le.classes_]   
print("Clases:", le.classes_, "->", class_names)


X_train, X_test, y_train, y_test = train_test_split(
    X, y,
    test_size=0.20,
    random_state=123,
    stratify=y
)


modelo = TabPFNClassifier()
modelo.fit(X_train, y_train)


y_pred  = modelo.predict(X_test)
y_proba = modelo.predict_proba(X_test)


acc = accuracy_score(y_test, y_pred)
print("Accuracy:", round(acc, 4))

cm = confusion_matrix(y_test, y_pred)
print("Matriz de confusion:")
print(pd.DataFrame(cm,
                   index=[f"real_{c}" for c in class_names],
                   columns=[f"pred_{c}" for c in class_names]))


print(classification_report(y_test, y_pred, target_names=class_names))

proba_df = pd.DataFrame(y_proba, columns=[f"prob_{n}" for n in class_names])
print(proba_df.head())

nuevo = X_test.iloc[[0]]
pred_nuevo  = modelo.predict(nuevo)
proba_nuevo = modelo.predict_proba(nuevo)
print("Prediccion de un tablero:", class_names[pred_nuevo[0]])
print(pd.DataFrame(proba_nuevo, columns=[f"prob_{n}" for n in class_names]))
print("Accuracy:", round(acc, 4))
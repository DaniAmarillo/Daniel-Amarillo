import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#Ejercicio 1

NA_ES_CATEGORIA = [
    'Alley', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1',
    'BsmtFinType2', 'FireplaceQu', 'GarageType', 'GarageFinish',
    'GarageQual', 'GarageCond', 'PoolQC', 'Fence', 'MiscFeature'
]

df = pd.read_csv('grupo_1/jerez_tomas/taller_07/house_prices.csv', keep_default_na=False, na_values=[''])

for col in df.columns:
    if col not in NA_ES_CATEGORIA:
        df[col] = df[col].replace('NA', np.nan)
        try:
            df[col] = pd.to_numeric(df[col])
        except (ValueError, TypeError):
            pass

print("(a) Top 5 columnas con mayor % de NA:")
pct_na = (df.isna().sum() / len(df) * 100).sort_values(ascending=False)
top5 = pct_na.head(5)
print(top5.round(2))

a_eliminar = top5[top5 > 50].index.tolist()
a_imputar = top5[top5 <= 50].index.tolist()
print("Eliminar:", a_eliminar)
print("Imputar:", a_imputar)

df = df.drop(columns=a_eliminar)

num_cols = df.select_dtypes(include=np.number).columns
cat_cols = df.select_dtypes(exclude=np.number).columns

for col in num_cols:
    df[col] = df[col].fillna(df[col].median())
for col in cat_cols:
    df[col] = df[col].fillna(df[col].mode()[0])

print("\n(b) Filas completas:", df.dropna().shape[0])

df['antiguedad'] = 2024 - df['YearBuilt']
df['antiguedad_grupo'] = pd.qcut(df['antiguedad'], q=4, labels=['Q1', 'Q2', 'Q3', 'Q4'])

print("\n(c) Cuartiles de antiguedad:")
print(df['antiguedad'].quantile([0, 0.25, 0.5, 0.75, 1.0]))
print("\nConteo por grupo:")
print(df['antiguedad_grupo'].value_counts().sort_index())

#Ejercicio 2

df = pd.read_csv('grupo_1/jerez_tomas/taller_07/titanic.csv', keep_default_na=False, na_values=[''])

q1 = df['fare'].quantile(0.25)
q3 = df['fare'].quantile(0.75)
iqr = q3 - q1
lim_inf = q1 - 1.5 * iqr
lim_sup = q3 + 1.5 * iqr
outliers = df[(df['fare'] < lim_inf) | (df['fare'] > lim_sup)]
pct_outliers = len(outliers) / len(df) * 100
 
print("(a) IQR:", round(iqr, 2))
print("Limites:", round(lim_inf, 2), "a", round(lim_sup, 2))
print("Outliers:", len(outliers), "de", len(df))
print("Porcentaje:", round(pct_outliers, 2), "%")
 
p5 = df['fare'].quantile(0.05)
p95 = df['fare'].quantile(0.95)
fare_wins = df['fare'].clip(lower=p5, upper=p95)
fare_log = np.log1p(df['fare'])
 
fig, axes = plt.subplots(1, 3, figsize=(15, 4))
axes[0].hist(df['fare'].dropna(), bins=40)
axes[0].set_title('fare original')
axes[1].hist(fare_wins.dropna(), bins=40)
axes[1].set_title('fare winsorizada (5-95)')
axes[2].hist(fare_log.dropna(), bins=40)
axes[2].set_title('log(1 + fare)')
plt.tight_layout()
plt.savefig('comparacion_fare.png')
plt.show()
 
print("\n(b) Estadisticas:")
print("Original  - media:", round(df['fare'].mean(), 2), "std:", round(df['fare'].std(), 2))
print("Winsoriz. - media:", round(fare_wins.mean(), 2), "std:", round(fare_wins.std(), 2))
print("Log       - media:", round(fare_log.mean(), 2), "std:", round(fare_log.std(), 2))
 
df_nuevo = df.copy()
df_nuevo['fare'] = df_nuevo['fare'].fillna(df_nuevo['fare'].median()).clip(lower=p5, upper=p95)
df_nuevo['age'] = df_nuevo.groupby('pclass')['age'].transform(lambda x: x.fillna(x.median()))
 
print("\n(c) NA en fare:", df_nuevo['fare'].isna().sum())
print("NA en age:", df_nuevo['age'].isna().sum())
print("Mediana age por pclass:")
print(df_nuevo.groupby('pclass')['age'].median())
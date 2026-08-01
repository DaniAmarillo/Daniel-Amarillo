"""Costo observado por utilización (beneficiario-fecha) y modelo exploratorio M54."""
from collections import Counter, defaultdict

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder

from common import (CHUNKSIZE, DATA, FIGURES, MODELS, RANDOM_STATE, cetipo_category, check_data,
                    clean_cid, clean_cost, clean_id, clean_text, is_positive, save_csv)

COLS=["CID","CHAVE_FUNCIONAL","DT_UTILIZACAO","VALOR_UTILIZACAO","SEXO_BENEFICIARIO","DT_NASCIMENTO_BENEFICIARIO",
      "INTERNADO","UTI","DESC_ESPECIALIDADE","CETIPO","TIPO_UNIDADE_PREST_HOSPITALAR",
      "UF_CNES_PREST_HOSPITALAR"]


def mode(counter): return counter.most_common(1)[0][0] if counter else pd.NA
def stats(values, label):
    s=pd.Series(values,dtype=float); d=s.describe(percentiles=[.25,.5,.75,.9,.95,.99]).to_dict()
    return {"grupo":label,"n_utilizaciones":len(s),"promedio":s.mean(),"mediana":s.median(),"desviacion_estandar":s.std(),
            "p25":s.quantile(.25),"p50":s.quantile(.5),"p75":s.quantile(.75),"p90":s.quantile(.9),"p95":s.quantile(.95),"p99":s.quantile(.99),"minimo":d.get("min"),"maximo":d.get("max")}


def analyze_costs(frame):
    """Genera tablas, modelo y figuras desde el checkpoint consolidado."""
    print("Calculando resúmenes y comparaciones de costos...", flush=True)
    frame["grupo_edad"]=pd.cut(frame.edad,[0,18,30,45,60,75,110],right=False).astype("string")
    m54=frame[frame.m54.eq(1)].copy(); non=frame[frame.m54.eq(0)].copy()
    save_csv(pd.DataFrame([stats(m54.costo,"M54")]),"resumen_costos_dorsalgia.csv")
    save_csv(pd.DataFrame([stats(m54.costo,"M54"),stats(non.costo,"No M54")]),"comparacion_costos_m54_vs_no_m54.csv")
    group_rows=[]
    for variable in ["sexo","grupo_edad","internacion","uti","especialidad","cetipo","unidad_hospitalaria","estado"]:
        for value,part in m54.groupby(variable,dropna=False):
            group_rows.append({"variable":variable,"categoria":value,"n_utilizaciones":len(part),"costo_promedio":part.costo.mean(),"costo_mediano":part.costo.median(),"costo_total":part.costo.sum()})
    save_csv(pd.DataFrame(group_rows),"costos_por_grupo.csv")
    beneficiary=m54.groupby("CHAVE_FUNCIONAL")["costo"].agg(costo_total="sum",n_utilizaciones="size",costo_promedio="mean").reset_index()
    save_csv(beneficiary,"costos_m54_por_beneficiario.csv")

    if len(m54)>=100:
        features=["sexo","edad","internacion","uti","especialidad","cetipo","unidad_hospitalaria","estado"]; X=m54[features]; target=np.log1p(m54.costo.clip(lower=0))
        num=["edad","internacion","uti"]; cat=[c for c in features if c not in num]
        prep=ColumnTransformer([("num",SimpleImputer(strategy="median"),num),("cat",Pipeline([("imp",SimpleImputer(strategy="most_frequent")),("ohe",OneHotEncoder(handle_unknown="ignore"))]),cat)])
        model=Pipeline([("preprocesamiento",prep),("modelo",RandomForestRegressor(n_estimators=250,min_samples_leaf=3,n_jobs=-1,random_state=RANDOM_STATE))])
        train,test=train_test_split(np.arange(len(X)),test_size=.2,random_state=RANDOM_STATE); model.fit(X.iloc[train],target.iloc[train]); prediction=np.expm1(model.predict(X.iloc[test])); actual=m54.costo.iloc[test].to_numpy()
        save_csv(pd.DataFrame([{"modelo":"RandomForestRegressor_log1p","n_train":len(train),"n_test":len(test),"MAE":mean_absolute_error(actual,prediction),"RMSE":mean_squared_error(actual,prediction)**.5,"R2":r2_score(actual,prediction)}]),"metricas_modelo_costos.csv")
        importance=pd.DataFrame({"variable":model.named_steps["preprocesamiento"].get_feature_names_out(),"importancia":model.named_steps["modelo"].feature_importances_}).sort_values("importancia",ascending=False)
        save_csv(importance,"importancia_variables_costos.csv"); top=importance.head(20).sort_values("importancia"); plt.figure(figsize=(9,7)); plt.barh(top.variable,top.importancia); plt.tight_layout(); plt.savefig(FIGURES/"importancia_variables_costos.png",dpi=150); plt.close()
    else:
        save_csv(pd.DataFrame([{"motivo":f"No se entrenó: solo {len(m54)} utilizaciones M54; mínimo definido: 100."}]),"metricas_modelo_costos.csv")
    upper=m54.costo.quantile(.99) if len(m54) else 1; plt.figure(); plt.hist(m54.costo.clip(upper=upper),bins=50); plt.xlabel("Costo M54 (p99)"); plt.tight_layout(); plt.savefig(FIGURES/"distribucion_costos_m54.png",dpi=150); plt.close()
    plt.figure(); plt.boxplot([non.costo,m54.costo],tick_labels=["No M54","M54"],showfliers=False); plt.ylabel("Costo por utilización"); plt.tight_layout(); plt.savefig(FIGURES/"boxplot_costos_m54_vs_no_m54.png",dpi=150); plt.close()
    for col,file in [("internacion","costos_por_internacion.png"),("uti","costos_por_uti.png")]:
        m54.groupby(col).costo.mean().plot(kind="bar"); plt.ylabel("Costo promedio"); plt.tight_layout(); plt.savefig(FIGURES/file,dpi=150); plt.close()
    print(f"Estimación de costos terminada: {len(m54):,} utilizaciones M54 y {len(non):,} no M54.", flush=True)


def main():
    print("Iniciando 08_estimacion_costos.py...", flush=True)
    check_data(COLS)
    checkpoint = MODELS / "checkpoint_costos_utilizacion.joblib"
    if checkpoint.exists():
        print("Checkpoint encontrado: se omite la lectura de la base original.", flush=True)
        analyze_costs(joblib.load(checkpoint))
        return
    uses=defaultdict(lambda:{"m54":0,"cost":0.,"valid":0,"sex":Counter(),"birth":Counter(),"interned":0,"uti":0,"specialty":Counter(),"cetipo":Counter(),"unit":Counter(),"state":Counter()})
    print("Agregando procedimientos a nivel de utilización beneficiario-fecha...", flush=True)
    for number,chunk in enumerate(pd.read_csv(DATA,usecols=COLS,chunksize=CHUNKSIZE,low_memory=False),1):
        ids=clean_id(chunk["CHAVE_FUNCIONAL"]); dates=pd.to_datetime(chunk["DT_UTILIZACAO"],errors="coerce"); cid=clean_cid(chunk["CID"]); cost=clean_cost(chunk["VALOR_UTILIZACAO"])
        birth=pd.to_datetime(chunk["DT_NASCIMENTO_BENEFICIARIO"],errors="coerce"); sex=clean_text(chunk["SEXO_BENEFICIARIO"]); spec=clean_text(chunk["DESC_ESPECIALIDADE"]); unit=clean_text(chunk["TIPO_UNIDADE_PREST_HOSPITALAR"]); state=clean_text(chunk["UF_CNES_PREST_HOSPITALAR"]); cet=cetipo_category(chunk["CETIPO"])
        interned=is_positive(chunk["INTERNADO"]); uti=is_positive(chunk["UTI"])
        for i,beneficiary in ids.items():
            if pd.isna(beneficiary) or pd.isna(dates.at[i]): continue
            key=(str(beneficiary),dates.at[i].strftime("%Y-%m-%d")); s=uses[key]; s["m54"]|=int(str(cid.at[i]).startswith("M54")) if pd.notna(cid.at[i]) else 0
            if pd.notna(cost.at[i]): s["cost"]+=float(cost.at[i]); s["valid"]+=1
            s["interned"]|=int(interned.at[i]); s["uti"]|=int(uti.at[i])
            for value,name in [(sex.at[i],"sex"),(birth.at[i],"birth"),(spec.at[i],"specialty"),(cet.at[i],"cetipo"),(unit.at[i],"unit"),(state.at[i],"state")]:
                if pd.notna(value): s[name][value]+=1
        print(f"Bloque {number}: {len(uses):,} utilizaciones", flush=True)
    print("Lectura terminada. Consolidando utilizaciones con costo válido...", flush=True)
    rows=[]
    total_uses = len(uses)
    for position, ((beneficiary,date),s) in enumerate(uses.items(), start=1):
        if not s["valid"]: continue
        reference = pd.Timestamp(date); b=mode(s["birth"])
        if pd.notna(b):
            age_value = (reference - b).days / 365.2425
            age = round(age_value, 1) if 0 <= age_value <= 110 else np.nan
        else:
            age = np.nan
        rows.append({"CHAVE_FUNCIONAL":beneficiary,"fecha":date,"m54":s["m54"],"costo":s["cost"],"sexo":mode(s["sex"]),"edad":age,
                     "internacion":s["interned"],"uti":s["uti"],"especialidad":mode(s["specialty"]),"cetipo":mode(s["cetipo"]),"unidad_hospitalaria":mode(s["unit"]),"estado":mode(s["state"])})
        if position % 100_000 == 0 or position == total_uses:
            print(f"Consolidación: {position:,} de {total_uses:,} utilizaciones", flush=True)
    frame=pd.DataFrame(rows)
    print("Guardando checkpoint para evitar repetir la lectura si ocurre otro error...", flush=True)
    joblib.dump(frame, checkpoint, compress=3)
    analyze_costs(frame)


if __name__=="__main__": main()

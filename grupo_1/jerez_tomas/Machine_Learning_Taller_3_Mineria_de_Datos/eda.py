import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd

OUT_DIR      = "outputs"
TARGET_LABEL = "Hipertensión (I10-I15)"   
NEG, POS = "#9aa0a6", "#7b2d8b"           

FIG_DIR = os.path.join(OUT_DIR, "figures")
TBL_DIR = os.path.join(OUT_DIR, "tablas")
os.makedirs(FIG_DIR, exist_ok=True)
os.makedirs(TBL_DIR, exist_ok=True)
sns.set_theme(style="whitegrid", palette="deep")


def save(fig, name):
    fig.tight_layout()
    path = os.path.join(FIG_DIR, name)
    fig.savefig(path, dpi=130, bbox_inches="tight")
    plt.close(fig)
    print(f"    figura -> {path}")


def main():
    df = pd.read_parquet(os.path.join(OUT_DIR, "beneficiarios.parquet"))
    n, pos = len(df), int(df["target"].sum())
    prev = pos / n * 100
    print(f">>> Beneficiarios: {n:,} | positivos: {pos:,} | prevalencia: {prev:.3f}%\n")

    num_feats = [c for c in [
        "edad", "ventana_dias", "n_procedimientos", "n_utilizaciones", "n_proc_distintos",
        "n_especialidades", "n_estados", "n_unidades", "n_exame", "n_consulta",
        "n_internacao", "n_terapia", "n_pronto_socorro", "n_cardiologia", "n_clinico",
        "n_internado", "n_uti", "costo_total", "costo_promedio", "costo_max",
    ] if c in df.columns]

    med = df.groupby("target")[num_feats].median().T
    med.columns = ["mediana_neg", "mediana_pos"]
    med["ratio_pos_neg"] = (med["mediana_pos"] /
                            med["mediana_neg"].replace(0, np.nan))
    med = med.sort_values("ratio_pos_neg", ascending=False)
    med.to_csv(os.path.join(TBL_DIR, "comparacion_target.csv"))
    print("=== Mediana por clase (ordenado por ratio pos/neg) ===")
    print(med.round(2).to_string(), "\n")

    fig, ax = plt.subplots(figsize=(5, 4))
    counts = df["target"].value_counts().sort_index()
    ax.bar(["Sin", "Con"], counts.values, color=[NEG, POS])
    for i, v in enumerate(counts.values):
        ax.text(i, v, f"{v:,}", ha="center", va="bottom", fontsize=10)
    ax.set_title(f"Balance de clases — {TARGET_LABEL}\nprevalencia = {prev:.3f}%")
    ax.set_ylabel("Beneficiarios")
    ax.set_yscale("log")
    save(fig, "01_balance_target.png")

    fig, ax = plt.subplots(figsize=(8, 4.5))
    for t, lbl, c in [(0, "Sin", NEG), (1, "Con", POS)]:
        sub = df[df["target"] == t]["edad"].dropna()
        if len(sub) > 1:
            sns.kdeplot(sub, ax=ax, label=lbl, fill=True, alpha=.35, color=c, clip=(0, 110))
    ax.set_title(f"Distribución de edad por clase — {TARGET_LABEL}")
    ax.set_xlabel("Edad"); ax.legend(title="Enfermedad")
    save(fig, "02_edad_por_target.png")

    fig, axes = plt.subplots(1, 2, figsize=(12, 4.5))
    for ax, col, ttl in [(axes[0], "sexo", "Sexo"),
                         (axes[1], "tipo_benef", "Tipo de beneficiario")]:
        ct = pd.crosstab(df[col], df["target"], normalize="columns") * 100
        ct = ct.reindex(ct.mean(axis=1).sort_values(ascending=False).index).head(6)
        ct.columns = ["Sin", "Con"]
        ct.plot(kind="bar", ax=ax, color=[NEG, POS])
        ax.set_title(f"{ttl} por clase (%)"); ax.set_ylabel("%"); ax.set_xlabel("")
        ax.tick_params(axis="x", rotation=30)
    save(fig, "03_sexo_tipo_por_target.png")

    box_feats = [c for c in ["n_utilizaciones", "n_procedimientos", "n_consulta",
                             "n_terapia", "n_cardiologia", "n_internado"] if c in df.columns]
    fig, axes = plt.subplots(2, 3, figsize=(14, 8))
    for ax, feat in zip(axes.ravel(), box_feats):
        data = [df[df["target"] == 0][feat], df[df["target"] == 1][feat]]
        bp = ax.boxplot(data, patch_artist=True, showfliers=False)
        for patch, c in zip(bp["boxes"], [NEG, POS]):
            patch.set_facecolor(c); patch.set_alpha(.6)
        ax.set_xticks([1, 2]); ax.set_xticklabels(["Sin", "Con"])
        ax.set_title(feat); ax.set_yscale("symlog")
    for ax in axes.ravel()[len(box_feats):]:
        ax.axis("off")
    fig.suptitle(f"Uso de servicios por clase — {TARGET_LABEL}", y=1.01)
    save(fig, "04_uso_por_target.png")

    fig, axes = plt.subplots(1, 2, figsize=(12, 4.5))
    ct_pos = df["costo_total_pos"].clip(lower=1)
    axes[0].hist(np.log10(ct_pos), bins=60, color=POS, alpha=.7)
    axes[0].set_title("log10(costo_total positivo)")
    axes[0].set_xlabel("log10(R$)"); axes[0].set_ylabel("Beneficiarios")
    data = [np.log10(df[df["target"] == 0]["costo_total_pos"].clip(lower=1)),
            np.log10(df[df["target"] == 1]["costo_total_pos"].clip(lower=1))]
    bp = axes[1].boxplot(data, patch_artist=True, showfliers=False)
    for patch, c in zip(bp["boxes"], [NEG, POS]):
        patch.set_facecolor(c); patch.set_alpha(.6)
    axes[1].set_xticks([1, 2]); axes[1].set_xticklabels(["Sin", "Con"])
    axes[1].set_title("Costo total por clase (log10)")
    save(fig, "05_costo_distribucion.png")

    corr = df[num_feats].corr(method="spearman")
    fig, ax = plt.subplots(figsize=(11, 9))
    sns.heatmap(corr, cmap="PuOr", center=0, square=True,
                cbar_kws={"shrink": .7}, ax=ax)
    ax.set_title("Correlación de Spearman entre features")
    save(fig, "06_correlacion.png")

    print(f"\n>>> Figuras en {FIG_DIR}")
    print(f">>> Tabla en {os.path.join(TBL_DIR, 'comparacion_target.csv')}")


if __name__ == "__main__":
    main()
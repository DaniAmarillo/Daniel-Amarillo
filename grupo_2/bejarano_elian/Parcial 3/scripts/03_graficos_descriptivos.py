from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns


PROJECT_DIR = Path(__file__).resolve().parents[1]


def main() -> None:
    out_figs = PROJECT_DIR / "output" / "figuras"
    out_tables = PROJECT_DIR / "output" / "tablas"
    out_figs.mkdir(parents=True, exist_ok=True)

    features_path = PROJECT_DIR / "data" / "processed" / "features_cardio_beneficiario.csv.gz"
    if not features_path.exists():
        features_path = PROJECT_DIR / "data" / "processed" / "features_cardio_beneficiario.csv"
    features = pd.read_csv(features_path)
    costs = pd.read_csv(out_tables / "costos_utilizacion_cardio_i20_i25.csv")
    sns.set_theme(style="whitegrid")

    target_counts = features["cardio_isquemica"].value_counts().sort_index().reset_index()
    target_counts.columns = ["cardio_isquemica", "n"]
    target_counts["grupo"] = target_counts["cardio_isquemica"].map({0: "No I20-I25", 1: "I20-I25"})
    plt.figure(figsize=(6, 4))
    ax = sns.barplot(data=target_counts, x="grupo", y="n", color="#2f6f9f")
    ax.set_yscale("log")
    ax.set_xlabel("")
    ax.set_ylabel("Beneficiarios (escala log)")
    ax.set_title("Distribucion de la variable objetivo")
    plt.tight_layout()
    plt.savefig(out_figs / "distribucion_objetivo.png", dpi=180)
    plt.close()

    age_df = features[["cardio_isquemica", "edad_referencia"]].dropna().copy()
    age_df["grupo"] = age_df["cardio_isquemica"].map({0: "No I20-I25", 1: "I20-I25"})
    plt.figure(figsize=(7, 4))
    sns.boxplot(data=age_df, x="grupo", y="edad_referencia", showfliers=False, color="#80b1d3")
    plt.xlabel("")
    plt.ylabel("Edad de referencia")
    plt.title("Edad por grupo objetivo")
    plt.tight_layout()
    plt.savefig(out_figs / "edad_por_objetivo.png", dpi=180)
    plt.close()

    cost = costs["costo_utilizacion"].clip(lower=0)
    log_cost = np.log10(cost + 1)
    quantiles = cost.quantile([0.25, 0.50, 0.75, 0.95])
    mean_cost = cost.mean()
    max_cost = cost.max()
    zero_count = int((cost == 0).sum())

    tick_values = [0, 100, 500, 1000, 5000, 10000, 50000, 250000]
    tick_positions = [np.log10(v + 1) for v in tick_values]
    tick_labels = ["0", "100", "500", "1k", "5k", "10k", "50k", "250k"]

    fig, axes = plt.subplots(
        1,
        2,
        figsize=(12, 4.8),
        gridspec_kw={"width_ratios": [1.25, 1]},
    )

    sns.histplot(log_cost, bins=34, kde=True, color="#4c956c", edgecolor="white", ax=axes[0])
    axes[0].set_title("Histograma en escala log10")
    axes[0].set_xlabel("Costo por utilizacion I20-I25")
    axes[0].set_ylabel("Numero de utilizaciones")
    axes[0].set_xticks(tick_positions)
    axes[0].set_xticklabels(tick_labels, rotation=25, ha="right")

    line_specs = [
        ("Q1", quantiles.loc[0.25], "#7b2d26"),
        ("Mediana", quantiles.loc[0.50], "#1f4e79"),
        ("Q3", quantiles.loc[0.75], "#7b2d26"),
        ("P95", quantiles.loc[0.95], "#b5651d"),
    ]
    ymax = axes[0].get_ylim()[1]
    for label, value, color in line_specs:
        xpos = np.log10(value + 1)
        axes[0].axvline(xpos, color=color, linestyle="--", linewidth=1.5)
        axes[0].text(xpos, ymax * 0.93, label, rotation=90, color=color, ha="right", va="top", fontsize=9)

    sorted_log = np.sort(log_cost)
    ecdf = np.arange(1, len(sorted_log) + 1) / len(sorted_log) * 100
    axes[1].plot(sorted_log, ecdf, color="#2f6f9f", linewidth=2.2)
    axes[1].set_title("Curva acumulada")
    axes[1].set_xlabel("Costo por utilizacion I20-I25")
    axes[1].set_ylabel("% acumulado de utilizaciones")
    axes[1].set_xticks(tick_positions)
    axes[1].set_xticklabels(tick_labels, rotation=25, ha="right")
    axes[1].set_ylim(0, 101)
    axes[1].set_yticks([0, 25, 50, 75, 90, 95, 100])
    for label, value, color in line_specs:
        axes[1].axvline(np.log10(value + 1), color=color, linestyle="--", linewidth=1.2, alpha=0.9)
    for pct in [50, 75, 95]:
        axes[1].axhline(pct, color="#777777", linestyle=":", linewidth=0.8)

    summary_text = (
        f"n = {len(cost):,}\n"
        f"media = {mean_cost:,.0f}\n"
        f"mediana = {quantiles.loc[0.50]:,.0f}\n"
        f"P95 = {quantiles.loc[0.95]:,.0f}\n"
        f"max = {max_cost:,.0f}\n"
        f"costo 0 = {zero_count}"
    )
    axes[1].text(
        0.04,
        0.96,
        summary_text,
        transform=axes[1].transAxes,
        va="top",
        ha="left",
        fontsize=9,
        bbox={"boxstyle": "round,pad=0.35", "facecolor": "white", "edgecolor": "#c7c7c7", "alpha": 0.95},
    )

    fig.suptitle("Distribucion asimetrica del costo asociado a cardiopatia isquemica", fontsize=14, y=1.02)
    fig.tight_layout()
    fig.savefig(out_figs / "costos_cardio_utilizacion.png", dpi=180, bbox_inches="tight")
    plt.close()

    imp_path = out_tables / "permutation_importance_random_forest.csv"
    if imp_path.exists():
        imp = pd.read_csv(imp_path).head(12).sort_values("importancia_media_pr_auc")
        plt.figure(figsize=(8, 5))
        sns.barplot(data=imp, x="importancia_media_pr_auc", y="feature_original", color="#4b6f44")
        plt.xlabel("Caida media en PR-AUC al permutar")
        plt.ylabel("")
        plt.title("Importancia por permutacion - Random Forest")
        plt.tight_layout()
        plt.savefig(out_figs / "importancia_permutacion_random_forest.png", dpi=180)
        plt.close()


if __name__ == "__main__":
    main()

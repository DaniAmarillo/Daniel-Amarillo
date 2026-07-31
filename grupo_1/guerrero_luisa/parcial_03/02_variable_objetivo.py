"""Construcción de dorsalgia (CID M54*) a nivel de beneficiario."""
import matplotlib.pyplot as plt
import pandas as pd

from common import CHUNKSIZE, DATA, FIGURES, check_data, clean_cid, clean_id, ensure_directories, save_csv


def main() -> None:
    ensure_directories(); check_data(["CID", "CHAVE_FUNCIONAL"])
    status: dict[str, int] = {}
    for number, chunk in enumerate(pd.read_csv(DATA, usecols=["CID", "CHAVE_FUNCIONAL"],
                                                chunksize=CHUNKSIZE, low_memory=False), 1):
        ids = clean_id(chunk["CHAVE_FUNCIONAL"]); cid = clean_cid(chunk["CID"])
        valid = ids.notna()
        part = pd.DataFrame({"CHAVE_FUNCIONAL": ids[valid],
                             "dorsalgia": cid[valid].str.startswith("M54", na=False).astype(int)})
        maxima = part.groupby("CHAVE_FUNCIONAL")["dorsalgia"].max()
        for key, value in maxima.items():
            if value or key not in status:
                status[key] = int(value)
        print(f"Bloque {number}: {len(status):,} beneficiarios acumulados")
    target = pd.Series(status, name="dorsalgia").rename_axis("CHAVE_FUNCIONAL").reset_index()
    save_csv(target, "variable_objetivo_dorsalgia.csv")
    counts = target["dorsalgia"].value_counts().reindex([0, 1], fill_value=0)
    distribution = pd.DataFrame({"dorsalgia": counts.index, "n_beneficiarios": counts.values})
    distribution["porcentaje"] = 100 * distribution["n_beneficiarios"] / max(len(target), 1)
    save_csv(distribution, "distribucion_objetivo.csv")
    print(f"Total: {len(target):,}\nCon dorsalgia: {counts[1]:,}\nSin dorsalgia: {counts[0]:,}\nPorcentaje: {distribution.loc[distribution.dorsalgia.eq(1), 'porcentaje'].iloc[0]:.2f}%")
    plt.figure(figsize=(6, 4)); plt.bar(["Sin dorsalgia", "Dorsalgia"], counts.values)
    plt.ylabel("Beneficiarios"); plt.tight_layout(); plt.savefig(FIGURES / "distribucion_objetivo.png", dpi=150); plt.close()


if __name__ == "__main__":
    main()

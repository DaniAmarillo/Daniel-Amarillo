from __future__ import annotations

import warnings

warnings.filterwarnings("ignore")

from src.build_dataset import ejecutar_pipeline


def main() -> None:
    print("Ejecutando pipeline de datos (limpieza + variable objetivo + variables)...")
    beneficiarios, df = ejecutar_pipeline()

    print("\nPipeline de datos completado.")
    print(f"Beneficiarios procesados: {len(beneficiarios):,}")
    print(f"Positivos A09: {beneficiarios['TARGET_A09'].sum():,}")
    print(
        "\nPara el modelamiento, interpretabilidad y estimación de costos, "
        "ejecutar notebooks/taller_3.ipynb, que reutiliza los mismos módulos "
        "de src/ documentados en este proyecto."
    )


if __name__ == "__main__":
    main()

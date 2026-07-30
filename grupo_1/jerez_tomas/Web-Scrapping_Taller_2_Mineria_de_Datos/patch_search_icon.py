from __future__ import annotations

import argparse
import shutil
from pathlib import Path


SVG_SEARCH_LINE = (
    '    const SVG_SEARCH = "data:image/svg+xml;utf8,'
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' "
    "stroke-linejoin='round'><circle cx='11' cy='11' r='8'/>"
    "<line x1='21' y1='21' x2='16.65' y2='16.65'/></svg>\";"
)

ROUTE_LINE = "        if (path.endsWith('/search')) return SVG_SEARCH;"


ANCHOR_CONST = "const SVG_INFO ="           
ANCHOR_ROUTE = "if (path.endsWith('/about')) return SVG_INFO;"


def patch(text: str) -> tuple[str, list[str]]:
    """Devuelve (texto_parcheado, lista_de_cambios)."""
    changes: list[str] = []
    lines = text.splitlines()


    if "SVG_SEARCH" in text:
        changes.append("· La constante SVG_SEARCH ya existía; no se toca.")
    else:
        idx = next((i for i, l in enumerate(lines) if ANCHOR_CONST in l), None)
        if idx is None:
            raise RuntimeError(
                f"No se encontró la línea de anclaje {ANCHOR_CONST!r}. "
                "¿Es este el ui_components.py del Taller 2?"
            )
        lines.insert(idx + 1, SVG_SEARCH_LINE)
        changes.append(f"· Constante SVG_SEARCH insertada tras la línea {idx + 1}.")

    if "'/search'" in "\n".join(lines):
        changes.append("· La ruta '/search' ya estaba mapeada; no se toca.")
    else:
        idx = next((i for i, l in enumerate(lines) if ANCHOR_ROUTE in l), None)
        if idx is None:
            raise RuntimeError(
                f"No se encontró la línea de anclaje {ANCHOR_ROUTE!r}."
            )
        lines.insert(idx + 1, ROUTE_LINE)
        changes.append(f"· Ruta '/search' añadida tras la línea {idx + 1}.")

    return "\n".join(lines) + "\n", changes


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--file", type=Path, default=Path("ui_components.py"))
    ap.add_argument("--dry-run", action="store_true",
                    help="muestra el resultado sin escribir el archivo")
    args = ap.parse_args()

    if not args.file.exists():
        raise SystemExit(
            f"No se encontró {args.file}. Ejecuta el script desde la carpeta "
            "raíz del proyecto (la que contiene app.py)."
        )

    original = args.file.read_text(encoding="utf-8")
    patched, changes = patch(original)

    print("=" * 68)
    print(" PARCHE · icono SVG del buscador en el menú lateral")
    print("=" * 68)
    for c in changes:
        print(" ", c)

    if patched == original:
        print("\nEl archivo ya estaba al día. No se ha modificado nada.")
        return

    if args.dry_run:
        print("\n--- Líneas que se añadirían ---")
        for line in patched.splitlines():
            if "SVG_SEARCH" in line:
                print("  +", line.strip()[:100] + ("…" if len(line.strip()) > 100 else ""))
        print("\n(--dry-run: no se ha escrito nada)")
        return

    backup = args.file.with_suffix(args.file.suffix + ".bak")
    shutil.copy(args.file, backup)
    args.file.write_text(patched, encoding="utf-8")

    print(f"\n  · Copia de seguridad: {backup.name}")
    print(f"  · Archivo actualizado: {args.file.name}")
    print("\nListo. Reinicia Streamlit y recarga con Ctrl+Shift+R.")


if __name__ == "__main__":
    main()

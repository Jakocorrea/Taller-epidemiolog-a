#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import math
import argparse
import textwrap
import subprocess
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# ---------- Utilidades de formato ----------
def formatea_miles(n):
    if pd.isna(n):
        return ""
    try:
        n_int = int(round(float(n)))
        return f"{n_int:,}".replace(",", ".")
    except Exception:
        return str(n)

def formatea_pct(x, dec=2):
    if pd.isna(x):
        return ""
    try:
        val = float(x)
        s = f"{val:.{dec}f}".replace(".", ",")
        return f"{s} %"
    except Exception:
        return str(x)

def envuelve(texto, ancho=28):
    if pd.isna(texto):
        return ""
    return "\n".join(textwrap.wrap(str(texto), width=ancho, break_long_words=False))

# ---------- Preparación de dataframe "para mostrar" ----------
def df_para_mostrar(df):
    df = df.copy()
    colmap = {
        "Variable": "Variable",
        "Categoria": "Categoría",
        "Frecuencia": "Frecuencia",
        "Porcentaje": "Porcentaje",
        "Prevalencia_SO_pct": "Prevalencia SO"
    }
    lower = {c.lower(): c for c in df.columns}
    for k, v in list(colmap.items()):
        if k not in df.columns:
            if k.lower() in lower:
                df.rename(columns={lower[k.lower()]: v}, inplace=True)
        else:
            df.rename(columns={k: v}, inplace=True)

    if "Frecuencia" in df.columns:
        df["Frecuencia"] = pd.to_numeric(df["Frecuencia"], errors="coerce")
    if "Porcentaje" in df.columns:
        df["Porcentaje"] = pd.to_numeric(df["Porcentaje"], errors="coerce")
    if "Prevalencia SO" in df.columns:
        df["Prevalencia SO"] = pd.to_numeric(df["Prevalencia SO"], errors="coerce")

    if "Frecuencia" in df.columns:
        df["Frecuencia"] = df["Frecuencia"].map(formatea_miles)
    if "Porcentaje" in df.columns:
        df["Porcentaje"] = df["Porcentaje"].map(lambda x: formatea_pct(x, dec=2))
    if "Prevalencia SO" in df.columns:
        df["Prevalencia SO"] = df["Prevalencia SO"].map(lambda x: formatea_pct(x, dec=2))

    if "Variable" in df.columns:
        df["Variable"] = df["Variable"].map(lambda s: envuelve(s, ancho=20))
    if "Categoría" in df.columns:
        df["Categoría"] = df["Categoría"].map(lambda s: envuelve(s, ancho=25))

    orden = [c for c in ["Variable", "Categoría", "Frecuencia", "Porcentaje", "Prevalencia SO"] if c in df.columns]
    df = df[orden]
    return df

# ---------- Cálculo de anchos ----------
def estimar_col_widths(df, min_w=0.10, max_w=0.45):
    lens = []
    for col in df.columns:
        valores = df[col].astype(str).fillna("")
        max_len = max([len(x) for x in valores] + [len(col)])
        mean_len = valores.map(len).mean()
        score = 0.7 * max_len + 0.3 * mean_len
        lens.append(score)
    total = sum(lens) if sum(lens) > 0 else 1.0
    widths = [max(min_w, min(max_w, L / total)) for L in lens]
    s = sum(widths)
    widths = [w / s * 0.96 for w in widths]
    return widths

# ---------- Render tabla ----------
def render_tabla_png(df, titulo, salida_png, dpi=300, filas_por_pagina=28, margen_sup=0.8):
    df_disp = df_para_mostrar(df)
    n = len(df_disp)
    if n == 0:
        raise ValueError("El dataframe está vacío; nada que renderizar.")
    paginas = int(math.ceil(n / float(filas_por_pagina)))
    if os.path.dirname(salida_png):
        os.makedirs(os.path.dirname(salida_png), exist_ok=True)

    imgs = []
    for p in range(paginas):
        ini = p * filas_por_pagina
        fin = min((p + 1) * filas_por_pagina, n)
        df_pag = df_disp.iloc[ini:fin].reset_index(drop=True)

        col_widths = estimar_col_widths(df_pag)
        alto_fila = 0.45
        h = margen_sup + alto_fila * (len(df_pag) + 1)
        w = sum(col_widths) * 20.0

        fig, ax = plt.subplots(figsize=(w, h), dpi=dpi)
        ax.axis("off")

        ax.set_title(titulo + (f" (página {p+1}/{paginas})" if paginas > 1 else ""),
                     fontsize=14, weight="bold", pad=18)

        tabla = ax.table(cellText=df_pag.values,
                         colLabels=df_pag.columns,
                         colWidths=col_widths,
                         cellLoc="left",
                         loc="upper center")
        tabla.auto_set_font_size(False)
        tabla.set_fontsize(10)

        ncols = df_pag.shape[1]
        for j in range(ncols):
            c = tabla[0, j]
            c.set_text_props(weight="bold", color="white")
            c.set_facecolor("#333333")
            c.set_edgecolor("#555555")

        nrows = df_pag.shape[0]
        for i in range(1, nrows + 1):
            for j in range(ncols):
                cell = tabla[i, j]
                cell.set_edgecolor("#D0D0D0")
                cell.set_linewidth(0.6)
                cell.set_facecolor("#FAFAFA" if i % 2 == 0 else "#FFFFFF")

        plt.tight_layout()
        base, ext = os.path.splitext(salida_png)
        out_path = f"{base}_p{p+1}.png" if paginas > 1 else salida_png
        fig.savefig(out_path, dpi=dpi, bbox_inches="tight")
        plt.close(fig)
        imgs.append(out_path)
    return imgs

# ---------- Abrir archivo(s) ----------
def abrir_archivo(path):
    try:
        if sys.platform.startswith("darwin"):      # macOS
            subprocess.run(["open", path], check=False)
        elif os.name == "nt":                      # Windows
            try:
                os.startfile(path)                 # type: ignore[attr-defined]
            except AttributeError:
                subprocess.run(["start", path], shell=True, check=False)
        else:                                      # Linux / otros
            subprocess.run(["xdg-open", path], check=False)
        return True
    except Exception:
        return False

# ---------- CLI ----------
def main():
    parser = argparse.ArgumentParser(description="Convierte un CSV en una tabla PNG bonita y la abre al terminar.")
    parser.add_argument("-i", "--input", required=False, default="tabla_punto7.csv",
                        help="CSV de entrada (default: tabla_punto7.csv). Si no existe, intenta 'taller2_tabla7_pub.csv'.")
    parser.add_argument("-o", "--output", required=False, default="tabla_punto7.png",
                        help="PNG de salida (si hay varias páginas: _p1, _p2, ...).")
    parser.add_argument("-t", "--title", required=False, default="Tabla 7. Prevalencia de SO por categoría",
                        help="Título mostrado arriba de la tabla.")
    parser.add_argument("-r", "--rows", required=False, type=int, default=28,
                        help="Filas por página.")
    parser.add_argument("--dpi", required=False, type=int, default=300, help="Resolución del PNG.")
    parser.add_argument("--no-open", action="store_true", help="No abrir automáticamente las imágenes generadas.")
    args = parser.parse_args()

    # Resolver CSV por defecto si no existe el primero
    csv_in = args.input
    if not os.path.exists(csv_in):
        alt = "Taller 2/Valentina/tabla_punto7.csv"
        if os.path.exists(alt):
            print(f"[INFO] No se encontró '{csv_in}'. Usando '{alt}'.")
            csv_in = alt
        else:
            raise FileNotFoundError(f"No se encontró '{args.input}' ni '{alt}' en la carpeta actual.")

    df = pd.read_csv(csv_in)
    imgs = render_tabla_png(df, args.title, args.output, dpi=args.dpi, filas_por_pagina=args.rows)

    print("[OK] Imágenes generadas:")
    for im in imgs:
        print("  -", im)

    if not args.no_open:
        print("[INFO] Abriendo imagen(es)...")
        for im in imgs:
            ok = abrir_archivo(im)
            if not ok:
                print(f"[WARN] No se pudo abrir: {im}")

if __name__ == "__main__":
    main()

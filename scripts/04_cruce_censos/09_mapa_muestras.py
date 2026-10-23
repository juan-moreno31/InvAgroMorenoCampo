# Mapa de muestras. Correr desde la raiz del repo, despues de 09_muestras_mapa.R
import json, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from shapely.geometry import shape
from shapely.ops import unary_union
plt.rcParams.update({"font.family": "Liberation Sans", "font.size": 9})
g = json.load(open("datos_mapa/col_municipios.geojson", encoding="utf-8"))["features"]
A = set(pd.read_csv("datos/muestraA.csv", dtype=str).cod_mpio)
B = set(pd.read_csv("datos/muestraB.csv", dtype=str).cod_mpio)
geo = {}
for f in g:
    props = f["properties"]
    c = props.get("MPIO_CCNCT", props.get("divipola_mun"))
    if c.startswith("88"):  # San Andrés: fuera del encuadre
        continue
    s = shape(f["geometry"])
    geo[c] = s if s.is_valid else s.buffer(0)  # repara poligonos invalidos del geojson
print("A sin geometría:", sorted(A - set(geo))[:10], "| B sin geometría:", sorted(B - set(geo))[:10])
dptos = {}
for c, s in geo.items(): dptos.setdefault(c[:2], []).append(s)
dbound = {k: unary_union(v) for k, v in dptos.items()}
IN, OUT, EDGE, DEDGE = "#2E5984", "#E4E3DE", "#FFFFFF", "#5B6573"
def draw(ax, s, color, ec, lw):
    polys = [s] if s.geom_type == "Polygon" else list(s.geoms)
    for p in polys:
        x, y = p.exterior.xy
        ax.fill(x, y, facecolor=color, edgecolor=ec, linewidth=lw)
def boundary(ax, s):
    polys = [s] if s.geom_type == "Polygon" else list(s.geoms)
    for p in polys:
        x, y = p.exterior.xy
        ax.plot(x, y, color=DEDGE, linewidth=0.45)
fig, axes = plt.subplots(1, 2, figsize=(10, 6.2))
for ax, S, tit in [(axes[0], A, f"A. Muestra principal (CNA 2014)\n{len(A):,} municipios · 30 departamentos".replace(",", ".")),
                   (axes[1], B, f"B. Submuestra con censo de 1960\n{len(B)} municipios · 14 departamentos")]:
    for c, s in geo.items():
        draw(ax, s, IN if c in S else OUT, EDGE, 0.15)
    for s in dbound.values(): boundary(ax, s)
    ax.set_title(tit, fontsize=10.5, color="#182B49", fontweight="bold", loc="left")
    ax.set_aspect("equal"); ax.axis("off")
fig.legend(handles=[Patch(facecolor=IN, label="En la muestra"), Patch(facecolor=OUT, label="Fuera de la muestra")],
           loc="lower center", ncol=2, frameon=False, fontsize=9)
fig.text(0.01, 0.01, "Muestra A incluye San Andrés y Providencia (no se muestran). Cartografía: DANE, MGN 2018.", fontsize=7.5, color="#52514e")
fig.tight_layout(rect=(0, 0.05, 1, 1))
fig.savefig("Presentación/mapa_muestras.png", dpi=250)
print("ok")

import pandas as pd

# Cargar base cruda descargada de la URT
raw_path = "datos/solicitudes_restitucion_urt_municipio.csv"
df = pd.read_csv(raw_path)

# Limpiar y normalizar código DANE a 5 dígitos
df['cod_mpio'] = (
    df['CodigoDANEMunicipio']
    .astype(str)
    .str.replace(',', '', regex=False)
    .str.replace('.', '', regex=False)
    .str.strip()
    .apply(lambda x: x.zfill(5) if x.isdigit() else x)
)

df['cod_dpto'] = df['cod_mpio'].str[:2]

# Limpiar variables numéricas (remover comas y puntos de miles)
cols_map = {
    'NumerodeSolicitudesenelSRTDAF': 'n_solicitudes_urt',
    'NumerodePredios': 'n_predios_urt',
    'NumerodeTitulares': 'n_titulares_urt',
    'NumerodeSolicitudesInscritasenelSRTDAF': 'n_inscritas_urt',
    'NumerodeSolicitudesNoInscritasenelSRTDAF': 'n_no_inscritas_urt',
    'NumerodeSolicitudesenTramiteAdministrativoVigente': 'n_tramite_urt',
    'NumeroSolicitudesMicrofocalizadas': 'n_microfocalizadas_urt'
}

for orig, new_col in cols_map.items():
    if orig in df.columns:
        s = df[orig].astype(str).str.replace(',', '', regex=False).str.replace('.', '', regex=False).str.strip()
        df[new_col] = pd.to_numeric(s, errors='coerce').fillna(0).astype(int)

# Columnas finales seleccionadas y ordenadas
cols_keep = [
    'cod_mpio', 'cod_dpto', 'DepartamentodelPredio', 'MunicipiodelPredio',
    'n_solicitudes_urt', 'n_predios_urt', 'n_titulares_urt',
    'n_inscritas_urt', 'n_no_inscritas_urt', 'n_tramite_urt', 'n_microfocalizadas_urt'
]

df_clean = df[cols_keep].copy()
df_clean.rename(columns={
    'DepartamentodelPredio': 'departamento',
    'MunicipiodelPredio': 'municipio'
}, inplace=True)

df_clean.to_csv("datos/solicitudes_restitucion_urt_municipio.csv", index=False)
print(f"Base limpia guardada exitosamente: {len(df_clean)} municipios.")
print(df_clean.head())

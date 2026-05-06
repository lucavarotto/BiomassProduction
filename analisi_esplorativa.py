import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Caricamento corretto saltando le righe di intestazione sporche
df = pd.read_excel('nostoc.xlsx', skiprows=2, names=['RowID', 'I', 'D', 'P', 'tempo', 'OD'])

# Pulizia: eliminiamo la colonna RowID e creiamo l'ID biomassa (1 a 53, ciascuno per 7 rilevazioni)
df = df.drop(columns=['RowID'])
df['id_biomassa'] = [i for i in range(1, 54) for _ in range(7)]

# --- 1. Visualizzazione delle Traiettorie ---
plt.figure(figsize=(10, 6))
sns.lineplot(data=df, x='tempo', y='OD', units='id_biomassa', estimator=None, color='skyblue', alpha=0.4)
sns.lineplot(data=df, x='tempo', y='OD', color='red', linewidth=2.5, label='Traiettoria Media')
plt.title('Traiettorie di crescita (OD) per le 53 biomasse', fontsize=14)
plt.xlabel('Tempo (giorni)')
plt.ylabel('Densità Ottica (OD)')
plt.legend()
plt.grid(True, alpha=0.3)
plt.savefig('traiettorie_crescita.png')

# --- 2. Analisi dell'effetto delle covariate al tempo finale (T=6) ---
df_t6 = df[df['tempo'] == 6]

fig, axes = plt.subplots(1, 3, figsize=(18, 5))
sns.boxplot(ax=axes[0], x='I', y='OD', data=df_t6)
axes[0].set_title('Effetto di I (Irradianza)')
sns.boxplot(ax=axes[1], x='D', y='OD', data=df_t6)
axes[1].set_title('Effetto di D (Diluizione)')
sns.boxplot(ax=axes[2], x='P', y='OD', data=df_t6)
axes[2].set_title('Effetto di P (Fosfati)')
plt.suptitle('Impatto delle covariate sulla Biomassa Finale (T=6)', fontsize=16)
plt.savefig('effetti_covariate.png')

# --- 3. Statistiche Descrittive ---
stats = df.groupby('tempo')['OD'].agg(['mean', 'std', 'min', 'max']).round(3)
print(stats)
library(tidyverse)

# Dados TSE (Brasil)

tse <- read_csv2(
  "data/consulta_cand_2026/consulta_cand_2026_BRASIL.csv",
  locale = locale(encoding = "latin1")
)

glimpse(tse)

# Quantos candidatos se candidataram usando o sobrenome "Bolsonaro"

# no nome da urna?

tse_nome <- tse |>
  select(NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO)

# pacote stringr

urna_bolsonaro <- tse_nome |>
  filter(str_detect(NM_URNA_CANDIDATO, "BOLSONARO"))

urna_bolsonaro

delegados <- tse |>
  filter(str_detect(NM_URNA_CANDIDATO, "DELEGADO")) |>
  select(NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_OCUPACAO)

tse |>
  filter(str_detect(NM_URNA_CANDIDATO, "PASTOR")) |>
  select(NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_OCUPACAO) |>
  View()

# Nomes coletivos

tse |>
  filter(str_detect(NM_URNA_CANDIDATO, "COLETIVO")) |>
  select(NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_OCUPACAO) |>
  View()

# Candidatas mulheres por partido

partido_genero <- tse |>
  group_by(SG_PARTIDO, DS_GENERO) |>
  summarise(total_candidatos = n()) |>
  ungroup()

partido_genero_wide <- partido_genero |>
  pivot_wider(names_from = DS_GENERO, values_from = total_candidatos)

partido_genero_wide |>
  mutate(pct_feminino = FEMININO / (FEMININO + MASCULINO)) |>
  View()

# Outra abordagem:

tse |>
  group_by(SG_PARTIDO) |>
  summarise(
    total = n(),
    feminino = sum(DS_GENERO == "FEMININO"),
    pct = feminino / total
  ) |>
  View()

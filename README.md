# Análise de Sobreviência - Intervalo Parto-Concepção de Vacas

**Avaliação dos fatores clínicos, reprodutivos, produtivos e ambientais no intervalo parto-concepção em vacas leiteiras**

Aplica análise de sobrevivência em R para investigar quais fatores influenciam o tempo entre o parto e a concepção em vacas leiteiras, considerando como censura as vacas que não conceberam no período observado.

## Objetivo

Estimar a associação entre o tempo até a concepção (dias pós-parto) e variáveis clínicas, reprodutivas, produtivas e ambientais, usando curvas de Kaplan-Meier, testes de log-rank e modelos de regressão de Cox.


## Dados


| Variável | Descrição |
|---|---|
| `ID_vaca` | Identificador do animal |
| `tempo_concep` | Tempo até a concepção (dias pós-parto) |
| `indic_tempo_concep` | Indicador do evento: `1` = concepção, `0` = censura |
| `leite_dia` | Produção diária de leite (litros), variável contínua |
| `producao_leite` | Produção de leite categorizada (abaixo / acima da mediana) |
| `Cidade` | Cidade de origem (A ou B) |
| `Estacao` | Estação do ano (outono, inverno, primavera, verão) |
| `problema_pre_parto` | Problema clínico pré-parto (não / sim) |
| `problema_pos_parto` | Problema clínico pós-parto (não / sim) |
| `sistema_reproducao` | Sistema reprodutivo (IA ou monta) |
| `tempo_parto_insem` | Intervalo parto-inseminação (até 85 dias / mais de 85 dias) |

## Metodologia

1. **Análise descritiva**: percentual de dados faltantes, resumo de `leite_dia`, proporção de censura vs. concepção, frequência das variáveis categóricas, histogramas e boxplots.
2. **Tratamento de dados faltantes**
   - `missForest`: imputação única, usada nas curvas de Kaplan-Meier e nos testes de log-rank.
   - `mice`: imputação múltipla (5 bancos), usada nos modelos de Cox multivariados.
3. **Análise não paramétrica**: curvas de Kaplan-Meier (concepção acumulada) e testes de log-rank para cada variável explicativa.
4. **Regressão de Cox**
   - Modelos univariados, com razões de risco (HR) e IC 95%.
   - Modelo multivariado com estimativas combinadas pelas regras de Rubin (`pool`).
   - Modelo alternativo sem `tempo_parto_insem`.
5. **Diagnóstico dos modelos**
   - Riscos proporcionais (`cox.zph`, resíduos de Schoenfeld).
   - Linearidade de `leite_dia` (comparação com spline natural, `ns(df = 3)`, por teste da razão de verossimilhança, e resíduos de Martingale).
   - Observações atípicas e influentes (resíduos de deviance e DFBeta).
   - Índice de concordância.
6. **Análise de sensibilidade**: reajuste do modelo sem a observação mais influente (nº 78) para comparar as HR.


## Autora

Isadora Fumagali — [@isadorafumagali](https://github.com/isadorafumagali)

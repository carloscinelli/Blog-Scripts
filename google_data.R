# Limpa área de trabalho
rm(list = ls())

# Carrega pacotes
library(dplyr)
library(jsonlite)


# Faz unzip dos dados do Google
# Troque o nome do arquivo pelo nome do seu arquivo
unzip("Searches-20160424T141843Z.zip")

# Lê e trata cada um dos JSON 
# salva resultados em uma lista
pasta <- "Searches/Searches/"
files <- list.files(pasta)
df_list <- vector("list", length(files))
for (i in seq_along(files)) {
  cat("Processando arquivo", files[[i]], "\n")
  dados <- fromJSON(txt = paste0(pasta, files[[i]]))
  dados <- dados[[1]][[1]]
  dados[[1]] <- sapply(dados[[1]], function(x)x[[1]][[1]])
  names(dados) <- c("Data", "Texto")
  dados$Data  <- substr(dados$Data, 1, 10)
  dados$Data  <- as.POSIXct(as.numeric(dados$Data), origin = '1970-01-01 00:00.00 UTC')
  df_list[[i]] <- dados
}

# Empilha todos os data.frames
dados <- bind_rows(df_list)

# Calcula ano, dia, mes, hora etc...
meses <- c("Janeiro","Fevereiro", "Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
dias_semana <- c("Segunda Feira", "Terça Feira", "Quarta Feira", "Quinta Feira", "Sexta Feira", "Sábado", "Domingo")
dados <- dados %>% mutate(ano = as.numeric(format(Data, "%Y")),
                          mes  = factor(format(Data, "%B"), levels = meses),
                          ano_mes = as.numeric(format(Data, "%Y"))*100 + as.numeric(format(Data, "%m")),
                          ano_mes = factor(ano_mes, levels = unique(sort(ano_mes))),
                          dia  = as.numeric(format(Data, "%d")),
                          hora = as.numeric(format(Data, "%H")),
                          dia_semana = factor(format(Data, "%A"), levels = dias_semana),
                          data = as.Date(format(Data, "%Y-%m-%d")))

# Calcula frequencias
freqs <- dados %>% group_by(data, ano, mes, dia, ano_mes, dia_semana, hora) %>% summarise(n = n()) %>% ungroup()

# Preenche dias e horas não existentes com zeros
zeros <- data.frame(Data = seq.POSIXt(from = min(dados$Data), to = max(dados$Data), by = "1 hour"))
zeros <- zeros %>% transmute(data = as.Date(format(Data, "%Y-%m-%d")),
                             ano = as.numeric(format(Data, "%Y")),
                             mes  = factor(format(Data, "%B"), levels = meses),
                             dia  = as.numeric(format(Data, "%d")),
                             ano_mes = as.numeric(format(Data, "%Y"))*100 + as.numeric(format(Data, "%m")),
                             ano_mes = factor(ano_mes, levels = unique(sort(ano_mes))),
                             dia_semana = factor(format(Data, "%A"), levels = dias_semana),
                             hora = as.numeric(format(Data, "%H")),
                             n = 0)
zeros <- zeros[names(freqs)]
manter <- !(with(zeros, paste0(data, hora)) %in% with(freqs, paste0(data, hora)))
zeros <- zeros[manter, ]
freqs <- bind_rows(zeros, freqs)

# Exemplo: média de pesquisas por hora
media_por_hora <- freqs %>% group_by(data, hora) %>% summarise(n = sum(n)) %>%
  group_by(hora) %>% summarise(media = mean(n))

# Exemplo: média de pesquisas por dia da semana
media_por_dia_semana <- freqs %>% group_by(data, dia_semana) %>% summarise(n = sum(n)) %>%
  group_by(dia_semana) %>% summarise(media = mean(n))
## CODE MUSITELLI AURORA MATRICOLA 856741 ##

##------------------------------------------- 0.CARICO LE LIBRERIE NECESSARIE ----------------------------------------------------## 
##-- Gestione dati 
suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(forcats)
  library(tidyverse)
  library(broom)
  library(sf)
  library(terra)
})
##-- Modellazione 
suppressPackageStartupMessages({
  library(tidymodels)
  library(parsnip)
  library(stacks)
  library(reshape2)
  library(splines)
  library(MASS)
  library(rpart)
  library(doParallel)
  library(rpart.plot)
  library(glmnet)
  library(caret)
  library(glmnet)
  library(randomForest)
  library(xgboost)
  library(Metrics)
  library(lars)
  library(grpreg)
  library(rpart)
  library(leaps)
  library(Metrics)
  library(splines)
  library(modeldata)
  library(gbm)
  library(xgboost)
  library(leaps)
})
##-- Visualizzazione 
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggpubr)
  library(hexbin)
  library(patchwork)
  library(colorRamps)
  library(corrplot)
})
##-- Altre utilità 
suppressPackageStartupMessages({
  library(rlang)
})
##-- Risoluzione conflitti 
tidymodels_prefer()
conflicted::conflicts_prefer(pls::corrplot)  



# Carico le funzioni utili per il progetto
source("https://raw.githubusercontent.com/auroraMusitelli/Milan-housing---Data-Mining/refs/heads/main/vet2onehot.R")




##------------------------------------------------- 1.IMPORTO I DATASET ---------------------------------------------------------## 
# Aggiungo i file caricati sul mio GitHub personale
house_training <- read.csv("https://raw.githubusercontent.com/auroraMusitelli/Milan-housing---Data-Mining/refs/heads/main/training.csv", 
                           header=TRUE)
house_training = as.data.frame(house_training)

house_test <- read.csv("https://raw.githubusercontent.com/auroraMusitelli/Milan-housing---Data-Mining/refs/heads/main/test.csv", 
                       header = TRUE)
house_test = as.data.frame(house_test)

# Guardo i dataset
str(house_training)
str(house_test)

# Unisco entrambi i dataset per fare un pre-processing su tutti i dati
dati = bind_rows(house_training, house_test)




##------------------------------------------------- 2.PRE-PROCESSING ----------------------------------------------------------##
## Analisi esplorativa delle variabili ## 
skimr::skim(dati)

# Osservo in generale le variabili
table(dati$square_meters)
table(is.na(dati$square_meters))
table(dati$bathrooms_number)
table(is.na(dati$bathrooms_number))
table(dati$lift)
table(is.na(dati$lift))
table(dati$rooms_number)
table(is.na(dati$rooms_number))
table(dati$other_features)
table(is.na(dati$other_features))
table(dati$total_floors_in_building)
table(is.na(dati$total_floors_in_building))
table(dati$car_parking)
table(is.na(dati$car_parking))
table(dati$availability)
table(is.na(dati$availability))
table(dati$condominium_fees)
table(is.na(dati$condominium_fees))
table(dati$year_of_construction)
table(is.na(dati$year_of_construction))
table(dati$conditions)
table(is.na(dati$conditions))
table(dati$zone)
table(is.na(dati$zone))
table(dati$floor)
table(is.na(dati$floor))
table(dati$heating_centralized)
table(is.na(dati$heating_centralized))
table(dati$energy_efficiency_class)
table(is.na(dati$energy_efficiency_class))



## Svolgo un pre-processing iniziale su tutto il dataset
# Rinomino le variabili per una migliore leggibilità dei nomi
nomi_vars = c(sell_price = "selling_price",
              sq_mtrs = "square_meters",   
              baths_n = "bathrooms_number",
              rooms_n = "rooms_number",
              of = "other_features",
              floor_build = "total_floors_in_building",
              available = "availability",
              cfees = "condominium_fees",
              year = "year_of_construction",
              cond = "conditions",
              stairs = "floor",
              hc = "heating_centralized",
              energy_class = "energy_efficiency_class")

year_median <- median(dati$year_of_construction, na.rm = TRUE)

dati_prepro <- dati %>% 
  rename(all_of(nomi_vars)) %>%
  mutate(
    sq_mtrs = ifelse(sq_mtrs==1, NA, sq_mtrs),  # case con 1 metro quadrato sostituito con NA
    
    floor_build = as.numeric(fct_recode(factor(floor_build), '1'='1 floor')),
    floor_build = ifelse(is.na(floor_build), 1, floor_build),
    
    stairs = 1 + as.numeric(fct_recode(factor(stairs), 
                                       '1'='mezzanine', 
                                       '1'='semi-basement', 
                                       '0'='ground floor')),
    
    rooms_n = as.numeric(fct_recode(factor(rooms_n), '6'='5+')),
    
    baths_n = as.numeric(fct_recode(factor(baths_n), '4'='3+')),
    baths_n  = ifelse(is.na(baths_n), 0, baths_n),
    
    lift = ifelse(is.na(lift), 0, ifelse(lift == 'yes', 1, 0)),
    
    hc = ifelse(hc=='central', 1, 0), 
    hc = ifelse(is.na(hc), 0, hc), 
    
    available = as.numeric(available == 'available'),
    available = ifelse(is.na(available),0, available),
    
    year = ifelse(is.na(year), year_median, year),
    age = 2030 - year,  # età stimata dell'immobile  
    
    cfees = suppressWarnings(as.numeric(replace(cfees, cfees == "No condominium fees", NA))),
    cfees = ifelse(cfees == 1, NA, cfees),
    
    energy_class = ifelse(energy_class != ',', energy_class, NA),
    energy_class = ifelse(is.na(energy_class), 'cna' , energy_class),
    energy_class = as.numeric(fct_recode(factor(energy_class),
                                         '1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd',
                                         '5' = 'e', '6' = 'f', '7' = 'g')),
    cond = factor(cond) %>%
      fct_recode(
        'perfetta' = 'excellent / refurbished',
        'buona' = 'good condition / liveable',
        'costruire' = 'new / under construction',
        'ristrutturare' = 'to be refurbished') %>%
      fct_explicit_na(na_level = 'cna'),     # valore non disponibile
    cond = as.numeric(fct_recode(cond,
                                 '1' = 'perfetta', '2' = 'buona',
                                 '3' = 'ristrutturare', '4' = 'costruire',
                                 '5' = 'cna')),
    
    of = ifelse(is.na(of), 'cna', of)
  )


# Osservo ora le variabili con pre-processing iniziale (molto meglio)
table(dati_prepro$sq_mtrs)
table(is.na(dati_prepro$sq_mtrs))  
table(dati_prepro$baths_n)
table(is.na(dati_prepro$baths_n))
table(dati_prepro$lift)
table(is.na(dati_prepro$lift))
table(dati_prepro$rooms_n)
table(is.na(dati_prepro$rooms_n))
table(dati_prepro$floor_build)
table(is.na(dati_prepro$floor_build))
table(dati_prepro$available)
table(is.na(dati_prepro$available))
table(dati_prepro$cfees)
table(is.na(dati_prepro$cfees))
table(dati_prepro$year)
table(is.na(dati_prepro$year))
table(dati_prepro$cond)
table(is.na(dati_prepro$cond))
table(dati_prepro$stairs)
table(is.na(dati_prepro$stairs))
table(dati_prepro$hc)
table(is.na(dati_prepro$hc))
table(dati_prepro$energy_class)
table(is.na(dati_prepro$energy_class))




##-- Osservo le VARIABILI QUALITATIVE --##

# Svolgo un one-hot-encode per le variabili dummy: creo una lista delle colonne da trasformare in dummy 
# (trasformo queste variabili in dummy per renderle utilizzabili in modelli di machine learning)
dummy_cols = c('energy_class', 'cond')
dummy_cols = dummy_cols[dummy_cols %in% colnames(dati_prepro)]

for (varname in dummy_cols) {
  var = dati_prepro[[varname]]           # estraggo la colonna dal dataframe
  tab = table(var)                       # conto le frequenze delle categorie
  min_id = which.min(tab)                # indice categoria meno frequente
  levs = names(tab)[-min_id]             # tutte le categorie tranne la meno frequente (reference level)
  
  # Creo la matrice one-hot per tutte le categorie tranne la reference
  dummy_mat = vet2onehot(var, levs)
  # Rinmino le colonne della matrice dummy
  colnames(dummy_mat) = paste0(varname, '_', levs)
  # Aggiungo le dummy al dataframe (bind colonne)
  dati_prepro = cbind(dati_prepro, dummy_mat)
}

# Rimuovo le colonne originali categoricali ora trasformate in dummy
dati_prepro = dati_prepro[, !(colnames(dati_prepro) %in% dummy_cols)]

# Elimino dummy per evitare collinearità nei modelli
dati_prepro$cond2 <- NULL
dati_prepro$energy_class_7 <- NULL


# Pre-processing Car_parking: informazioni sui parcheggi, se disponibili 
table(dati_prepro$car_parking)
table(is.na(dati_prepro$car_parking)) 

cp <- dati_prepro$car_parking
N <- nrow(dati_prepro)

# Inizializzo due vettori numerici per il numero di posti box e shared
cp_box <- cp_shared <- numeric(N)
# Identifico le righe in cui il valore è 'no' (nessun parcheggio)
id_bool0 <- (cp == 'no')
# Identifico le righe che contengono 'box'
id_bool1 <- grepl('box', cp)
# Identifico le righe che contengono 'shared'
id_bool2 <- grepl('shared', cp)

# Funzione per estrarre tutti i numeri da una stringa usando una "regex"
extract_regex <- function(pattern, string){
  matches <- gregexec(pattern, string)         # trovo tutte le corrispondenze
  regmatches(string, matches)[[1]]             # estraggo la prima lista di match
}

# Eseguo un ciclo for su ogni elemento della colonna 'car_parking'
for (i in seq_along(cp)) {
  if (!id_bool0[i]) {  # Se non è 'no'
    # Se ci sono sia 'box' che 'shared'
    if (id_bool1[i] & id_bool2[i]) {
      nums <- extract_regex('(\\d+)', cp[i])   
      if (length(nums) >= 2) {
        cp_box[i] <- as.numeric(nums[1])       # Primo numero -> posti box
        cp_shared[i] <- as.numeric(nums[2])    # Secondo numero -> posti condivisi
      }
      
    } else {
      # Se contiene solo 'box'
      if (id_bool1[i]) {
        num <- extract_regex('(\\d+)', cp[i])
        if (length(num) >= 1) cp_box[i] <- as.numeric(num[1])
      }
      # Se contiene solo 'shared'
      if (id_bool2[i]) {
        num <- extract_regex('(\\d+)', cp[i])
        if (length(num) >= 1) cp_shared[i] <- as.numeric(num[1])
      }
    }
  }
}

# Aggiungo le due nuove colonne al dataset ed elimino la colonna originale 'car_parking'
dati_prepro <- dati_prepro %>%
  mutate(cp_box = cp_box,            # nuova colonna per i posti box
         cp_shared = cp_shared) %>%  # nuova colonna per i posti condivisi 
  select(-car_parking)               # rimuovo la colonna originale




# Pre-processing Zone: zone di Milano (quartieri di Milano) dove si trovano le case 
table(dati_prepro$zone)

sort(unique(dati_prepro$zone))          # per ispezionare tutte le zone disponibili
dati_prepro[is.na(dati_prepro$zone), ]  # verifica righe con NA

# Boxplot con il prezzo 
ggplot(dati_prepro, aes(x = zone, y = sell_price)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Prezzo di vendita per zona di Milano",
       x = "Zona",
       y = "Prezzo di vendita") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Pulizia e correzione dei nomi delle zone 
# Trovo la zona più frequente per imputare il dato mancante (un solo dato mancante)
zona_piu_frequente <- dati_prepro %>%
  filter(!is.na(zone)) %>%
  count(zone) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(zone)
print(paste("Zona più frequente:", zona_piu_frequente))
dati_prepro$zone[is.na(dati_prepro$zone)] <- 'cittÃ  studi'  

dati_prepro$zone[dati_prepro$zone == 'corso magenta'] <- "cadorna - castello"
dati_prepro$zone[dati_prepro$zone == 'via marignano, 3'] <- 'rogoredo'
dati_prepro$zone[dati_prepro$zone == 'largo caioroli 2'] <- 'duomo'

# Uniformo nomi e caratteri
dati_prepro$zone <- gsub("[\u00A0\t\r\n]+", " ", dati_prepro$zone)
dati_prepro$zone[dati_prepro$zone == "cittÃ  studi"] <- "città_studi"
dati_prepro$zone <- gsub("\\s+", " ", dati_prepro$zone)
dati_prepro$zone <- trimws(dati_prepro$zone)
dati_prepro$zone <- gsub(' ', '_', dati_prepro$zone)
dati_prepro$zone <- gsub('_-_', '_', dati_prepro$zone)


# Calcolo del prezzo medio per zona
average_prices <- dati_prepro %>%
  group_by(zone) %>%
  summarise(avg_price = mean(sell_price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))  

# Plot 
ggplot(average_prices, aes(x = reorder(zone, -avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(avg_price, 0)), vjust = -0.5, angle = 90, size = 3) +
  labs(title = "Prezzi medi in ogni zona di Milano",
       x = NULL,
       y = "Prices") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calcolo la frequenza delle zone
tab <- table(dati_prepro$zone)

# Seleziono solo le zone con almeno 20 osservazioni
zone_frequenti <- names(tab[tab >= 20])

# Riassegno le zone meno frequenti a "altre"
dati_prepro$zone <- as.character(dati_prepro$zone)
dati_prepro$zone[!(dati_prepro$zone %in% zone_frequenti)] <- "altre"

# Ricreo il factor con livelli ordinati (zone frequenti + "altre")
zone_finali <- c(sort(zone_frequenti), "altre")
dati_prepro$zone <- factor(dati_prepro$zone, levels = zone_finali)

# Creo le dummy variables
dummies <- model.matrix(~ zone - 1, data = dati_prepro)
colnames(dummies) <- gsub("^zone", "", colnames(dummies))

# Unisco le dummy al dataset e rimuovi la colonna originale
dati_prepro <- cbind(dati_prepro, dummies)
dati_prepro$zone <- NULL


# Pre-processing Other_features: altre caratteristiche che possiede la casa (tengo solo le caratteristiche più frequenti)
table(dati_prepro$of)
table(is.na(dati_prepro$of)) 

# Sostituisco i valori mancanti con "none" ovvero nessuna caretteristica aggiuntiva della casa
dati_prepro$of[is.na(dati_prepro$of)] <- "none"
# Estraggo le singole caratteristiche, rimuovo i duplicati e conto le occorrenze
features_df <- dati_prepro %>%
  filter(of != "none") %>%
  separate_rows(of, sep = "\\s*\\|\\s*") %>%
  distinct() %>%
  count(of, sort = TRUE)

# Seleziono le 10 feature più frequenti
top_features <- features_df$of[1:10]

# Creo variabili binarie per ciascuna delle top feature
for (f in top_features) {
  var_name <- paste0("feat_", gsub(" ", "_", f))  # Rimuovo eventuali spazi
  dati_prepro[[var_name]] <- grepl(f, dati_prepro$of)
}

# Imposto a FALSE le nuove feature per le righe che hanno "none"
dati_prepro[dati_prepro$of == "none", paste0("feat_", gsub(" ", "_", top_features))] <- FALSE

# Converto le variabili in fattoriali con livelli 0/1
for (f in top_features) {
  var_name <- paste0("feat_", gsub(" ", "_", f))
  dati_prepro[[var_name]] <- factor(dati_prepro[[var_name]], levels = c(FALSE, TRUE), labels = c(0, 1))
}

dati_prepro <- dati_prepro %>% select(-of)   # rimuovo la colonna originale 'of' perché ora è codificata come dummy




##-- VALORI MANCANTI --##
# Valori mancanti: frequenza dei valori mancanti per ogni variabile tranne per la variabile risposta selling_price
freq_missing <- apply(dati_prepro[, names(dati_prepro) != "sell_price"], 2, function(x) sum(is.na(x)))
freq_missing[freq_missing > 0]  # 2 variabili con valori mancanti

# Sostituisco i valori NA con mediana
dati_prepro$sq_mtrs[is.na(dati_prepro$sq_mtrs)] <- median(dati_prepro$sq_mtrs, na.rm = TRUE)
dati_prepro$cfees[is.na(dati_prepro$cfees)] <- median(dati_prepro$cfees, na.rm = TRUE)

# Rimozione colonne costanti
dati_prepro <- dati_prepro[, !sapply(dati_prepro, function(x) length(unique(x)) == 1)]
dati_prepro$zara <- NULL
dati_prepro$via_fra._cristoforo <- NULL


##-- DATASET COMPLETO PRE-PROCESSATO --##
dim(dati_prepro)
colnames(dati_prepro)
str(dati_prepro) 

colonne_da_convertire <- c(
  "feat_security_door", "feat_centralized_tv_system", "feat_optic_fiber", "feat_balcony",
  "feat_cellar", "feat_video_entryphone", "feat_electric_gate", "feat_shared_garden",
  "feat_full_day_concierge", "feat_double_exposure"
)

dati_prepro[colonne_da_convertire] <- lapply(
  dati_prepro[colonne_da_convertire],
  function(x) as.numeric(as.character(x))
)

all(sapply(dati_prepro, is.numeric))  # tutte colonne numeriche

# Controllo ed elimino colonne non utili
dati_prepro <- dati_prepro %>%
  select(-year)

# Salvo tutto in un altro dataset
filtro = sapply(dati_prepro, is.numeric)
dati_clean = dati_prepro[,filtro]
dim(dati_clean)




##-- Osservo la VARIABILE RISPOSTA --##
dim(dati_clean)
colnames(dati_clean)

# Distribuzione del prezzo
# La trasformata di Boxcox conferma che la trasformazone logaritmica è migliore, dando lambda molto vicino a 0.
# Osservo la variabile target
summary(dati_clean$sell_price)  
par(mfrow = c(1, 2))
hist(dati_clean$sell_price, xlab = "Price", main = "SellingPrice")
hist(log(dati_clean$sell_price), xlab = "Price", main = "Log Selling Price")

y = dati_clean$sell_price
result = boxcox(y~1, lambda = seq(-5,5,0.5))
mylambda = result$x[which.max(result$y)]
mylambda
# BoxCox
y_bxcx = (y^mylambda-1)/mylambda
hist(y_bxcx, breaks=50)


# Inserisco il logaritmo di selling_price
#dati_clean$log_price <- log(dati_clean$sell_price)




##-- CORRELAZIONE --##
# Osservo le correlazioni tra le varibili numeriche 
# Seleziono solo le variabili numeriche (quelle iniziali e non le dummy che ho creato)
vars_of_interest <- c("sq_mtrs", "lift", "floor_build", "available", "cfees", "stairs", "hc", "sell_price", "age")

# Creo un sotto-dataframe con solo queste colonne
df_selected <- dati_clean[, vars_of_interest]

# Calcolo la matrice di correlazione (usando i dati disponibili, ignorando i NA)
correlation_matrix <- cor(df_selected, use = "pairwise.complete.obs")

# Plot
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.7, col = colorRampPalette(c("blue", "white", "red"))(200))


##-- Outlier detection --##
# Square meters
par(mfrow=c(1,2))
boxplot(dati_clean$sq_mtrs)
hist(dati_clean$sq_mtrs, breaks=50)
# Con il log
dati_clean$log_sq_mtrs <- log(dati_clean$sq_mtrs)
par(mfrow=c(1,2))
boxplot(dati_clean$log_sq_mtrs)
hist(dati_clean$log_sq_mtrs, breaks=50)

dati_clean <- dati_clean %>% select(-log_sq_mtrs)


## Osservo che considerare il logaritmo del rapporto prezzo/mq (log(price/sqmtrs)) nelle previsioni del prezzo delle case 
#  stabilizza la varianza (omogeneità della varianza), linearizza le relazioni moltiplicative e riduce l'effetto di outlier 
dati_clean = dati_clean %>% mutate(logp_mq = log(sell_price/sq_mtrs)) #%>% dplyr::select(-c(sell_price, sq_mtrs))

par(mfrow=c(1,2))
boxplot(dati_clean$logp_mq)
hist(dati_clean$logp_mq, breaks=50)


# Inserisco il logaritmo di cfees
# cfees
par(mfrow=c(1,2))
boxplot(dati_clean$cfees)
hist(dati_clean$cfees, breaks=50)
# Con il log
dati_clean <- dati_clean %>% mutate(log_cfees = log(1 + cfees))
par(mfrow=c(1,2))
boxplot(dati_clean$log_cfees)
hist(dati_clean$log_cfees, breaks=50)

dati_clean <- dati_clean %>% select(-cfees)


# Inserisco trasformazione per age
# age
par(mfrow=c(1,2))
boxplot(dati_clean$age)
hist(dati_clean$age, breaks=50)
# Con la trasformazione
dati_clean <- dati_clean %>% mutate(log_age = log(1 + age))
par(mfrow=c(1,2))
boxplot(dati_clean$log_age)
hist(dati_clean$log_age, breaks=50)

dati_clean <- dati_clean %>% select(-age)



##-- Output finale del dataset pre-processato --##
# Vettore con le colonne da mettere all'inizio
cols_first <- c("ID", "lift", "sq_mtrs", "floor_build", "available", "stairs", "hc", 
                "log_cfees", "log_age", "sell_price", "logp_mq")
# Colonne del dataset
all_cols <- colnames(dati_clean)
# Colonne da mettere dopo quelle specificate (quelle non in cols_first)
cols_after <- setdiff(all_cols, cols_first)
# Riordino colonne
dati_clean <- dati_clean[, c(cols_first, cols_after)]


# Salvo il dataset pre-processato
write.csv(data.frame(dati_clean), "houseCleaned.csv", row.names = FALSE)

# Pulizia globalenvironment
rm(list = ls())




##--------------------------------------- 3.TRAINING, VALIDATION & TEST SET -----------------------------------------------------##
# Carico il global environment del progetto
url <- "https://www.dropbox.com/scl/fi/s71e2nk8us50jlssk1anm/globalenvironment.RData?rlkey=10pv4kjs01jsqjoln2nlo3137&st=kxdiftep&dl=1"
# Scarico il file nella directory corrente
download.file(url, destfile = "globalenvironment.RData", mode = "wb")
# Carico il file .RData
load("globalenvironment.RData")


# Training e test set utilizzo la suddivsione della data challenge
# Importo il nuovo dataset pre-processato
download.file("https://raw.githubusercontent.com/auroraMusitelli/Milan-housing---Data-Mining/refs/heads/main/houseCleaned.csv", 
              destfile = "houseCleaned.csv", 
              mode = "wb"
)

houseMilan <- read.csv("houseCleaned.csv", fileEncoding = "latin1")

# Suddivido il dataset di train e test come erano separati all'inizio del pre-processing
train_full <- houseMilan[1:8000, ]   
test <- houseMilan[8001:12800,] 

# Suddiviso il dataset di training in: training per addestrare i modelli e validation per testarli e selezionare il modello migliore
# Suppongo 70% per train, 30% per validation
n <- nrow(train_full)
idx <- sample(1:n, size = 0.7 * n)

train <- train_full[idx, ]             # 70% per training 
validation <- train_full[-idx, ]       # 30% per validation


# Osservo le dimensioni dei dataset
dim(train)      
dim(validation)  
dim(test)        


# Elimino il valore ID
train <- train %>% select(-ID)
validation <- validation %>% select(-ID)
test <- test %>% select(-ID)




##-------------------------------------------------------- 4.MODELLI ------------------------------------------------------------##

# Stimo diversi modelli

# Scopo: prevedere logaritmo del prezzo per metro quadro: log(price / sqm)
# Vantaggi: riduce varianza, gestisce outlier, migliora simmetria della distribuzione


# Funzioni metriche unificate (le definisco solo una volta)
mae_prezzo_totale <- function(pred_logp_mq, sq_mtrs, prezzo_reale) {
  pred_prezzo <- exp(pred_logp_mq) * sq_mtrs
  mean(abs(pred_prezzo - prezzo_reale))
}

msle_prezzo_totale <- function(pred_logp_mq, sq_mtrs, prezzo_reale) {
  pred_prezzo <- exp(pred_logp_mq) * sq_mtrs
  mean((log1p(pred_prezzo) - log1p(prezzo_reale))^2)
}

mae_logp_mq <- function(pred, actual) {
  mean(abs(pred - actual))
}

prezzo_reale <- validation$sell_price

# Formula comune per tutti i modelli lineari
formula_logp <- logp_mq ~ . - sell_price - sq_mtrs




##-----------------------------------------------------MODELLI LINEARI-----------------------------------------------------------##

##-- OLS (REGRESSIONE LINEARE) --##
# pro:stima una relazione lineare semplice tra le covariate
# contro: soffre di multicollinearità, non gestisce bene variabili irrilevanti, nessuna regolarizzazione

# Escludo una dummy per ciascuna variabile categoriale
dati_ols <- train[, !names(train) %in% c("energy_class_1", "cond_1")]
ols_model <- lm(logp_mq ~ . - sell_price - sq_mtrs, data = dati_ols)
summary(ols_model)

ols_preds <- predict(ols_model, newdata = validation)

# Metriche
mae_ols <- mae_prezzo_totale(ols_preds, validation$sq_mtrs, validation$sell_price)
mae_log <- mae_logp_mq(ols_preds, validation$logp_mq)
msle_ols <- msle_prezzo_totale(ols_preds, validation$sq_mtrs, validation$sell_price)

cat("MAE OLS:", mae_ols, "\n")
cat("MAE su logp_mq:", mae_log, "\n")
cat("MSLE su prezzo totale:", msle_ols, "\n")
cat("R-squared OLS:", summary(ols_model)$r.squared, "\n")


##-- MODEL MATRIX (per Lasso e Ridge e Elastic Net) --##
x_train <- model.matrix(formula_logp, data = train)[, -1]
y_train <- train$logp_mq

x_valid <- model.matrix(formula_logp, data = validation)[, -1]
y_valid <- validation$logp_mq


##-- LASSO --##
# pro: utile in presenza di molte variabili (specialmente collinearità o rumore), con penalità L1 sui coefficienti.
# pro: Può azzerare coefficienti -> fa anche selezione delle variabili.
set.seed(32)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv_lasso)
title("Cross-validation per LASSO", line = 2.5)

lasso_preds <- predict(cv_lasso, s = cv_lasso$lambda.min, newx = x_valid)[, 1]

# Variabili selezioante da Lasso
coefs <- coef(cv_lasso, s = "lambda.min")
selected_vars <- rownames(coefs)[which(coefs != 0)]
selected_vars

# Metriche
mae_lasso <- mae_prezzo_totale(lasso_preds, validation$sq_mtrs, prezzo_reale)
mae_log_lasso <- mae_logp_mq(lasso_preds, y_valid)
msle_lasso <- msle_prezzo_totale(lasso_preds, validation$sq_mtrs, prezzo_reale)

cat("MAE Lasso:", round(mae_lasso, 2), "euro\n")
cat("MAE logp_mq:", round(mae_log_lasso, 4), "\n")
cat("MSLE prezzo totale:", round(msle_lasso, 5), "\n")


##-- RIDGE --##
# pro: non azzera coefficienti, ma li riduce.
# pro: come LASSO, ma usa penalità L2.
set.seed(32)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
plot(cv_ridge)
title("Cross-validation Ridge", line = 2.5)

ridge_preds <- predict(cv_ridge, s = cv_ridge$lambda.min, newx = x_valid)[, 1]

coefs_ridge <- coef(cv_ridge, s = "lambda.min")
coefs_ridge_df <- data.frame(
  variable = rownames(coefs_ridge),
  coefficient = as.numeric(coefs_ridge)
)

# Ordina per importanza
coefs_ridge_df <- coefs_ridge_df[order(abs(coefs_ridge_df$coefficient), decreasing = TRUE), ]
head(coefs_ridge_df, 10)  # Le 10 variabili più influenti
tail(coefs_ridge_df, 10)  # Le 10 meno influenti

# Metriche
mae_ridge <- mae_prezzo_totale(ridge_preds, validation$sq_mtrs, prezzo_reale)
mae_log_ridge <- mae_logp_mq(ridge_preds, y_valid)
msle_ridge <- msle_prezzo_totale(ridge_preds, validation$sq_mtrs, prezzo_reale)

cat("MAE Ridge:", round(mae_ridge, 2), "euro\n")
cat("MAE logp_mq Ridge:", round(mae_log_ridge, 4), "\n")
cat("MSLE prezzo totale Ridge:", round(msle_ridge, 5), "\n")


##-- Elastic Net --##
# pro: combina vantaggi di Lasso e Ridge (penalità mista).
# importante: Funziona bene se ci sono molte feature correlate.
set.seed(32)

ctrl <- trainControl(method = "cv", number = 5,   savePredictions = "final",  
                     returnResamp = "all")
enet_grid <- expand.grid(alpha = seq(0, 1, length = 6), 
                         lambda = 10^seq(-3, -1, length = 10))

enet_caret <- train(
  x = x_train, y = y_train,
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = enet_grid,
  metric = "MAE"
)
plot(enet_caret)
print(enet_caret)

best_alpha <- enet_caret$bestTune$alpha
best_lambda <- enet_caret$bestTune$lambda
cat("Best alpha:", best_alpha, "; Best lambda:", best_lambda, "\n")

enet_preds <- predict(enet_caret, newdata = x_valid)
mae_enet <- mae_prezzo_totale(enet_preds, validation$sq_mtrs, validation$sell_price)
mae_log_enet <- mae_logp_mq(enet_preds, validation$logp_mq)
msle_enet <- msle_prezzo_totale(enet_preds, validation$sq_mtrs, validation$sell_price)

cat("MAE Elastic Net:", round(mae_enet, 2), "euro\n")
cat("MAE logp_mq EN:", round(mae_log_enet, 4), "\n")
cat("MSLE prezzo totale EN:", round(msle_enet, 5), "\n")


##-- CONFRONTO MODELLI --##
error_df <- data.frame(
  Modello = c("OLS", "Ridge", "Lasso", "Elastic Net"),
  MAE_Prezzo = c(mae_ols, mae_ridge, mae_lasso, mae_enet),
  MSLE_Prezzo = c(msle_ols, msle_ridge, msle_lasso, msle_enet),
  MAE_logp_mq = c(mae_log, mae_log_ridge, mae_log_lasso, mae_log_enet)
)

## Long format
error_long <- melt(error_df, id.vars = "Modello", 
                   variable.name = "Metrica", value.name = "Errore")

## Ordino per errore crescente
error_long <- error_long %>%
  group_by(Metrica) %>%
  mutate(Modello = reorder(Modello, Errore))
error_long

## Plot unico con facet
ggplot(error_long, aes(x = Modello, y = Errore, fill = Metrica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~Metrica, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Confronto delle Prestazioni dei Modelli",
       x = "Modello", y = "Errore") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 30, hjust = 1))




##----------------------------------------------------- ALBERI E BOOSTING -------------------------------------------------------##

# Gli alberi da soli non sono ideali se ho molte variabili dummy -> considero Random Forest e XGBoost.

##-- RANDOM FOREST --##
set.seed(32)

# Calcolo numero predittori da considerare per ogni split: default è sqrt(numero variabili predittive)
# Escludo target e variabili non predittive (logp_mq, sell_price, sq_mtrs)
num_pred <- ncol(train) - length(c("logp_mq", "sell_price", "sq_mtrs"))
mtry_default <- floor(sqrt(num_pred))

# Tuning del parametro mtry con caret (5-fold CV)
ctrl <- trainControl(method = "cv", number = 5)

tunegrid <- expand.grid(mtry = seq(max(1, mtry_default - 3), mtry_default + 3, by = 1))

rf_caret <- train(
  formula_logp,
  data = train,
  method = "rf",
  metric = "MAE",
  trControl = ctrl,
  tuneGrid = tunegrid,
  ntree = 500,
  importance = TRUE
)

print(rf_caret)
best_mtry <- rf_caret$bestTune$mtry
cat("Miglior mtry:", best_mtry, "\n")

# Modello finale con miglior mtry
rf_model <- randomForest(
  formula = formula_logp,
  data = train,
  ntree = 500,
  mtry = best_mtry,
  importance = TRUE
)

# Predizioni e metriche
rf_preds <- predict(rf_model, newdata = validation)
mae_rf <- mae_prezzo_totale(rf_preds, validation$sq_mtrs, validation$sell_price)
mae_log_rf <- mae_logp_mq(rf_preds, validation$logp_mq)
msle_rf <- msle_prezzo_totale(rf_preds, validation$sq_mtrs, validation$sell_price)

cat("MAE Random Forest:", round(mae_rf, 2), "euro\n")
cat("MAE logp_mq RF:", round(mae_log_rf, 4), "\n")
cat("MSLE Random Forest:", round(msle_rf, 5), "\n")

# Importanza variabili
varImpPlot(rf_model)


##-- XGBoost --##
# Preparazione dati DMatrix
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dvalid <- xgb.DMatrix(data = x_valid, label = y_valid)

# Parametri base migliorati
params <- list(
  objective = "reg:squarederror",
  eval_metric = "mae",       
  eta = 0.05,                 # learning rate più basso per maggiore precisione
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,            # riduce overfitting
  colsample_bytree = 0.8,     # riduce overfitting
  nthread = parallel::detectCores() - 1  # usa quasi tutti i core disponibili
)

# Cross-validation per scegliere nrounds ottimali
set.seed(32)
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 30,
  maximize = FALSE,
  verbose = 1,
  metrics = "mae"
)

best_nrounds <- cv_results$best_iteration
cat("Best nrounds from CV:", best_nrounds, "\n")

# Allenamento modello finale con nrounds ottimale
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(validation = dvalid),
  early_stopping_rounds = 30,
  verbose = 1
)

# Ottiengo i nomi delle feature dalla matrice dtrain (senza colonna d'intercetta)
feature_names <- colnames(x_train)
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_model)
print(importance_matrix)
# Plot dell'importanza
xgb.plot.importance(importance_matrix)

# Predizioni e valutazioni
xgb_preds <- predict(xgb_model, newdata = dvalid)
mae_xgb <- mae_prezzo_totale(xgb_preds, validation$sq_mtrs, prezzo_reale)
mae_log_xgb <- mae_logp_mq(xgb_preds, validation$logp_mq)
msle_xgb <- msle_prezzo_totale(xgb_preds, validation$sq_mtrs, validation$sell_price)

cat("MAE logp_mq XGB:", round(mae_log_xgb, 4), "\n")
cat("MSLE prezzo totale XGB:", round(msle_xgb, 5), "\n")
cat("MAE XGBoost:", round(mae_xgb, 2), "euro\n")




##----------------------------------------------------- MODELLI PONDERATI -------------------------------------------------------##

##-- IWLS (Weighted Iteratively Least Squares) --##
# Parte da un modello OLS semplice.
# Calcola la varianza dei residui e la approssima con smoothing per stabilizzare la stima.
# Usa l'inverso della varianza come peso, limitando i valori estremi.
# Rifit il modello WLS con i nuovi pesi.
# Ripete il processo fino a che il MAE su validation non migliora significativamente o si arriva al massimo delle iterazioni.

##-- Step 1: Variabile selection con stepwise su modello OLS --##
# Fit iniziale con tutte le variabili (formula_logp già include tutte le dummy senza ridondanze)
initial_ols <- lm(formula_logp, data = train)

# Stepwise selection (both directions) usando AIC
step_model <- stats::step(initial_ols, direction = "both", trace = FALSE)

# Estrai formula selezionata
formula_selected <- formula(step_model)
cat("Formula selezionata dopo stepwise:\n")
print(formula_selected)


##-- Step 2: IWLS con formula selezionata --##
max_iter <- 20
tol <- 1e-4
prev_mae <- Inf

# Fit iniziale con formula selezionata
wls_model <- lm(formula_selected, data = train)

for (i in 1:max_iter) {
  residui <- resid(wls_model)
  residui_sq <- residui^2
  fit_vals <- fitted(wls_model)
  
  # Provo vari span per smoothing della varianza dei residui
  span_grid <- seq(0.2, 0.9, by = 0.05)
  mae_spans <- numeric(length(span_grid))
  loess_preds_list <- vector("list", length(span_grid))
  
  for (s in seq_along(span_grid)) {
    loess_fit <- loess(residui_sq ~ fit_vals, span = span_grid[s])
    var_stimata_smooth <- predict(loess_fit, newdata = fit_vals)
    var_stimata_smooth[is.na(var_stimata_smooth)] <- mean(var_stimata_smooth, na.rm = TRUE)
    
    weights_tmp <- 1 / (var_stimata_smooth + 1e-6)
    # Limito pesi a quantili 1% e 99%
    lower_q <- quantile(weights_tmp, 0.01)
    upper_q <- quantile(weights_tmp, 0.99)
    weights_tmp <- pmin(pmax(weights_tmp, lower_q), upper_q)
    
    # Rifito modello WLS con pesi temporanei
    model_tmp <- lm(formula_selected, data = train, weights = weights_tmp)
    preds_tmp <- predict(model_tmp, newdata = validation)
    
    # Calcolo MAE su validation (usa tua funzione mae_prezzo_totale)
    mae_spans[s] <- mae_prezzo_totale(preds_tmp, validation$sq_mtrs, validation$sell_price)
    loess_preds_list[[s]] <- list(model = model_tmp, weights = weights_tmp)
  }
  
  best_idx <- which.min(mae_spans)
  best_span <- span_grid[best_idx]
  wls_model_new <- loess_preds_list[[best_idx]]$model
  weights <- loess_preds_list[[best_idx]]$weights
  mae_wls <- mae_spans[best_idx]
  
  cat(sprintf("Iterazione %d - Best span: %.2f - MAE WLS: %.4f euro\n", i, best_span, mae_wls))
  cat("prev_mae =", prev_mae, " mae_wls =", mae_wls, "\n")
  
  # Controllo convergenza
  if (is.na(mae_wls) || is.infinite(mae_wls) || is.na(prev_mae) || is.infinite(prev_mae)) {
    cat("MAE non valido, salto controllo di convergenza.\n")
  } else if (abs(prev_mae - mae_wls)/prev_mae < tol) {
    cat("Convergenza raggiunta.\n")
    break
  }
  wls_preds <- predict(wls_model, newdata = validation)
  wls_model <- wls_model_new
  prev_mae <- mae_wls
}


##-- Stepwise (forward) con pesi --##
set.seed(32)
# Calcolo dei pesi: qui uso pesi inversamente proporzionali alla superficie 
if (!"sq_mtrs" %in% names(train)) stop("La variabile 'sq_mtrs' non è presente nel dataset.")
w <- 1 / train$sq_mtrs
w <- w / mean(w)  # normalizzazione dei pesi

# Definizione formule
if (!exists("formula_logp")) stop("La formula 'formula_logp' non è definita.")

# Fitting dei modelli
full_lm <- lm(formula_logp, data = train, weights = w)
null_lm <- lm(logp_mq ~ 1, data = train, weights = w)

# Stepwise Forward con penalizzazione BIC (k = log(n))
n <- nrow(train)
step_lm <- stepAIC(null_lm,
                   scope = list(lower = null_lm, upper = full_lm),
                   direction = "forward",
                   trace = 0,
                   k = log(n))  # penalizzazione tipo BIC

# Predizione sul validation set
step_preds <- predict(step_lm, newdata = validation)

# Calcolo metriche di performance
mae_step <- mae_prezzo_totale(step_preds, validation$sq_mtrs, validation$sell_price)
mae_log_step <- mae_logp_mq(step_preds, validation$logp_mq)
msle_step <- msle_prezzo_totale(step_preds, validation$sq_mtrs, validation$sell_price)

# Output dei risultati
cat("Model selezionato (stepwise con pesi):\n")
print(formula(step_lm))

cat("MAE (prezzo totale):", round(mae_step, 2), "euro\n")
cat("MAE log prezzo/mq:", round(mae_log_step, 4), "\n")
cat("MSLE prezzo totale:", round(msle_step, 5), "\n")




##--------------------------------------------------- CONFRONTO MODELLI ---------------------------------------------------------##

# Prima tabella: MAE puro
mae_results <- data.frame(
  Modello = c("OLS", "Ridge", "Lasso", "ElasticNet", "RandomForest", "XGBoost", "WLS", "Stepwise"),
  MAE = round(c(mae_ols, mae_ridge, mae_lasso, mae_enet, mae_rf, mae_xgb, mae_wls, mae_step), 2)
)

print(mae_results)

# Tabella completa con più metriche
confronto <- data.frame(
  Modello = c("OLS", "Ridge", "Lasso", "ElasticNet", "RandomForest", "XGBoost", "WLS", "Stepwise"),
  MAE_Prezzo = round(c(mae_ols, mae_ridge, mae_lasso, mae_enet, mae_rf, mae_xgb, mae_wls, mae_step), 2),
  
  MAE_logp_mq = round(c(
    mae_log, mae_log_ridge, mae_log_lasso, mae_log_enet,
    mae_logp_mq(rf_preds, y_valid),
    mae_logp_mq(xgb_preds, y_valid),
    #mae_logp_mq(gbm_preds, y_valid),
    mae_logp_mq(wls_preds, y_valid),
    #mae_logp_mq(rlm_preds, y_valid),
    mae_logp_mq(step_preds, y_valid)
  ), 4),
  MSLE = round(c(
    msle_ols, msle_ridge, msle_lasso, msle_enet,
    msle_prezzo_totale(rf_preds, validation$sq_mtrs, prezzo_reale),
    msle_prezzo_totale(xgb_preds, validation$sq_mtrs, prezzo_reale),
    #msle_prezzo_totale(gbm_preds, validation$sq_mtrs, prezzo_reale),
    msle_prezzo_totale(wls_preds, validation$sq_mtrs, prezzo_reale),
    #msle_prezzo_totale(rlm_preds, validation$sq_mtrs, prezzo_reale),
    msle_prezzo_totale(step_preds, validation$sq_mtrs, prezzo_reale)
  ), 6)
)

print(confronto)

# Plot comparativo
confronto_long <- melt(confronto, id.vars = "Modello", variable.name = "Metrica", value.name = "Errore")
ggplot(confronto_long, aes(x = Modello, y = Errore, fill = Metrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 14) +
  labs(title = "Confronto Errori tra Modelli", y = "Errore") +
  scale_fill_manual(values = c("MAE_Prezzo" = "#1f77b4")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))




##---------------------------------------------------- 5.SCELTA MODELLO ---------------------------------------------------------##

# SCELGO IL MODELLO MIGLIORE #
best_model_name <- "WLS"  
cat("Modello scelto:", best_model_name, "\n")

# Creo model matrix per modelli penalizzati
x_test <- model.matrix(formula_logp, data = test)[, -1]

# Predizione finale coerente con logp_mq --> prezzo = exp(logp_mq_pred) * sq_mtrs
logp_pred_test <- switch(best_model_name,
                         "OLS" = predict(ols_model, newdata = test),
                         "Ridge" = predict(cv_ridge, newx = x_test, s = cv_ridge$lambda.min)[, 1],
                         "Lasso" = predict(cv_lasso, newx = x_test, s = cv_lasso$lambda.min)[, 1],
                         "ElasticNet" = predict(enet_caret, newx = x_test, s = enet_caret$lambda.min)[, 1],
                         "RandomForest" = predict(rf_model, newdata = test),
                         "XGBoost" = predict(xgb_model, newdata = xgb.DMatrix(data = x_test)),
                         #"GBM" = predict(gbm_model, newdata = test, n.trees = best_iter),
                         "WLS" = predict(wls_model_new, newdata = test),
                         #"RobustRegression" = predict(rlm_model, newdata = test),
                         "Stepwise" = predict(step_lm, newdata = test),
                         stop("Modello non riconosciuto!")
)

# Calcolo la previsione finale del prezzo totale
y_hat_final <- exp(logp_pred_test) * test$sq_mtrs
summary(y_hat_final)




##----------------------------------------- ESPORTAZIONE PREVISIONI CASE A MILANO -----------------------------------------------##

# Salvo file in CSV
dati_submission <- data.frame(ID = 1:nrow(test), prediction = y_hat_final)
write.csv(dati_submission, file = "submission_musitelli.csv", row.names = FALSE)








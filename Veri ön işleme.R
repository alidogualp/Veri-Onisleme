# Gerekli kütüphaneler
library(tidyverse)
library(naniar)
library(imputeTS) # Zaman serisi doldurma için özel paket
library(mice)     # Gelismis istatistiksel doldurma

# Veriyi yükle
izmir_hava <- airquality

# Kayip veri haritasi (Sunumun ilk görseli)
vis_miss(izmir_hava) + 
  ggtitle("Izmir Hava Kalitesi Veri Röntgeni")



View(airquality)



# 1. PAKETLER VE VERI (NY Hava Kalitesi 1973)
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(imputeTS)) install.packages("imputeTS")
if(!require(VIM)) install.packages("VIM")
if(!require(mice)) install.packages("mice")

library(tidyverse)
library(imputeTS)
library(VIM)
library(mice)

df <- airquality

# 2. YONTEMLERIN UYGULANMASI (Tum Sutunlar Icin)

# Ortalama (Mean)
df_mean <- df %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(Method = "Mean Imputation")

# Interpolation (Zaman Serisi)
df_interp <- na_interpolation(df) %>%
  mutate(Method = "Interpolation")

# KNN (En Yakin Komsu - k=5)
df_knn <- kNN(df, k = 5, imp_var = FALSE) %>%
  mutate(Method = "KNN (k=5)")

# MICE (Coklu Atama)
mice_mod <- mice(df, method='pmm', m=1, printFlag=FALSE)
df_mice <- complete(mice_mod) %>%
  mutate(Method = "MICE")

# 3. VERILERIN BIRLESTIRILMESI
all_data <- bind_rows(df_mean, df_interp, df_knn, df_mice)

# 4. AYRI PANELLERDE GORSELLESTIRME (Ozone sutunu uzerinden kiyas)
ggplot(all_data, aes(x = Ozone)) +
  # Referans Alan (Gri: Orijinal Veri)
  geom_density(data = df, aes(x = Ozone), fill = "gray80", color = "black", alpha = 0.5) +
  # Yontem Cizgisi
  geom_density(aes(color = Method), size = 1.2) +
  # Panellere Ayirma
  facet_wrap(~ Method) +
  # Renk ve Tema
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Kayip Veri Doldurma Yontemlerinin Ayri Panellerde Kiyasi",
    subtitle = "Gri alan gercek veriyi, renkli cizgiler doldurma sonuclarini temsil eder.",
    x = "Ozon Degeri (ppb)",
    y = "Yogunluk"
  ) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))










library(dplyr)

# Her yöntem için istatistikleri hesaplayan tablo
ozet_tablo <- data.frame(
  Yontem = c("Orijinal (NA'li)", "Mean", "Interpolation", "KNN", "MICE"),
  Ozone_Mean = c(mean(df$Ozone, na.rm=T), mean(df_mean$Ozone), mean(df_interp$Ozone), mean(df_knn$Ozone), mean(df_mice$Ozone)),
  Ozone_SD = c(sd(df$Ozone, na.rm=T), sd(df_mean$Ozone), sd(df_interp$Ozone), sd(df_knn$Ozone), sd(df_mice$Ozone))
)

print(ozet_tablo)






# Orijinal veri setindeki tum iliskileri gorelim (NA'lar haric)
cor_matrix <- cor(airquality, use = "complete.obs")
print(round(cor_matrix, 3))




# Korelasyon Karsilastirmasi (Ozone vs Temp)
cor_values <- data.frame(
  Yontem = c("Orijinal (NA'siz)", "Mean", "Interpolation", "KNN", "MICE"),
  Korelasyon = c(cor(df$Ozone, df$Temp, use = "complete.obs"),
                 cor(df_mean$Ozone, df_mean$Temp),
                 cor(df_interp$Ozone, df_interp$Temp),
                 cor(df_knn$Ozone, df_knn$Temp),
                 cor(df_mice$Ozone, df_mice$Temp))
)
print(cor_values)









# ============================================================
# FINAL ASAMASI: RMSE (HATA PAYI) ANALIZI - HATASIZ
# ============================================================
set.seed(123) 

# 1. Sadece tam olan satirlari alalim
df_full <- na.omit(airquality)

# 2. Yapay bosluklar olusturalim (%20 oraninda)
n <- nrow(df_full)
test_indices <- sample(1:n, size = round(n * 0.20))
gercek_degerler <- df_full$Ozone[test_indices] 

df_test <- df_full
df_test$Ozone[test_indices] <- NA 

# 3. YONTEMLERI TEST EDELIM
# Mean
mean_val <- mean(df_test$Ozone, na.rm=T)
pred_mean <- rep(mean_val, length(test_indices))

# Interpolation
df_interp_test <- na_interpolation(df_test)
pred_interp <- df_interp_test$Ozone[test_indices]

# KNN
df_knn_test <- kNN(df_test, variable="Ozone", k=5, imp_var=FALSE)
pred_knn <- df_knn_test$Ozone[test_indices]

# MICE
mice_test_mod <- mice(df_test, method='pmm', m=1, printFlag=FALSE)
df_mice_test <- complete(mice_test_mod)
pred_mice <- df_mice_test$Ozone[test_indices]

# 4. RMSE HESAPLAMA FONKSIYONU
rmse_calc <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# 5. FINAL SONUC TABLOSU (Isim hatasi duzeltildi!)
rmse_sonuclari <- data.frame(
  Metot = c("Mean Imputation", "Interpolation", "KNN", "MICE"),
  RMSE_Skoru = c(
    rmse_calc(gercek_degerler, pred_mean),
    rmse_calc(gercek_degerler, pred_interp),
    rmse_calc(gercek_degerler, pred_knn),
    rmse_calc(gercek_degerler, pred_mice)
  )
)

print("--- Hangi Yontem Daha Az Hata Yapti? (Dusuk RMSE Iyidir) ---")
print(rmse_sonuclari)











# 1. Gerekli Kütüphaneler
if(!require(mice)) install.packages("mice")
library(mice)

# 2. Veriyi Hazirlama ve MICE ile Doldurma
set.seed(123) # Sonuçlarin her seferinde ayni çikmasi için
imp <- mice(airquality, m = 5, method = 'pmm', printFlag = FALSE)
df_complete <- complete(imp) # Artik elimizde tertemiz, NA'siz veri var.

# --- ÖLÇEKLENDIRME YÖNTEMLERI ---

# A. Standardization (Z-Score Scaling)
# Ortalamayi 0, Standart Sapmayi 1 yapar.
df_std <- as.data.frame(scale(df_complete))

# B. Min-Max Scaling (Normalization)
# Degerleri 0 ile 1 arasina sikistirir.
min_max <- function(x) { (x - min(x)) / (max(x) - min(x)) }
df_minmax <- as.data.frame(lapply(df_complete, min_max))

# C. Robust Scaling
# Medyan ve IQR kullanir, aykiri degerlere dayaniklidir.
robust_scale <- function(x) { (x - median(x)) / IQR(x) }
df_robust <- as.data.frame(lapply(df_complete, robust_scale))

# D. Max Abs Scaling
# Veriyi maksimum mutlak degere bölerek -1 ile 1 (veya 0-1) arasina getirir.
max_abs <- function(x) { x / max(abs(x)) }
df_maxabs <- as.data.frame(lapply(df_complete, max_abs))

# E. Log Scaling
# Saga çarpik veriyi düzeltir. (Not: Veride 0 varsa log(x+1) kullanilir)
df_log <- as.data.frame(lapply(df_complete, log))

# F. Unit Vector Scaling
# Her bir sütunu birim vektör haline getirir (L2 Normu).
unit_vector <- function(x) { x / sqrt(sum(x^2)) }
df_unit <- as.data.frame(lapply(df_complete, unit_vector))

# --- SONUÇLARI KONTROL ETME ---
# Örnek olarak Ozone degiskeninin ilk 5 satirina bakalim
comparison <- data.frame(
  Orijinal = df_complete$Ozone,
  Standard = df_std$Ozone,
  MinMax = df_minmax$Ozone,
  Robust = df_robust$Ozone,
  MaxAbs = df_maxabs$Ozone,
  Log = df_log$Ozone,
  Unit = df_unit$Ozone
)

print(head(comparison, 5))









# 1. Gerekli Kütüphaneleri Yükle
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(tidyr)
library(dplyr)

# 2. Ölçeklendirme Islemlerini Yap ve Birlestir
# (Not: df_complete'in MICE ile dolduruldugunu varsayiyoruz)

scaling_results <- data.frame(
  Original = df_complete$Ozone,
  Standardization = as.vector(scale(df_complete$Ozone)),
  MinMax = (df_complete$Ozone - min(df_complete$Ozone)) / (max(df_complete$Ozone) - min(df_complete$Ozone)),
  Robust = (df_complete$Ozone - median(df_complete$Ozone)) / IQR(df_complete$Ozone),
  MaxAbs = df_complete$Ozone / max(abs(df_complete$Ozone)),
  Log_Scaling = log(df_complete$Ozone),
  Unit_Vector = df_complete$Ozone / sqrt(sum(df_complete$Ozone^2))
)

# 3. Veriyi Görsellestirme Için "Uzun" Formata Getir
plot_data <- scaling_results %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Value")

# 4. Facet_wrap ile Görsellestirme
ggplot(plot_data, aes(x = Value, fill = Method)) +
  geom_density(alpha = 0.6, color = "black") +
  facet_wrap(~Method, scales = "free", ncol = 3) + # Her panelin kendi X ekseni ölçegi olsun
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Ozon Degiskeni Uzerinde Olceklendirme Yontemlerinin Kiyasi",
    subtitle = "X eksenindeki sayisal araliklara ve dagilimin sekline dikkat ediniz.",
    x = "Deger Araligi",
    y = "Yogunluk"
  ) +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold", size = 11))











# 1. Gerekli Kütüphaneler
if(!require(mice)) install.packages("mice")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("dplyr")

library(mice)
library(ggplot2)
library(tidyr)
library(dplyr)

# 2. MICE ile Doldurulmus Veriyi Hazirla (Eksiksiz Veri Seti)
# Not: 'imp' objesinin önceden olusturulmus olmasi gerekir.
df_clean <- complete(imp) %>% select(Ozone, Solar.R, Wind, Temp)

# 3. Ölçeklendirme Fonksiyonu (Tüm Yöntemler)
get_scaled_long <- function(data, method_name) {
  if(method_name == "Original") {
    res <- data
  } else if(method_name == "Standardization") {
    res <- as.data.frame(scale(data))
  } else if(method_name == "Min-Max") {
    res <- as.data.frame(lapply(data, function(x) (x - min(x)) / (max(x) - min(x))))
  } else if(method_name == "Robust") {
    res <- as.data.frame(lapply(data, function(x) (x - median(x)) / IQR(x)))
  } else if(method_name == "Max-Abs") {
    res <- as.data.frame(lapply(data, function(x) x / max(abs(x))))
  } else if(method_name == "Log") {
    # Logaritma için 0 degerlerine karsi önlem: log(x + 1)
    res <- as.data.frame(lapply(data, function(x) log(x + 1)))
  } else if(method_name == "Unit-Vector") {
    res <- as.data.frame(lapply(data, function(x) x / sqrt(sum(x^2))))
  }
  
  res %>% 
    mutate(Method = method_name) %>%
    pivot_longer(cols = -Method, names_to = "Variable", values_to = "Value")
}

# 4. Tüm Yöntemleri Birlestir
all_methods <- c("Original", "Standardization", "Min-Max", "Robust", "Max-Abs", "Log", "Unit-Vector")
plot_data <- do.call(rbind, lapply(all_methods, function(m) get_scaled_long(df_clean, m)))

# 5. Görsellestirme (Hatasiz Etiketler)
ggplot(plot_data, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Method, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(
    title = "Tum Degiskenler Uzerinde Olceklendirme Etkisi",
    subtitle = "Farkli birimlerin (ppb, mph, F) ortak olcege donusumu",
    x = "Deger", 
    y = "Yogunluk"
  ) +
  theme(strip.text = element_text(face = "bold"))

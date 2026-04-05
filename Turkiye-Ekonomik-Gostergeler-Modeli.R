install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "gridExtra", 
                   "reshape2", "moments", "zoo", "tseries", "urca", 
                   "lmtest", "car", "sandwich", "strucchange", "vars"))
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "gridExtra", 
          "reshape2", "moments", "zoo", "tseries", "urca", 
          "lmtest", "car", "sandwich", "strucchange", "vars")
invisible(lapply(pkgs, library, character.only = TRUE))
setwd("C:/Program Files")
df <- read_excel("berkesenturk.xlsx", sheet = "Sayfa1")
df$tarih <- as.Date(paste0(df$tarih, "-01"), format = "%Y-%m-%d")
gsyh_vec <- df$gsyh
altrez_vec <- df$altrez
mat <- as.matrix(df[, c("gsyh", "altrez", "ihr", "fdi")])
df$yil <- as.factor(format(df$tarih, "%Y"))
ozet_liste <- list(
  gozlem = nrow(df),
  degiskenler = names(df),
  tarih_araligi = range(df$tarih)
)
head(df, 5)
str(df)
dim(df)
son_gsyh <- tail(na.omit(df$gsyh), 1)
if (length(son_gsyh) > 0 && son_gsyh > 1000) {
  cat("Y??ksek b??y??me d??nemi\n")
} else {
  cat("Normal b??y??me d??nemi\n")
}
for (deg in c("gsyh", "altrez", "ihr", "fdi")) {
  cat(deg, "-> ortalama:", round(mean(df[[deg]], na.rm=TRUE), 3), "\n")
}
i <- 1
while (i <= 5) {
  cat("Sat??r", i, ": GSYH =", df$gsyh[i], "\n")
  i <- i + 1
}
df_model <- df %>% dplyr::select(tarih, gsyh, altrez, ihr, fdi)
df_son_donem <- df_model %>% filter(tarih >= "2020-01-01")
df_model <- df_model %>%
  mutate(
    ihr_fdi_fark = ihr - fdi,
    altrez_oran = (altrez / gsyh) * 100,
    yil = format(tarih, "%Y"),
    fdi_yuksek = ifelse(fdi > 20, 1, 0)
  )
yillik_ozet <- df_model %>%
  group_by(yil) %>%
  summarise(
    ort_gsyh   = mean(gsyh, na.rm=TRUE),
    ort_altrez = mean(altrez, na.rm=TRUE),
    ort_ihr    = mean(ihr, na.rm=TRUE)
  )
deg_ozet <- function(x, degisken_adi = "x") {
  cat("=== De??i??ken:", degisken_adi, "===\n")
  cat(" G??zlem Say??s?? (N) :", length(na.omit(x)), "\n")
  cat(" Ortalama          :", round(mean(x, na.rm=TRUE), 3), "\n")
  cat(" Standart Sapma    :", round(sd(x, na.rm=TRUE), 3), "\n\n")
}
for (v in c("gsyh", "altrez", "ihr", "fdi")) {
  deg_ozet(df_model[[v]], v)
}
df_model <- df_model %>%
  mutate(
    gsyh_lag1   = lag(gsyh, 1),
    altrez_lag1 = lag(altrez, 1),
    gsyh_ma3    = rollmean(gsyh, 3, fill=NA, align="right")
  )
df_temiz <- na.omit(df_model)
degiskenler <- c("gsyh", "altrez", "ihr", "fdi")
ozet_tablo <- df %>%
  dplyr::select(dplyr::all_of(degiskenler)) %>%  
  summarise(across(everything(), list(  
    N      = ~sum(!is.na(.)),
    Ort    = ~mean(., na.rm=TRUE),
    StdSap = ~sd(., na.rm=TRUE),
    Carp   = ~moments::skewness(., na.rm=TRUE),
    Kurt   = ~moments::kurtosis(., na.rm=TRUE)
  )))
print(t(ozet_tablo))
p1 <- ggplot(df_temiz, aes(x = tarih, y = gsyh)) +
  geom_line(color = "#1F4E79", linewidth = 1) +
  labs(title = "GSYH Zaman Serisi", x = "Yil", y = "GSYH") +
  theme_minimal()
p2 <- ggplot(df_temiz, aes(x = tarih, y = altrez)) +
  geom_line(color = "#C00000", linetype = "dashed", linewidth = 1) +
  labs(title = "Altin Rezervleri", x = "Yil", y = "Miktar") +
  theme_minimal()
p3 <- ggplot(df_temiz, aes(x = gsyh)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#1F4E79", color = "white", bins = 20) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "GSYH Dagilim Analizi", x = "GSYH", y = "Yogunluk") +
  theme_minimal()
p4 <- ggplot(df_temiz, aes(x = ihr, y = fdi)) +
  geom_point(color = "#2E75B6", alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkred", se = TRUE) +
  labs(title = "Ihracat - FDI Iliskisi", x = "Ihracat", y = "FDI") +
  theme_minimal()
while(!is.null(dev.list())) dev.off()
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Veri Seti Gorsel Analiz Paneli")

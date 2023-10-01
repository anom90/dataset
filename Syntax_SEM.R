#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_SEM.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data), 3)

#Spesifikasi Model
model_sem <- '
                  PEND_K =~ PEND_K1 + PEND_K2 + PEND_K3 + 
                            PEND_K4 + PEND_K5
                  PAND_K =~ PAND_K1 + PAND_K2 + PAND_K3 + 
                            PAND_K4 + PAND_K5
                  PENG_K =~ PENG_K1 + PENG_K2 + PENG_K3 + 
                            PENG_K4 + PENG_K5
                  PERS_K =~ PERS_K1 + PERS_K2 + PERS_K3 + 
                            PERS_K4 + PERS_K5
                  PERS_K ~ PEND_K + PAND_K + PENG_K
                  '

#Estimasi Model
uji_sem <- sem(model_sem, data = data)
summary(uji_sem, fit.measure = T, standardized = T, 
        rsquare = T)

#Reliabilitas
compRelSEM(uji_sem)

#Validitas Konvergen
AVE(uji_sem)

#Validitas Diskriminan
htmt(model_sem, data)

#1. Visualisasi Model Hipotetik
#Gambar Model Hipotetik
p_pa <- semPaths(uji_sem, whatLabels="path",
              style = "lisrel", edge.label.cex = 0.8, 
              nDigits = 2, edge.color = 'black', 
              shapeMan = 'rectangle', sizeMan = 8, 
              sizeMan2 = 4, sizeLat = 9, sizeLat2 = 6, 
              equalizeManifests = T, nCharNodes = 0,
              nCharEdges = 0, rotation = 2, 
              intercepts = F)

#Perbaiki Gambar Model Hipotetik
#Spesifikasi Item/Indikator
indicator_order <- c('PEND_K1', 'PEND_K2', 'PEND_K3', 
                     'PEND_K4', 'PEND_K5', 
                     'PAND_K1', 'PAND_K2', 'PAND_K3', 
                     'PAND_K4', 'PAND_K5',
                     'PENG_K1', 'PENG_K2', 'PENG_K3', 
                     'PENG_K4', 'PENG_K5', 
                     'PERS_K1', 'PERS_K2', 'PERS_K3', 
                     'PERS_K4', 'PERS_K5')

#Spesifikasi Faktor dari Item/Indikator
indicator_factor <- c('PEND_K', 'PEND_K', 'PEND_K', 
                      'PEND_K', 'PEND_K',
                      'PAND_K', 'PAND_K', 'PAND_K', 
                      'PAND_K', 'PAND_K',
                      'PENG_K', 'PENG_K', 'PENG_K', 
                      'PENG_K', 'PENG_K',
                      'PERS_K', 'PERS_K', 'PERS_K', 
                      'PERS_K', 'PERS_K')

#Spesifikasi Tata Letak Faktor
factor_layout <- matrix(c('PEND_K', NA, NA,
                          NA, 'PAND_K', 'PERS_K',
                          'PENG_K', NA, NA), 
                        byrow = TRUE, 3, 3)

#Spesifikasi Tata Letak Item/Indiktor setiap Faktor
factor_point_to <- matrix(c("up", NA, NA,
                            NA, "left", "right",
                            "down", NA, NA), 
                          byrow = TRUE, 3, 3)

#Spesifikasi Jarak Item/Indikator terhadap Faktor
indicator_push <- c(PEND_K =1.5, PAND_K = 1.6, 
                    PENG_K = 1.5, PERS_K = 2)

#Spesifikasi Jarak antar Item/Indikator setiap Faktor
indicator_spread <- c(PEND_K =2.8, PAND_K = 1.6, 
                      PENG_K = 2.8, PERS_K = 2)

#Spesifikasi Jarak Factor Loading
loading_position <- c(PEND_K = 0.5, PAND_K = 0.5, 
                      PENG_K = 0.5, PERS_K = 0.5)

#Gambar Model Hipotetik sesuai Spesifikasi
p_pa2 <- set_sem_layout(p_pa,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_push = indicator_push,
                     indicator_spread = indicator_spread,
                     loading_position = loading_position)

#Menyesuaikan Posisi Atribut Kurva 
p_pa3 <- set_curve(p_pa2, 
                   c('PENG_K ~~ PEND_K' = -5,
                   'PAND_K ~~ PEND_K' = 0.5,
                   'PENG_K ~~ PAND_K' = 0.5))

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(PERS_K = 0)
p_pa4 <- rotate_resid(p_pa3, rotate_resid_vector)

#Gambar Model Hipotetik yang Disesuaikan
plot(p_pa4)

pdf('SEM-Model Hipotetik.pdf')
plot(p_pa4)
dev.off()

#2. Visualisasi Hitungan Estimasi
#Gambar Hitungan Estimasi
p_est <- semPaths(uji_sem, whatLabels="est",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 4, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#Gambar Hitungan Estimasi sesuai Spesifikasi
p_est2 <- set_sem_layout(p_est,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_push = indicator_push,
                     indicator_spread = indicator_spread,
                     loading_position = loading_position)

#Menyesuaikan Posisi Atribut Kurva 
p_est3 <- set_curve(p_est2, 
                    c('PENG_K ~~ PEND_K' = -5,
                      'PAND_K ~~ PEND_K' = 0.5,
                      'PENG_K ~~ PAND_K' = 0.5))

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(PERS_K = 0)
p_est4 <- rotate_resid(p_est3, rotate_resid_vector)

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_est5 <- mark_sig(p_est4, uji_sem, 
                   alphas = c('*' = 0.05, ' ' = 1))

#Gambar Model Hipotetik yang Disesuaikan
plot(p_est5)

pdf('SEM-Hitungan Estimasi.pdf')
plot(p_est5)
dev.off()

#3. Visualisasi Hitungan Standardized
#Gambar Hitungan Standardized
p_std <- semPaths(uji_sem, whatLabels="std",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 4, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#Gambar Hitungan Standardized sesuai Spesifikasi
p_std2 <- set_sem_layout(p_std,
                         indicator_order = indicator_order,
                         indicator_factor = indicator_factor,
                         factor_layout = factor_layout,
                         factor_point_to = factor_point_to,
                         indicator_push = indicator_push,
                         indicator_spread = indicator_spread,
                         loading_position = loading_position)

#Menyesuaikan Posisi Atribut Kurva 
p_std3 <- set_curve(p_std2, 
                    c('PENG_K ~~ PEND_K' = -5,
                      'PAND_K ~~ PEND_K' = 0.5,
                      'PENG_K ~~ PAND_K' = 0.5))

#Menyesuaikan Posisi Residual
p_std4 <- rotate_resid(p_std3, rotate_resid_vector)

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_std5 <- mark_sig(p_std4, uji_sem, 
                   alphas = c('*' = 0.05, ' ' = 1))

#Gambar Model Hipotetik yang Disesuaikan
plot(p_std5)

pdf('SEM-Hitungan Standardized.pdf')
plot(p_std5)
dev.off()


#Simpan
sink('Hasil Analisis SEM.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil SEM***', '\n')
summary(uji_sem, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_sem)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
htmt(model_sem, data)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
compRelSEM(uji_sem)
cat('\n')
sink()

pdf('SEM-Model Hipotetik.pdf')
plot(p_pa4)
dev.off()

pdf('SEM-Hitungan Estimasi.pdf')
plot(p_est5)
dev.off()

pdf('SEM-Hitungan Standardized.pdf')
plot(p_std5)
dev.off()


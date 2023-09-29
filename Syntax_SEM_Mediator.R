
data <- read.csv('Data_SEM_MED.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(semTools::mardiaKurtosis(data), 3)

#Spesifikasi Model
model_sem_med <- '
                  PTO =~ PTO1 + PTO2 + PTO3 + PTO4 + PTO5
                  PTM =~ PTM1 + PTM2 + PTM3 + PTM4 + PTM5
                  MMS =~ MMS1 + MMS2 + MMS3 + MMS4 + MMS5
                  PMB =~ PTT + PMI + PMR
                  PKS =~ PKK + PKP
                  PMB ~ a*PTO + b*PTM + c*MMS
                  PKS ~ d*PMB
                  
                  #Efek Tidak Langsung (ETL)
                  etlPTO.PKS := a*d
                  etlPTM.PKS := b*d
                  etlMMS.PKS := c*d
                  '

#Estimasi Model
uji_sem_med <- sem(model_sem_med, data=data)
summary(uji_sem_med, fit.measure = T, standardized = T, 
        rsquare = T)

#Reliabilitas
semTools::compRelSEM(uji_sem_med)

#Validitas Konvergen
semTools::AVE(uji_sem_med)

#Validitas Diskriminan
semTools::htmt(model_sem_med, data)

#1. Visualisasi Model Hipotetik
#Gambar Model Hipotetik
p_pa <- semPaths(uji_sem_med, whatLabels = "path",
                 style = "lisrel", edge.label.cex = 0.8, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 7, 
                 sizeMan2 = 4, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0,
                 nCharEdges = 0, rotation = 2, 
                 intercepts = F)

#Perbaiki Gambar Model Hipotetik
#Spesifikasi Item/Indikator
indicator_order <- c('PTO1', 'PTO2', 'PTO3', 
                     'PTO4', 'PTO5', 
                     'PTM1', 'PTM2', 'PTM3', 
                     'PTM4', 'PTM5', 
                     'MMS1', 'MMS2', 'MMS3', 
                     'MMS4', 'MMS5', 
                     'PTT', 'PMI', 'PMR', 
                     'PKK', 'PKP')

#Spesifikasi Faktor dari Item/Indikator
indicator_factor <- c('PTO', 'PTO', 'PTO', 
                      'PTO', 'PTO',
                      'PTM', 'PTM', 'PTM', 
                      'PTM', 'PTM',
                      'MMS', 'MMS', 'MMS', 
                      'MMS', 'MMS',
                      'PMB', 'PMB', 'PMB', 
                      'PKS', 'PKS')

#Spesifikasi Tata Letak Faktor
factor_layout <- matrix(c('PTO', NA, NA, NA,
                          NA, 'PTM', 'PMB', 'PKS',
                          'MMS', NA, NA, NA), 
                        byrow = TRUE, 3, 4)

#Spesifikasi Tata Letak Item/Indiktor setiap Faktor
factor_point_to <- matrix(c("up", NA, NA, NA,
                            NA, "left", "down", 'right',
                            "down", NA, NA, NA), 
                          byrow = TRUE, 3, 4)

#Spesifikasi Jarak Item/Indikator terhadap Faktor
indicator_push <- c(PTO =1.5, PTM = 2, 
                    MMS = 1.5, PMB = 2,
                    PKS = 2)

#Spesifikasi Jarak antar Item/Indikator setiap Faktor
indicator_spread <- c(PTO = 3.5, PTM = 1.6, 
                      MMS = 3.5, PMB = 2.5,
                      PKS = 2)

#Spesifikasi Jarak Factor Loading
loading_position <- c(PTO = 0.5, PTM = 0.5, 
                      MMS = 0.5, PMB = 0.5,
                      PKS = 0.5)

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
                   c('MMS ~~ PTO' = -5,
                     'PTM ~~ PTO' = 0.5,
                     'MMS ~~ PTM' = 0.5))

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(PKS = 0)
p_pa4 <- rotate_resid(p_pa3, rotate_resid_vector)

#Gambar Model Hipotetik yang Disesuaikan
plot(p_pa4)

pdf('SEM Mediator-Model Hipotetik.pdf')
plot(p_pa4)
dev.off()

#2. Visualisasi Hitungan Estimasi
#Gambar Hitungan Estimasi
p_est <- semPaths(uji_sem_med, whatLabels="est",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 7, 
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
                    c('MMS ~~ PTO' = -5,
                      'PTM ~~ PTO' = 0.5,
                      'MMS ~~ PTM' = 0.5))

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(PKS = 0)
p_est4 <- rotate_resid(p_est3, rotate_resid_vector)

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_est5 <- mark_sig(p_est4, uji_sem_med, 
                   alphas = c('*' = 0.05, ' ' = 1))

#Gambar Model Hipotetik yang Disesuaikan
plot(p_est5)

pdf('SEM Mediator-Hitungan Estimasi.pdf')
plot(p_est5)
dev.off()

#3. Visualisasi Hitungan Standardized
#Gambar Hitungan Standardized
p_std <- semPaths(uji_sem_med, whatLabels="std",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 7, 
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
                    c('MMS ~~ PTO' = -5,
                      'PTM ~~ PTO' = 0.5,
                      'MMS ~~ PTM' = 0.5))

#Menyesuaikan Posisi Residual
p_std4 <- rotate_resid(p_std3, rotate_resid_vector)

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_std5 <- mark_sig(p_std4, uji_sem_med, 
                   alphas = c('*' = 0.05, ' ' = 1))

#Gambar Model Hipotetik yang Disesuaikan
plot(p_std5)

pdf('SEM Mediator-Hitungan Standardized.pdf')
plot(p_std5)
dev.off()


#Simpan
sink('Hasil Analisis SEM Mediator.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(semTools::mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil SEM Mediator***', '\n')
summary(uji_sem_med, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
semTools::compRelSEM(uji_sem_med)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
semTools::AVE(uji_sem_med)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
semTools::htmt(model_sem_med, data)
cat('\n')
sink()

pdf('SEM Mediator-Model Hipotetik.pdf')
plot(p_pa4)
dev.off()

pdf('SEM Mediator-Hitungan Estimasi.pdf')
plot(p_est5)
dev.off()

pdf('SEM Mediator-Hitungan Standardized.pdf')
plot(p_std5)
dev.off()

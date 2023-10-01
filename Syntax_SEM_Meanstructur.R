#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_SEM.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data), 3)

#Spesifikasi Model
model_sem_mean <- '
                  PEND_K =~ PEND_K1 + PEND_K2 + PEND_K3 + 
                            PEND_K4 + PEND_K5
                  PAND_K =~ PAND_K1 + PAND_K2 + PAND_K3 + 
                            PAND_K4 + PAND_K5
                  PENG_K =~ PENG_K1 + PENG_K2 + PENG_K3 + 
                            PENG_K4 + PENG_K5
                  PERS_K =~ PERS_K1 + PERS_K2 + PERS_K3 + 
                            PERS_K4 + PERS_K5
                  PERS_K ~ PEND_K + PAND_K + PENG_K
                  
                  #Atur Intecept Salah Satu Indikator
                  #Variabel Laten = 0
                  PEND_K1 ~ 0*1
                  PAND_K1 ~ 0*1
                  PENG_K1 ~ 0*1
                  PERS_K1 ~ 0*1
                  
                  #Atur Intecept Salah Satu 
                  #Variabel Laten = 0
                  PERS_K ~ 0*1
                  PEND_K ~ 1
                  PAND_K ~ 1
                  PENG_K ~ 1
                  '

#Estimasi Model
uji_sem_mean <- sem(model_sem_mean, data=data, 
                    meanstructure = T)
summary(uji_sem_mean, fit.measure = T, standardized = T, 
        rsquare = T)

#Reliabilitas
compRelSEM(uji_sem_mean)

#Validitas Konvergen
AVE(uji_sem_mean)

#Validitas Diskriminan
htmt(model_sem_mean, data)


#1. Visualisasi Model Hipotetik
p_pa <- semPaths(uji_sem_mean, whatLabels = 'path',
                 style = "lisrel", edge.label.cex = 0.4, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0,
                 nCharEdges = 0, rotation = 2, 
                 mar = c(2,1,2,2), curvature = c(-4, -3))
plot(p_pa)

pdf('SEM Meanstructure-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

#2. Visualisasi Hitungan Estimasi
p_est <- semPaths(uji_sem_mean, whatLabels = 'est',
                  style = "lisrel", edge.label.cex = 0.4, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  mar = c(2,1,2,2), curvature = c(-4, -3))
plot(p_est)

pdf('SEM Meanstructure-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

#3. Visualisasi Hitungan Standardized
p_std <- semPaths(uji_sem_mean, whatLabels = 'std',
                  style = "lisrel", edge.label.cex = 0.4, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  mar = c(2,1,2,2), curvature = c(-4, -3))
plot(p_std)

pdf('SEM Meanstructure-Hitungan Standardized.pdf')
plot(p_std)
dev.off()

#Simpan
sink('Hasil Analisis SEM Meanstructure.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil SEM Meanstructure***', '\n')
summary(uji_sem, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
compRelSEM(uji_sem_mean)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_sem_mean)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
htmt(model_sem_mean, data)
cat('\n')
sink()

pdf('SEM Meanstructure-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

pdf('SEM Meanstructure-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

pdf('SEM Meanstructure-Hitungan Standardized.pdf')
plot(p_std)
dev.off()

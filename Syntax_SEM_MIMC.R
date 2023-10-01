#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_SEM_MIMC.csv', sep = ';')
head(data)

#Buat Variabel Dummy
data[5] <- ifelse(data[,5] == 2, 1, 0)
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data[,-5]), 3)

#Spesifikasi Model
model_sem_mimc <- '
                  MI =~ MI1 + MI2 + MI3 + MI4
                  MI ~ JK
                  '

#Estimasi Model
uji_sem_mimc <- sem(model_sem_mimc, data = data, 
                   estimator = 'WLSMV')
summary(uji_sem_mimc, fit.measure = T, standardized = T, 
        rsquare = T)

#Reliabilitas
compRelSEM(uji_sem_mimc)

#Validitas Konvergen
AVE(uji_sem_mimc)

#1. Visualisasi Model Hipotetik
#Gambar Model Hipotetik
p_pa <- semPaths(uji_sem_mimc, whatLabels="path",
                 style = "lisrel", edge.label.cex = 0.8, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 4, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0,
                 nCharEdges = 0, rotation = 2, 
                 intercepts = F)

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(MI = -180)
p_pa1 <- rotate_resid(p_pa, rotate_resid_vector)

#Gambar Model Hipotetik yang Disesuaikan
plot(p_pa1)


pdf('MIMC-Model Hipotetik.pdf')
plot(p_pa1)
dev.off()

#2. Visualisasi Hitungan Estimasi
#Gambar Hitungan Estimasi
p_est <- semPaths(uji_sem_mimc, whatLabels = "est",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 4, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#Menyesuaikan Posisi Residual
p_est1 <- rotate_resid(p_est, rotate_resid_vector)

#Buat Tanda Signifikansi Jalur
p_est2 <- mark_sig(p_est1, uji_sem_mimc, 
                  alphas = c('*' = 0.05, ' ' = 1))

#Gambar Hitungan Estimasi yang Disesuaikan
plot(p_est2)


pdf('MIMC-Hitungan Estimasi.pdf')
plot(p_est2)
dev.off()

#3. Visualisasi Hitungan Standardized
#Gambar Hitungan Standardized
p_std <- semPaths(uji_sem_mimc, whatLabels = "std",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 4, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#Menyesuaikan Posisi Residual
p_std1 <- rotate_resid(p_std, rotate_resid_vector)

#Buat Tanda Signifikansi Jalur
p_std2 <- mark_sig(p_std1, uji_sem_mimc, 
                   alphas = c('*' = 0.05, ' ' = 1))

#Gambar Hitungan Standardized yang Disesuaikan
plot(p_std2)


pdf('MIMC-Hitungan Standardized.pdf')
plot(p_std2)
dev.off()

#Simpan
sink('Hasil Analisis MIMC.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data[,-5]), 3)
cat('\n')
cat('***Ringkasan Hasil MIMC***', '\n')
summary(uji_sem_mimc, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
compRelSEM(uji_sem_mimc)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_sem_mimc)
cat('\n')
sink()

pdf('MIMC-Model Hipotetik.pdf')
plot(p_pa1)
dev.off()

pdf('MIMC-Hitungan Estimasi.pdf')
plot(p_est2)
dev.off()

pdf('MIMC-Hitungan Standardized.pdf')
plot(p_std2)
dev.off()


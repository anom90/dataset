
data <- read.csv('Data_ESEM.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(semTools::mardiaKurtosis(data), 3)

#Spesifikasi Model
model_esem <- '
              #efa
              efa("KS")*F1 +
              efa("KS")*F2 +
              efa("KS")*F3 +
              efa("KS")*F4 =~ KS1 + KS2 + KS3 + KS4 +
                              KS5 + KS6 + KS7 + KS8 +
                              KS9 + KS10 + KS11 + KS12
                  
              #cfa
              KA =~ SIK + KOG + KET
                  
              #regresi
              KA ~ F1 + F2 + F3 + F4
              '

#Estimasi Model
uji_esem <- sem(model_esem, data=data)
summary(uji_esem, fit.measure = T, standardized = T, 
        rsquare = T)

#1. Visualisasi Model Hipotetik
#Gambar Model Hipotetik
p_pa <- semPaths(uji_esem, whatLabels = 'path',
              style = 'ram', edge.label.cex = 0.5, 
              nDigits = 2, edge.color = 'black', 
              shapeMan = 'rectangle', sizeMan = 8, 
              sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
              equalizeManifests = T, nCharNodes = 0, 
              nCharEdges = 0, rotation = 2,
              mar = c(1,3,1,3), curvature = -5)

#Menyesuaikan Posisi Atribut Kurva 
p_pa2 <- set_curve(p_pa, 
                   c('F1 ~~ F2' = -1.5,
                     'F1 ~~ F3' = -2.8,
                     'F1 ~~ F4' = -2.8,
                     'F2 ~~ F3' = -1.5,
                     'F2 ~~ F4' = -2.8,
                     'F3 ~~ F4' = -1.5))

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(F3 = 0, F2 = 0, KA = 0)
p_pa3 <- rotate_resid(p_pa2, rotate_resid_vector)

#Gambar Model Hipotetik yang Disesuaikan
plot(p_pa3)

#2. Visualisasi Hitungan Estimasi
#Gambar Hitungan Estimasi
p_est <- semPaths(uji_esem, whatLabels = 'est',
                 style = 'ram', edge.label.cex = 0.5, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0, 
                 nCharEdges = 0, rotation = 2,
                 mar = c(1,3,1,3), curvature = -5)

#Menyesuaikan Posisi Atribut Kurva 
p_est2 <- set_curve(p_est, 
                   c('F1 ~~ F2' = -1.5,
                     'F1 ~~ F3' = -2.8,
                     'F1 ~~ F4' = -2.8,
                     'F2 ~~ F3' = -1.5,
                     'F2 ~~ F4' = -2.8,
                     'F3 ~~ F4' = -1.5))

#Menyesuaikan Posisi Residual
p_est3 <- rotate_resid(p_est2, rotate_resid_vector)

#Gambar Hitungan Estimasi yang Disesuaikan
plot(p_est3)

#3. Visualisasi Hitungan Standardized
#Gambar Hitungan Standardized
p_std <- semPaths(uji_esem, whatLabels = 'std',
                  style = 'ram', edge.label.cex = 0.5, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2,
                  mar = c(1,3,1,3), curvature = -5)

#Menyesuaikan Posisi Atribut Kurva 
p_std2 <- set_curve(p_std, 
                    c('F1 ~~ F2' = -1.5,
                      'F1 ~~ F3' = -2.8,
                      'F1 ~~ F4' = -2.8,
                      'F2 ~~ F3' = -1.5,
                      'F2 ~~ F4' = -2.8,
                      'F3 ~~ F4' = -1.5))

#Menyesuaikan Posisi Residual
p_std3 <- rotate_resid(p_std2, rotate_resid_vector)

#Gambar Hitungan Standardized yang Disesuaikan
plot(p_std3)


pdf('ESEM-Model Hipotetik.pdf')
plot(p_pa3)
dev.off()

pdf('ESEM-Hitungan Estimasi.pdf')
plot(p_est3)
dev.off()

pdf('ESEM-Hitungan Standardized.pdf')
plot(p_std3)
dev.off()

#Simpan
sink('Hasil Analisis ESEM.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(semTools::mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil ESEM***', '\n')
summary(uji_esem, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
sink()

pdf('ESEM-Model Hipotetik.pdf')
plot(p_pa3)
dev.off()

pdf('ESEM-Hitungan Estimasi.pdf')
plot(p_est3)
dev.off()

pdf('ESEM-Hitungan Standardized.pdf')
plot(p_std3)
dev.off()




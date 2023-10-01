#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_Path.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data), 3)

#Spesifikasi Model

mod_path <- '
            K_Pend ~ a*K_Dos + b*K_Mhs + d*K_Prog
            K_Prog ~ e*K_Dos + f*K_Mhs + g*K_Staf
            
            #Efek Tidak Langsung dan Total Efek
            etlAE := e*d
            totAE := a + e*d
            etlBE := f*d
            totBE := b + f*d
            etlCE := g*d
            '
#Estimasi Model
uji_path <- sem(mod_path, data=data)
summary(uji_path, fit.measure = T, standardized = T, 
        rsquare = T)

#1. Visualisasi Model Hipotetik
#Buat Matriks Plot Analisis Jalur
m <- matrix(c('K_Dos',   NA,   NA,
              'K_Staf', 'K_Prog', 'K_Pend',
              'K_Mhs',   NA,   NA), byrow = TRUE, 3, 3)

#Buat Plot Analisis Jalur sesuai Matriks
p_pa <- semPaths(uji_path, whatLabels = 'path',
                 style = 'lisrel', edge.label.cex = 1, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0, 
                 nCharEdges = 0, rotation = 2, layout = m)

#Atur Jalur Korelasi sesuai Kebutuhan
curve_vector <- c('K_Mhs ~~ K_Dos' = -3,
                  'K_Staf ~~ K_Dos' = -1.5,
                  'K_Staf ~~ K_Mhs' = 1.5)

#Buat Plot Analisis Jalur sesuai "curve_vector"
p_pa2 <- set_curve(p_pa, curve_vector)
plot(p_pa2)
#Atur Jalur Residual sesuai Kebutuhan
resid_vector <- c(K_Prog = 1)

#Buat Plot Analisis Jalur sesuai "resid_vector"
p_pa3 <- rotate_resid(p_pa2, resid_vector)
plot(p_pa3)

pdf('Analisis Jalur-Model Hipotetik.pdf')
plot(p_pa3)
dev.off()

#2. Visualisasi Hitungan Estimasi
#Buat Plot Analisis Jalur sesuai Matriks
p_est <- semPaths(uji_path, whatLabels = 'est',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2, layout = m)

#Buat Plot Analisis Jalur sesuai "resid_vector"
p_est2 <- set_curve(p_est, curve_vector)
plot(p_est2)

#Buat Plot Analisis Jalur sesuai "resid_vector"
p_est3 <- rotate_resid(p_est2, rotate_resid_vector)
plot(p_est3)

pdf('Analisis Jalur-Hitungan Estimasi.pdf')
plot(p_est3)
dev.off()

#3. Visualisasi Hitungan Standardized
#Buat Plot Analisis Jalur sesuai Matriks
p_std <- semPaths(uji_path, whatLabels = 'std',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2, layout = m)

#Buat Plot Analisis Jalur sesuai "resid_vector"
p_std2 <- set_curve(p_std, curve_vector)
plot(p_std2)

#Buat Plot Analisis Jalur sesuai "resid_vector"
p_std3 <- rotate_resid(p_std2, rotate_resid_vector)
plot(p_std3)

pdf('Analisis Jalur-Hitungan Standardized.pdf')
plot(p_std3)
dev.off()

#Simpan
sink('Hasil Analisis Jalur.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil Analisis Jalur***', '\n')
summary(uji_path, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
sink()

pdf('Analisis Jalur-Model Hipotetik.pdf')
plot(p_pa3)
dev.off()

pdf('Analisis Jalur-Hitungan Estimasi.pdf')
plot(p_est3)
dev.off()

pdf('Analisis Jalur-Hitungan Standardized.pdf')
plot(p_std3)
dev.off()
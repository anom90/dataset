#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_CFA_4.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data), 3)

#Spesifikasi Model
model_cfa_modif <- '
                    K =~ K_1 + K_2 + K_3 + K_4 + K_5
                  '

#Estimasi Model
uji_cfa_modif <- sem(model_cfa_modif, data = data)
summary(uji_cfa_modif, fit.measure = T, standardized = T, 
        rsquare = T)

#Modifikasi Model
modificationindices(uji_cfa_modif, sort. = T)

#Spesifikasi Model Modifikasi
model_cfa_modif <- '
                    K =~ K_1 + K_2 + K_3 + K_4 + K_5
                    
                    #Tambahkan Error Varians
                    K_1 ~~ K_2
                  '
#Estimasi Model Modifikasi
uji_cfa_modif <- sem(model_cfa_modif, data=data)
summary(uji_cfa_modif, fit.measure = T, standardized = T, 
        rsquare = T)

#Reliabilitas
compRelSEM(uji_cfa_modif)

#Validitas Konvergen
AVE(uji_cfa_modif)

#1. Visualisasi Model Hipotetik
p_pa <- semPaths(uji_cfa_modif, whatLabels = 'path', 
                 style = 'lisrel', edge.label.cex = 1, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0, 
                 nCharEdges = 0, rotation = 2)

pdf('CFA Modifikasi-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

#2. Visualisasi Hitungan Estimasi
p_est <- semPaths(uji_cfa_modif, whatLabels = 'est',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Modifikasi-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

#3. Visualisasi Hitungan Standardized
p_std <- semPaths(uji_cfa_modif, whatLabels = 'std',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Modifikasi-Hitungan Standardized.pdf')
plot(p_std)
dev.off()

#Simpan
sink('Hasil Analisis CFA Modifikasi.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil CFA Modifikasi***', '\n')
summary(uji_cfa_modif, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Estimasi Reliabilitas***', '\n')
compRelSEM(uji_cfa_modif)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_cfa_modif)
cat('\n')
sink()

pdf('CFA Modifikasi-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

pdf('CFA Modifikasi-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

pdf('CFA Modifikasi-Hitungan Standardized.pdf')
plot(p_std)
dev.off()
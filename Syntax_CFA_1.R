pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

data <- read.csv('Data_CFA_1.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(semTools::mardiaKurtosis(data), 3)

#Spesifikasi Model
model_cfa_uni <- '
                  MB =~ MB_1 + MB_2 + 
                        MB_3 + MB_4 + 
                        MB_5
                  '

#Estimasi Model
uji_cfa_uni <- sem(model_cfa_uni, data=data)
summary(uji_cfa_uni, fit.measure = T, standardized = T, 
        rsquare = T)

#Validitas Konvergen
semTools::AVE(uji_cfa_uni)

#Reliabilitas
semTools::compRelSEM(uji_cfa_uni)

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavaan::lavPredict(uji_cfa_uni)

#Menggabungkan Skor Faktor dengan Data Asli
data_gab <- round(cbind(data, s_faktor),2)
head(data_gab)

#1. Visualisasi Model Hipotetik
p_pa <- semPaths(uji_cfa_uni, whatLabels = 'path', 
                 style = 'lisrel', edge.label.cex = 1, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0, 
                 nCharEdges = 0, rotation = 2)

pdf('CFA Unidimensi-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

#2. Visualisasi Hitungan Estimasi
p_est <- semPaths(uji_cfa_uni, whatLabels = 'est',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Unidimensi-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

#3. Visualisasi Hitungan Standardized
p_std <- semPaths(uji_cfa_uni, whatLabels = 'std',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Unidimensi-Hitungan Standardized.pdf')
plot(p_std)
dev.off()

#Simpan
sink('Hasil Analisis CFA Unidimensi.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(semTools::mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil CFA Unidimensi***', '\n')
summary(uji_cfa_uni, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
semTools::AVE(uji_cfa_uni)
cat('\n')
cat('***Hasil Estimasi Relibilitas Komposit***', '\n')
semTools::compRelSEM(uji_cfa_uni)
cat('\n')
cat('***Hasil Estimasi Skor Faktor***', '\n')
data_gab
sink()

pdf('CFA Unidimensi-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

pdf('CFA Unidimensi-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

pdf('CFA Unidimensi-Hitungan Standardized.pdf')
plot(p_std)
dev.off()






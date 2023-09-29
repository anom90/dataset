
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

data <- read.csv('Data_CFA_2.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(semTools::mardiaKurtosis(data[,-11]), 3)

#Spesifikasi Model
model_cfa_multi <- '
                  AK =~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5
                  MK =~ MK_1 + MK_2 + MK_3 + MK_4 + MK_5
                  '

#Estimasi Model
uji_cfa_multi <- sem(model_cfa_multi, data=data)
summary(uji_cfa_multi, fit.measure = T, standardized = T, 
        rsquare = T)

#Validitas Konvergen
semTools::AVE(uji_cfa_multi)

#Validitas Diskriminan
semTools::htmt(model_cfa_multi, data)

#Reliabilitas
semTools::compRelSEM(uji_cfa_multi, return.total = T)

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavaan::lavPredict(uji_cfa_multi)

#Menggabungkan Skor Faktor dengan Data Asli
data_gab <- round(cbind(data[,-11], s_faktor),2)
head(data_gab)

#1. Visualisasi Model Hipotetik
p_pa <- semPaths(uji_cfa_multi, whatLabels = 'path', 
                 style = 'lisrel', edge.label.cex = 1, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0, 
                 nCharEdges = 0, rotation = 2)

pdf('CFA Multidimensi-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

#2. Visualisasi Hitungan Estimasi
p_est <- semPaths(uji_cfa_multi, whatLabels = 'est',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Multidimensi-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

#3. Visualisasi Hitungan Standardized
p_std <- semPaths(uji_cfa_multi, whatLabels = 'std',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Multidimensi-Hitungan Standardized.pdf')
plot(p_std)
dev.off()

#Simpan
sink('Hasil Analisis CFA Multidimensi.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(semTools::mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil CFA Multidimensi***', '\n')
summary(uji_cfa_uni, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
semTools::AVE(uji_cfa_multi)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
semTools::htmt(model_cfa_multi, data)
cat('***Hasil Estimasi Relibilitas***', '\n')
semTools::compRelSEM(uji_cfa_multi, return.total = T)
cat('\n')
sink()

pdf('CFA Multidimensi-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

pdf('CFA Multidimensi-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

pdf('CFA Multidimensi-Hitungan Standardized.pdf')
plot(p_std)
dev.off()



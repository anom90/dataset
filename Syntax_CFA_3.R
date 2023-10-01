#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_CFA_2.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data[,-11]), 3)

#Spesifikasi Model
model_cfa_2orde <- '
                  AK =~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5
                  MK =~ MK_1 + MK_2 + MK_3 + MK_4 + MK_5
                  KP =~ AK + MK

                  #Atur Nilai Varians Variabel Laten
                  AK ~~ e*AK
                  MK ~~ e*MK
                  '

#Estimasi Model
uji_cfa_2orde <- sem(model_cfa_2orde, data=data)
summary(uji_cfa_2orde, fit.measure = T, standardized = T, 
        rsquare = T)

#Validitas Konvergen
AVE(uji_cfa_2orde)

#Reliabilitas
compRelSEM(uji_cfa_2orde, higher = "KP")

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavPredict(uji_cfa_2orde)

#Menggabungkan Skor Faktor dengan Data Asli
data_gab <- round(cbind(data[,-11], s_faktor),2)
head(data_gab)

#1. Visualisasi Model Hipotetik
p_pa <- semPaths(uji_cfa_2orde, whatLabels = 'path', 
                 style = 'lisrel', edge.label.cex = 1, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 8, 
                 sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0, 
                 nCharEdges = 0, rotation = 2)

pdf('CFA Second Orde-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

#2. Visualisasi Hitungan Estimasi
p_est <- semPaths(uji_cfa_2orde, whatLabels = 'est',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Second Orde-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

#3. Visualisasi Hitungan Standardized
p_std <- semPaths(uji_cfa_2orde, whatLabels = 'std',
                  style = 'lisrel', edge.label.cex = 1, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 8, 
                  sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0, 
                  nCharEdges = 0, rotation = 2)

pdf('CFA Second Orde-Hitungan Standardized.pdf')
plot(p_std)
dev.off()

#Simpan
sink('Hasil Analisis CFA Second Orde.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil CFA Second Orde***', '\n')
summary(uji_cfa_2orde, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_cfa_2orde)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
compRelSEM(uji_cfa_2orde, higher = "KP")
cat('\n')
cat('***Hasil Estimasi Skor Faktor***', '\n')
data_gab
cat('\n')
sink()

pdf('CFA Second Ordei-Model Hipotetik.pdf')
plot(p_pa)
dev.off()

pdf('CFA Second Orde-Hitungan Estimasi.pdf')
plot(p_est)
dev.off()

pdf('CFA Second Orde-Hitungan Standardized.pdf')
plot(p_std)
dev.off()



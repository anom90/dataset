#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_CFA_2.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data[,-11]), 3)

#Spesifikasi Model
model_cfa_group <- '
                  AK =~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5
                  MK =~ MK_1 + MK_2 + MK_3 + MK_4 + MK_5
                  KP =~ AK + 1*MK
                  '

#Estimasi Model
uji_mgcfa <- measurementInvariance(model = model_cfa_group,
                                  data = data,
                                  group = "JK",
                                  strict = T)

summary(uji_mgcfa$fit.configural, fit.measure = T, 
        standardized = T, rsquare = T) #untuk model CI

summary(uji_mgcfa$fit.loadings, fit.measure = T, 
        standardized = T, rsquare = T) #untuk model WI

summary(uji_mgcfa$fit.intercepts, fit.measure = T, 
        standardized = T, rsquare = T) #untuk model SI

summary(uji_mgcfa$fit.residuals, fit.measure = T, 
        standardized = T, rsquare = T) #untuk model STI

summary(uji_mgcfa$fit.means, fit.measure = T, 
        standardized = T, rsquare = T) #untuk model M


#Reliabilitas
compRelSEM(uji_mgcfa$fit.configural, higher = "KP")

#Validitas Konvergen
AVE(uji_mgcfa$fit.configural)

#1. Visualisasi Model Hipotetik
par(mfrow = c(1,2))
semPaths(uji_mgcfa$fit.configural, whatLabels = 'path', 
        style = 'lisrel', edge.label.cex = 1, 
        nDigits = 2, edge.color = 'black', 
        shapeMan = 'rectangle', sizeMan = 8, 
        sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
        equalizeManifests = T, nCharNodes = 0, 
        nCharEdges = 0, rotation = 2, intercepts = F)

pdf('CFA Second Orde-Model Hipotetik%03d.pdf', onefile = F)
semPaths(uji_mgcfa$fit.configural, whatLabels = 'path', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)
dev.off()

#2. Visualisasi Hitungan Estimasi
semPaths(uji_mgcfa$fit.configural, whatLabels = 'est', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)

pdf('CFA Second Orde-Hitungan Estimasi%03d.pdf', onefile = F)
semPaths(uji_mgcfa$fit.configural, whatLabels = 'est', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)
dev.off()

#3. Visualisasi Hitungan Standardized
semPaths(uji_mgcfa$fit.configural, whatLabels = 'std', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)

pdf('CFA Second Orde-Hitungan Standardized%03d.pdf', onefile = F)
semPaths(uji_mgcfa$fit.configural, whatLabels = 'std', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)
dev.off()

#Simpan
sink('Hasil Analisis CFA Multiple-Group.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data[,-11]), 3)
cat('\n')
cat('***Ringkasan Hasil CFA Multiple-Group***', '\n')
summary(uji_mgcfa$fit.configural, fit.measure = T, 
        standardized = T, rsquare = T)
cat('\n')
cat('***Ringkasan Hasil Perbadingan Model***', '\n')
uji_mgcfa <- measurementInvariance(model = model_cfa_group,
                                   data = data,
                                   group = "JK",
                                   strict = T)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_mgcfa$fit.configural)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
compRelSEM(uji_mgcfa$fit.configural, higher = "KP")
cat('\n')
sink()

pdf('CFA Second Orde-Model Hipotetik.pdf')
par(mfrow = c(1,2))
semPaths(uji_cfa1, whatLabels = 'path', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)
dev.off()

pdf('CFA Second Orde-Hitungan Estimasi.pdf')
par(mfrow = c(1,2))
semPaths(uji_cfa1, whatLabels = 'est', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)
dev.off()

pdf('CFA Second Orde-Hitungan Standardize.pdf')
par(mfrow = c(1,2))
semPaths(uji_cfa1, whatLabels = 'std', 
         style = 'lisrel', edge.label.cex = 1, 
         nDigits = 2, edge.color = 'black', 
         shapeMan = 'rectangle', sizeMan = 8, 
         sizeMan2 = 5, sizeLat = 9, sizeLat2 = 6, 
         equalizeManifests = T, nCharNodes = 0, 
         nCharEdges = 0, rotation = 2, intercepts = F)
dev.off()




#Jalankan Paket Analisis
pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

#Panggil Data
data <- read.csv('Data_MSEM.csv', sep = ',')
head(data)

round(mardiaKurtosis(data[,-c(1:2)]), 3)

model_msem <- '
              level: within
                    wses =~ a*focc + b*meduc + c*feduc
                    galo ~ wa*wses
                    advice ~ wc*wses + wb*galo

              level: between
                    bses =~ a*focc + b*meduc + c*feduc
                    galo ~ ba*bses + denom
                    advice ~ bc*bses + bb*galo
                    
                    feduc ~~ 0*feduc
                    
                    #Efek Tidak Langsung dan Total Efek
                    wi := wa * wb
                    bi := ba * bb
                    wt := wc + wa * wb
                    bt := bc + ba * bb
                '
uji_msem <- sem(model_msem, data = data, 
                cluster = "school")        
summary(uji_msem, fit.measures = T, standardized = T)

#ICC
lavInspect(uji_msem, "icc")
mean(lavInspect(uji_msem, "icc")[1:3])

#Reliabilitas
compRelSEM(uji_msem)

#Validitas Konvergen
AVE(uji_msem)

#1. Visualisasi Model Hipotetik
#Spesifikasi Model Level: 1 (Student-Level)
lev1 <- '
        wses =~ a*focc + b*meduc + c*feduc
        advice ~ wc*wses + wb*galo
        galo ~ wa*wses
        '

#Spesifikasi Model Level: 2 (School-Level)
lev2 <- '
         bses =~ a*focc + b*meduc + c*feduc
                    
         feduc ~~ 0*feduc
                    
         advice ~ bc*bses + bb*galo
         galo ~ ba*bses + denom
        '

#Pengujian Model Level: 1 dan Level: 2
uji.lev1 <- sem(lev1, data = data)
uji.lev2 <- sem(lev2, data = data, 
                cluster = "school")

#Gambar Model Hipotetik Level: 1
p_pa <- semPaths(uji.lev1, whatLabels = "path",
                 style = "lisrel", edge.label.cex = 0.8, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 7, 
                 sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0,
                 nCharEdges = 0, rotation = 2, 
                 intercepts = F)


#Gambar Model Hipotetik Level: 2
p_pas <- semPaths(uji.lev2, whatLabels = "path",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 7, 
                  sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#2. Visualisasi Hitungan Estimasi
lavInspect(uji_msem, 'est')
#Spesifikasi Model Level: 1 (Student-Level)
lev1_est <- '
            wses =~ 1.00*focc + 1.41*meduc + 2.09*feduc
            advice ~ 0.12*wses + 0.12*galo
            galo ~ 0.34*wses
            
            feduc ~~ 1.44*feduc
            focc ~~ 2.31*focc
            meduc ~~ 1.24*meduc
            advice ~~ 0.61*advice
            galo ~~ 0.85*galo
            wses ~~ 0.81*wses
            '

#Spesifikasi Model Level: 2 (School-Level)
lev2_est <- '
            bses =~ 1.00*focc + 1.41*meduc + 2.09*feduc
                    
            feduc ~~ 0.00*feduc
            focc ~~ 0.03*focc
            meduc ~~ 0.04*meduc
            advice ~~ 0.02*advice
            galo ~~ 0.04*galo
            bses ~~ 0.46*bses
                    
            advice ~ 0.39*bses + 0.65*galo
            galo ~ 0.50*bses + -0.15*denom
            '

#Pengujian Model Level: 1 dan Level: 2
uji.lev.est1 <- sem(lev1_est, data = data)
uji.lev.est2 <- sem(lev2_est, data = data, 
                    cluster = "school")

#Gambar Hitungan Estimasi Level: 1
p_est <- semPaths(uji.lev.est1, whatLabels = "est",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 7, 
                  sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#Gambar Hitungan Estimasi Level: 2
p_ests <- semPaths(uji.lev.est2, whatLabels = "est",
                   style = "lisrel", edge.label.cex = 0.8, 
                   nDigits = 2, edge.color = 'black', 
                   shapeMan = 'rectangle', sizeMan = 7, 
                   sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                   equalizeManifests = T, nCharNodes = 0,
                   nCharEdges = 0, rotation = 2, 
                   intercepts = F)

#3. Visualisasi Hitungan Standardized
lavInspect(uji_msem, 'std')
#Spesifikasi Model Level: 1 (Student-Level)
lev1_std <- '
            wses =~ 0.60*focc + 0.64*meduc + 0.86*feduc
            advice ~ 0.08*wses + 0.78*galo
            galo ~ 0.33*wses
            
            feduc ~~ 0.64*feduc
            focc ~~ 0.59*focc
            meduc ~~ 0.26*meduc
            advice ~~ 0.35*advice
            galo ~~ 0.89*galo
            wses ~~ 1.00*wses
            '

#Spesifikasi Model Level: 2 (School-Level)
lev2_std <- '
            bses =~ 0.97*focc + 0.98*meduc + 1.00*feduc
                    
            feduc ~~ 0.00*feduc
            focc ~~ 0.06*focc
            meduc ~~ 0.04*meduc
            advice ~~ 0.06*advice
            galo ~~ 0.23*galo
            bses ~~ 1.00*bses
                    
            advice ~ 0.51*bses + 0.51*galo
            galo ~ 0.84*bses + -0.18*denom
            '

#Pengujian Model Level: 1 dan Level: 2
uji.lev.std1 <- sem(lev1_std, data = data)
uji.lev.std2 <- sem(lev2_std, data = data, 
                    cluster = "school")

#Gambar Hitungan Standardized Level: 1
p_std <- semPaths(uji.lev.std1, whatLabels = "std",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 7, 
                  sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#Gambar Hitungan Standardized Level: 2
p_stds <- semPaths(uji.lev.std2, whatLabels = "std",
                   style = "lisrel", edge.label.cex = 0.8, 
                   nDigits = 2, edge.color = 'black', 
                   shapeMan = 'rectangle', sizeMan = 7, 
                   sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                   equalizeManifests = T, nCharNodes = 0,
                   nCharEdges = 0, rotation = 2, 
                   intercepts = F)




#Simpan
sink('Hasil Analisis SEM Multilevel.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(mardiaKurtosis(data[,-c(1:2)]), 3)
cat('\n')
cat('***Ringkasan Hasil SEM Multilevel***', '\n')
summary(uji_msem, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
compRelSEM(uji_msem)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_msem)
cat('\n')
sink()

graphics.off()
pdf('SEM Multilevel-Model Hipotetik.pdf')
par(mfcol = c(2, 1))
plot(p_pa)
text(x = 1.5, y = -1.35, labels = 'Student-Level', cex = 0.9)
abline(h = -1.5)
plot(p_pas)
text(x = 1.5, y = 1.35, labels = 'School-Level', cex = 0.9)
dev.off()

pdf('SEM Multilevel-Hitungan Estimasi.pdf')
par(mfcol = c(2, 1))
plot(p_est)
text(x = 1.5, y = -1.35, labels = 'Student-Level', cex = 0.9)
abline(h = -1.5)
plot(p_ests)
text(x = 1.5, y = 1.35, labels = 'School-Level', cex = 0.9)
dev.off()

pdf('SEM Multilevel-Hitungan Standardized.pdf')
par(mfcol = c(2, 1))
plot(p_std)
text(x = 1.5, y = -1.35, labels = 'Student-Level', cex = 0.9)
abline(h = -1.5)
plot(p_stds)
text(x = 1.5, y = 1.35, labels = 'School-Level', cex = 0.9)
dev.off()

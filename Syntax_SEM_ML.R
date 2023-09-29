pacman::p_load('semTools', 'lavaan', 'semPlot', 
               'semptools')

data <- read.csv('Data_SEM_ML.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(semTools::mardiaKurtosis(data[,-c(1:3)]), 3)

#Multilevel
model_sem_ml <- '
                level: 1
                    CBM =~ pv1math + pv2math + pv3math + 
                           pv4math + pv5math
                    IKD =~ st81q01 + st81q02 + st81q03 + 
                           st81q04 + st81q05
                    KUB =~ st62q01 + st62q03 + st62q06 + 
                           st62q07 + st62q12 + st62q15 + 
                           st62q16
                    CBM ~ KUB
        
                level: 2
                    CBMS =~ pv1math + pv2math + pv3math + 
                            pv4math + pv5math
                    IKDS =~ st81q01s + st81q02s + st81q03s + 
                           st81q04s + st81q05s
                    KUBS =~ st62q01s + st62q03s + st62q06s + 
                            st62q07s + st62q12s + st62q15s + 
                            st62q16s
                    CBMS ~ a*IKDS + b*KUBS
                    KUBS ~ c*IKDS
                    
                    #Atur Varians Indikator CBMS
                    pv1math ~~ 1*pv1math
                    pv2math ~~ 1*pv2math
                    pv3math ~~ 1*pv3math
                    pv4math ~~ 1*pv4math
                    pv5math ~~ 1*pv5math

                    #Efek Tidak Langsung dan Total Efek
                    etl_ac := a*c
                    tot_abc := a*c + b
                '
uji_sem_ml <- sem(model_sem_ml, data = data, 
           cluster = "schoolid", estimator = 'MLR')        

summary(uji_sem_ml, fit.measures = T, standardized = T)

#ICC
lavInspect(uji_sem_ml, "icc")[1:5]
mean(lavInspect(uji_sem_ml, "icc")[1:5])

#Reliabilitas
semTools::compRelSEM(uji_sem_ml)

#Validitas Konvergen
semTools::AVE(uji_sem_ml)

#1. Visualisasi Model Hipotetik
#Spesifikasi Model Level: 1 (Student-Level)
lev1 <- '
        CBM =~ pv1math + pv2math + pv3math + 
               pv4math + pv5math
        IKD =~ st81q01 + st81q02 + st81q03 + 
                st81q04 + st81q05
        KUB =~ st62q01 + st62q03 + st62q06 + 
                st62q07 + st62q12 + st62q15 + 
                st62q16
        CBM ~ KUB
        '

#Spesifikasi Model Level: 2 (School-Level)
lev2 <- '
        CBMS =~ pv1math + pv2math + pv3math + 
                pv4math + pv5math
        IKDS =~ st81q01s + st81q02s + st81q03s + 
                st81q04s + st81q05s
        KUBS =~ st62q01s + st62q03s + st62q06s + 
                st62q07s + st62q12s + st62q15s + 
                st62q16s
        CBMS ~ a*IKDS + b*KUBS
        KUBS ~ c*IKDS
        
        #Atur Varians Indikator CBMS
        pv1math ~~ 1*pv1math
        pv2math ~~ 1*pv2math
        pv3math ~~ 1*pv3math
        pv4math ~~ 1*pv4math
        pv5math ~~ 1*pv5math
                    
        #Efek Tidak Langsung dan Total Efek
        etl_ac := a*c
        tot_abc := a*c + b
        '

#Pengujian Model Level: 1 dan Level: 2
uji.lev1 <- sem(lev1, data = data, 
                estimator = 'MLR')
uji.lev2 <- sem(lev2, data = data, 
                cluster = "schoolid", 
                estimator = 'MLR')

#Gambar Model Hipotetik Level: 1
p_pa <- semPaths(uji.lev1, whatLabels = "path",
                 style = "lisrel", edge.label.cex = 0.8, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 7, 
                 sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0,
                 nCharEdges = 0, rotation = 2, 
                 intercepts = F)

#Perbaiki Gambar Model Hipotetik Level: 1
#Spesifikasi Item/Indikator
indicator_order <- c('st81q01', 'st81q02', 'st81q03', 
                     'st81q04', 'st81q05', 
                     'st62q01', 'st62q03','st62q06', 
                     'st62q07', 'st62q12', 'st62q15', 
                     'st62q16', 
                     'pv1math', 'pv2math', 'pv3math', 
                     'pv4math', 'pv5math')

#Spesifikasi Faktor dari Item/Indikator
indicator_factor <- c('IKD', 'IKD', 'IKD', 'IKD', 'IKD',
                      'KUB', 'KUB', 'KUB', 'KUB', 'KUB',
                      'KUB', 'KUB', 'CBM', 'CBM', 'CBM',
                      'CBM', 'CBM')

#Spesifikasi Tata Letak Faktor
factor_layout <- matrix(c('KUB', NA,
                          NA, 'CBM',
                          'IKD', NA), 
                        byrow = TRUE, 3, 2)

#Spesifikasi Tata Letak Item/Indiktor setiap Faktor
factor_point_to <- matrix(c('up', NA,
                            NA, 'right',
                            'down', NA), 
                          byrow = TRUE, 3, 2)

#Spesifikasi Jarak Item/Indikator terhadap Faktor
indicator_push <- c(KUB =1.3, CBM = 1.3, 
                    IKD = 1.3)

#Spesifikasi Jarak antar Item/Indikator setiap Faktor
indicator_spread <- c(KUB =2.5, CBM = 3, 
                      IKD = 2.5)

#Spesifikasi Jarak Factor Loading
loading_position <- c(KUB = 0.5, CBM = 0.5, 
                      IKD = 0.5)

#Gambar Model Hipotetik Level: 1 sesuai Spesifikasi
p_pa2 <- set_sem_layout(p_pa,
                  indicator_order = indicator_order,
                  indicator_factor = indicator_factor,
                  factor_layout = factor_layout,
                  factor_point_to = factor_point_to,
                  indicator_push = indicator_push,
                  indicator_spread = indicator_spread,
                  loading_position = loading_position)

#Menyesuaikan Posisi Atribut Kurva 
p_pa3 <- set_curve(p_pa2, 
                   c('IKD ~~ KUB' = 1.5))

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(CBM = 0)
p_pa4 <- rotate_resid(p_pa3, rotate_resid_vector)

#Gambar Model Hipotetik Level: 1 yang Disesuaikan
plot(p_pa4)

#Gambar Model Hipotetik Level: 2
p_pas <- semPaths(uji.lev2, whatLabels = "path",
                 style = "lisrel", edge.label.cex = 0.8, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 7, 
                 sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0,
                 nCharEdges = 0, rotation = 2, 
                 intercepts = F)

#Perbaiki Gambar Model Hipotetik Level: 2
#Spesifikasi Item/Indikator
indicator_orders <- c('st81q01s', 'st81q02s', 'st81q03s', 
                     'st81q04s', 'st81q05s', 
                     'st62q01s', 'st62q03s','st62q06s', 
                     'st62q07s', 'st62q12s', 'st62q15s', 
                     'st62q16s', 
                     'pv1math', 'pv2math', 'pv3math', 
                     'pv4math', 'pv5math')

#Spesifikasi Faktor dari Item/Indikator
indicator_factors <- c('IKDS', 'IKDS', 'IKDS', 
                      'IKDS', 'IKDS',
                      'KUBS', 'KUBS', 'KUBS', 
                      'KUBS', 'KUBS',
                      'KUBS', 'KUBS', 'CBMS', 
                      'CBMS', 'CBMS',
                      'CBMS', 'CBMS')

#Spesifikasi Tata Letak Faktor
factor_layouts <- matrix(c('KUBS', NA,
                          NA, 'CBMS',
                          'IKDS', NA), 
                        byrow = TRUE, 3, 2)

#Spesifikasi Tata Letak Item/Indiktor setiap Faktor
factor_point_tos <- matrix(c('up', NA,
                            NA, 'right',
                            'down', NA), 
                          byrow = TRUE, 3, 2)

#Spesifikasi Jarak Item/Indikator terhadap Faktor
indicator_pushs <- c(KUBS =1.3, CBMS = 1.3, 
                    IKDS = 1.3)

#Spesifikasi Jarak antar Item/Indikator setiap Faktor
indicator_spreads <- c(KUBS =2.5, CBMS = 3, 
                      IKDS = 2.5)

#Spesifikasi Jarak Factor Loading
loading_positions <- c(KUBS = 0.5, CBMS = 0.5, 
                      IKDS = 0.5)

#Gambar Model Hipotetik Level: 2 sesuai Spesifikasi
p_pas2 <- set_sem_layout(p_pas,
                  indicator_order = indicator_orders,
                  indicator_factor = indicator_factors,
                  factor_layout = factor_layouts,
                  factor_point_to = factor_point_tos,
                  indicator_push = indicator_pushs,
                  indicator_spread = indicator_spreads,
                  loading_position = loading_positions)

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(CBMS = 0)
p_pas3 <- rotate_resid(p_pas2, rotate_resid_vector)

#Gambar Model Hipotetik Level: 2 yang Disesuaikan
plot(p_pas3)

#2. Visualisasi Hitungan Estimasi
lavInspect(uji_sem_ml, 'est')
#Spesifikasi Model Level: 1 (Student-Level)
lev1_est <- '
        CBM =~ 1.00*pv1math + 0.99*pv2math 
               + 0.98*pv3math + 1.01*pv4math 
               + 0.97*pv5math
        IKD =~ 1.00*st81q01 + 1.05*st81q02
               + 1.19*st81q03 + 1.17*st81q04 
               +1.06* st81q05
        KUB =~ 1.00*st62q01 + 1.22*st62q03 
               + 1.54*st62q06 + 1.63*st62q07 
               + 1.29*st62q12 + 1.49*st62q15 + 
               1.71*st62q16
        CBM ~ 0.20*KUB
        
        #Residual
        pv1math ~~ 0.14*pv1math
        pv2math ~~ 0.16*pv2math
        pv3math ~~ 0.15*pv3math
        pv4math ~~ 0.14*pv4math
        pv5math ~~ 0.16*pv5math
        st81q01 ~~ 0.37*st81q01 
        st81q02 ~~ 0.41*st81q02
        st81q03 ~~ 0.43*st81q03
        st81q04 ~~ 0.34*st81q04
        st81q05 ~~ 0.43*st81q05
        st62q01 ~~ 1.00*st62q01
        st62q03 ~~ 0.54*st62q03
        st62q06 ~~ 0.83*st62q06
        st62q07 ~~ 1.17*st62q07
        st62q12 ~~ 1.26*st62q12
        st62q15 ~~ 1.15*st62q15
        st62q16 ~~ 1.42*st62q16
        '

#Spesifikasi Model Level: 2 (School-Level)
lev2_est <- '
        CBMS =~ 1.00*pv1math + 0.99*pv2math 
                + 1.02*pv3math + 0.98*pv4math 
                + 1.01*pv5math
        IKDS =~ 1.00*st81q01s + 0.99*st81q02s 
                + 1.13*st81q03s + 1.09*st81q04s 
                + 1.05*st81q05s
        KUBS =~ 1.00*st62q01s + 2.05*st62q03s 
                + 3.58*st62q06s + 2.28*st62q07s 
                + 1.60*st62q12s + 2.57*st62q15s + 
                3.56*st62q16s
        CBMS ~ -0.11*IKDS + 3.26*KUBS
        KUBS ~ 0.12*IKDS
        
        #Residual
        pv1math ~~ 1.00*pv1math
        pv2math ~~ 1.00*pv2math
        pv3math ~~ 1.00*pv3math
        pv4math ~~ 1.00*pv4math
        pv5math ~~ 1.00*pv5math
        st81q01s ~~ 0.01*st81q01s 
        st81q02s ~~ 0.01*st81q02s
        st81q03s ~~ 0.10*st81q03s
        st81q04s ~~ 0.05*st81q04s
        st81q05s ~~ 0.07*st81q05s
        st62q01s ~~ 0.23*st62q01s
        st62q03s ~~ 0.10*st62q03s
        st62q06s ~~ 0.20*st62q06s
        st62q07s ~~ 0.35*st62q07s
        st62q12s ~~ 0.24*st62q12s
        st62q15s ~~ 0.37*st62q15s
        st62q16s ~~ 0.63*st62q16s
        '

#Pengujian Model Level: 1 dan Level: 2
uji.lev.est1 <- sem(lev1_est, data = data, 
                estimator = 'MLR')
uji.lev.est2 <- sem(lev2_est, data = data, 
                cluster = "schoolid", 
                estimator = 'MLR')

#Gambar Hitungan Estimasi Level: 1
p_est <- semPaths(uji.lev.est1, whatLabels = "est",
                 style = "lisrel", edge.label.cex = 0.8, 
                 nDigits = 2, edge.color = 'black', 
                 shapeMan = 'rectangle', sizeMan = 7, 
                 sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                 equalizeManifests = T, nCharNodes = 0,
                 nCharEdges = 0, rotation = 2, 
                 intercepts = F)

#Gambar Hitungan Estimasi Level: 1 sesuai Spesifikasi
p_est2 <- set_sem_layout(p_est,
                  indicator_order = indicator_order,
                  indicator_factor = indicator_factor,
                  factor_layout = factor_layout,
                  factor_point_to = factor_point_to,
                  indicator_push = indicator_push,
                  indicator_spread = indicator_spread,
                  loading_position = loading_position)

#Menyesuaikan Posisi Atribut Kurva 
p_est3 <- set_curve(p_est2, 
                   c('IKD ~~ KUB' = 1.5))

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(CBM = 0)
p_est4 <- rotate_resid(p_est3, rotate_resid_vector)

#Gambar Hitungan Estimasi Level: 1 yang Disesuaikan
plot(p_est4)

#Gambar Hitungan Estimasi Level: 2
p_ests <- semPaths(uji.lev.est2, whatLabels = "est",
                  style = "lisrel", edge.label.cex = 0.8, 
                  nDigits = 2, edge.color = 'black', 
                  shapeMan = 'rectangle', sizeMan = 7, 
                  sizeMan2 = 3, sizeLat = 9, sizeLat2 = 6, 
                  equalizeManifests = T, nCharNodes = 0,
                  nCharEdges = 0, rotation = 2, 
                  intercepts = F)

#Gambar Hitungan Estimasi Level: 2 sesuai Spesifikasi
p_ests2 <- set_sem_layout(p_ests,
                    indicator_order = indicator_orders,
                    indicator_factor = indicator_factors,
                    factor_layout = factor_layouts,
                    factor_point_to = factor_point_tos,
                    indicator_push = indicator_pushs,
                    indicator_spread = indicator_spreads,
                    loading_position = loading_positions)

#Menyesuaikan Posisi Residual
rotate_resid_vector <- c(CBMS = 0,
                         KUBS = 90)
p_ests3 <- rotate_resid(p_ests2, rotate_resid_vector)

#Gambar Model Hipotetik Level: 2 yang Disesuaikan
plot(p_ests3)

#Simpan
sink('Hasil Analisis SEM Multilevel.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
round(semTools::mardiaKurtosis(data), 3)
cat('\n')
cat('***Ringkasan Hasil SEM Multilevel***', '\n')
summary(uji_sem, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('***Hasil Estimasi Relibilitas***', '\n')
semTools::compRelSEM(uji_sem_ml)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
semTools::AVE(uji_sem_ml)
cat('\n')
sink()

graphics.off()
pdf('SEM Multilevel-Model Hipotetik.pdf')
par(mfcol = c(2, 1))
plot(p_pa4)
text(x = 1.5, y = -1.35, labels = 'Student-Level', cex = 0.9)
abline(h = -1.5)
plot(p_pas3)
text(x = 1.5, y = 1.35, labels = 'School-Level', cex = 0.9)
dev.off()

pdf('SEM Multilevel-Hitungan Estimasi.pdf')
par(mfcol = c(2, 1))
plot(p_est4)
text(x = 1.5, y = -1.35, labels = 'Student-Level', cex = 0.9)
abline(h = -1.5)
plot(p_ests3)
text(x = 1.5, y = 1.35, labels = 'School-Level', cex = 0.9)
dev.off()



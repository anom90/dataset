#Jalankan Paket Analisis
pacman::p_load('semTools','lavaan','semPlot','semptools', 
               'lavaanPlot','DiagrammeRsvg','rsvg','png')

#Panggil Data
data <- read.csv('Data_CFA_2.csv', sep = ',')
head(data)

#Normalitas Multivariat
round(mardiaKurtosis(data[,-11]), 3)

#Spesifikasi Model
model_cfa_group <- '
                  AK =~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5
                  MK =~ MK_1 + MK_2 + MK_3 + MK_4 + MK_5
                  KP =~ AK + MK
                  AK ~~ e*AK
                  MK ~~ e*MK
                  '

#Estimasi Model
#The measurementInvariance function is deprecated
#uji_mgcfa <- measurementInvariance(model = model_cfa_group,
#                                  data = data,
#                                  group = "JK",
#                                  strict = T)
#Ganti sbb:

#https://bookdown.org/annabrown/psychometricsR/exercise23.html
#Fitting Baseline / Configural Model
uji_mgcfa_cfg <- cfa(model = model_cfa_group,data = data,group = 'JK')
summary(uji_mgcfa_cfg,fit.measure = T, standardized = T, 
        rsquare = T)

##Fitting Measurement Invariance Model/full measurement invariance model
uji_mgcfa <- cfa(model_cfa_group, data = data, group = "JK",
              group.equal = c("loadings", "intercepts", "residuals"))
summary(uji_mgcfa, fit.measure = T, standardized = T, 
        rsquare = T)


#Validitas Konvergen Configural Model
AVE(uji_mgcfa_cfg)

#Reliabilitas Configural Model
compRelSEM(uji_mgcfa_cfg, higher = "KP")

#Validitas Konvergen Full Measurement Invariance Model
AVE(uji_mgcfa)

#Reliabilitas Full Measurement Invariance Model
compRelSEM(uji_mgcfa, higher = "KP")

#Configural Model
#1. Visualisasi Model Hipotetik
p_pa1 <- semPaths(uji_mgcfa_cfg, 'path', style = 'lisrel',
                 edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                 layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                 rotation = 2,intercepts = F,panelGroups = T,ask = F)

#2. Visualisasi Hitungan Estimasi
p_est1 <- semPaths(uji_mgcfa_cfg, 'path', 'est',style = 'lisrel',
                  edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                  layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                  rotation = 2,intercepts = F,panelGroups = T,ask = F)

#3. Visualisasi Hitungan Standardized
p_std1 <- semPaths(uji_mgcfa_cfg, 'path', 'std',style = 'lisrel',
                  edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                  layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                  rotation = 2,intercepts = F,panelGroups = T,ask = F)

#4. Memberi tanda signifikan
#Multiple-group models are not currently supported
#sig <- mark_sig(p_std,uji_mgcfa_cfg,alphas = c(`*` = 0.05, `**` = 0.01, `***` = 0.001))
#plot(sig)

#5. Visualisasi dengan lavaanPlot
# stand=T ada masalah
lp1 <- lavaanPlot(model=uji_mgcfa_cfg,coef=T,stand=F,digits=3,stars=c('regress','latent','covs'),
                 cov=T,graph_options=list(rankdir='LR'))
lp1


#Full Measurement Invariance Model
#1. Visualisasi Model Hipotetik
p_pa2 <- semPaths(uji_mgcfa, 'path', style = 'lisrel',
                 edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                 layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                 rotation = 2,intercepts = F,panelGroups = T,ask = F)

#2. Visualisasi Hitungan Estimasi
p_est2 <- semPaths(uji_mgcfa, 'path', 'est',style = 'lisrel',
                  edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                  layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                  rotation = 2,intercepts = F,panelGroups = T,ask = F)

#3. Visualisasi Hitungan Standardized
p_std2 <- semPaths(uji_mgcfa, 'path', 'std',style = 'lisrel',
                  edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                  layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                  rotation = 2,intercepts = F,panelGroups = T,ask = F)

#4. Memberi tanda signifikan
#Multiple-group models are not currently supported
#sig <- mark_sig(p_std,uji_mgcfa_cfg,alphas = c(`*` = 0.05, `**` = 0.01, `***` = 0.001))
#plot(sig)

#5. Visualisasi dengan lavaanPlot
# stand=T ada masalah
lp2 <- lavaanPlot(model=uji_mgcfa,coef=T,stand=F,digits=3,stars=c('regress','latent','covs'),
                 cov=T,graph_options=list(rankdir='LR'))
lp2

#Simpan output text
sink('Hasil Syntax_CFA_Multigroup.txt')
cat('Normalitas Multivariat\n')
round(mardiaKurtosis(data[,-11]), 3)
cat('\nConfigural Model\n\n')
summary(uji_mgcfa_cfg,fit.measure = T, standardized = T, 
        rsquare = T)
cat('\nFull Measurement Invariance Model\n\n')
summary(uji_mgcfa, fit.measure = T, standardized = T, 
        rsquare = T)
cat('\n')
cat('AVE Configural Model:\n')
AVE(uji_mgcfa_cfg)
cat('\ncompRelSEM Configural Model:\n')
compRelSEM(uji_mgcfa_cfg, higher = "KP")
cat('\n')
cat('AVE Full Measurement Invariance Model:\n')
AVE(uji_mgcfa)
cat('\ncompRelSEM Full Measurement Invariance Model:\n')
compRelSEM(uji_mgcfa, higher = "KP")
sink()

##Simpan output gambar
pdf('Hasil Syntax_CFA_Multigroup.pdf',paper = 'a4')
p_pa1 <- semPaths(uji_mgcfa_cfg, 'path', style = 'lisrel',
                  edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                  layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                  rotation = 2,intercepts = F,panelGroups = T,ask = F)
p_est1 <- semPaths(uji_mgcfa_cfg, 'path', 'est',style = 'lisrel',
                   edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                   layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                   rotation = 2,intercepts = F,panelGroups = T,ask = F)
p_std1 <- semPaths(uji_mgcfa_cfg, 'path', 'std',style = 'lisrel',
                   edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                   layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                   rotation = 2,intercepts = F,panelGroups = T,ask = F)
p_pa2 <- semPaths(uji_mgcfa, 'path', style = 'lisrel',
                  edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                  layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                  rotation = 2,intercepts = F,panelGroups = T,ask = F)
p_est2 <- semPaths(uji_mgcfa, 'path', 'est',style = 'lisrel',
                   edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                   layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                   rotation = 2,intercepts = F,panelGroups = T,ask = F)
p_std2 <- semPaths(uji_mgcfa, 'path', 'std',style = 'lisrel',
                   edge.label.cex = .7,nDigits = 3, edge.color = 'black',
                   layout='tree', sizeMan = 8,sizeMan2 = 3, sizeLat = 8,
                   rotation = 2,intercepts = F,panelGroups = T,ask = F)
dev.off()

#Simpan output gambar lavaanPlot
save_png(lp1,'Hasil Syntax_CFA_Multigroup1.png')
save_png(lp2,'Hasil Syntax_CFA_Multigroup2.png')

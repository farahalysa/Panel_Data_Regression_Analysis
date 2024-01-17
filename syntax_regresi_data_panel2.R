## Menginput Data ##
data = read.csv("D:\\STATISTIKA\\SEMESTER 6\\STATISTICAL CONSULTING\\Penelitian\\data\\data panel\\data.csv")
View(data)

## Melihat Deskripsi Data ##
str(data)
summary(data)
summary(is.na(data))
plot(data)

## REGRESI DATA PANEL ##
### Estimasi Regresi Data Panel ###
## Common Effect ##
library(plm)
common = plm(Pengangguran~Upah.Minimum+IPM+Rata.rata.Lama.Sekolah, data = data, model = "pooling")
summary(common)
## Fixed Effect ##
fixed = plm(Pengangguran~Upah.Minimum+IPM+Rata.rata.Lama.Sekolah, data = data, model = "within")
summary(fixed)
## Random Effect ##
random = plm(Pengangguran~Upah.Minimum+IPM+Rata.rata.Lama.Sekolah, data = data, model = "random")
summary(random)

### Pemilihan Model Regresi Data Panel ###
## Uji Chow ##
pooltest(common,fixed)
# Uji Haussman
phtest(fixed,random)
## Uji Breusch Pagan ##
gr = plm(Pengangguran~Upah.Minimum+IPM+Rata.rata.Lama.Sekolah, data = data, model = "within")
# Efek Dua Arah (uji efek individu maupun waktu)
plmtest(gr, effect = "twoways", type = "bp")
# Uji Efek Individu / Cross Section
plmtest(gr, effect = "individual",type = "bp")
# Uji Waktu
plmtest(gr, effect = "time", type = "bp")

### Pembuatan Model ###
model1 = plm(Pengangguran~Upah.Minimum+IPM+Rata.rata.Lama.Sekolah, data = data, model = "within", effect = "individual", index = c("Wilayah","Tahun"))
summary(model1)
model2 = plm(Pengangguran~Upah.Minimum+IPM, data = data, model = "within", effect = "individual", index = c("Wilayah","Tahun"))
summary(model2)
model3 = plm(Pengangguran~Upah.Minimum, data = data, model = "within", effect = "individual", index = c("Wilayah","Tahun"))
summary(model3)
fixef(model3, type = "level")

### Uji Asumsi Klasik ###   
## #Uji Normalitas ##
shapiro.test(model3$residuals)
## Uji Diagnostik 
## Uji Korelasi Serial ##
pbgtest(model3)
## Uji Heteroskedastisitas ##
library(lmtest)
bptest(model3)


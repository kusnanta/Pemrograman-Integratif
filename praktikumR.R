puskesmas = read.csv("jumlahdanrasiodokterperawatterhadappuskesmas2015.csv")
head(puskesmas)

require(tree)
library(rpart)
library(ggplot2)
ggplot(puskesmas.df, aes(Jumlah_Puskesmas2015, Dokter_Umum_puskesmas2015, Dokter_Gigi_puskesmas2015, color = Wilayah)) + geom_point()
ggplot(puskesmas.df, aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, color = puskesmas.df$Wilayah)) + geom_point()

puskesmasCluster <- kmeans(puskesmas[, 3:4], 3, nstart = 20)
table(irisCluster$cluster, iris$Species)

table(puskesmasCluster$cluster, puskesmas.df$Wilayah)
irisCluster$cluster <- as.factor(irisCluster$cluster) 
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()
ggplot(puskesmas, aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah)) + geom_point() + geom_text() + geom_hline(yintercept = 20)
ggplot(puskesmas, aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah)) + geom_point() + geom_text(size = 3) + lines(x = c(0,100), y = c(0,100))






#http://www.olahdatamedan.com/?p=1530
tree <- rpart(Wilayah ~ Jumlah_Puskesmas2015 + Dokter_Umum_puskesmas2015 + Dokter_Gigi_puskesmas2015, puskesmas)
prp(tree, faclen = 0, cex = 0.8 , extra = 1)
 #wilayah nanti diganti sama cukup tidak cukup
 #kolom jumlah_puskesmas, dokter dsb diganti sama rasio jumlah dokter sama penduduk

ggplot(puskesmas,aes(Rasio_dokter_umum,Rasio_dokter_gigi,Rasio_perawat,Rasio_bidan))+geom_point(aes(shape = Wilayah))
 #cuma bisa dua dimensi

geom_line(size = 0.1)

########################################################
#Fix
# 1. klasifikasi pake garis
library(ggplot2)
library(ggrepel)
ggplot(puskesmas, aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah)) + geom_point() + geom_text()
 ggplot(data=df, aes(x=Jumlah_Puskesmas2015, y=Dokter_Umum_puskesmas2015, color = factor(fit$cluster), label= puskesmas$Wilayah)) + geom_point() + geom_text_repel(aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah))
 pus <- ggplot(data=df, aes(x=Jumlah_Puskesmas2015, y=Dokter_Umum_puskesmas2015, color = factor(fit$cluster), label= puskesmas$Wilayah)) + geom_point() + geom_text_repel(aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah)) + scale_color_manual(values = c("purple", "black", "blue", "green", "yellow", "gray", "red" ))
# xend sama yend diatur sesuai perbandingan tenagakesehatan:masyarakat
pus + geom_segment(aes(x = 0, y = 0, xend = 1000, yend = 1000, colour = "segment"))

# 2. print klasterisasi pake Visualisasi
# a) klaster dua dimensi
puskesmas = read.csv("jumlahdanrasiodokterperawatterhadappuskesmas2015.csv")
df=puskesmas
#membuat puskesmas sbg matrix
puskesmasMatrix=as.matrix(cbind(df$Jumlah_Puskesmas2015, df$Dokter_Umum_puskesmas2015, df$Dokter_Gigi_puskesmas2015, df$Perawat_puskesmas2015, df$Bidan_puskesmas2015, df$Rasio_dokter_umum, df$Rasio_dokter_gigi, df$Rasio_perawat, df$Rasio_bidan),ncol=9)
fit <- kmeans(puskesmasMatrix, 6)
fit
fit$size
fit$withinss
clusplot(puskesmasMatrix, fit$cluster, color=TRUE, shade=TRUE, labels = 2, lines=0)


# 3. Geo spasial
install.packages("ggmap")
library(ggmap)
#https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
ina_center = as.numeric(geocode("Indonesia"))
INAMap = ggmap(get_googlemap(center=ina_center, scale=2, zoom=4), extent="normal")
INAMap + coord_fixed(xlim= c(95, 140), ylim = c(-12, 10))
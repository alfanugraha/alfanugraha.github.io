#NO 1

#Membangkitkan data populasi yang terdiri atas 1000 amatan
library(tidyverse)
#install.packages("DT")
library(DT)
# Peubah No
No <- c(seq(500),seq(9001, 9500))

# Peubah ID
ID <- sprintf("G%04d", No)

#Peubah Kelas
Kelas <- factor(ifelse(No %% 2 == 0, "Paralel 1", "Paralel 2"))


#Peubah UTS
set.seed(5612) #set.seed dibuat 5612 menandakan STA561 kelompok 2
UTS <- round(rnorm(1000, 70, 5),4) #membangkitkan bilangan acak yang menyebar normal dengan 4 angka di belakang koma

#Peubah Indeks
set.seed(5612) #set.seed dibuat 5612 menandakan STA561 kelompok 2
Indeks <- round(rexp(1000, 1),4) #membangkitkan bilangan acak yang menyebar eksponensial dengan 4 angka di belakang koma

# Membuat Data Frame data.Populasi
data.Populasi <- data.frame(No, ID, Kelas, UTS, Indeks)

# Menampilkan data frame data populasi dengan menggunakan formatStyle
data.Populasi %>%  datatable(data.Populasi, class = 'cell-border stripe')  %>%
  formatStyle('Indeks',  color = styleInterval(1, c('black', 'red')), backgroundColor = '#b3ffb3', fontWeight = 'bold',)

#Menghilangkan data Indeks > 1
#Menghilangkan data peubah Indeks yang bernilai lebih dari 1 (menjadi missing value)
#Mengubah data indeks > 1 menjadi NA
data.Populasi$Indeks <- ifelse(data.Populasi$Indeks >1, NA, data.Populasi$Indeks)

#Menampilkan tabel data populasi dengan formatStyle
data.Populasi %>%  datatable(data.Populasi, class = 'cell-border stripe')  %>%
  formatStyle('Indeks',  color = styleInterval(1, c('black', '  ')), backgroundColor = '#b3ffb3', fontWeight = 'bold',) 

#Menghitung rata-rata dari UTS yang Indeks-nya ada.
# Filter data untuk yang ada Indeks nya
data.adaindeks <- data.Populasi %>% 
  filter(!is.na(Indeks)) 

# Menghitung Rata-rata UTS yang ada indeksnya
rata2UTS <- mean(data.adaindeks$UTS)
cat("rata-rata dari UTS yang Indeks-nya ada sebesar", rata2UTS)

#Menarik data sampel berukuran 100 amatan dari Kelas "Paralel 1" yang Indeks-nya ada.
# Filter data untuk kelas Paralel 1
data.adaindekspar1 <- data.adaindeks %>% 
  filter(Kelas =="Paralel 1")

# Menarik sampel dari populasi dengan 100 amatan
set.seed(5612) #set.seed dibuat 5612 menandakan STA561 kelompok 2
data.sampel <- data.adaindekspar1 %>% 
  sample_n(100)

# Menampilkan data sampel dengan formatStyle
data.sampel %>%  datatable(data.sampel, class = 'cell-border stripe')  %>%
  formatStyle('Kelas',  color = ' ', backgroundColor = '#b3ffb3', fontWeight = 'bold',) %>%
  formatStyle('Indeks',  color = ' ', backgroundColor = '#b3ffb3', fontWeight = 'bold',) 

#Menghitung rata-rata dari UTS pada data sampel yang terambil, lalu membandingkan dengan populasi.
# Menghitung rata-rata UTS data sampel
rata2UTSsampel <- mean(data.sampel$UTS)
cat("rata-rata dari UTS pada data sampel yang terambil sebesar", rata2UTSsampel)
# Menghitung rata-rata UTS data populasi
rata2UTSpop <- mean(data.Populasi$UTS)
cat("rata-rata dari UTS pada data populasi sebesar", rata2UTSpop)


#NO 2
RKU <- function(data.x, data.y){
  
  #Transformasi Ke Komponen Utama
  pca <- prcomp(data.x, center=TRUE, scale.=TRUE) #melakukan analisis komponen utama
  
  vektor.ciri <- pca$rotation #vektor ciri 
  matriks.vector.ciri <- as.data.frame(vektor.ciri) #Matriks Vector Ciri
  akar.ciri <- pca$sdev #akar ciri
  
  # Memilih komponen utama dengan akar ciri >= 1 (Sriningsih, M., Hatidja, D., dan Prang, J.D. (2018))
  k <- which(akar.ciri >= 1, arr.ind = TRUE)
  k <- as.integer(k)
  
  # Memilih komponen utama yang bersesuaian dengan akar ciri 
  PKU = c();
  for(i in 1:length(k)){
    PKU[i+1] <-paste0("PC",k[i])
  }
  
  # Membuat matriks vektor ciri terpilih
  eigen.baru <- matriks.vector.ciri[names(matriks.vector.ciri)[names(matriks.vector.ciri) %in% PKU]]
  eigen.baru <- as.matrix(eigen.baru)
  
  
  #Membuat dataframe komponen utama terpilih dan y
  matriks.skor <- pca$x #matriks skor komponen utama
  pcs <- as.data.frame(matriks.skor) #membuat dataframe dari skor komponen utama
  data.pcr <- cbind(data.y, pcs) #menambahkan data y
  PKU.terpilih <- pcs[names(pcs)[names(pcs) %in% PKU]]
  
  
  #Pemodelan Regresi Komponen Utama
  pcr <- lm(data.y ~ ., data = data.pcr) #melakukan pendugaan koefisien regresi
  beta.pcr <- as.matrix(pcr$coefficients[-1]) 
  KoefisienPC <- as.vector(pcr$coefficients)[1:as.integer(max(k)+1)] #matriks koefisien regresi komponen utama
  
  #Transformasi Balik Model Regresi Komponen Utama ke Peubah X
  V <- as.matrix(vektor.ciri) #matriks vektor ciri
  beta.X <- V[,1:max(k)] %*% as.matrix(beta.pcr[1:max(k)]) #melakukan transformasi peubah komponen utama menjadi peubah asal x (NCSS LLC 2019)
  
  
  #Menampilkan Output utama koefisien penduga parameter regresi komponen utama yang sudah ditransformasi balik untuk peubah X (Agustini N, Nugroho S dan Agustina D (2015))
  vector.xbar <- pca$center #vektor xbar (nilai tengah)
  intersepTransformasi <- mean(data.y)- vector.xbar%*%beta.X #menghitung intercept akhir
  namakolom=as.factor(colnames(data.x))
  beta.X <- rbind(intersepTransformasi, beta.X)#menambahkan intercept akhir dan koefisien beta hasil transformasi
  rownames(beta.X) = c("intercept",(if (length(colnames(data.x))==0) {rep(paste("beta",1:ncol(data.x),sep=""))} else {levels(namakolom)}))
  
  #Menampilkan Output Keseluruhan
  akarCiriTerpilih <- akar.ciri[1:max(k)] #memilih akar ciri terpilih
  output <- list(Akar.Ciri = akarCiriTerpilih, Vektor.Ciri =eigen.baru,  skor.KU= datatable(PKU.terpilih, class = 'cell-border stripe'), Koefisien.PC = KoefisienPC, Koefisien.X = beta.X)
  return(output)
}

#NO 3
library(MASS) 
library(car)
# Misal Besar korelasi antarpeubah bebas adalah 0.96
b0 <- 10; b1 <- 3; b2 <- 5; b3 <- 7  #???? = 10 + 3????1 + 5????2 + 7????3 + ????
b0topi <- NULL; b1topi <- NULL; b2topi <- NULL; b3topi <- NULL 
Sigma <- matrix(c(1,0.96,0.96,0.96,1,0.96,0.96,0.96,1),nrow=3,ncol=3) #matriks ragam peragam

#membangkitkan data sebanyak 1000
set.seed(5612) #set.seed dibuat 5612 menandakan STA561 kelompok 2
eps <- rnorm(1000) # membangkitkan e~N(0,1) sebanyak 1000
mu <- c(1, 1, 1)
x <- round(mvrnorm(1000,mu,Sigma),4) #membangkitkan peubah X dengan ketentuan 4 angka di belakang koma
y <- round((b0 + b1*x[,1] + b2*x[,2] + b3*x[,3]+ eps),4) #menghitung peubah y dengan ketentuan 4 angka di belakang koma


pembangkitandata <- data.frame(y,x) #peubah Y dan X yang telah dibangkitkan


# Menampilkan data yang telah dibangkitkan dengan datatable
datatable(pembangkitandata, class = 'cell-border stripe') 
#menampilkan nilai korelasi antar peubah bebas
data_cor <- cor(x)
korelasi <- function(data_cor){
  hasil <- data_cor %>% corrplot::corrplot(method="color",  
                                           type="upper", 
                                           order="hclust", 
                                           addCoef.col = "white",
                                           tl.col="black", 
                                           insig = "blank",
                                           diag=FALSE) 
  return(hasil)
}
korelasi(data_cor)
#Melakukan pengecekan multikolinieritas dengan VIF
cekmultikolinearitas <-lm(pembangkitandata$y~pembangkitandata$X1+pembangkitandata$X2+pembangkitandata$X3)
vif(cekmultikolinearitas)
#menghitung koefisien penduga parameter regresi komponen utama yang sudah ditransformasi balik menggunakan fungsi RKU yang sudah dibuat di soal nomor 2

PCR = RKU(x,y)
PCR 

#NO 4
#bangkitkan data secara acak
library(tidyverse)
data("iris")

#membuat peubah baru
iris<- iris %>% mutate(Petal=Petal.Length+Petal.Width)
head(Petal)
iris<- iris %>% mutate(Sepal=Sepal.Length+Sepal.Width)
head(Sepal)

#Data Subsetting
#Subsetting baris dengan menggunakan fungsi filter
iris %>% filter(Species=="versicolor")

#Subsetting baris dengan menggunakan fungsi select
iris %>% select(Species,Sepal,Petal)

#Data Sorting
#scr ascending
iris %>% arrange(Sepal)
#scr desc
iris %>% arrange(desc(Sepal.Length),desc(Sepal.Width))
#scr
iris<-iris %>% arrange(Petal,desc(Sepal))
head(iris)

#Join data frame
irisx<-data.frame(Jenis=c("setosa","versicolor","rose"),kode=c(1,2,3))
head(irisx)

#innerjoin
x<-iris %>% inner_join(irisx,by=c("Species"="Jenis"))
head(x)
view(x)

#leftjoin
y<-iris %>% left_join(irisx,by=c("Species"="Jenis"))
head(y)
tail(y)
view(y)

#right join
y<-iris %>% right_join(irisx,by=c("Species"="Jenis"))
head(y)
tail(y)

#fulljoin
y<-iris %>% full_join(irisx,by=c("Species"="Jenis"))
head(y)
tail(y)

#Agregat
#summarise
iris1<-iris %>% summarise(n=n(), maks=max(Sepal), min=min(Sepal), rataan=mean(Sepal))
iris1
iris2<-iris %>% summarise(n=n(), maks=max(Petal), min=min(Petal), rataan=mean(Petal))
iris2
#group by
iris3<-iris %>% group_by(Species) %>% summarise(n=n(),rataan=mean(Sepal),s=sd(Sepal))
iris3
iris4<-iris %>% group_by(Species) %>% summarise(n=n(),rataan=mean(Petal),s=sd(Petal))
iris4

#reshaping/pivoting
#(xx <- x %>% select(-(Sepal)) %>% pivot_wider(names_from=kode,
#                                                            values_from=Petal,names_prefix="Species"))
#data18 %>% pivot_longer(!Perl,names_to="Kel",values_to="Resp",names_prefix="Species")
#


#NO 5
library(readxl)
ipm <- read_excel("C:/Users/PC/Downloads/ipm_metode_baru.xlsx")
View(ipm)
str(ipm)
#membuat peubah baru
iris<- iris %>% mutate(Petal=Petal.Length+Petal.Width)
head(Petal)
iris<- iris %>% mutate(Sepal=Sepal.Length+Sepal.Width)
head(Sepal)
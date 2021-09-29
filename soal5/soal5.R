# buat 3 gambar visualisasi dari data IPM 2010-2018

library(plotly)
library(dplyr)
library(reshape)


ipmdata <- read.table('ipm_metode_comb2.csv', header = T, sep = ";")
prov <- unique(ipmdata$Provinsi)
colnames(ipmdata)[5:13] <- c(2010:2018)

ipmfilter <- ipmdata %>% filter(Komponen == "IPM" & Provinsi == "ACEH") %>% select(-Komponen, -Provinsi)
ipmfilter <- ipmfilter[2:nrow(ipmfilter), ]
ipmselected <- ipmfilter %>% melt(id=c("Kode","KabKota")) %>% mutate(kategori=ifelse(value > 80, "IPM Sangat Tinggi", ifelse(value > 70, "IPM Tinggi", "IPM Sedang")))

tahun <- 2010
ipmtahun <- ipmselected %>% filter(variable == 2010) %>% arrange(desc(value))
ipmtahun$value <- as.numeric(ipmtahun$value)

fig <- plot_ly(x = ~ipmtahun$value, y = ~reorder(ipmtahun$KabKota, ipmtahun$value), color = ipmtahun$kategori, type = 'bar', orientation = 'h') %>% 
  layout(fig, xaxis = list(range = c(55,83))) %>%
  layout(title = 'Index Pembangunan Manusia Kabupaten Tahun ',
                 xaxis = list(title = ''),
                 yaxis = list(title = ''),
                 legend = list(x = 0.729, y = 0.138,font = list(size = 10)),
                 margin = list(l = 100, r = 20, t = 70, b = 70),
                 paper_bgcolor = 'rgb(248, 248, 255)',
                 plot_bgcolor = 'rgb(248, 248, 255)')
fig


ipmfilter_eys <- ipmdata %>% filter(Komponen == "EYS" & Provinsi == "ACEH") %>% select(-Komponen, -Provinsi)
ipmfilter_eys <- ipmfilter_eys[2:nrow(ipmfilter_eys), ]
ipmselected_eys <- ipmfilter_eys %>% melt(id=c("Kode","KabKota")) %>% mutate(kategori=ifelse(value > 13, "Nilai/Angka di atas Prov & Nasional", ifelse(value > 12, "Nilai/Angka di antara Prov & Nasional", "Nilai/Angka di bawah Prov & Nasional")))

tahun <- 2010
ipmtahun_eys <- ipmselected_eys %>% filter(variable == 2010) %>% arrange(desc(value))
ipmtahun_eys$value <- as.numeric(ipmtahun_eys$value)

fig <- plot_ly(x = ~ipmtahun_eys$value, y = ~reorder(ipmtahun_eys$KabKota, ipmtahun_eys$value), color = ipmtahun_eys$kategori, colors = "Dark2", type = 'bar', orientation = 'h') %>% 
  layout(plot_bgcolor='#e5ecf6', xaxis = list(range = c(8,20))) %>%
  layout(title = 'Harapan Lama Sekolah Menurut ',
         xaxis = list(title = ''),
         yaxis = list(title = ''),
         legend = list(x = 0.729, y = 0.138,font = list(size = 10)),
         margin = list(l = 100, r = 20, t = 70, b = 70))
fig


ipmfilter_mys <- ipmdata %>% filter(Komponen == "MYS" & Provinsi == "ACEH") %>% select(-Komponen, -Provinsi)
ipmfilter_mys <- ipmfilter_mys[2:nrow(ipmfilter_mys), ]
ipmselected_mys <- ipmfilter_mys %>% melt(id=c("Kode","KabKota")) %>% mutate(kategori=ifelse(value > 9, "Nilai/Angka di atas Prov & Nasional", ifelse(value > 6, "Nilai/Angka di antara Prov & Nasional", "Nilai/Angka di bawah Prov & Nasional")))

tahun <- 2010
ipmtahun_mys <- ipmselected_mys %>% filter(variable == 2010) %>% arrange(desc(value))
ipmtahun_mys$value <- as.numeric(ipmtahun_mys$value)

fig <- plot_ly(x = ~ipmtahun_mys$value, y = ~reorder(ipmtahun_mys$KabKota, ipmtahun_mys$value), color = ipmtahun_mys$kategori, colors = "Set1", type = 'bar', orientation = 'h') %>% 
  layout(plot_bgcolor='#e5ecf6', xaxis = list(range = c(4,14))) %>%
  layout(title = 'Rata-rata Lama Sekolah Menurut ',
         xaxis = list(title = ''),
         yaxis = list(title = ''),
         legend = list(x = 0.729, y = 0.138,font = list(size = 10)),
         margin = list(l = 100, r = 20, t = 70, b = 70))
fig

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyWidgets)
library(shiny.i18n)
library(knitr)
library(markdown)
library(rmarkdown)
library(stringr)
library(gsubfn)
library(webshot)
library(RMySQL)
library(zip)
library(mailR)
library(dplyr)
library(tidyr)
library(wordcloud2)
library(magrittr)
library(tm)
library(quanteda)
library(ggplot2)
library(readr)
library(lubridate)
library(textcat)
library(htmlwidgets)
library(blastula)
library(keyring)
library(tidytext)
library(cluster)
library(lsa)
library(ggtext)
library(magick)
webshot::install_phantomjs()
library(waiter)



#imatge del correu
img<-add_image(file = "./needs/logo.png",
               align = "left",
               width = 200)


load("./needs/stop_words.R")     #save(stop_words, file="../needs/stop_words.R")

load("./needs/emojis_dataset.R")
load("./needs/labels_emoji.R")
load("./needs/sent_emoji.R")
load("./needs/paletes_color.R")
#load("./needs/colors_hores.R")


FREQ<-c(634,501,445,379,372,326,321,316,313,269,269,258,257,256,253,251,239,234,233,232,226,220,205,190,184,184,183,180,177,172,172,170,168,167,161,160,158,151,147,142,141,140,140,139,135,134,134,133,131,129,126,124,122,121,119,119,117,114,113,112,112,111,105,105,104,102,100,99,97,96,96,96,92,92,91,91,90,89,89,89,89,89,88,88,88,87,85,84,84,83,83,82,82,79,77,77,77,76,76,75,75,75,75,75,75,75,75,75,75,75,74,74,74,74,74,74,74,74,74,74,73,73,73,73,73,73,73,73,73,73,72,72,72,72,72,72,72,72,72,72,71,71,71,71,71,71,71,71,71,71,70,70,70,70,70,70,70,70,70,70,69,69,69,69,69,69,69,69,69,69,68,68,68,68,68,68,68,68,68,68,67,67,67,67,67,67,67,67,67,67,66,66,66,66,66,66,66,66,66,66)
unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )





infolectura_fitxer<- function(nom_xat, input, w, tmp, color_triat){
  
  "
  Llig el fitxer i el retorna estructurat si es correcte. Si no, retorna 'problema_xat' i es notifica
  
  
  "
  
  #Lectura del fitxer
  f <- read.delim(file=nom_xat, quote = "" , encoding = "UTF-8", header = F, colClasses = "character")
  
  #llevem les linies repetides
  f<-unique(f)
  
  #Distincio entre IOS i la RESTA. IOS té "[]".
  if (grepl(f[1,1],pattern= "\\[.*\\]")){       #És IOS. Arxius i audios omesos tenen un caracter especial al principi
    
    #guardem el tipus de l'estructura
    tipus<-"IOS"
    
    
    #açò se pot abreviar en la versió online
    ########################################################################################
    #Mirem quins tenen l'estructura. Servix per a /n i missatges de grup
    f<- f %>% mutate(estructura=grepl(pattern="\\[\\d*\\/.*\\]",V1))  #ESTRUCTURA: tipus "[num/algo]"
    
    #Agafem sols els que seguixen l'esquema general
    estructura_false<- f %>% filter(estructura==F)
    f<- f %>% filter(estructura==T)
    ########################################################################################
    
    
    #Idioma
    #Determinem idioma                            #Son els que no comencem per [, ja que la multimedia comensa per un caracter especial
    if(any(grepl("omitido|omitida", head(f$V1[!grepl("^(\\[|\\s)", f$V1)]), ignore.case = T))){
      idioma_movil<-"spanish"
      
    }else if(any(grepl("omès|omesa", head(f$V1[!grepl("^(\\[|\\s)", f$V1)]), ignore.case = T))){
      idioma_movil<-"catalan"
      
    }else if(any(grepl("omitted", head(f$V1[!grepl("^(\\[|\\s)", f$V1)]), ignore.case = T))){
      idioma_movil<-"english"
      
    }else {
      idioma_movil<-"x"
    }
    
    
    #Busquem si té coma entre [], si en té assumim que és ANGLÉS
    if (idioma_movil=="english"){  
      
      #Separem el string que teniem en DATA, HORA, USUARI i COMMENT
      f<-f%>% separate(V1, "\\[", into=c("res", "V1"), extra="merge") %>%
        select(-res)%>% #Servix per a llevar-nos un element que sols té IOS en audios, imatges
        separate(V1, "\\] ", into=c("temps", "resta"), extra="merge") %>%  
        separate(resta, "\\:", into = c("usuari", "comm"), extra="merge") %>%
        separate(temps, "\\,", into = c("data", "hora"), extra="merge") %>%
        select(-estructura)
      
      #Per als fs que seguixen parcialment l'estructura. Llevar-los
      f<-f %>% filter(comm != "NA")
      
      #Adaptem el tipus de dada
      f$data<- as.Date(f$data, format="%d/%m/%Y")
      f$hora<-parse_time(f$hora, format= "%H:%M:%S" )
      f$hora2<-as.integer(hour(f$hora))  #Ho necessitael plot del temps x hores
      f$usuari<-as.factor(f$usuari)
      
      
      
      
    }else if(idioma_movil=="spanish" | idioma_movil=="catalan") {#No és ANGLÉS
      
      #Separem el string que teniem en DATA, HORA, USUARI i COMMENT
      f<-f%>% separate(V1, "\\[", into=c("res", "V1"), extra="merge") %>%
        select(-res)%>%
        separate(V1, "\\] ", into=c("temps", "resta"), extra="merge") %>%  
        separate(resta, "\\:", into = c("usuari", "comm"), extra="merge") %>%
        separate(temps, "\\s", into = c("data", "hora"), extra="merge")%>%
        select(-estructura)
      
      #Per als fs que seguixen parcialment l'estructura. Llevar-los
      f<-f %>% filter(comm != "NA")
      
      #Adaptem el tipus de dada
      f$data<- as.Date(f$data, format="%d/%m/%y")
      f$hora<-hms(f$hora )
      f$hora2<-as.integer(hour(f$hora))  #Ho necessita el plot del temps x hores
      f$usuari<-as.factor(f$usuari)
      
      
      
    }
  }else{ #No és IOS. RESTA 
    
    #guardem el tipus de l'estructura
    tipus<-"resta"
    
    
    #Afegim el valor estructura, per si seguix l'estructura que voliem:  num/num/num algo num:num algo -espai algo:
    f<- f %>% mutate(estructura=grepl(pattern="^\\d*\\/\\d*\\/\\d*.*\\d*\\:\\d*.*\\-\\s.*\\:",V1)) 
    
    #Agafem sols els que seguixen l'esquema general i guardem els que no
    estructura_false<- f %>% filter(estructura==F)
    f<- f %>% filter(estructura==T)
    
    
    #ANgles i Catala tenen una coma entre data i hora!!!!!!
    if (grepl("^\\d*\\/\\d*\\/\\d*\\,\\s*\\d*\\:\\d*.*\\-\\s.*\\:", f$V1[1])){ #el primer Té coma. És català o anglés
      
      #Separem el string que teniem en DATA, HORA, USUARI i COMMENT
      f<-f%>% separate(V1, "\\,", into=c("data", "resta"), extra="merge") %>%  
        separate(resta, "\\- ", into = c("hora", "resta"), extra="merge") %>%
        separate(resta, "\\:", into = c("usuari", "comm"), extra="merge")
      
      
      #Per als xats que seguixen parcialment l'estructura. Llevar-los
      f<-f %>% filter(comm != "NA")
      
      
      #Determinem idioma_movil
      if(any(grepl("\\<Fitxers multimèdia omesos\\>", f$comm))){
        idioma_movil<-"catalan"
      }else if(any(grepl("\\<Media omitted\\>", f$comm))){
        idioma_movil<-"english"
        
      }else{
        idioma_movil<-"x"
      }
      
      
      #Adaptament al tipus de dada per idioma_movil
      if (idioma_movil=="catalan"){
        
        if(!grepl(pattern = "[ap]\\..m\\.", f[1,2] )){  #ANDROID
          
          #Adaptem el tipus de dada
          f$data<- as.Date(f$data, format="%d/%m/%y")
          f$hora<-hm(f$hora)
          f$hora2<-as.integer(hour(f$hora))  #Ho necessitael plot del temps x hores
          f$usuari<-as.factor(f$usuari)
          
        }else{  #HUAWEI
          
          #substituim el p.m i tal
          f$hora<-gsub("p\\..m\\.","PM",f$hora)
          f$hora<-gsub("a\\..m\\.","AM",f$hora)
          
          
          #Adaptem el tipus de dada
          f$hora<-parse_time(f$hora, format= "%H:%M %p" )
          f$data<- as.Date(f$data, format="%d/%m/%y")
          f$hora2<-as.integer(hour(f$hora))  #Ho necessitael plot del temps x hores
          f$usuari<-as.factor(f$usuari)
          
        }
        
      }else if(idioma_movil=="english"){#és anglés
        
        #Data
        if (length(f[1, 1])==10){ #Si el valor de l'any té 4 digits. Seguix estructura %d/%m/%Y
          f$data<- as.Date(f$data, format="%d/%m/%Y")
          
        }else{ 
          f$data<- as.Date(f$data, format="%d/%m/%y")
        }
        
        #Hora
        if(grepl("M",f[1,2])){ #Si té AM o PM
          f$hora<-parse_time(f$hora, format= "%H:%M %p" )
        }else{
          f$hora<-hm(f$hora)
          
        }
        
        f$hora2<-as.integer(hour(f$hora))  #Ho necessitael plot del temps x hores
        f$usuari<-as.factor(f$usuari)
        
      }
    }else{  #Es castella
      
      #Determinem idioma_movil
      if(any(grepl("\\<Multimedia omitido\\>", f$V1))){
        
        idioma_movil<-"spanish"
        
        
        #Separem el string que teniem en DATA, HORA, USUARI i COMMENT
        f<-f%>% separate(V1, "\\s", into=c("data", "resta"), extra="merge") %>%  
          separate(resta, "\\- ", into = c("hora", "resta"), extra="merge") %>% #l'espai!!!
          separate(resta, "\\:", into = c("usuari", "comm"), extra="merge")
        
        #Per als fs que seguixen parcialment l'estructura. Llevar-los
        f<-f %>% filter(comm != "NA")
        
        
        if(!grepl(pattern = "[ap]\\..m\\.", f[1,2] )){  #ANDROID
          
          #Adaptem el tipus de dada
          f$data<- as.Date(f$data, format="%d/%m/%y")
          f$hora<-hm(f$hora)
          f$hora2<-as.integer(hour(f$hora))  #Ho necessitael plot del temps x hores
          f$usuari<-as.factor(f$usuari)
          
        }else{ #HUAWEI
          
          #substituim el p.m i tal
          f$hora<-gsub("p\\..m\\.","PM",f$hora)
          f$hora<-gsub("a\\..m\\.","AM",f$hora)
          
          
          #Adaptem el tipus de dada
          f$hora<-parse_time(f$hora, format= "%H:%M %p" )
          f$data<- as.Date(f$data, format="%d/%m/%y")
          f$hora2<-as.integer(hour(f$hora))  #Ho necessitael plot del temps x hores
          f$usuari<-as.factor(f$usuari)
          
        }
        
      }else{
        idioma_movil<-"x"
      }
      
      
    }
  }
  
  
  if (tipus=="IOS"){
    # En els IOS, quan reenvies missatges, es copien igual amb la mateixa estructura pero no tenen els SEGONS I ANYS, i ixen NA. emprem complete cases per a canviar-ho.
    #[9/2/19 1:58:05] Etarra: [9/2 1:55] Olatz: En casa
    #[9/2 1:55] Amatxu Les Corts: Arribant en 10 minuts
    #[9/2 1:56] Amatxu Les Corts: No enforrelleu
    f<-f[complete.cases(f),]    #Açò ha demostrat que no funciona en alguns casos. En Mariola no m'ha funcionat, així que seria mes interessant mirar quins usuaris tenen una proporció menor i els que tinguen un percentatge menor al 1 per cent, pafuera.
    #DE totes maneres, deixar-ho fora 'es interessant per si passara alguna cosa
    
    #Dataframe amb el recompte d'intervencions per usuari
    d<-as.data.frame(table(f$usuari))
    
    #Quins son els usuaris que no intervenen mes d'un 0.0075
    usuaris_no<-as.character(d[which(d$Freq < quantile(1:nrow(f),0.0015)),]$Var1)
    
    f<- f[!f$usuari %in% usuaris_no,]
    
    #Eliminem el missatge "Ara els missatges d'aquest grup estan protegits amb encriptació d'extrem a extrem." que reconeix el nom del grup com a usuari
    #Se podria mirar d'optimitzar ja que apareix pel principi    ######################################
    if (idioma_movil=="catalan"){
      f<- f[!str_detect(f$comm, "Ara els missatges d'aquest grup"),]
      
    }else if (idioma_movil=="english"){
      f<- f[!str_detect(f$comm, "Messages to this chat and calls are"),]
    }
    
    
    f$usuari<-as.character(f$usuari)
    f$usuari<-as.factor(f$usuari)
  }
  
  #canviar
  if (idioma_movil=="x" | any(!complete.cases(f)) | nrow(f) < nrow(estructura_false)| length(unique(f$usuari))>256){
    
    bo<-F
    
    
  }else {
    
    bo<-T
    
    cat("SUCCESS!!!!")
    
    xat<-f
    #-------------------------------------- FEM ELS Analytics
    
    #canvi en el progr'es
    w$update(html = tagList(spin_2(),tags$br(),  tags$span(style="color:#831923; font-weight: 400; font-size:17px; background-color:white;", "Construyendo tu Experiencia! (+60s)")))
    
    #Idioma de l'Analytics
    idioma <- input$idioma_analytics
    if (idioma=="val"){
      
      out <- rmarkdown::render('Analytics_PRO_val.Rmd',
                               output_dir = tmp )
      
      file.rename(out, file.path(tmp,"Analytics PRO.html"))
      
      
    }else if(idioma=="cast"){
      
      out <- rmarkdown::render('Analytics_PRO_cast.Rmd',
                               output_dir = tmp )
      
      file.rename(out, file.path(tmp,"Analytics PRO.html"))
      
      
    }else{}
    
    
  }
  
  return(bo)
}


imatge_tota_conversa<- function(paraules, color_triat, tmp){
  
  #Creem la ruta particular i la carpeta per a cada opcio
  ruta_particular=paste0(tmp, "/Tangible/")
  dir.create(ruta_particular)
  
  Data<-data.frame(words=paraules,
                   freq=FREQ[1:length(paraules)])
  
  my_graph<-wordcloud2(data=Data, color =paletes_color[,color_triat][1:nrow(Data)],gridSize= 2, fontFamily = "sans-serif")
  
  # save it in html
  saveWidget(my_graph, file.path(tmp,"x.html"), selfcontained = F)
  
  # save it in pdf on png
  webshot(file.path(tmp,"x.html"),file.path(ruta_particular,"Palabras.png"), delay =10, vwidth =1350, vheight=1080)
  
  
}


imatges_emojis<-function(xat,tmp){
  
  #Creem la ruta particular i la carpeta per a cada opcio
  ruta_particular=paste0(tmp, "/Tangible Emojis/")
  dir.create(ruta_particular)
  
  
  xat_emoji<-xat %>%
    unnest(emoji) %>%
    group_by(emoji)%>%
    summarise(total=n()) %>%
    arrange(desc(total))
  
  #Aci li posem les correspondencies en la info dels emojis
  xat_emoji <- inner_join(xat_emoji, emojis,by="emoji")
  
  #Per a les coordenades, mirem si les hem de fer o gastem les predeterminades
  if (nrow(xat_emoji)>=49){
    
    load("./needs/coordenades.R")
    
    #el limitem a 49 entrades
    xat_emoji<-xat_emoji[1:49,]
    
  } else{
    coordenades<-fer_coordenades(nrow(xat_emoji))
    
  }
  
  #fem cada una de les imatges
  fer_imatges_emojis(xat_emoji, coordenades = coordenades, ruta_particular =ruta_particular)
  
  
  cat("EMOJIS FETS!")
  
}


fer_imatges_emojis<-function(xat_emoji, coordenades , ruta_particular){
  # Se fan imatges amb els emojis
  #Tipus d'imatges: 1 mobil sense fons, 2 mobil amb fons, 3 cercle sense fons, 4 cercle amb fons
  
  #oval
  center=c(563.5, 1004)
  
  comp<- image_blank(width=1127, height=2008 ,color="#ffffff")
  comp<-image_resize(comp, geometry_size_pixels(width=1127, height=2008))
  comp<- image_convert(comp, colorspace = "sRGB")
  
  #tamanys de les imatges
  geom<-c("185x185", "185x185", rep("160x160", 8), rep("155x155", 16), rep("145x145", 23))
  
  #servix per a corregir el centre de la imatge
  ajust<-c(185/2,185/2, rep(160/2, 8), rep(155/2, 16), rep(145/2, 23))
  
  #coordenades
  x=coordenades$oval_x
  y=coordenades$oval_y
  
  
  i=1
  while (i <= nrow(xat_emoji)){
    comp<-image_composite(comp,image_scale(image_read(paste0('./emojis3/', xat_emoji$urls[i])), geometry = geom[i] ) , offset = paste0("+", center[1]+ x[i] - ajust[i],"+",center[2]+y[i]-ajust[i]) )
    
    i=i+1
  }
  
  image_write(comp, paste0(ruta_particular, "ovalo.png"),'png')
  
  remove(comp)
  
  gc()
  
  Sys.sleep(5)
  
  
  
  #cercle
  center=c(600, 600)
  
  comp<- image_blank(width=1200, height=1200 ,color="#ffffff")
  comp<-image_resize(comp, geometry_size_pixels(width=1200, height=1200))
  comp<- image_convert(comp, colorspace = "sRGB")
  
  #tamanys de les imatges
  geom<-c("160x160", rep("144x144", 8), rep("112x112", 15), rep("96x96", 25))
  
  #servix per a corregir el centre de la imatge
  ajust<-c(160/2, rep(144/2, 8), rep(112/2, 15), rep(96/2, 25))
  
  #coordenades
  x=coordenades$cercle_x
  y=coordenades$cercle_y
  
  
  i=1
  while (i <= nrow(xat_emoji)){
    comp<-image_composite(comp,image_scale(image_read(paste0('./emojis3/', xat_emoji$urls[i])), geometry = geom[i] ) , offset = paste0("+", center[1]+ x[i] - ajust[i],"+",center[2]+y[i]-ajust[i]) )
    
    i=i+1
  }
  
  image_write(comp, paste0(ruta_particular, "circulo.png"),'png')
  
  remove(comp)
  
  gc()
  
}

netejar_nom<- function(x){
  x<-gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,x)
  
  x<-str_extract(x, "(\\w|\\d|\\s)+")
  
  
  if (x==""){                  # si es un emoji
    x="emoji_name"
  }
  
  return(x)
}


fer_coordenades<- function(num){
  #se creen les coordenades dels emojis si el nombre d'emojis es menor a 49
  
  #cercle
  #per al tercer cercle
  n=num-24
  r=520
  
  x3=c()
  y3=c()
  
  for (theta in 1:(n) *((2*pi)/n)){
    
    x3=c(x3,  r* sin(theta))
    y3=c(y3, r* cos(theta))
    
  }
  
  coordenades<-data.frame(cercle_x=c(0 , 1.484924e+02 , 2.100000e+02 , 1.484924e+02 , 2.571673e-14 ,-1.484924e+02, -2.100000e+02, -1.484924e+02, -5.143347e-14,  1.545599e+02, 2.823950e+02 , 3.614015e+02 , 3.779183e+02,  3.290897e+02 , 2.233584e+02 , 7.900644e+01 ,-7.900644e+01, -2.233584e+02, -3.290897e+02, -3.779183e+02,-3.614015e+02, -2.823950e+02, -1.545599e+02, -9.307008e-14, x3 ),
                          cercle_y=c(0, 1.484924e+02,  1.285837e-14, -1.484924e+02, -2.100000e+02 ,-1.484924e+02 ,-3.857510e-14,  1.484924e+02 , 2.100000e+02,  3.471473e+02,2.542696e+02,  1.174265e+02, -3.972082e+01, -1.900000e+02, -3.074265e+02, -3.716961e+02, -3.716961e+02, -3.074265e+02, -1.900000e+02, -3.972082e+01,  1.174265e+02,  2.542696e+02,  3.471473e+02,  3.800000e+02, y3))
  
  
  #oval
  
  #per al tercer cercle
  n=num-24
  a=900
  b=460
  
  x3=c()
  y3=c()
  
  for (theta in 1:(n) *((2*pi)/n)){
    
    x3=c(x3,  a* cos(theta))
    y3=c(y3, b* sin(theta))
    
  }
  
  coordenades$oval_x<-c(0,0,137.5262 , 165.0000,  137.5262, -137.5262, -165.0000, -137.5262 ,   0.0000 ,   0.0000,  124.3721 , 229.8097 , 300.2608 , 325.0000 , 300.2608,  229.8097 , 124.3721, -124.3721 ,-229.8097, -300.2608,-325.0000, -300.2608, -229.8097, -124.3721, y3)
  coordenades$oval_y<-c(-9.500000e+01,  9.500000e+01,  2.210125e+02 , 1.071796e-05, -2.210125e+02, -2.210125e+02,  7.846124e-06 , 2.210125e+02, -3.450000e+02 , 3.450000e+02, 6.929096e+02 , 5.303301e+02 , 2.870126e+02 , 2.009617e-05 ,-2.870126e+02 ,-5.303301e+02 ,-6.929097e+02, -6.929097e+02, -5.303301e+02, -2.870126e+02, 1.471148e-05 , 2.870126e+02 , 5.303301e+02,  6.929096e+02, x3)
  
  return(coordenades)
}








arreglar_paraules<-function(paraules){
  i=1
  new_v<-c()
  later<-c()
  
  while(i<=round(length(paraules)*0.55)){
    if (grepl("^(xd|qe|ya|si|no|tens|ps|dps|vl|nah|mica|lol|dsp|passe|jaj|jej|heh|jij|hah|tb|esq|xq|pos|vrd|ns|pq|dnd|sii|pork|cmo|porq|tngo|oki|ay|perf|hora|vd|aunk|puf|vdd|pos|perq|res|hola|ara|sha)",
              paraules[i])){
      later<-c(later, paraules[i])
      
    }else{
      new_v<-c(new_v, paraules[i])
    }
    
    i=i+1
  }
  
  
  new_v<-c(new_v, later, paraules[i:length(paraules)])
  return(new_v)
  
}

my_css<- c(" @import url('https://fonts.googleapis.com/css?family=Cabin:400,400i,700&display=swap');



   article {
    width: 100%;
    opacity: 0.97;
    padding: 20px;
    margin: 0px auto 0px;
    background-color: #fff;
    box-shadow: #c0c0c0 5px 0px 35px -8px;
}


.site-content-inner,
.front-page-section-inner {
	width: 100%;
}

.header {
      text-align: center;
    background: white;
    color: #831923;
    font-size: 40px;
    font-weight: bold;
    text-shadow: -1.5px 1.5px #e77e89;
}

subheader {
    font-style: italic;
    font-size: 30px;
}

.temps{
  text-align:center;
  font-size: 26px;

}

.persones{
  text-align:center;
  font-size: 25px;

}


* {
    box-sizing: border-box;
}
article {
    display: block;
}




#TOC:hover {
  opacity: 1;
}




.nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {
    color: #fff;
    background-color: #831923;
}


.tocify {

  width: 100% !important;
  border: none;
}

   /* TOC links */

.list-group-item {
    color: #7b8a8b;
    font-size: 16px;
    opacity: 0.96;
}

.list-group-item.active {
    color: #2c3e50;
    background-color: white;
    border: none;
}

.list-group-item:hover, 
.list-group-item.active:hover {
    color: #131b23;
    background-color: white;
}

a {
    color: #831923;
    text-decoration: auto;
}

a:hover {
 color: #780A15; /* darker color when hovering */
}



.navbar-default {
    background-color: #141c25f2;
}

.navbar-default .navbar-nav>.open>a, 
.navbar-default .navbar-nav>.active>a, 
a.dropdown-toggle:hover {
  background-color: #141c25 !important;
}

/* Dropdown menu color */
.navbar-default .dropdown-menu {
  background-color: #141c25;
}

/* Dropdown menu hover color */
  .navbar-default .dropdown-menu>li>a:hover {
    background-color: #202831f2;
  }

/* Navbar Links when hovered*/
.navbar-default .dropdown-menu>.active>a
.navbar-default .navbar-nav>.active>a:hover, 
.navbar-default .navbar-nav:hover, 
.navbar-default .navbar-nav>li>a:hover, 
a.navbar-brand:hover {
  color: #ffffffab !important;
  background-color: #141c25;
}

ftitol{
  font-size:25px;
  font-weight:bold;
  text-align: center;
  
}

.funfact{
text-align: center; 
font-size:25px;  
font-weight:bold; 
color: gold;

}

/* funfact */

.card {
  background: #fff;
  border-radius: 2px;
  display: block;
  height: auto;
  margin: 1rem;
  position: relative;
  width: 85%;
  margin-left: auto;
  margin-right: auto;
  padding-left: 4%;
  padding-block: 2%;
  padding-right: 4%;
  text-align: center;
}

.card-1{

  box-shadow: rgba(131, 25, 35, 0.4) -5px 5px, rgba(131, 25, 35, 0.3) -10px 10px, rgba(131, 25, 35, 0.2) -15px 15px, rgba(131, 25, 35, 0.1) -20px 20px, rgba(131, 25, 35, 0.05) -25px 25px;


}

.card-2{

box-shadow: rgba(240, 46, 170, 0.4) 0px 5px, rgba(240, 46, 170, 0.3) 0px 10px, rgba(240, 46, 170, 0.2) 0px 15px, rgba(240, 46, 170, 0.1) 0px 20px, rgba(240, 46, 170, 0.05) 0px 25px;

}

.card-3{
box-shadow: rgba(240, 46, 170, 0.4) 5px 5px, rgba(240, 46, 170, 0.3) 10px 10px, rgba(240, 46, 170, 0.2) 15px 15px, rgba(240, 46, 170, 0.1) 20px 20px, rgba(240, 46, 170, 0.05) 25px 25px;

}

.card-4{
box-shadow: rgba(0, 0, 0, 0.07) 0px 1px 2px, rgba(0, 0, 0, 0.07) 0px 2px 4px, rgba(0, 0, 0, 0.07) 0px 4px 8px, rgba(0, 0, 0, 0.07) 0px 8px 16px, rgba(0, 0, 0, 0.07) 0px 16px 32px, rgba(0, 0, 0, 0.07) 0px 32px 64px;

}

body{
  font-family:  'Cabin','Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
  font-size: 12pt;
  background-color:#ffffff;

 # background-image: url('https://tangibleanalytics.eu/wp-content/uploads/2021/01/back6.gif');
  background-repeat: repeat;
  background-size: 450px 450px;
}


#nav_logo {
  width: 100%;
  margin-top: 20px;
}


code, kbd, pre, samp {
    font-family:'Cabin','Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
    text-align: center;
    
}

pre {
    display: flow-root;
    padding: 9.5px;
    margin: 0 0 10px;
    font-size: 16px;
    line-height: 1.42857143;
    border: 0px
    
}

p.caption {
  font-size: 0.9em;
  font-style: italic;
  color: grey;
  margin-right: 10%;
  margin-left: 10%;  
  text-align: center;
}

#Button de les imatges
.btn-group-vertical>.btn, .btn-group>.btn  {
  padding:0px;
}


#downloadData{
   font-size: 20px;

}




  /* Space Between TOC and 
  Righthand side content on large screens */

 @media (min-width: 992px) {
    .col-md-9 {
      width: 75%;
      padding-left: 5em !important;
    }
 }
 
 
#TOC::before {
  content: '';
  display: block;
  height: 111px;
  margin: 10px 0px 0px 0px;
  #background-image: url('https://tangibleanalytics.eu/wp-content/uploads/2021/01/logo.png');
  background-size: contain;
  background-position: center center;
  background-repeat: no-repeat;
  opacity: 0.97;
  background-color: #ffffff;

}


@media only screen and (max-width: 600px) {
   article {
    width: 115%;
    opacity: 0.96;
    padding: 8px;
    margin: 0px -5.8% -0px;
    background-color: #fff;
    box-shadow:#fff 0px 0px 0px 0px;
    
}


.funfact{
text-align: center; 
font-size:20px;  
font-weight:bold; 
color: gold;

}

body{
  background-size: 150px 150px;
  font-size: 14pt;
}


h1 {
    font-size: 24px;
}

.card {
  background: #fff;
  border-radius: 2px;
  display: block;
  height: auto;
  margin: 1rem;
  position: relative;
  width: 95%;
  margin-left: auto;
  margin-right: auto;
  padding-left: 4%;
  padding-block: 2%;
  padding-right: 4%;
  text-align: center;
}
.temps{
  text-align:center;
  font-size: 20px;

}

.persones{
  text-align:center;
  font-size: 17px;

}

subheader {
    font-style: italic;
    font-size: 19px;
}
.header {

    font-size: 28px;

}



} ")






ui <- fluidPage(
  
  use_waiter(),
  
  #css
  tags$head(tags$style(HTML(my_css))),
  
  
  # ---------------------------- UI
  fluidRow(
    #Espai
    column(7,
           #Pujada del fitxer
           #tags$h4(("Puja ací la conversa:")),
           fileInput(inputId = "upload", 
                     label = "Subida de la conversación:",
                     buttonLabel=("Upload:"),
                     accept="text/plain",
                     multiple = FALSE),
           
           
           # Idioma ANALYTICS
           selectInput(
             inputId='idioma_analytics',
             label=("Idioma del Analytics:"),
             choices = c("Castellano"="cast","Valencià"="val")),
           
           tags$br(), 
           # ------------------- INPUTS PER A PERSONALITZAR LA CONVERSA
           #-------- EN UNA
           
           radioGroupButtons(
             inputId = "T",
             label = ("Color de la imagen de las palabras:"), 
             choices = c(`<img src='nenufar_tria2.png' width=100%><div class='jhr'></div></img> Nenúfar` = "4", 
                         `<img src='giverny_tria2.png' width=100%><div class='jhr'></div></img> Giverny` = "1", 
                         `<img src='arearea_tria2.png' width=100%><div class='jhr'></div></img> Arearea` = "3",
                         `<img src='soleil_tria2.png' width=100%><div class='jhr'></div></img> Soleil` = "2"),
             justified = TRUE,
             width = "95%"
           ),
           
           # ---------------- EMAIL I ENVIAMENT
           tags$br(),
           
           tags$div(downloadButton("downloadData", "Tangibilizar!",icon = shiny::icon("gift")),style="display: flex;  justify-content: center;  align-items: center; font-size: 20px;"),
           
           tags$br()
           
    ) #fi mainPanel
  )
)






server = function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  # -------------------------------------------------------------- IDIOMA ANALYTICS
  
  w <- Waiter$new(
    html = tagList(spin_2(), tags$br(), tags$span(style="color:#831923; font-weight: 400; font-size:17px; background-color:white;", "Esperando...")),
    color = transparent(.5))
  
  
  #------------- Quan s'apreta el boto enviar. se desencadena tot
  
  output$downloadData <- downloadHandler(
    filename = function(){
      "Experiencia Tangible.zip"
    },
    content = function(file)
    {
      
      
      #Requeriments. queda millorar
      req(input$upload)
      
      #Creem el directori temporal
      dir.create(tmp <- tempfile())
      
      #COmençar el 
      w$show()
      w$update(html = tagList(spin_2(),tags$br(),  tags$span(style="color:#831923; font-weight: 400; font-size:17px; background-color:white;", "¡Leyendo y estructurando el chat! (+30s)")))
      
      #-------------------------- ACCIONS SOBRE EL FITXER
      
      #rebem el df del fitxer
      file2<-input$upload
      
      #llegim el fitxer
      bo<-infolectura_fitxer(file2$datapath, input, w, tmp, as.numeric(input$T))
      
      if (bo){
        
        cat("jasta!")
        w$hide()
        
        files<- paste0(tmp,c("/Analytics PRO.html", "/Tangible", "/Tangible Emojis"))
        
        x<-zip(file, files, mode="cherry-pick")
        
        unlink(tmp)
        session$reload()
        
      }else{
        cat("HEEEEEy")
        
        w$hide()
        unlink(tmp)
        x<-zip(file, "Ooops_Problemas.html", mode="cherry-pick")
      }
      
      
      x
      
    })
  
  
  
  
  
  
}




shinyApp(ui, server)
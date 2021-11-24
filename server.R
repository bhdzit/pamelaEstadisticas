library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(zoo)
library(Hmisc)
library(sqldf)
library(stringi)
library(tidyverse)
library(data.table)




options(shiny.maxRequestSize = 30*1024^2)# cambiar el límtite del tamaño del archivo a 30mb

shinyServer(function(input,output,session) {
  
  output$filedf <- renderTable({#nombre y direccion de los archivos
    if(is.null(input$file)){return ()}
    input$file
  })
  
  output$filedf2 <- renderTable({#tabla con las dirrecciones de los archvios subidos
    if(is.null(input$file)){return ()}
    input$file$datapath
    
    
  
  })
  

  

  output$selectfile <- renderUI({# seleccionar el archivo
   
    selectInput("Parcial", "Elige número del Parcial:",
                list(`Parcial` = list(1, 2, 3,4,5)
                    ) 
    )

  })

  output$table <-renderPrint({
    if(is.null(input$file)){return()}#nombre del archivo = al select
    r <- read.table(file=input$file$datapath[input$file$name=="Fuente final semestre.csv"], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    output$tablaUsuario <- DT::renderDataTable(r)#leer tablas segun la selcción
    })
  
  output$reportes <-renderPrint({ 
    
    if(is.null(input$file)){return()}#nombre del archivo = al select
    ffid <- read.table(file=input$file$datapath[input$file$name=="FFS.csv"], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    FParcial <- read.table(file=input$file$datapath[input$file$name=="FuenteParcial.csv"], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    clasAsi<- read.table(file=input$file$datapath[input$file$name=="ClasifAsig.csv"], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    porc <- read.csv(file=input$file$datapath[input$file$name=="porcentajes.csv"], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
   
    
    FParcial <- FParcial[!duplicated(FParcial), ]
    depesp <- select(FParcial,5,3,2,5,6,7,8,9)
    
    porc <- select(porc,2,3,4,5,6,7,8,9,10,11,12)
    porc <- porc[!apply(porc == "", 1, all),]
    
    
    porc <-porc[-1, ]
    
    nms <- c("PROGRAMA.EDUCATIVO", "semestre","Materia","1","2","3","4","5","6","7","8")
    setnames(porc, nms)
    
    p <- porc %>%
      separate(Materia, c("NOMBRE.DE.LA.ASIGNATURA","ret_Clave"), sep = "-")
    
    p$ret_Clave<-gsub(" ", "",p$ret_Clave)
    
    p <- select(p,4,5,6,7,8,9,10,11,12)
    
    ot <- p %>%
      pivot_longer(!ret_Clave, names_to = "tema", values_to = "parcial")
    
    ot$parcial[ot$parcial==""] <-  NA 
    ot <- filter(ot, !is.na(parcial))
    #temas por parcial-View(ot)
    colnames(ot)[2]  <- 'lsc_NumPArcial'
    
    
    
    # parcial1 <- parcialestema[parcialestema$parcial == 1,]
    # View(parcial1)
    
    
    
    depesp <- select(FParcial,5,3,2,5,6,7,8,9)
    
    
    
    
    z<-data.frame(PROGRAMA.EDUCATIVO=c("INGENIERÍA ELECTROMECÁNICA","INGENIERÍA ELECTROMECÁNICA","INGENIERÍA ELECTROMECÁNICA","INGENIERÍA ELECTROMECÁNICA","INGENIERÍA ELECTROMECÁNICA","INGENIERÍA ELECTROMECÁNICA","INGENIERÍA ELECTROMECÁNICA","INGENIERÍA INDUSTRIAL"),
                  NOMBRE.DE.LA.ASIGNATURA=c("INGENIERÍA DE CONTROL CLÁSICO","INSTALACIONES ELÉCTRICAS","INSTALACIONES MECÁNICAS","MÁQUINAS Y EQUIPOS TÉRMICOS I","PROTECCIÓN DE SISTEMAS ELÉCTRICOS DE POTENCIA","SISTEMAS HIDRAULICOS Y NEUMATICOS DE POTENCIA","TÓPICOS DE DISEÑO MECÁNICO","SISTEMAS DE MANUFACTURA"),
                  AREA.DE.CONOCIMIENTO=c("INGENIERÍA APLICADA Y DISEÑO EN INGENIERÍA","INGENIERÍA APLICADA Y DISEÑO EN INGENIERÍA","DEPTO.DE INGENIERIA ELECTROMECANICA","INGENIERÍA APLICADA Y DISEÑO EN INGENIERÍA","DEPTO.DE INGENIERIA ELECTROMECANICA","INGENIERÍA APLICADA Y DISEÑO EN INGENIERÍA","DEPTO.DE INGENIERIA ELECTROMECANICA","DPTO.DE INGENIERÍA INDUSTRIAL")) 
    
    clasAsi<-rbind(clasAsi,z)   
    
    clasAsi <- clasAsi[!grepl("ACTIVIDADES", clasAsi$NOMBRE.DE.LA.ASIGNATURA),]
    clasAsi <- clasAsi[!grepl("SERVICIO SOCIAL", clasAsi$NOMBRE.DE.LA.ASIGNATURA),]
    clasAsi <- clasAsi[!grepl("RESIDENCIA", clasAsi$NOMBRE.DE.LA.ASIGNATURA),]
    #*******
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("VISIÓN POR COMPUTADORA EN DISPOSITIVOS MÓVILES."='VISIÓN POR COMPUTADORA EN TECNOLOGÍA MÓVIL'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("PROBABILIDAD Y  ESTADÍSTICA"='PROBABILIDAD Y ESTADISTICA'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("LENGUAJES MULTIPLATAFORMA PARA EL DESARROLLO MÓVIL."='LENGUAJES MULTIPLATAFORMAS PARA EL DESARROLLO MÓVIL'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("FUNDAMENTOS DE BASES DE DATOS"='FUNDAMENTOS DE BASE DE DATOS'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("ADMINISTRACIÓN DE OPERACIONES II"='ADMINISTRACIÓN DE LAS OPERACIONES II'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("CADENA DE SUMINISTRO Y COMERCIALIZACIÓN ALIMENTARIA"='CADENA DE SUMINISTROS Y COMERCIALIZACION ALIMENTARIA'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("EMPAQUE, ENVASE Y EMBALAJE"='EMPAQUE, ENVASE Y EMBALAJE'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("INDUCCIÓN A LA ADMINISTRACIÓN Y ECONOMÍA"='INTRODUCCION A LA ADMINISTRACION Y ECONOMIA'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("MODELADO DE SISTEMAS ELÉCTRICOS DE POTENCIA (ESP. SISTEMAS)"='MODELADO DE SISTEMAS ELECTRICOS DE POTENCIA'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("QUALITY CORE TOOLS"='QUALITY CORTE TOOLS'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("SISTEMAS DE MANUFACTURA"='SISTEMAS DE MANUFACTURA'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("SISTEMAS ELÉCTRICOS DE POTENCIA 2 (ESP. SISTEMAS)"='SISTEMAS ELECTRICOS DE POTENCIA 2'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("TALLER DE CONTROL ESTADÍSTICO DE PROCESO"='TALLER DE CONTROL ESTADISTICO DE PROCESOS'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("TECNOLOGÍA DE CONSERVACIÓN"='TECNOLOGIA DE LA CONSERVACION'))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("TRANSFERENCIA Y COMERCIALIZACIÓN DE TECNOLOGÍA"="TRANSFERENCIA Y COMERCIALIZACION DE TECNOLOGIA"))
    clasAsi[2]<-revalue(clasAsi[, c(2)], c("ENVASADO DE ALIMENTOS  Y LA INDUSTRIA 4.0"="ENVASADO DE ALIMENTOS Y LA INDUSTRIA 4.0"))
    
    #*****
    
    colnames(FParcial)
    clasAsi[1] <- chartr('ÁÉÍÓÚ','AEIOU',clasAsi[, c(1)])
    clasAsi[2]<- chartr('ÁÉÍÓÚ','AEIOU',clasAsi[, c(2)])
    
    clasAsi[2]<-gsub("[[:punct:]]", "",clasAsi[, c(2)])
    
    
    colnames(clasAsi)
    colnames(FParcial)
    FParcial <- FParcial[, -c(19,13,14,15)]
    
    
    
    colnames(clasAsi)[1]  <- 'PROGRAMA.EDUCATIVO'
    colnames(clasAsi)[2] <- 'NOMBRE.DE.LA.ASIGNATURA'
    colnames(FParcial)[1]  <- 'PROGRAMA.EDUCATIVO'
    colnames(FParcial)[3] <- 'NOMBRE.DE.LA.ASIGNATURA'
    
    
    FParcial[3]<-revalue(FParcial[, c(3)], c("SEGURIDAD Y SALUD EN ELTRABAJO"='SEGURIDAD Y SALUD EN EL TRABAJO'))
    FParcial[3]<-revalue(FParcial[, c(3)], c("CERTIFICACIÓN EN CALIDAD E INOCUIDAD EN LA INDUSTRIA AL"="CERTIFICACIÓN EN CALIDAD E INNOCUIDAD EN LA INDUSTRIA ALIMENTARIA"))
    FParcial[3]<-revalue(FParcial[, c(3)], c("FORMULACIÓN Y EVALUACIÓN  DE PROYECTOS"="FORMULACIÓN Y EVALUACIÓN DE PROYECTOS"))
    FParcial[3]<-revalue(FParcial[, c(3)], c("INGENERÍA ECONÓMICA"="INGENIERÍA ECONÓMICA"))
    FParcial[3]<-revalue(FParcial[, c(3)], c("TRANFERENCIA Y COMERCIALIZACIÓN DE TECNOLOGÍA"="TRANSFERENCIA Y COMERCIALIZACION DE TECNOLOGIA"))
    FParcial[3] <- chartr('ÁÉÍÓÚ','AEIOU',FParcial[, c(3)])
    FParcial[3] <-gsub("[[:punct:]]", "",FParcial[, c(3)])
    
    #**************
    
    
    
    FParcial[1]<-revalue(FParcial[, c(1)], c("I GEST EMP"='INGENIERÍA EN GESTIÓN EMPRESARIAL'))
    FParcial[1]<-revalue(FParcial[, c(1)], c('IND. ALIM.'='INGENIERÍA EN INDUSTRIAS ALIMENTARIAS'))
    FParcial[1]<-revalue(FParcial[, c(1)], c('ING. ELECMEC'='INGENIERÍA ELECTROMECÁNICA'))
    FParcial[1]<-revalue(FParcial[, c(1)], c('ING. IND.'='INGENIERÍA INDUSTRIAL'))            
    FParcial[1]<-revalue(FParcial[, c(1)], c('ING. LOGISTICA'='INGENIERÍA EN LOGÍSTICA'))
    FParcial[1]<-revalue(FParcial[, c(1)], c('ING. SIS. COMP.'='INGENIERÍA EN SISTEMAS COMPUTACIONALES'))
    FParcial[1]<-revalue(FParcial[, c(1)], c('ING. EN TIC'= 'INGENIERIA EN TIC´S'))
    
    #quitar acentos
    FParcial[1] <- chartr('ÁÉÍÓÚ','AEIOU',FParcial[, c(1)])
    
    
    FParcial$comp <-FParcial$NOMBRE.DE.LA.ASIGNATURA %in% clasAsi$NOMBRE.DE.LA.ASIGNATURA
    estaEnClasificacion <- select(FParcial,NOMBRE.DE.LA.ASIGNATURA,comp)
    NestaEnClasificacion <-estaEnClasificacion[estaEnClasificacion$comp == FALSE,]
    
    
    output$porcentajes <- DT::renderDataTable(asigno)#leer tablas segun la selcción
    grupno <- NestaEnClasificacion %>% group_by(NOMBRE.DE.LA.ASIGNATURA)
    asigno <- grupno %>% dplyr::summarise(n = n())
    
    
    FParcial <- merge(FParcial, clasAsi, by = c("PROGRAMA.EDUCATIVO", "NOMBRE.DE.LA.ASIGNATURA"), all.x = TRUE)
    
    
    colnames(FParcial)[1]  <- 'PROGRAMA.EDUCATIVO'
    colnames(FParcial)[2] <- 'NOMBRE.DE.LA.ASIGNATURA'
    colnames(FParcial)[3] <- 'gse_Nombre'
    colnames(FParcial)[4] <- 'ret_Creditos'
    colnames(FParcial)[5] <- 'dep_Nombre'
    colnames(FParcial)[6] <- 'cat_Nombre'
    colnames(FParcial)[7] <- 'cat_ApePat'
    colnames(FParcial)[8] <- 'cat_ApeMAt'
    colnames(FParcial)[9] <- 'alu_NumControl'
    colnames(FParcial)[10] <- 'Alu_ApePaterno'
    colnames(FParcial)[11]  <- 'alu_ApeMaterno'
    colnames(FParcial)[12] <- 'alu_Nombre'
    colnames(FParcial)[13] <- 'lsc_NumPArcial'
    colnames(FParcial)[14] <- 'lsc_Calificacion'
    colnames(FParcial)[15] <- 'ind_Nombre'
    colnames(FParcial)[16] <- 'calificacion'
    
    
    FParcial <-select(FParcial, 1:16,18)
    
 
    #no visualizar datos donde los datos del area de conocimiento es vacio
    FParcial <- FParcial[!is.na(FParcial$calificacion),]
    
    
    #no visualizar datos donde los datos del area de conocimiento es vacio
    FParcial <- FParcial[!is.na(FParcial$AREA.DE.CONOCIMIENTO),]
    
    
    
    #**************************filtrar parciales
    FParcial <- # para guardar la nueva variable, se 'asigna' al df  
      FParcial %>% # hacer substring del nombre del grupo solo las primeras tres letras para la clave de reticula
      mutate( ret_Clave= substring(gse_Nombre,  1, 3))
    #View(FParcial) ver fuente parcial con nueva colmna
    #unir los dataframes con parciales correspondientes a los temas
    parcialestema<- merge(ot, FParcial, by = c("lsc_NumPArcial","ret_Clave"), all.Y=TRUE)
    #View(parcialestema)
    
    #***********************************Fuente del primer parcial
   
    FParcial <- parcialestema[parcialestema$parcial == input$Parcial,]
    View(FParcial)
    
    temasdiferentes<- FParcial %>%
      group_by(gse_Nombre) %>%
      dplyr::summarise(tem =max(lsc_NumPArcial))
    
    # temasdiferentester<- FParcial %>%
    #   group_by(gse_Nombre) %>%
    #   dplyr::summarise(ter =max(lsc_NumPArcial))
    # 
    # temasdiferentesem<- FParcial %>%
    #   group_by(gse_Nombre) %>%
    #   dplyr::summarise(emp =min(lsc_NumPArcial))
    
 
    
    unidades<- FParcial
    
    unidad <- lst()
    unidadApro <- lst()
    AprobadosxGrupoUnidad <- lst()
    unidadRep <- lst()
    ReprobadosxGrupoUnidad <- lst()
    
    min(temasdiferentes$tem)
    for (i in min(temasdiferentes$tem):max(temasdiferentes$tem) ) {
      unidad[[i]]<-unidades[unidades$lsc_NumPArcial == i, ]
      View( unidad[[i]])
      unidadApro[[i]] <- unidad[[i]][unidad[[i]]$calificacion >=70,]
      AprobadosxGrupoUnidad[[i]] <- unidadApro[[i]] %>% 
        group_by(gse_Nombre) %>% 
        dplyr::summarise(numap = n_distinct(alu_NumControl))
      colnames(AprobadosxGrupoUnidad[[i]])[2] <- paste("Aprobados Unidad",i)
      unidadRep[[i]] <- unidad[[i]][unidad[[i]]$calificacion <70,]
      ReprobadosxGrupoUnidad[[i]] <- unidadRep[[i]] %>% 
        group_by(gse_Nombre) %>% 
        dplyr::summarise(numRep = n_distinct(alu_NumControl))
      colnames(ReprobadosxGrupoUnidad[[i]])[2] <- paste("Reprobados Unidad",i)
      
    }
    
    
    output$fuente <- DT::renderDataTable(FParcial)
    
    #View(FParcial) visualizar solo el primer parcial
    
    #***********************************
    
    #suma de calificaciones por grupo
    r<-aggregate(FParcial$lsc_Calificacion, by=list(gse_Nombre=FParcial$gse_Nombre), FUN=sum)
    names(r)[names(r) == 'x'] <- 'sumaCalificaciones'
    
    
    #si la suma de calificaciones por grupo es 0 no mostrar
    r$'sumaCalificaciones'[r$'sumaCalificaciones'== 0] <- NA
    
    #no visualizar dato
    r <- r[!is.na(r$sumaCalificaciones),]
    
    
    
    #Right (outer) join: unir todas las filas del segundo data frame con las correspondientes en el primero. 
    sumaFParcial <- merge(r, FParcial, by = c("gse_Nombre"),all.x = TRUE)
    
    sumaFParcial <- within(sumaFParcial, C <- paste(cat_Nombre,cat_ApePat,cat_ApeMAt, sep=' '))
    
    
    
    
    mostrarSolo <- select(sumaFParcial,"gse_Nombre","lsc_NumPArcial","parcial","PROGRAMA.EDUCATIVO", "NOMBRE.DE.LA.ASIGNATURA","AREA.DE.CONOCIMIENTO","alu_NumControl","calificacion")
    
    #funcion
    mi_sumaria <- function(tabla, funcion){
      tabla %>% 
        group_by(gse_Nombre) %>% 
        summarise_all(funcion) %>% 
        mutate(categoria = funcion)
    } 
    
    
  #**************  
    todasCalif<- sumaFParcial[sumaFParcial$calificacion >= 0,] 
    
    # maxPGruposReg<- todasCalif %>% 
    #   group_by(gse_Nombre) %>% 
    #   dplyr::summarise(n=n())
    # 
    
    
    
    numeroEvaluaciones<- sumaFParcial %>%
      group_by(gse_Nombre) %>%
      dplyr::summarise(n1 = n_distinct(alu_NumControl))

    maxiscparc<- sumaFParcial %>%
      group_by(gse_Nombre) %>%
      dplyr::summarise(n2 = max(lsc_NumPArcial))


    numeval<- merge(maxiscparc,numeroEvaluaciones,by = c("gse_Nombre"))

    numeval[, c(2)] <- sapply(numeval[, c(2)], as.numeric)
     numeval <- numeval %>%
       mutate(n = n1*n2)

     maxPGruposReg <- select(numeval,"gse_Nombre","n")
    
    
    
    gsnareaasi<-sumaFParcial[,c("PROGRAMA.EDUCATIVO" ,"gse_Nombre", "AREA.DE.CONOCIMIENTO","NOMBRE.DE.LA.ASIGNATURA","C","sumaCalificaciones")]
    gsnareaasi<-gsnareaasi[!duplicated(gsnareaasi), ]
    
    #no visualizar datos donde la temporada es vacío
    gsnareaasi <- gsnareaasi[!is.na(gsnareaasi$AREA.DE.CONOCIMIENTO),]
    
    
    
    rep1<- merge(gsnareaasi,maxPGruposReg,by = c("gse_Nombre"),all.x = TRUE)
    
    
    rep1 <- # para guardar la nueva variable, se 'asigna' al df  
      rep1 %>% # del data frame completo
      mutate( promedio = sumaCalificaciones / n )
    
    rep1$promedio <- formatC(rep1$promedio, format = "f", digits=2)
    
    #********************APROBADOS
    
    aprobados <- sumaFParcial[sumaFParcial$calificacion >=70,]
    
    
    
    grupoParcialap<-aprobados[,c("gse_Nombre", "lsc_NumPArcial")]
    
    
    nControlGrupoapr<-aprobados[,c("alu_NumControl", "gse_Nombre")]
    
    
    
    max <- bind_rows( mi_sumaria(grupoParcialap, "max"))
    
    registrosAlumnap<- nControlGrupoapr %>% 
      group_by(gse_Nombre) %>% 
      dplyr::summarise(n = n())
    
    maxPGruposRegap<- merge(registrosAlumnap,max)
    
    colnames(maxPGruposRegap)
    maxPGruposRegap <-select(maxPGruposRegap,gse_Nombre,n)
    names(maxPGruposRegap)[names(maxPGruposRegap) == 'n'] <- 'Numero_aprobados'
    
    
    #********************APROBADOS
    
    
    
    
    reprobados <- sumaFParcial[sumaFParcial$calificacion < 70,]
    
    
    
    grupoParcialrep<-reprobados[,c("gse_Nombre", "lsc_NumPArcial")]
    
    
    nControlGruporep<-reprobados[,c("alu_NumControl", "gse_Nombre")]
    
    
    
    max <- bind_rows( mi_sumaria(grupoParcialrep, "max"))
    
    
    registrosAlumnrep<- nControlGruporep %>% 
      group_by(gse_Nombre) %>% 
      dplyr::summarise(n = n())
    
    maxPGruposRegrep<- merge(registrosAlumnrep,max)
    
    colnames(maxPGruposRegrep)
    maxPGruposRegrep <-select(maxPGruposRegrep,gse_Nombre,n)
    names(maxPGruposRegrep)[names(maxPGruposRegrep) == 'n'] <- 'Numero_reprobados'
    
    
    
    #****************REPORTE GENERAL
    
    
    
    
    
    
    rep1<- merge(rep1,maxPGruposRegap,by = c("gse_Nombre"),all.x = TRUE)
    rep1<- merge(rep1,maxPGruposRegrep,by = c("gse_Nombre"),all.x = TRUE)
    
    
    
    
    rep1 <- # para guardar la nueva variable, se 'asigna' al df  
      rep1 %>% # del data frame completo
      mutate( porcentje_Aprobacion = Numero_aprobados / n)
    
    rep1 <- # para guardar la nueva variable, se 'asigna' al df  
      rep1 %>% # del data frame completo
      mutate( porcentje_Reprobacion = Numero_reprobados / n)
    
    colnames(rep1)
    
    rep1 <-rep1 %>%
      select(PROGRAMA.EDUCATIVO,gse_Nombre,NOMBRE.DE.LA.ASIGNATURA,AREA.DE.CONOCIMIENTO, C,sumaCalificaciones,promedio,n,Numero_aprobados,porcentje_Aprobacion,Numero_reprobados,porcentje_Reprobacion)
    
    
    
    rep1$porcentje_Aprobacion <- formatC(rep1$porcentje_Aprobacion, format = "f", digits=2)
    rep1 <- rep1 %>%
      mutate(porcentje_Aprobacion = as.numeric(porcentje_Aprobacion) * 100) 
    
    rep1$porcentje_Aprobacion <- paste(rep1$porcentje_Aprobacion , "%", sep = "")
    
    
    rep1 <- mutate_all(rep1, ~replace(., is.na(.), 0))
    
    
    rep1$porcentje_Reprobacion <- formatC(rep1$porcentje_Reprobacion, format = "f", digits=2)
    rep1 <- rep1 %>%
      mutate(porcentje_Reprobacion = as.numeric(porcentje_Reprobacion) * 100) 
    
    rep1$porcentje_Reprobacion <- paste(rep1$porcentje_Reprobacion , "%", sep = "")
    
    #*************Reporte  general de todas carreras
    aprepunidad<-  rep1 
    for (i in 1:length(unidad) ) {
      
      aprepunidad<- merge(aprepunidad,AprobadosxGrupoUnidad[[i]],by = c("gse_Nombre"),all.x = TRUE) 
      aprepunidad<- merge(aprepunidad, ReprobadosxGrupoUnidad[[i]],by = c("gse_Nombre"),all.x = TRUE)
      
    } 
    
  
    
    reporte1 <- aprepunidad
    colnames(reporte1)[1]  <- "GRUPO"
    colnames(reporte1)[2] <- 'PROGRAMA.EDUCATIVO'
    colnames(reporte1)[3] <-  "ASIGNATURA" 
    colnames(reporte1)[4] <-    "EJE"  
    colnames(reporte1)[5] <-    "DOCENTE"  
    colnames(reporte1)[6] <-     "SUMA/CALIFICACIONES"  
    colnames(reporte1)[7] <-    "PROMEDIO"  
    colnames(reporte1)[8] <- "TOTAL DE ESTUDIANTES" 
    colnames(reporte1)[9] <- "APROBADOS"  
    colnames(reporte1)[10] <- "% APROBACION"     
    colnames(reporte1)[11] <-"REPROBADOS"
    colnames(reporte1)[12] <-   "% REPPROBACION"
  
    
    output$repgen1 <- DT::renderDataTable(reporte1)
    
    
    #***********reporte de ing en sistemas
    colnames(mostrarSolo)[1] <- "GRUPO" 
    colnames(mostrarSolo)[5] <-  "ASIGNATURA" 
    colnames(mostrarSolo)[6] <-   "EJE" 
    
    
    carreras <- unique(reporte1$PROGRAMA.EDUCATIVO)
    
    
    
    reportexCarrera <- lst()
    s <- lst()
    reportexArea<- lst()
    sumaCalifs<- lst()
    totalEst<- lst()
    dsumacalifs<- lst()
    nsuma<- lst()
    unir<- lst()
    sumanAREA<- lst()
    reprobadosArea <- lst()
    areaTotalRep<- lst()
    areaTotalRepAp <- lst()
    totalesxAreaCarrera<- lst()
    for (i in  1:length(carreras)){
      reportexCarrera[[i]]<-reporte1[reporte1$PROGRAMA.EDUCATIVO == carreras[i], ]
      reporteAlumnosunir<- merge( reportexCarrera[[i]],mostrarSolo, by = c(  "ASIGNATURA" ,"EJE","PROGRAMA.EDUCATIVO", "GRUPO" ))
      reportexCarrera[[i]] <-  reportexCarrera[[i]][,-2]
      
      s[[i]]<- unique(reportexCarrera[[i]]$EJE)
      
      #***********REPORTES X AREA
      reportexArea[[i]]<- lst()
      sumaCalifs[[i]] <- lst()
      totalEst[[i]]<- lst()
      
      for (j in  1:length(s[[i]])){
        reportexArea[[i]][[j]]<-reportexCarrera[[i]][reportexCarrera[[i]]$EJE == s[[i]][j], ]
        # View(reportexArea[[i]][[j]])
        sumaCalifs[[i]][[j]] <- c(s[[i]][j],sum(reportexArea[[i]][[j]]$"SUMA/CALIFICACIONES"))
        totalEst[[i]][[j]] <- c(s[[i]][j],sum(reportexArea[[i]][[j]]$"TOTAL DE ESTUDIANTES"))
      }
      
      dsumacalifs[[i]] <- data.frame(matrix(unlist(  sumaCalifs[[i]]), nrow = length( sumaCalifs[[i]]), byrow = TRUE))
      colnames(dsumacalifs[[i]])[2]  <- 'totalCalifs'
      
      
      nsuma[[i]]<- data.frame(matrix(unlist(totalEst[[i]]), nrow = length(totalEst[[i]]), byrow = TRUE))
      colnames(nsuma[[i]])[2]  <- 'totalEval'
      
      unir[[i]]<- merge(nsuma[[i]],dsumacalifs[[i]],by = c("X1"))
      unir[[i]][, c(2,3)] <- sapply(unir[[i]][, c(2,3)], as.numeric)
      
      unir[[i]] <- # para guardar la nueva variable, se 'asigna' al df
        unir[[i]] %>% # del data frame completo
        mutate(promedioc = (totalCalifs)/totalEval)
      
      colnames(unir[[i]])[1]  <- 'EJE'
      
      sumanAREA[[i]]<- reporteAlumnosunir %>%
        group_by(EJE) %>%
        dplyr::summarise(estudiantesEval = n_distinct(alu_NumControl))
      
      #*REPROBADOS POR PARCIAL
      reprobadosArea[[i]]<- reporteAlumnosunir[reporteAlumnosunir$calificacion <70,]
      
      reprobadosArea[[i]]<- reprobadosArea[[i]] %>%
        group_by(EJE) %>%
        dplyr::summarise(rep = n_distinct(alu_NumControl))
      
      areaTotalRep[[i]]<- merge(sumanAREA[[i]],reprobadosArea[[i]],by = c("EJE"))
      
      areaTotalRepAp [[i]]<- # para guardar la nueva variable, se 'asigna' al df
        areaTotalRep[[i]] %>% # del data frame completo
        mutate( totalAprobados= estudiantesEval - rep)
      
      
      #*******porcentajes apro
      areaTotalRepAp [[i]] <- # para guardar la nueva variable, se 'asigna' al df
        areaTotalRepAp [[i]]%>% # del data frame completo
        mutate( porcentjeAprobados = totalAprobados/ estudiantesEval)
      
      areaTotalRepAp [[i]] $porcentjeAprobados <- formatC(areaTotalRepAp [[i]] $porcentjeAprobados, format = "f", digits=4)
      areaTotalRepAp [[i]]  <- areaTotalRepAp [[i]]  %>%
        mutate(porcentjeAprobados = as.numeric(porcentjeAprobados) * 100)
      areaTotalRepAp [[i]] $porcentjeAprobados <- paste(areaTotalRepAp [[i]] $porcentjeAprobados , "%", sep = "")
      
      
      areaTotalRepAp [[i]] <- # para guardar la nueva variable, se 'asigna' al df
        areaTotalRepAp [[i]] %>% # del data frame completo
        mutate( porcentjeReprobados = rep/ estudiantesEval)
      
      areaTotalRepAp [[i]]$porcentjeReprobados <- formatC(areaTotalRepAp [[i]]$porcentjeReprobados, format = "f", digits=4)
      areaTotalRepAp [[i]] <- areaTotalRepAp [[i]] %>%
        mutate(porcentjeReprobados = as.numeric(porcentjeReprobados) * 100)
      areaTotalRepAp [[i]]$porcentjeReprobados <- paste(areaTotalRepAp [[i]]$porcentjeReprobados , "%", sep = "")
      
      totalesxAreaCarrera[[i]]<- merge(areaTotalRepAp[[i]],unir [[i]],by = c("EJE"))
      totalesxAreaCarrera[[i]] <- select(totalesxAreaCarrera[[i]],"EJE","totalCalifs","totalEval","promedioc","estudiantesEval","totalAprobados","porcentjeAprobados","rep","porcentjeReprobados")
      
      
      colnames(totalesxAreaCarrera[[i]])[2] <- 'Suma total/calificaciones'
      colnames(totalesxAreaCarrera[[i]])[3] <- 'Suma total/Evaluados'
      colnames(totalesxAreaCarrera[[i]])[4] <- 'Suma total/promedio'
      colnames(totalesxAreaCarrera[[i]])[5] <- 'Total Estudiantes-Eval'
      colnames(totalesxAreaCarrera[[i]])[6] <- 'Aprobados'
      colnames(totalesxAreaCarrera[[i]])[7] <- '% de Aprobacion'
      colnames(totalesxAreaCarrera[[i]])[8] <- 'Reprobados'
      colnames(totalesxAreaCarrera[[i]])[9] <- '% de Reprobacion'
      totalesxAreaCarrera [[i]] $'Suma total/promedio'<- formatC(totalesxAreaCarrera [[i]] $'Suma total/promedio', format = "f", digits=2) 
    }
    
  
    
       output$repSistemas <- DT::renderDataTable(reportexCarrera[[5]])


    output$reparea1 <- DT::renderDataTable(reportexArea[[5]][[1]])
    output$reparea2  <- DT::renderDataTable(reportexArea[[5]][[2]])
    output$reparea3 <- DT::renderDataTable(reportexArea[[5]][[3]])
    output$reparea4  <- DT::renderDataTable(reportexArea[[5]][[4]])


    output$totalesxAreaSIS  <- DT::renderDataTable(totalesxAreaCarrera[[5]])


    #**********gestion x area

    output$repGestion  <- DT::renderDataTable(reportexCarrera[[2]])


    output$repareag1 <- DT::renderDataTable(reportexArea[[2]][[1]])
    output$repareag2  <- DT::renderDataTable(reportexArea[[2]][[2]])
    output$repareag3 <- DT::renderDataTable(reportexArea[[2]][[3]])
    output$repareag4  <- DT::renderDataTable(reportexArea[[2]][[4]])
    output$repareag4  <- DT::renderDataTable(reportexArea[[2]][[5]])



    output$totalesxAreaGES  <- DT::renderDataTable(totalesxAreaCarrera[[2]])




    #**********************************GESTION  FIN

#**********INICIO ALIMENTARIAS

    output$reporteAlim  <- DT::renderDataTable(reportexCarrera[[1]])


    output$repareaa1 <- DT::renderDataTable(reportexArea[[1]][[1]])
    output$repareaa2  <- DT::renderDataTable(reportexArea[[1]][[2]])
    output$repareaa3 <- DT::renderDataTable(reportexArea[[1]][[3]])
    output$repareaa4  <- DT::renderDataTable(reportexArea[[1]][[4]])
    output$repareaa5  <- DT::renderDataTable(reportexArea[[1]][[5]])


    output$totalesareaAlm  <- DT::renderDataTable(totalesxAreaCarrera[[1]])
#*******FIN ALIMENTARIAS

    #*******Incio electromecanica

    output$reporteEl  <- DT::renderDataTable(reportexCarrera[[3]])

    output$repareae1 <- DT::renderDataTable(reportexArea[[3]][[1]])
    output$repareae2  <- DT::renderDataTable(reportexArea[[3]][[2]])
    output$repareae3 <- DT::renderDataTable(reportexArea[[3]][[3]])
    output$repareae4  <- DT::renderDataTable(reportexArea[[3]][[4]])
    output$repareae5  <- DT::renderDataTable(reportexArea[[3]][[5]])
    output$repareae6  <- DT::renderDataTable(reportexArea[[3]][[6]])


    output$totalesEltroar <- DT::renderDataTable(totalesxAreaCarrera[[3]])
 
    #*********INICIO INDUSTRIAL

    output$repoIn <- DT::renderDataTable(reportexCarrera[[6]])


    output$repareai1 <- DT::renderDataTable(reportexArea[[6]][[1]])
    output$repareai2  <- DT::renderDataTable(reportexArea[[6]][[2]])
    output$repareai3 <- DT::renderDataTable(reportexArea[[6]][[3]])
    output$repareai4  <- DT::renderDataTable(reportexArea[[6]][[4]])
    output$repareai5  <- DT::renderDataTable(reportexArea[[6]][[5]])
    output$repareai6  <- DT::renderDataTable(reportexArea[[6]][[6]])


    output$totalesxAreaIn <- DT::renderDataTable(totalesxAreaCarrera[[6]])

     #*********INICIO LOGISTICA

    output$reporteLog <- DT::renderDataTable(reportexCarrera[[4]])

    output$repareal1 <- DT::renderDataTable(reportexArea[[4]][[1]])
    output$repareal2  <- DT::renderDataTable(reportexArea[[4]][[2]])
    output$repareal3 <- DT::renderDataTable(reportexArea[[4]][[3]])
    output$repareal4  <- DT::renderDataTable(reportexArea[[4]][[4]])
    output$repareal5  <- DT::renderDataTable(reportexArea[[4]][[5]])

    output$totalesarealog <- DT::renderDataTable(totalesxAreaCarrera[[4]])

     #*TICS


    output$reptic <- DT::renderDataTable(reportexCarrera[[7]])
  

    output$repareat1 <- DT::renderDataTable(reportexArea[[7]][[1]])
    output$repareat2  <- DT::renderDataTable(reportexArea[[7]][[2]])
    output$repareat3 <- DT::renderDataTable(reportexArea[[7]][[3]])
    output$repareat4  <- DT::renderDataTable(reportexArea[[7]][[4]])
    output$repareat5  <- DT::renderDataTable(reportexArea[[7]][[5]])
    output$repareat6  <- DT::renderDataTable(reportexArea[[7]][[6]])


    #***************TOTALES POR AREA
    #*ESTUDIANTES EVALUADOS



    output$totalesxareaTic  <- DT::renderDataTable(totalesxAreaCarrera[[7]])

     #*FIN TICS

    
       
 #**********************totales x carrera   
    sumastod<-rep1 %>% group_by(PROGRAMA.EDUCATIVO) %>%
      dplyr::summarise(sumaCalificaciones = sum(sumaCalificaciones))
    
    
    sumasalum<-rep1 %>% group_by(PROGRAMA.EDUCATIVO) %>%
      dplyr::summarise(sumaAlumnos = sum(n))
    
    
    sumastotalespro<- merge(sumasalum,sumastod,by = c("PROGRAMA.EDUCATIVO"))
    
    
    sumastotalespro <- # para guardar la nueva variable, se 'asigna' al df  
      sumastotalespro %>% # del data frame completo
      mutate( promedioCarrera = sumaCalificaciones / sumaAlumnos)
    sumastotalespro 
    sumastotalespro$promedioCarrera <- formatC(sumastotalespro$promedioCarrera, format = "f", digits=2)
    
    
    
    resultado<-FParcial %>% group_by(PROGRAMA.EDUCATIVO) %>%
      summarise(count = n_distinct(alu_NumControl))
    
    
    resultado<- FParcial %>% 
      group_by(PROGRAMA.EDUCATIVO) %>% 
      dplyr::summarise(mat = n_distinct(alu_NumControl))
    
    
    reprodadossTotales<-FParcial[FParcial$lsc_Calificacion < 70,]
    resultadorep<- reprodadossTotales %>% 
      group_by(PROGRAMA.EDUCATIVO) %>% 
      dplyr::summarise(rep = n_distinct(alu_NumControl))
    
    
    totalret<- merge(resultadorep,resultado,by = c("PROGRAMA.EDUCATIVO"))
    
    
    totalret <- # para guardar la nueva variable, se 'asigna' al df  
      totalret %>% # del data frame completo
      mutate( aprobados = mat-rep)
    
    totalret <- # para guardar la nueva variable, se 'asigna' al df  
      totalret %>% # del data frame completo
      mutate( porcentjeReprobados = rep / mat)
    
    totalret$porcentjeReprobados <- formatC(totalret$porcentjeReprobados, format = "f", digits=4)
    totalret <- totalret %>%
      mutate(porcentjeReprobados = as.numeric(porcentjeReprobados) * 100) 
    
    totalret$porcentjeReprobados <- paste(totalret$porcentjeReprobados , "%", sep = "")
    
    
    totalret <- # para guardar la nueva variable, se 'asigna' al df  
      totalret %>% # del data frame completo
      mutate( porcentjeAprobados = aprobados / mat)
    
    totalret$porcentjeAprobados <- formatC(totalret$porcentjeAprobados, format = "f", digits=4)
    totalret <- totalret %>%
      mutate(porcentjeAprobados = as.numeric(porcentjeAprobados) * 100) 
    
    totalret$porcentjeAprobados <- paste(totalret$porcentjeAprobados , "%", sep = "")
    
    
    
    
    totalesxCarrera<- merge(sumastotalespro,totalret,by = c("PROGRAMA.EDUCATIVO"))
    totalesxCarrera <- select(totalesxCarrera,1,3,2,4,6,7,9,5,8)
    
    
    colnames(totalesxCarrera)[2] <- 'Suma total/calificaciones'
    colnames(totalesxCarrera)[3] <- 'Suma total/Evaluados'
    colnames(totalesxCarrera)[4] <- 'Suma total/promedio'
    colnames(totalesxCarrera)[5] <- 'Total Estudiantes-Eval'
    colnames(totalesxCarrera)[6] <- 'Aprobados'
    colnames(totalesxCarrera)[7] <- '% de Aprobacion'
    colnames(totalesxCarrera)[8] <- 'Reprobados'
    colnames(totalesxCarrera)[9] <- '% de Reprobacion'
    
    
    
    output$totalesxcarrera  <- DT::renderDataTable(totalesxCarrera)
    
    
    
    
    #********************fuente final idonea
    
    
    
    depesp <-depesp[!duplicated(depesp), ]
    depesp <- within(depesp, NombreDocente <- paste(cat_Nombre,cat_ApePat,cat_ApeMAt, sep=' '))
    
    depesp <- select(depesp,2,8,1,3,7)
    
    
    
    colnames(ffid)[8] <- 'alu_NumControl'
    
    ffidonea <- merge(ffid, depesp, by = c("alu_NumControl","ret_NomCompleto","dep_Nombre"), all.Y=TRUE)
  
    output$fuentefido <- DT::renderDataTable(ffidonea)
    })
  
  
  
 

    
    
 

  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(#mostrar primero las dos tablas
        tabPanel("Datos de los archivos", tableOutput("filedf"), tableOutput("filedf2"),tableOutput("tables")),
        tabPanel(DT::dataTableOutput("ReporteGeneral")))
    #mostrar tablas
  })
  
  df <- function(r){
    View(r)
  }
  
  
  output$fuentefid <- renderUI({
    
    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Fuente Final",verbatimTextOutput("reportes"),
               DT::dataTableOutput("fuentefido"), downloadButton("downloadData", "Download"),
               
               )
      
    )
    
  })
  
  output$primerPaso <- renderUI({
   
      tabsetPanel(#mostrar primero las dos tablas
        tabPanel("Asignaturas NO evaluadas",verbatimTextOutput("reportes"),
                 DT::dataTableOutput("porcentajes")),
        tabPanel("Fuente Parcial",verbatimTextOutput("reportes"),
                 DT::dataTableOutput("fuente")),
        tabPanel("Reporte del Parcial",verbatimTextOutput("reportes"),
                 DT::dataTableOutput("repgen1")),
        tabPanel("Totales x carrera",verbatimTextOutput("reportes"),
                 DT::dataTableOutput("totalesxcarrera"))     
        )
   
  })
  
  output$repxCarrera <- renderUI({

    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Sistemas Computacionales",verbatimTextOutput("reportes"),
               DT::dataTableOutput("repSistemas")),
      tabPanel("Reportes por area",verbatimTextOutput("reportes"),
               DT::dataTableOutput("reparea1"),DT::dataTableOutput("reparea2"),DT::dataTableOutput("reparea3"),DT::dataTableOutput("reparea4")),
      tabPanel("Totales",verbatimTextOutput("reportes"),
               DT::dataTableOutput("totalesxAreaSIS"))
    )

  })
  
  output$repxCarreraGe <- renderUI({
    
    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Ing. Gestion empresarial x Grupo",verbatimTextOutput("reportes"),
               DT::dataTableOutput("repGestion")),
    tabPanel("Reportes por area",verbatimTextOutput("reportes"),
            DT::dataTableOutput("repareag1"),DT::dataTableOutput("repareag2"),DT::dataTableOutput("repareag3"),DT::dataTableOutput("repareag4"),DT::dataTableOutput("repareag5")),
       tabPanel("Totales",verbatimTextOutput("reportes"),
             DT::dataTableOutput("totalesxAreaGES"))
    )
    
  }) 
  
  output$repxCarreraAL <- renderUI({
    
    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Ing. en industrias Alimentarias",verbatimTextOutput("reportes"),
               DT::dataTableOutput("reporteAlim")),
       tabPanel("Reportes por area",verbatimTextOutput("reportes"),
              DT::dataTableOutput("repareaa1"),DT::dataTableOutput("repareaa2"),DT::dataTableOutput("repareaa3"),DT::dataTableOutput("repareaa4"),DT::dataTableOutput("repareaa5")),
       tabPanel("Totales",verbatimTextOutput("reportes"),
              DT::dataTableOutput("totalesareaAlm"))
    )

  }) 
  output$repxCarreraEl <- renderUI({
    
    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Ing. en Electromecánica",verbatimTextOutput("reportes"),
               DT::dataTableOutput("reporteEl")),
       tabPanel("Reportes por area",verbatimTextOutput("reportes"),
                DT::dataTableOutput("repareae1"),DT::dataTableOutput("repareae2"),DT::dataTableOutput("repareae3"),DT::dataTableOutput("repareae4"),DT::dataTableOutput("repareae5"),DT::dataTableOutput("repareae6")),
       tabPanel("Totales",verbatimTextOutput("reportes"),
                DT::dataTableOutput("totalesEltroar"))
    )
    
  }) 
  
  output$repxCarreraIn <- renderUI({
    
    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Ing. en TIC'S",verbatimTextOutput("reportes"),
               DT::dataTableOutput("repoIn")),
      tabPanel("Reportes por area",verbatimTextOutput("reportes"),
               DT::dataTableOutput("repareai1"),DT::dataTableOutput("repareai2"),DT::dataTableOutput("repareai3"),DT::dataTableOutput("repareai4"),DT::dataTableOutput("repareai5"),DT::dataTableOutput("repareai6")),
      tabPanel("Totales",verbatimTextOutput("reportes"),
               DT::dataTableOutput("totalesxAreaIn"))
    )
    
  })
  
  output$repxCarreraLog <- renderUI({
    
    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Ing. Logística",verbatimTextOutput("reportes"),
               DT::dataTableOutput("reporteLog")),
      tabPanel("Reportes por area",verbatimTextOutput("reportes"),
               DT::dataTableOutput("repareal1"),DT::dataTableOutput("repareal2"),DT::dataTableOutput("repareal3"),DT::dataTableOutput("repareal4"),DT::dataTableOutput("repareal5")),
      tabPanel("Totales",verbatimTextOutput("reportes"),
               DT::dataTableOutput("totalesarealog"))
    )
    
  })
  output$repxCarreraTi <- renderUI({
    
    tabsetPanel(#mostrar primero las dos tablas
      tabPanel("Ing. en TIC'S",verbatimTextOutput("reportes"),
               DT::dataTableOutput("reptic")),
      tabPanel("Reportes por area",verbatimTextOutput("reportes"),
               DT::dataTableOutput("repareat1"),DT::dataTableOutput("repareat2"),DT::dataTableOutput("repareat3"),DT::dataTableOutput("repareat4"),DT::dataTableOutput("repareat5"),DT::dataTableOutput("repareat6")),
      tabPanel("Totales",verbatimTextOutput("reportes"),
               DT::dataTableOutput("totalesxareaTic"))
    )
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ffidonea-", "csv", sep=".")
    },
    content = function(file) {
      write.csv("reporte1", file)
    }
  )
  
  
  
  
  
  
})
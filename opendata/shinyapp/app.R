library(shiny)
library(dplyr)
library(magrittr)
library(shinydashboard)
library(DT)
library(ggplot2)
library(reshape2)
library(shinymaterial)
library(shinyjs)

a <- read.csv('./www/total.csv', header = TRUE)

a <- select(a, -X )



positions <- c(4:73)
pos <- c(4:73)
var <- a %>% select(positions)
vare <- a %>% select(pos)

# distinguimos variables "a nivel de intervalo" ("continuas" para ggplot)
nums <- sapply(a, is.numeric)
continuas <- names(a)[nums]

# y variables "categóricas" ("discretas" para ggplot)
cats <- sapply(a, is.character)
categoricas <- names(a)[cats]

#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Changing the scale of a variable to 0-100
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
vare.reescale <- rescale(vare)
b <- a[1]
a_vare <- cbind(b,vare)

# eliminamos valores na
df <- na.omit(a_vare)
# Poner Nombre como Index
# Eliminamos NOMBRE DE LA TABLA
result <- df[-1]
# Indexamos Nombre
rownames(result) <- a$BARRIO
# Escalar
df <- scale(result)
# Semilla
set.seed(123)
# Muestra
ss <- sample(1:18,18) # get the row index of randomly selected 15 rows
df <- result[ss,] # subset the rows basis this row index
df.scaled <- scale(df) # Standardise the variable
dist.eucl <- dist(df.scaled,method="euclidean")
# ClusteringDistanceMeasures  ########################################################
round(as.matrix(dist.eucl)[1:15,1:15],1)
library(factoextra)
fviz_dist(dist.eucl)


stand <- function(x){ (x-mean(x))/sd(x) }              # function to standardise
toinclude <- c("RENTA_FAMILIA","UNIVERSITARIOS_._xBarrio", "ingresoIBI_X_PERS", "ingresoAGUA_X_PERS", "COCHES.MOTOS", "TASA_PARO", "TASA_NACIMIENTOS" ) # selected variables
stand_a <- sapply(subset(a, select=toinclude), "stand")       # apply fun
rownames(stand_a) <- rownames(a)                              # player names


ex1_url <- a("text", href="http://www.oscarrojo.com")

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "OpenData 2020"),
                    dashboardSidebar(sidebarMenuOutput("Semi_collapsible_sidebar")
                                     ),
                    dashboardBody(
                        tags$head(tags$style(HTML(
                            '.myClass { 
            font-size: 20px;
            line-height: 50px;
            text-align: left;
            font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
            padding: 0 12px;
            overflow: hidden;
            color: white;
            }
            '))),
                        
                        tabItems(
                            tabItem(tabName = "dashboard",
                                    fluidPage(
                                        
                                        tags$head(tags$style(
                                            HTML('body, label, input, button, select { 
                                background-color: #fafafa;
                                     font-family: monospace; }')
                                        )),
                                        p('Comparador de Barrios // Auzoen konparadorea.'),
                                        p('En este apartado debemos seleccionar 2 barrios a comparar. Atal honetan, konparatu beharreko bi auzo hautatu behar ditugu.'),
                                        
                                        
                                        fluidRow(
                                            column(2),
                                            
                                            column(4,
                                                   align = 'center',
                                                   selectInput(inputId = "p1",
                                                               label = "",
                                                               choices = unique(a$BARRIO),
                                                               selected = 'AIETE')),
                                            
                                            column(4,
                                                   align = 'center',       
                                                   selectInput(inputId = "p2",
                                                               label = "",
                                                               choices = unique(a$BARRIO),
                                                               selected = 'ALTZA')),
                                            
                                            column(2)
                                        ),
                                        
                                        fluidRow(
                                            column(3),
                                            
                                            column(6, 
                                                   style = "font-size: 4px;
                                       padding: 0px 0px; margin-top:-14em;
                                       margin-left:4em;", 
                                                   plotOutput(outputId = "compare")),
                                            
                                            column(3)
                                        ),
                                        
                                        fluidRow(
                                            column(3),
                                            column(3,
                                                   align = 'center',
                                                   style = "margin-top:8em;",
                                                   tableOutput("profile1")),
                                            column(3,
                                                   align = 'center',
                                                   style = "margin-top:8em;",
                                                   tableOutput("profile2")),
                                            column(3)
                                        )
                                    )
                            ),
                            
                            tabItem(tabName = "means",
                                    fluidPage(
                                        fluidRow(style = "padding-bottom: 20px;",
                                                 p('K-medias es un método de agrupamiento, que tiene como objetivo la partición de un conjunto 
                                                   de n observaciones en k grupos en el que cada observación pertenece al grupo cuyo valor medio es más cercano.'),
                                                 column(4, selectInput('xcol', 'X Variable', names(vare))),
                                                 column(4, selectInput('ycol', 'Y Variable', names(vare),
                                                                       selected=names(vare)[[2]]))#,
                                                 #column(4, numericInput('clusters', 'Cluster count', 4,
                                                                        #min = 1, max = 9))
                                        ),
                                        fluidRow(
                                            plotOutput('kmeans', height = "400")
                                        ),
                                        
                                    )
                            ),
                            
                            
                            
                            
                            
                            
                            tabItem(tabName = "presentation", 
                                    img(src = "udala.jpeg", height = 100, width = 100, align = 'center'),
                                    img(src = "w79-logo_opendata.gif",  align = 'center'),
                                    h1("Concursos Open Data 2020 Open data Lehiaketa", align = "center"),
                                    h2("Creado/Sortua: Oscar Rojo", align = "center"),
                                    p(""),
                                    p(""),
                                    align = 'center',
                                    material_button(
                                        input_id = "button1",
                                        label = "https://www.donostia.eus/datosabiertos/",
                                        color = "blue"
                                    ),
                                    p(""),
                                    p(""),
                                    align = 'left',
                                    p("Este proyecto facilita el análisis y la visualización de los datos obtenidos que suministra el Ayuntamiento de 
                                      Donostia a través de su sección web DATOS ABIERTOS"),
                                    p("En ello podemos ver una pequeña parte de los datos publicados, comparativa entre los diferentes datos, y podemos realizar 
                                      agrupaciones de los datos utilizando algoritmos de Machine Learning"),
                                    p("Se ha utilizado como lenguaje R, como IDE, RSTUDIO y el SO es UBUNTU 20.04"),
                                    p(""),
                                    p("Este primer apartado describe el proyecto"),
                                    p("El segundo muestra en una tabla los datos con los que se han trabajado"),
                                    p("El tercero se utiliza un modelo de Machine Learning para obte similitudes de barrios por diferentes atributos"),
                                    p("El cuarto es otro modelo de Machine Learning - K-means para obtener similitudes entre barrios"),
                                    p("El quinto es un scatterplot con selección de variables categóricas y númericas"),
                                    p(""),
                                    p("-----------------------------------------------------------------------"),
                                    p("Proiektu honek Donostiako Udalak DATU IREKIAK web atalaren bidez ematen dituen datuak aztertzea eta bistaratzea errazten du"),
                                    p("Argitaratutako datuen zati txiki bat ikus dezakegu, datuak konparatuz, eta datuak multzokatu ditzakegu Machine Learning algoritmoak erabiliz."),
                                    p("R lengoaia gisa erabili da, hala nola IDE, RSTUDIO eta SO UBUNTU 20.04"),
                                    p("Lehenengo atal honek proiektua deskribatzen du"),
                                    p("Bigarrenak taula batean erakusten ditu landu diren datuak"),
                                    p("Hirugarrena Machine Learning eredu bat erabiltzen da auzoen antzekotasunak lortzeko, hainbat ezaugarriren arabera"),
                                    p("Laugarrena Machine Learning - K-means-en beste eredu bat da, auzoen arteko antzekotasunak lortzeko"),
                                    p("Bosgarrena scatterplot bat da, eta aldagai kategorikoen eta zenbakikoen hautapena du")
                                    
                            ),
                            
                            tabItem(tabName = "cluster",# Sidebar with a slider input for number of observations and checkboxes
                                    sidebarPanel(
                                        
                                        sliderInput("clusters", "Number of clusters:", 
                                                    min = 2,        # 1 cluster is pointless
                                                    max = 10,       # too many is too crowded
                                                    value = 4) ,    # sensible start
                                        
                                        helpText("Nota: Esta Shiny App ejecuta la agrupación jerárquica de Ward",
                                                 "en el conjunto de datos, utilizando una métrica de distancia" ,
                                                 "euclidiana y versiones estandarizadas de las variables" ,
                                                 "(es decir, con media=0 sd=1) que seleccione en las casillas",
                                                 "de verificación de abajo.",
                                                 "Puedes elegir el número de clusters con el desplazador de arriba.") ,
                                        checkboxInput("UNIVERSITARIOS_._xBarrio", "UNIVERSITARIOS_._xBarrio",     TRUE) , # as in regression project
                                        checkboxInput("TASA_PARO", "TASA_PARO",      FALSE) ,
                                        checkboxInput("RENTA_FAMILIA", "RENTA_FAMILIA",    FALSE) ,
                                        checkboxInput("ingresoIBI_X_PERS","ingresoIBI_X_PERS",    FALSE) ,
                                        checkboxInput("ingresoAGUA_X_PERS", "ingresoAGUA_X_PERS",   TRUE) , 
                                        checkboxInput("COCHES.MOTOS","COCHES.MOTOS",  FALSE),
                                        checkboxInput('TASA_NACIMIENTOS','TASA_NACIMIENTOS', FALSE)
                                        #input$RENTA_FAMILIA, input$UNIVERSITARIOS_._xBarrio, input$ingresoIBI_X_PERS, input$COCHES.MOTOS, input$ingresoAGUA_X_PERS, input$TASA_NACIMIENTOS
                                    ),
                             
                                    # Show a plot of the generated cluster dendrogram
                                    mainPanel("Cluster Dendograma",
                                              p("En este panel podemos generar agrupaciones de Barrios por similitud."),
                                              p('Los barrios son:'),
                                              h6('[1]   "AIETE"  .  .  .  .  .  [2]   "ALTZA"  .  .  .  .  .  [3]   "AMARA BERRI"',  align = 'center'),
                                              h6('[4]   "ANTIGUO"  .  .  .  .  .  [5]   "AÑORGA"  .  .  .  .  .  [6]   "ATEGORRIETA- ULIA"',  align = 'center'), 
                                              h6('[7]   "CENTRO"  .  .  .  .  .  [8]   "EGIA"  .  .  .  .  .  [9]   "GROS"',  align = 'center'),             
                                              h6('[10]  "IBAETA"  .  .  .  .  .  [11]  "IGELDO"  .  .  .  .  .  [12]  "INTXAURRONDO"',  align = 'center'),       
                                              h6('[13]  "LANDARBASO"  .  .  .  .  .  [14]  "LOIOLA"  .  .  .  .  .  [15]  "MARTUTENE"',  align = 'center'),         
                                              h6('[16]  "MIRACRUZ- BIDEBIETA"  .  .  .  .  .  [17]  "MIRAMON - ZORROAGA"  .  .  .  .  .  [18]  "ZUBIETA"',  align = 'center'), 
                                              p(''),
                                        plotOutput("distPlot", width = "100%")
                                    ),
                                    
                                    
                                
                            ),
                            
                            
                            
                            tabItem(tabName = "charts",
                                    sidebarPanel( 
                                        
                                        selectInput('x', 'Elige variable para eje X', continuas, continuas[[1]]),
                                        selectInput('y', 'Elige variable para eje Y', continuas, continuas[[2]]),
                                        selectInput('color', 'CATEGORIAS', c('None', 'BARRIO', 'SITUACION_RENTA' ,'MOTORIZACION', 'UNIVERSITARIOS', 'MUJERES', 'HOMBRE', 'EXTRANJEROS',
                                                                             'EXTR_._._s.total',
                                                                             'SIN_ESTUDIOS_PREES_SECUND',
                                                                             'SIN_ESTUDIOS_PREES_SECUND_._xBarrio',
                                                                             'SIN_ESTUDIOS_PREES_SECUND_._s.total',
                                                                             'ED.SECUNDARIA',
                                                                             'ED.SECUNDARIA_._xBarrio',
                                                                             'ED.SECUNDARIA_._s.total',
                                                                             'UNIVERSITARIOS',
                                                                             'UNIVERSITARIOS_._xBarrio',
                                                                             'UNIVERSITARIOS_._s.total',
                                                                             'PROFESIONALES',
                                                                             'PROFESIONALES_._xBarrio',
                                                                             'PROFESIONALES_._s.total',
                                                                             'TASA_PARO',
                                                                             'TASA_NACIMIENTOS',
                                                                             'TASA_ENVEJECIMIENTO',
                                                                             'SITUACION_RENTA',
                                                                             'RENTA_FAMILIA',
                                                                             'AGUA_PERSONA',
                                                                             'ingresoIBI_X_PERS',
                                                                             'ingresoIAE_X_PERS',
                                                                             'VEHICxPERSONA',
                                                                             'N_VEHICULOS',
                                                                             'COCHES.MOTOS')),
                                        
                                        checkboxInput('lm', 'Línea de Regresión'),
                                        checkboxInput('smooth', 'Suavizado LOESS'),
                                        
                                        selectInput('facet_row', 'Elige variable para facetas por filas', c(None='.', categoricas)),
                                        downloadButton("descarga", "IMPRIMIR")
                                    ),
                                    
                                    mainPanel(
                                        plotOutput('charts',
                                                   height=580)
                                    )
                            ),
                            
                            tabItem(tabName = "data",
                                    fluidPage(
                                        fluidRow(
                                            column(12,
                                                   DTOutput('table')
                                                   )
                                            )
                                        )
                            ),
                            
                            tabItem(tabName = "link",
                                    fluidPage(
                                        uiOutput("link"),
                                        h2("http://www.oscarrojo.es/")
                                        )
                                    ),
                            
                            tabItem(tabName = "link1",
                                    fluidPage(
                                        uiOutput("link1"),
                                        h2("https://www.linkedin.com/in/oscar-rojo-martin/")
                                    )
                            ),
                            
                            tabItem(tabName = "link2",
                                    fluidPage(
                                        uiOutput("link2"),
                                        h2("https://www.medium.com/@zumaia/")
                                    )
                            ),
                            
                            tabItem(tabName = "database",
                                    fluidPage(
                                      uiOutput("database"),
                                      p(""),
                                      h2("", align = "center"),
                                      p(""),
                                      p(""),
                                      p("Los datos que se han utilizado son los obtenidos directamente de los enlaces abajo indicado. 
                                        Se han realizado una serie de transformaciones y filtros para facilitar su consulta."),
                                      p("Erabili diren datuak behean adierazitako loturetatik zuzenean lortutakoak dira.
                                        Hainbat eraldaketa eta iragazki egin dira kontsulta errazteko. "),
                                      h3("Catalogo de datos/Datu-katalogo:"),
                                      p(""),
                                      p("demografia <- (https://www.donostia.eus/datosabiertos/recursos/demografia-origen/demografianacionalidadbarriockan.csv)"),
                                      p("vehiculos <- (https://www.donostia.eus/datosabiertos/recursos/vehiculos_barrio/vehiculosbarrio.csv)"),
                                      p("tasas <- https://www.donostia.eus/datosabiertos/recursos/tasas_tipo/pfitasastipobarriockan.csv)"),
                                      p("recibos <- (https://www.donostia.eus/datosabiertos/recursos/impuestos_tipo/pfiimpuestostipobarriockan.csv)"),
                                      p("habitantes <- (https://www.donostia.eus/datosabiertos/recursos/habitantes-barrios/habitantesporbarrio.csv)"),
                                      p("renta <- (https://www.donostia.eus/datosabiertos/recursos/eustat_renta/eustatrentabarrio.csv)"),
                                      p("indices_demografia <- (https://www.donostia.eus/datosabiertos/recursos/demografia-indices/demografiaindicesbarriockan.csv)"),
                                      p("paro <- (https://www.donostia.eus/datosabiertos/recursos/eustat_paro/eustatparobarrio.csv, sep=,)"),
                                      p("extranjeros <- (https://www.donostia.eus/datosabiertos/recursos/demografia-extranjeros/demografiaextranjerosbarriockan.csv)"),
                                      p("estudios <- (https://www.donostia.eus/datosabiertos/recursos/demografia-nivelestudios/demografianivelestudiosbarriockan.csv)"),
                                      p("")
                                    )
                            )
                        ),
                        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> OpenData //  Datos públicos del Ayuntamiento de Donostia-San Sebastian  / Donostiako Udalaren datu publikoak </span>\');
      })
     '))
                            
                        
                        )
                    )


server <- function(input, output){
    
    require(ggplot2)
    require(reshape2)
    require(dplyr)
    
    
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        vare[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    runjs("document.getElementById('button1').onclick = function() { 
           window.open('https://www.linkedin.com/in/oscar-rojo-martin/', '_blank');
         };"
    )
    
    url <- a("OSCAR ROJO Homepage", href="http://www.oscarrojo.es/")
    url1 <- a("OSCAR ROJO LinkedIn", href="https://www.linkedin.com/in/oscar-rojo-martin/")
    url2 <- a("OSCAR ROJO Medium", href="https://www.medium.com/@zumaia")
    
    output$Semi_collapsible_sidebar=renderMenu({
        sidebarMenu(
            menuItem("Aurkezpena", tabName = "presentation", icon = icon("presentation")),
            menuItem("Datuak", tabName = "data", icon = icon("table")),
            menuItem("Comparador", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Clustering", icon = icon("th"), tabName = "cluster",
                     badgeLabel = "new",
                     badgeColor = "green"),
            menuItem("K means", icon = icon("th"), tabName = "means",
                     badgeLabel = "wow",
                     badgeColor = "red"),
            menuItem("Scatter", icon = icon("bar-chart-o"), tabName = "charts"),
            menuItem("Source", icon = icon("database"), tabName = "database"),
            menuItem("OSCAR ROJO",
                     menuSubItem("www.oscarrojo.es", tabName = "link", icon = icon("arrow-circle-right")),
                     menuSubItem("zumaia@gmail.com", tabName = "email", icon = icon("envelope")),
                     menuSubItem("linkedin.com/in/oscar-rojo-martin", tabName = "link1", icon = icon("arrow-circle-right")),
                     menuSubItem("medium.com/@zumaia", tabName = "link2", icon = icon("arrow-circle-right"))
                     
                     
                     )
            )
        })
    
    output$link <- renderUI({
        tagList("URL link:", url)
    })
    
    output$link1 <- renderUI({
        tagList("URL link:", url1)
    })
    
    output$link2 <- renderUI({
        tagList("URL link:", url2)
    })
    
    output$profile1 <- renderTable({
        
        p1_sum <- a %>% 
            filter(BARRIO == input$p1) %>% 
            select(BARRIO,
                   MUJERES,
                   HOMBRES,
                   SIN_ESTUDIOS_PREES_SECUND,
                   ED.SECUNDARIA,
                   UNIVERSITARIOS,
                   PROFESIONALES,
                   EXTRANJEROS,
                   EXTR_._._s.total,
                   SITUACION_RENTA,
                   RENTA_FAMILIA,
                   RENTA.39,
                   RENTA40.64,
                   RENTA.65,
                   TASA_PARO,
                   PARO.39,
                   PARO39.64,
                   PARO39.64,
                   TASA_NACIMIENTOS,
                   TASA_ENVEJECIMIENTO,
                   SITUACION_RENTA,
                   RENTA_FAMILIA,
                   AGUA_PERSONA,
                   ingresoIBI_X_PERS,
                   ingresoIAE_X_PERS,
                   VEHICxPERSONA,
                   N_VEHICULOS,
                   N_MOTOS,
                   COCHES.MOTOS,
                   ED.SECUNDARIA_._xBarrio,
                   ED.SECUNDARIA_._s.total,
                   UNIVERSITARIOS_._xBarrio,
                   UNIVERSITARIOS_._s.total,
                   PROFESIONALES_._xBarrio,
                   PROFESIONALES_._s.total
                   )
        return(t(p1_sum))
        
    }, colnames = FALSE, rownames = TRUE, align = 'l')
    
    output$profile2 <- renderTable({
        
        p2_sum <- a %>% 
            filter(BARRIO == input$p2) %>% 
            select(BARRIO,
                   MUJERES,
                   HOMBRES,
                   SIN_ESTUDIOS_PREES_SECUND,
                   ED.SECUNDARIA,
                   UNIVERSITARIOS,
                   PROFESIONALES,
                   EXTRANJEROS,
                   EXTR_._._s.total,
                   SITUACION_RENTA,
                   RENTA_FAMILIA,
                   RENTA.39,
                   RENTA40.64,
                   RENTA.65,
                   TASA_PARO,
                   PARO.39,
                   PARO39.64,
                   PARO39.64,
                   TASA_NACIMIENTOS,
                   TASA_ENVEJECIMIENTO,
                   SITUACION_RENTA,
                   RENTA_FAMILIA,
                   AGUA_PERSONA,
                   ingresoIBI_X_PERS,
                   ingresoIAE_X_PERS,
                   VEHICxPERSONA,
                   N_VEHICULOS,
                   N_MOTOS,
                   COCHES.MOTOS,
                   ED.SECUNDARIA_._xBarrio,
                   ED.SECUNDARIA_._s.total,
                   UNIVERSITARIOS_._xBarrio,
                   UNIVERSITARIOS_._s.total,
                   PROFESIONALES_._xBarrio,
                   PROFESIONALES_._s.total
                   )
        return( t(p2_sum) )
        
    }, colnames = FALSE, rownames = TRUE, align = 'r')
    
    output$compare <- renderPlot({
        BARRIO_A <- input$p1
        BARRIO_B <- input$p2
        
        # Attributes
        att <- melt(data.frame(a[which(a$BARRIO %in% c(BARRIO_A, BARRIO_B)),c(1:73)]), id.vars = c('BARRIO'))
        
        att$value <- as.integer(as.character(att$value))
        
        att$desc <-  ifelse(att$variable %in% c('TASA_PARO'), 
                                  'TASA_PARO',
                                  ifelse(att$variable %in% c('TASA_NACIMIENTOS'),
                                         'TASA_NACIMIENTOS',
                                         ifelse(att$variable %in% c('ED.SECUNDARIA_._xBarrio'),
                                                'ED.SECUNDARIA_._xBarrio',
                                                ifelse(att$variable %in% c('PROFESIONALES_._xBarrio'),
                                                       'PROFESIONALES_._xBarrio',
                                                       ifelse(att$variable %in% c('EXTR_._._s.total'),
                                                              'EXTR_._._s.total', 
                                                              ifelse(att$variable %in% c('MUJERES_._s.total'),
                                                                     'MUJERES_._s.total', 
                                                                     ifelse(att$variable %in% c('HOMBRES_._s.total'),
                                                                            'HOMBRES_._s.total',
                                                                                            'NA')
                                                                                 )
                                                                )
                                                )
                                         )
                                  )
                           )
        att <- att %>% 
            filter(desc != 'NA') %>% 
            group_by(BARRIO, desc) %>% 
            summarise(value = mean(value))
        
        coord_radar <- function (theta = "x", start = 0, direction = 1) 
            {
            theta <- match.arg(theta, c("x", "y"))
            r <- if (theta == "x") 
                "y"
            else "x"
            ggproto("CoordRadar", 
                    CoordPolar, 
                    theta = theta, 
                    r = r, 
                    start = start, 
                    direction = sign(direction),
                    is_linear = function(coord) TRUE)
            }
        
        ggplot(att, 
               aes(x = desc, 
                   y = value,
                   fill = BARRIO,
                   colour = BARRIO,
                   group = BARRIO)) + 
            geom_polygon(alpha = .2) + 
            geom_point() + 
            scale_fill_manual(values = c("#4CC2C4", "hotpink")) +
            scale_colour_manual(values = c("#4CC2C4", "hotpink")) + 
            coord_radar() + 
            theme_minimal() + labs(title = "", subtitle = "") + ylim(0,15) +
            theme( panel.grid.major = element_line(colour = 'grey'), 
                   panel.grid.minor = element_line(colour = 'red'), 
                   plot.background = element_rect(fill='gray98', colour='gray98'),
                   panel.background = element_rect(fill='gray98', colour='gray98'),
                   plot.subtitle = element_text(hjust = .5),
                   legend.position = 'bottom', 
                   legend.direction = 'horizontal', 
                   legend.text.align = .5,
                   legend.title = element_blank(), 
                   legend.text = element_text(size = 14),
                   legend.spacing.x = unit(.25, 'cm'),
                   axis.text.x = element_text(size = 10), 
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(), 
                   strip.text = element_text(size = 10),
                   text = element_text(family = "Andale Mono"),
                   plot.title = element_text(hjust = 0.5, size = 14) )
    } , bg = 'transparent', height = 500 , width = 450)
    
    
    output$distPlot <- renderPlot({
        
        # las casillas de verificación y los números de cúmulos significan que la trama se ha redibujado, por lo que son reactivos
        tocluster <- c(input$RENTA_FAMILIA, input$UNIVERSITARIOS_._xBarrio, input$ingresoIBI_X_PERS, input$COCHES.MOTOS, input$ingresoAGUA_X_PERS, input$TASA_PARO, input$TASA_NACIMIENTOS)
        if (sum(tocluster)==0){ 
            plot(c(0,1,3), c(1,0,2), type="l", xaxt='n', yaxt='n',       #tick shape
                 main="Por favor, elija una o más casillas de verificación de variables",   #reminder
                 xlab="No funciona si no lo haces.",                    #please
                 ylab="Sabes que quieres")                            #joke"RENTA_FAMILIA","UNIVERSITARIOS_._xBarrio", "ingresoIBI_X_PERS", "ingresoAGUA_X_PERS", "COCHES.MOTOS", "TASA_PARO", "TASA_NACIMIENTOS" 
        }else{
            dmat <- dist(stand_a[, tocluster], method = "euclidean")                   # distances 
            fit <- hclust(dmat, method="ward.D")                           # Hierarchical clustering
            plot(fit, main=paste("Cluster Dendrogram. Mean height (i.e. distance):", round(mean(dmat),1)),
                 xlab="Puedes generar cuántos clusters quieras y seleccionar las variables que quieras variables")    # Display dendogram
            rect.hclust(fit, k=input$clusters, border="red")              # red boxes round clusters
        }
    })
    
    
    output$kmeans <- renderPlot(
        height = 560, {
            palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
            par(mar = c(5.1, 4.1, 0, 1))
            plot(selectedData(),
                 col = clusters()$cluster,
                 pch = 20, cex = 6)
            with(selectedData(), text(selectedData(), labels = row.names(selectedData())), pos = 4)
            points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
            legend('bottomright',c("1-AIETE", "2-ALTZA", "3-AMARA BERRI", "4-ANTIGUO", "5-AÑORGA", "6-ATEGORRIETA- ULIA", "7-CENTRO", "8-EGIA", "9-GROS", "10-IBAETA",
                                   "11-IGELDO", "12-INTXAURRONDO", "13-LANDARBASO", "14-LOIOLA", "15-MARTUTENE", 
                                   "16-MIRACRUZ- BIDEBIETA", "17-MIRAMON - ZORROAGA", "18-ZUBIETA"),cex=.8,pch=c(1,2))
    })
    
    output$charts <- renderPlot({
        p <- ggplot(a, 
                    aes_string(x=input$x, y=input$y)) + geom_point(size = 6)
        if (input$color != 'None')
            p <- p + aes_string(color=input$color)
        facets <- paste(input$facet_row, "~ .")
        if (facets != '. ~ .')
            p <- p + facet_grid(facets)
        if (input$lm) 
            p <- p + geom_smooth(method='lm',formula=y~x, na.rm = T)
        if (input$smooth)
            p <- p + geom_smooth(method='loess',formula=y~x, na.rm = T)
        print(p)
        
    })
    
    output$table <- renderDT(a[1:73],
                             filter = "top",
                             options = list(
                                 pageLength = 8,
                                 scrollX = TRUE
                                 )
                             )
    
    
    }




shinyApp(ui, server)

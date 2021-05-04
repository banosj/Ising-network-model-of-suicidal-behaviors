# ______________________________________________________________
# Conductas suicidas en adultos peruanos: un analisis de redes
# Autor: Jonatan Baños-Chaparro, Paul Ynquillay-Lima, 
#        Fernando Lamas Delgado, Fiorella Fuster Guillen
# _________ 2021

# / cargando paquetes / 

library(pacman)
pacman::p_load(haven, bootnet, qgraph, psych, MBESS, mgm)

# / cargando base de datos y ordenandolo /

data <- read_sav("cc_lima.sav")
names(data)
demografico <- data[,1:4] 
paykel <- data[,5:9] 

# _______________________________________________
# / Analisis descripivo de variables demograficas

names(demografico)

# / ordenando variables demograficas

demografico$sexo <- factor(demografico$sexo,
                           levels = c("1", "2"),
                           labels = c("Mujeres",
                                      "Hombres"))

demografico$estado_civil <- factor(demografico$estado_civil,
                                   levels = c("1", "2", "3", "4"),
                                   labels = c("Soltero",
                                              "Casado",
                                              "Divorciado",
                                              "Viudo"))

demografico$situacion_laboral <- factor(demografico$situacion_laboral,
                                        levels = c("1", "2", "3"),
                                        labels = c("Tengo un trabajo fijo",
                                                   "Tengo un trabajo temporal",
                                                   "Estoy desempleado"))

# / sexo
table(demografico$sexo) #frecuencia
por.sex = as.table(table(demografico$sexo))
prop.table(por.sex)*100 #%

# / edad
describe(demografico$edad)#media y desviacion estandar

# / estado civil
table(demografico$estado_civil) #frecuencia
por.estado = as.table(table(demografico$estado_civil))
prop.table(por.estado)*100 #%

# / situacion laboral
table(demografico$situacion_laboral) #frecuencia
por.trabajo = as.table(table(demografico$situacion_laboral))
prop.table(por.trabajo)*100 #%

# _____________________________________
# / analisis descriptivo de los items

describe(paykel)

ci.reliability(paykel,type="omega",conf.level=0.95,
               interval.type="bca",B=1000) #confiabilidad

# _________________________________
# / Analisis de red

# / Estimacion de Modelo Ising (dicotomico)

Model <- estimateNetwork(paykel, 
                         default="IsingFit", 
                         missing = "listwise", 
                         rule = "OR")
Model$graph #matriz

plot(Model, edge.labels=T, palette='pastel', layout = "circle", color=c('#a8e6cf'),
     border.width=1.5)

# / Estimacion de red usando el paquete mgm para la predictibilidad

paykel_types <- c(rep("c", 5))
paykel_levels <- c(rep("2", 5))

mgm_paykel <- mgm(paykel, 
                  type = paykel_types, 
                  level = paykel_levels, 
                  lambdaSel = "EBIC", 
                  lambdaGam = .5, 
                  ruleReg = "AND")

mgm_paykel_predic <- predict(mgm_paykel, paykel)

mgm_paykel_predic$errors #nCC % de predictibilidad

# / Grafico

paykel_pie_errors <- c(mgm_paykel_predic$errors[1:5, 3])
label_e <- c("P1", "P2", "P3", "P4", "P5")
names <- c("La vida no merece la pena", "Desear estar muerto", 
           "Pensar en quitarse la vida", "Considerar quitarse la vida",
           "Intentar quitarse la vida")

plot_paykel_mgm <- qgraph(mgm_paykel$pairwise$wadj,
                          palette='pastel',
                          layout = "spring",
                          pie = paykel_pie_errors,
                          pieColor = c("#737070"),
                          labels = label_e,
                          nodeNames = names,
                          legend.cex = 0.5,
                          color = c('#53b2ed'),
                          edge.color=c("#003399"),
                          border.width=1.5)

# _______________________________________________________________________________
# / Estabilidad de centralidad y Coeficiente de estabilidad de correlacion (CS)

# / Estabilidad de centralidad (fuerza)

boot_np <- bootnet(model, 
                   nBoots=1000,
                   statistics=c("Strength"),
                   type=c("case"),
                   verbose=F)

plot(boot_np, c("Strength")) 
corStability(boot_np) #CS

# / Bootstrapping no parametrico para bordes

boot_np2 <- bootnet(model,
                    nBoots=1000,
                    statistics=c("edge"),
                    type=c("nonparametric"),
                    verbose=F)

plot(boot_np2, c("edge"),labels=T,order="sample")

# / medida de centralidad (fuerza)

centrality(Model) #info
centralityPlot(Model,
               include= c("Strength"),
               orderBy="Strength",
               labels= names)

# ............. :)
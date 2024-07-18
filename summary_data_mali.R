---
title: "Dashboard 3SG"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: scroll
---

<style type="text/css">

.navbar {
  background-color:rgb(0,33,115);
  font-size: 15px;
  border-color:black;
}

.custom {
  background-color:rgb(80,80,80);
  color: white;
  height: 320px;
}

.custom2 { Das
  height: 320px;
  background-color:rgb(230,230,230);
}

.chart-title {  /* chart_title  */
   font-size: 15px;
}

.bgdark {
  background-color: grey;
}

.dossiers {
    position: relative;
    clear: both;
    width : 100% !important;
    max-width: 100%;
    overflow: auto;
    zoom: 80%;
    font-size: 18px;
}

.dossiers th:nth-child(1) {
  width: 250px;
  max-width: 250px;
  word-break: break-all;
  white-space: pre-line;
}

.queries {
    position: relative;
    clear: both;
    width : 100% !important;
    overflow: auto;
    zoom: 80%;
    font-size: 18px;
}

.queries th:nth-child(1) {
  width: 100px;
  max-width: 100px;
  word-break: break-all;
  white-space: pre-line;
}

</style>

```{r setup, include=FALSE}
rm(list = ls())
library(flexdashboard)
library(tidyverse)
library(glue)
library(readxl)
library(openxlsx)
library(stringr)
library(dbplyr)
library(plotly)
library(ggplot2)
library(DT)
library(plotly)
library(reshape2)
library(readxl)
library(gtsummary)
library(data.table)
library(tidyverse)
library(arsenal) 
library(data.table)
library(htmltools)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(webr)
library(readxl)


choise = Sys.Date() - 2

hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}


setwd("/Users/mac/Documents/IPD/RILSNET/Gambia/")
#source("Import_data_chikungunya_tamba_2023.R")
#---restriction au dernier id de la date limite de l'epidemie

gambia  = read.csv2("aggregation_db_2023_2023-12-15_09-58-11.csv")

colsss = c( "#999999", "#CC3333")
#"#149691"
clo2 = c( "#999999", "#CC3333")


gambia$Date.of.Sent = as.Date(gambia$Date.of.Sent)
gambia$Date.of.Collect = as.Date(gambia$Date.of.Collect)
gambia$delay = as.numeric(difftime(gambia$Date.of.Collect, gambia$Date.of.Sent, units = "days"))
gambia$delay_cat = ifelse(gambia$delay <= 1, " 24 hours", ifelse(gambia$delay > 1 & gambia$delay <= 2, " 48 hours", ifelse(gambia$delay > 2 & gambia$delay <= 3, " 72 hours",ifelse(gambia$delay > 3 & gambia$delay <= 7, " Within one week", "Above one week"))))


gambia$Centername = toupper(gambia$Centername)
gambia$Centername[str_detect(gambia$Centername, "BANSANG") & !is.na(gambia$Centername)] =  "Bansang Hospital"
gambia$Centername[str_detect(gambia$Centername, "BRIKAMA") & !is.na(gambia$Centername)] = "Brikama District Hospital"



gambia_all = gambia


gambia =  gambia[gambia$Date.of.Sent == choise & !is.na(gambia$Date.of.Sent ), ]


```

Daily {data-icon="fa-user-plus"}
=============================
Row {data-width=150}
-----------------------------------------------------------------------



### <b><font size=3> Date of sent </b></font>

```{r, fig.height = 2}
valueBox(choise, color = "blueviolet",  icon = "fa-sharp fa-light fa-users-medical")
```

### <b><font size=3> Daily total Consultations from all sites</b></font>

```{r, fig.height = 2}

x = sum(gambia$Total.number.Consul., na.rm = TRUE)
valueBox(x, color = "orange",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily percentage consulations from Bansang general hospital

```{r}

p  = sum(gambia$Total.number.Consul.[gambia$Centername == "Bansang Hospital" & !is.na(gambia$Centername)], na.rm = TRUE)

gauge(round((p/x)*100, 2), min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(50, 100), warning = c(5, 49), danger = c(0, 5)
))
```


### Daily percentage consulations from Brikama district hospital

```{r}

p  = sum(gambia$Total.number.Consul.[gambia$Centername == "Brikama District Hospital" & !is.na(gambia$Centername)], na.rm = TRUE)

gauge(round((p/x)*100, 2), min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(50, 100), warning = c(5, 49), danger = c(0, 5)
))
```



### Daily Suspected Arboviruses

```{r}
x = sum(gambia$Total.Suspected.Arbo, na.rm = TRUE)
valueBox(x, color = "darkgreen",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily Febrile consultations

```{r}
x = sum(gambia$Total.Consult.Febrile, na.rm = TRUE)
valueBox(x, color = "mediumvioletred",  icon = "fa-sharp fa-light fa-users-medical")
```


### Daily Malaria RDT

```{r}
x = sum(gambia$Total.Malaria.RDT, na.rm = TRUE)
valueBox(x, color = "turquoise",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily positive Malaria cases

```{r}

p  = sum(gambia$Total.Confirm.Malaria, na.rm = TRUE)

gauge(round((p/x)*100, 2), min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(0, 25), warning = c(25, 50), danger = c(50,100 )
))
```


### Daily Covid-SARI cases

```{r}
x = sum(gambia$Total.SARI.COVID, na.rm = TRUE)
valueBox(x, color = "rgb(244,0,78)",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily Diarrhea cases
```{r}
x = sum(gambia$Total.Diarrhea, na.rm = TRUE)
valueBox(x, color = "lawngreen",  icon = "fa-sharp fa-light fa-users-medical")
```

Row {data-width=400}
-----------------------------------------------------------------------

### Distribution of patients by sites

```{r}
gambia$Centername = factor(gambia$Centername)
h5 = gambia %>%
  filter(Centername %in% c(levels(gambia$Centername))) %>%
  group_by(Centername) %>%
  group_by(count = Total.number.Consul.)

# p5=plot_ly(h5) %>%
#   add_pie(labels=h5$Centername,values=h5$count,hole=0.6)
# p5
```



### Distribution of patients per age group (year)

```{r}
x = str_detect(names(gambia), "All")
y =  NULL
for (i in names(gambia)[x]) {
  y = c(y, sum(gambia[,i]))
}
agegroup = c(" Below 1", "01-04", "05-14", "15-49", "50-64", "65-100" )
ta = data.frame(age = agegroup, value = y)
names(ta) = c("Age group (year)",  "value")

p=ggplot(data=ta, aes(x=`Age group (year)`, y=value, fill=`Age group (year)`)) +
   geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE) + guides(fill=guide_legend(title="")) +
  scale_fill_manual(values=c(rep("aquamarine4",nrow(ta)))) +
  theme_minimal() + theme(legend.position = "none")
ggplotly(p)
```
 

Overview {data-icon="fa-user-plus"}
=============================
Row {data-width=150}
-----------------------------------------------------------------------



<!-- ### <b><font size=3> Date of sent </b></font> -->

<!-- ```{r, fig.height = 2} -->


<!-- valueBox(choise, color = "orange",  icon = "fa-sharp fa-light fa-users-medical") -->
<!-- ``` -->

### <b><font size=3> Daily total Consultations from all sites</b></font>

```{r, fig.height = 2}
gambia = gambia_all
x = sum(gambia$Total.number.Consul., na.rm = TRUE)
valueBox(x, color = "orange",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily percentage consulations from Bansang general hospital

```{r}

p  = sum(gambia$Total.number.Consul.[gambia$Centername == "Bansang Hospital" & !is.na(gambia$Centername)], na.rm = TRUE)

gauge(round((p/x)*100, 2), min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(50, 100), warning = c(5, 49), danger = c(0, 5)
))
```


### Daily percentage consulations from Brikama district hospital

```{r}

p  = sum(gambia$Total.number.Consul.[gambia$Centername == "Brikama District Hospital" & !is.na(gambia$Centername)], na.rm = TRUE)

gauge(round((p/x)*100, 2), min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(50, 100), warning = c(5, 49), danger = c(0, 5)
))
```



### Daily Suspected Arboviruses

```{r}
x = sum(gambia$Total.Suspected.Arbo, na.rm = TRUE)
valueBox(x, color = "darkgreen",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily Febrile consultations

```{r}
x = sum(gambia$Total.Consult.Febrile, na.rm = TRUE)
valueBox(x, color = "mediumvioletred",  icon = "fa-sharp fa-light fa-users-medical")
```


### Daily Malaria RDT

```{r}
x = sum(gambia$Total.Malaria.RDT, na.rm = TRUE)
valueBox(x, color = "turquoise",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily positive Malaria cases

```{r}

p  = sum(gambia$Total.Confirm.Malaria, na.rm = TRUE)

gauge(round((p/x)*100, 2), min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(0, 25), warning = c(25, 50), danger = c(50,100 )
))
```


### Daily Covid-SARI cases

```{r}
x = sum(gambia$Total.SARI.COVID, na.rm = TRUE)
valueBox(x, color = "rgb(244,0,78)",  icon = "fa-sharp fa-light fa-users-medical")
```

### Daily Diarrhea cases
```{r}
x = sum(gambia$Total.Diarrhea, na.rm = TRUE)
valueBox(x, color = "lawngreen",  icon = "fa-sharp fa-light fa-users-medical")
```

Row {data-width=150}
-----------------------------------------------------------------------

### Distribution of patients by sites

```{r}
gambia$Centername = factor(gambia$Centername)
h5 = gambia %>%
  filter(Centername %in% c(levels(gambia$Centername))) %>%
  group_by(Centername) %>%
  group_by(count = Total.number.Consul.)

p5=plot_ly(h5) %>%
  add_pie(labels=h5$Centername,values=h5$count,hole=0.6)
p5
```



### Distribution of patients per age group (year)

```{r}
x = str_detect(names(gambia), "All")
y =  NULL
for (i in names(gambia)[x]) {
  y = c(y, sum(gambia[,i], na.rm = TRUE))
}
agegroup = c(" Below 1", "01-04", "05-14", "15-49", "50-64", "65-100" )
ta = data.frame(age = agegroup, value = y)
names(ta) = c("Age group (year)",  "value")

p=ggplot(data=ta, aes(x=`Age group (year)`, y=value, fill=`Age group (year)`)) +
   geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE) + guides(fill=guide_legend(title="")) +
  scale_fill_manual(values=c(rep("aquamarine4",nrow(ta)))) +
  theme_minimal() + theme(legend.position = "none")
ggplotly(p)
```

Row {data-width=150}
-----------------------------------------------------------------------

### Timeliness of samples collection and sending

```{r}
tab = table(gambia$delay_cat)
ta = data.frame(tab)
names(ta) = c("Time",  "value")

p=ggplot(data=ta, aes(x=`Time`, y=value, fill=`Time`)) +
   geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE) + guides(fill=guide_legend(title="")) +
  scale_fill_manual(values=c(rep("navy",nrow(ta)))) +
  theme_minimal() + theme(legend.position = "none")
if(sum(ta$value, na.rm =  TRUE) != 0 ){ggplotly(p)}else{print("No case")}

```


### Distribution of SARI or Covid-19 cases per age group (year)


```{r}
x = str_detect(names(gambia), "Covid.SARI")
y =  NULL
for (i in names(gambia)[x]) {
  y = c(y, sum(gambia[,i], na.rm = TRUE))
}
agegroup = c(" Below 1", "01-04", "05-14", "15-49", "50-64", "65-100" )
ta = data.frame(age = agegroup, value = y)
names(ta) = c("Age group (year)",  "value")

p=ggplot(data=ta, aes(x=`Age group (year)`, y=value, fill=`Age group (year)`)) +
   geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE) + guides(fill=guide_legend(title="")) +
  scale_fill_manual(values=c(rep("magenta",nrow(ta)))) +
  theme_minimal() + theme(legend.position = "none")
if(sum(ta$value, na.rm =  TRUE) != 0 ){ggplotly(p)}else{print("No case")}

```


Epidemiological data  {data-icon="fa-user-plus"}
=============================
Row {data-width=400}
-----------------------------------------------------------------------

### Distribution of Consultations


```{r}
demografic = gambia_all
Nbconsul  = sum(demografic$Total.number.Consul., na.rm = TRUE)
Nbfebriles  = sum(demografic$Total.Consult.Febrile, na.rm = TRUE)
Nbgrippes  = sum(demografic$Covid.SARI_0.1y,demografic$Covid.SARI_1.4y, demografic$Covid.SARI_5.14y, demografic$Covid.SARI_15.49y, demografic$Covid.SARI_50.64y, demografic$Covid.SARI_65y. , na.rm = TRUE)
Nbdiarrhees = sum(demografic$Total.Diarrhea, na.rm = TRUE)
NbdiarrheesFeb = sum(demografic$Total.Diarrhea.Feb, na.rm = TRUE)
NbdiarrheesSang = sum(demografic$Total.Diarrhea.blood, na.rm = TRUE)
Nbarbo = sum(demografic$Total.Suspected.Arbo, na.rm = TRUE)
Nbtdrpalu = sum(demografic$Total.Malaria.RDT, na.rm = TRUE)
NbtdrpaluCONFIRME = sum(demografic$Total.Malaria.RDT, na.rm = TRUE)


fievres_febriles = round((Nbfebriles/Nbconsul)*100, 2)
diarrrr = round((Nbdiarrhees/Nbconsul)*100, 2)
autreconsul = 100 - sum(fievres_febriles, diarrrr)

data <- data.frame("Consultations" = c('Febrile syndromes', 'Diarrhea', "Other"), "amount" = c(fievres_febriles, diarrrr, autreconsul))


ggplot(data, aes(x="", y=amount, fill=Consultations)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, title = "Distribution of Consultations") +
  theme_classic() +
  theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")


```

### Distribution of febrile syndromes

```{r}

grippecovid = round((Nbgrippes/Nbfebriles)*100, 2)
Arboviroves = round((Nbarbo/Nbfebriles)*100, 2)
diarrhee = round((NbdiarrheesFeb/Nbfebriles)*100, 2)
autre = 100 - sum(grippecovid, Arboviroves, diarrhee)

data <- data.frame("Febrile_syndromes" = c('Flu_covid', 'Arboviruves', 'Febrile diarrhea', "Other"), "amount" = c(grippecovid, Arboviroves , diarrhee, autre))

ggplot(data, aes(x = "", y = amount, fill = Febrile_syndromes)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, title = "Distribution of febrile syndromes") +
  theme_classic() +
  theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")

```

Row {data-width=400}
-----------------------------------------------------------------------

###  Distribution of diarrhoea

```{r}

diarrheesanglantes = round((NbdiarrheesSang/Nbdiarrhees)*100, 2)
diarrheeNonsanglantes = round(((Nbdiarrhees - NbdiarrheesSang)/Nbdiarrhees)*100, 2)


data <- data.frame("Diarrhea" = c('Bloody', 'Bloodless'), "amount" = c(diarrheesanglantes, diarrheeNonsanglantes))

ggplot(data, aes(x="", y=amount, fill=Diarrhea)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, title = "Distribution of diarrhoea") +
  theme_classic() +
  theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")

```

### Malaria RDT results

```{r}

data <- data.frame("TDR_malaria" = c('Confirmed', 'Unconfirmed'), "amount" = c(round((NbtdrpaluCONFIRME/Nbtdrpalu)*100, 2), round(((Nbtdrpalu - NbtdrpaluCONFIRME)/Nbtdrpalu)*100, 2)))

ggplot(data, aes(x="", y=amount, fill=TDR_malaria)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, title = "Malaria RDT results") +
  theme_classic() +
  theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")

```

Cumulative consultations  {data-icon="fa-user-plus"}
=============================
Row {data-width=400}
-----------------------------------------------------------------------

### Cumulative consultations

```{r, fig.height = 2}

gambia_all = gambia_all[gambia_all$Total.number.Consul. <= 400, ]
tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.number.Consul.)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.number.Consul.)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```


Row {data-width=400}
-----------------------------------------------------------------------


### Cumulative Febrile Consultations

```{r, fig.height = 2}


tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.Consult.Febrile)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.Consult.Febrile)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```







Cumulative suspected cases {data-icon="fa-user-plus"}
=============================
Row {data-width=400}
-----------------------------------------------------------------------
### Evolution of Suspected Arboviruses 

```{r, fig.height = 2}


tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.Suspected.Arbo)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.Suspected.Arbo)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```



Row {data-width=400}
-----------------------------------------------------------------------

### Cumulative Malaria RDT

```{r, fig.height = 2}


tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.Malaria.RDT)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.Malaria.RDT)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```


Cumulative Cases {data-icon="fa-user-plus"}
=============================
Row {data-width=400}
-----------------------------------------------------------------------

### Evolution of Diarrhea cases

```{r, fig.height = 2}


tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.Diarrhea)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.Diarrhea)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```


Row {data-width=400}
-----------------------------------------------------------------------

### Evolution of Malaria cases

```{r, fig.height = 2}


tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.Confirm.Malaria)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.Confirm.Malaria)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```


Diarrhea cases {data-icon="fa-user-plus"}
=============================
Row {data-width=400}
-----------------------------------------------------------------------

### Evolution of Febrile Diarrhea cases

```{r, fig.height = 2}


tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.Diarrhea.Feb)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.Diarrhea.Feb)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```


Row {data-width=400}
-----------------------------------------------------------------------

### Evolution of blood Diarrhea cases

```{r, fig.height = 2}


tab =gambia_all[gambia_all$Centername == "Bansang Hospital",]
h1 = data.frame(Month = tab$Date.of.Sent, count = tab$Total.Diarrhea.blood)



tab2 = gambia_all[gambia_all$Centername == "Brikama District Hospital", ]
h2 = data.frame(Month = tab2$Date.of.Sent, count =  tab2$Total.Diarrhea.blood)



p1 = plot_ly(data = h1, x=h1$Month, y=h1$count, name = "Bansang Hospital") %>% 
     layout(yaxis = list(range = c(0,(max(h1$count, h2$count)+5)))) %>% 
     add_lines(linetype = h1$`Type of movement`, data = h1$count) %>% 
     add_lines(x=h2$Month, y=h2$count, name = "Brikama District Hospital")
p1
```




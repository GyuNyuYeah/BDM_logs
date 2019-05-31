#Magic word!
options(jupyter.plot_mimetypes = 'image/png')
#Load library
targetPackages <- c(
  "ggplot2",
  "extrafont",
  "RColorBrewer",
  "knitr",
  "data.table",
  "GGally",
  "mclust",
  "mixtools",
  "dplyr",
  "latticeExtra",
  "devtools",
  "IRdisplay",
  "repr",
  "dtplyr",
  "ggrepel",
  "mixtools",
  "RMySQL",
  "rmarkdown",
  "reshape",
  "reshape2",
  "plotly",
  "htmlwidgets",
  "formatR",
  "webshot",
  "gridExtra",
  "TTR",
  "lubridate"
) 
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) {
  install.packages(newPackages, repos = "http://cran.us.r-project.org")
}
for(package in targetPackages) {
  suppressMessages(library(package, character.only = T))
}
## For 1st time only
# webshot::install_phantomjs()
# devtools::install_github("IRkernel/IRkernel")
# font_import()
# loadfonts()
rm(package, newPackages, targetPackages)

#Plot color
colfunc<-colorRampPalette(
  c(
    "red",
    "yellow",
    "springgreen",
    "royalblue"
  )
)
# Define Export plot size
p.width <- 900
p.height <- 400
g.width <- 18
g.height <- 8
g.scale <- 1.2

dt.cp <- fread("./CP.csv")
dt.cp$Date <- as.Date(dt.cp$Date)
dt.cp <- as.data.table(dt.cp)
dt.cp <- dt.cp[Date > "2019-05-01"]

color.pal <- brewer.pal(9,"Spectral")
p.title <- paste("GyuNyuYeah, Combat point & payments log (", now(), ")", sep = "")
p1 <- plot_ly()
p1 <- layout(p1, autosize = TRUE, barmode = "stack", hovermode = "x", font = list(family = "Meiryo UI"),
         xaxis = list(autorange = TRUE, type = "date"),
         yaxis = list(autorange = TRUE, type = "linear")
         ) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Growth_bonus, hoverinfo = "y+name",name = "�������T", type = "bar",
            marker = list(color = color.pal[9],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Shine_stone, hoverinfo = "y+name",name = "������", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Magic_stamps, hoverinfo = "y+name",name = "���͍���", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Crystal, hoverinfo = "y+name",name = "����", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Artifact, hoverinfo = "y+name",name = "�╨", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Accessories, hoverinfo = "y+name",name = "�A�N�Z", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Equipments, hoverinfo = "y+name",name = "����", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Costume, hoverinfo = "y+name",name = "�ߑ�", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Skill_training, hoverinfo = "y+name",name = "�X�L���C��Lv", type = "bar",
            marker = list(color = color.pal[7],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Character_Lv, hoverinfo = "y+name", name = "�L����Lv", type = "bar",
            marker = list(color = color.pal[6],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Picture_Book, hoverinfo = "y+name", name = "�}��", type = "bar",
            marker = list(color = color.pal[5],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Knowledge_Boss_Lv_reward, hoverinfo = "y+name",name = "�m��(�{�XLv��V)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Knowledge_Boss, hoverinfo = "y+name",name = "�m��(�{�X)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Knowledge_General, hoverinfo = "y+name",name = "�m��(���)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Sustatined_Skill, hoverinfo = "y+name", name = "�����X�L��", type = "bar",
            marker = list(color = color.pal[3],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Dark_Spirit, hoverinfo = "y+name", name = "�ł̐���", type = "bar",
            marker = list(color = color.pal[2],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Total_CP, hoverinfo = "x+y+text+name", mode = "markers", name = "�퓬��", text = dt.cp$Comment, type = "scatter",
            marker = list(color = "rgb(246, 0, 0)", size = 10, symbol = "star"))
# p1
p2 <- plot_ly()
p2 <- layout(p2, autosize = TRUE, barmode = "stack", hovermode = "x", title = p.title, font = list(family = "Meiryo UI"),
         xaxis = list(autorange = TRUE, type = "date"),
         yaxis = list(autorange = TRUE, type = "linear")
  ) %>%
  add_trace(p2, x = dt.cp$Date, y = dt.cp$Total_payment, hoverinfo = "y+name",name = "�ۋ��z", mode = "markers+lines", fill = 'tozeroy')
# p2
p <- subplot(p1, p2, nrows = 2, shareX = TRUE)
p
setwd("./docs")
saveWidget(p, "Index.html")
setwd("../")
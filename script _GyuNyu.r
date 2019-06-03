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

dt.cp <- fread("./CP_GyuNyu.csv")
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
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Growth_bonus, hoverinfo = "y+name",name = "成長特典", type = "bar",
            marker = list(color = color.pal[9],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Shine_stone, hoverinfo = "y+name",name = "光原石", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Magic_stamps, hoverinfo = "y+name",name = "魔力刻印", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Crystal, hoverinfo = "y+name",name = "水晶", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Artifact, hoverinfo = "y+name",name = "遺物", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Accessories, hoverinfo = "y+name",name = "アクセ", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Equipments, hoverinfo = "y+name",name = "装備", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Costume, hoverinfo = "y+name",name = "衣装", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Skill_training, hoverinfo = "y+name",name = "スキル修練Lv", type = "bar",
            marker = list(color = color.pal[7],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Character_Lv, hoverinfo = "y+name", name = "キャラLv", type = "bar",
            marker = list(color = color.pal[6],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Picture_Book, hoverinfo = "y+name", name = "図鑑", type = "bar",
            marker = list(color = color.pal[5],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Knowledge_Boss_Lv_reward, hoverinfo = "y+name",name = "知識(ボスLv報酬)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Knowledge_Boss, hoverinfo = "y+name",name = "知識(ボス)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Knowledge_General, hoverinfo = "y+name",name = "知識(一般)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Sustatined_Skill, hoverinfo = "y+name", name = "持続スキル", type = "bar",
            marker = list(color = color.pal[3],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Dark_Spirit, hoverinfo = "y+name", name = "闇の精霊", type = "bar",
            marker = list(color = color.pal[2],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Date, y = dt.cp$Total_CP, hoverinfo = "x+y+text+name", mode = "markers", name = "戦闘力", text = dt.cp$Comment, type = "scatter",
            marker = list(color = "rgb(246, 0, 0)", size = 10, symbol = "star"))
# p1
p2 <- plot_ly()
p2 <- layout(p2, autosize = TRUE, barmode = "stack", hovermode = "x", title = p.title, font = list(family = "Meiryo UI"),
         xaxis = list(autorange = TRUE, type = "date"),
         yaxis = list(autorange = TRUE, type = "linear")
  ) %>%
  add_trace(p2, x = dt.cp$Date, y = dt.cp$Total_payment, hoverinfo = "y+name",name = "課金額", mode = "markers+lines", fill = 'tozeroy')
# p2
p <- subplot(p1, p2, nrows = 2, shareX = TRUE)
p
setwd("./docs")
saveWidget(p, "Index.html")
setwd("../")


color.pal <- brewer.pal(9,"Spectral")
p.title <- paste("GyuNyuYeah, Combat point & payments log (", now(), ")", sep = "")
p1 <- plot_ly()
p1 <- layout(p1, autosize = TRUE, barmode = "stack", hovermode = "x", font = list(family = "Meiryo UI"),
             xaxis = list(autorange = TRUE, type = "linear"),
             yaxis = list(autorange = TRUE, type = "linear")
) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Growth_bonus, hoverinfo = "y+name",name = "成長特典", type = "bar",
            marker = list(color = color.pal[9],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Shine_stone, hoverinfo = "y+name",name = "光原石", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Magic_stamps, hoverinfo = "y+name",name = "魔力刻印", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Crystal, hoverinfo = "y+name",name = "水晶", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Artifact, hoverinfo = "y+name",name = "遺物", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Accessories, hoverinfo = "y+name",name = "アクセ", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Equipments, hoverinfo = "y+name",name = "装備", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Costume, hoverinfo = "y+name",name = "衣装", type = "bar",
            marker = list(color = color.pal[8],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Skill_training, hoverinfo = "y+name",name = "スキル修練Lv", type = "bar",
            marker = list(color = color.pal[7],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Character_Lv, hoverinfo = "y+name", name = "キャラLv", type = "bar",
            marker = list(color = color.pal[6],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Picture_Book, hoverinfo = "y+name", name = "図鑑", type = "bar",
            marker = list(color = color.pal[5],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Knowledge_Boss_Lv_reward, hoverinfo = "y+name",name = "知識(ボスLv報酬)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Knowledge_Boss, hoverinfo = "y+name",name = "知識(ボス)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Knowledge_General, hoverinfo = "y+name",name = "知識(一般)", type = "bar",
            marker = list(color = color.pal[4],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Sustatined_Skill, hoverinfo = "y+name", name = "持続スキル", type = "bar",
            marker = list(color = color.pal[3],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Dark_Spirit, hoverinfo = "y+name", name = "闇の精霊", type = "bar",
            marker = list(color = color.pal[2],
                          line = list(color = "Black",width = 1))) %>%
  add_trace(p1, x = dt.cp$Total_CP, y = dt.cp$Total_CP, hoverinfo = "x+y+text+name", mode = "markers", name = "戦闘力", text = dt.cp$Comment, type = "scatter",
            marker = list(color = "rgb(246, 0, 0)", size = 10, symbol = "star"))
p1

dt.cp.temp <- dt.cp[, Total_CP.trunc:=trunc(Total_CP/10)*10]
dt.cp.trunc <- dt.cp.temp %>%
  group_by(Total_CP.trunc) %>%
  summarize(Dark_Spirit.mean = mean(Dark_Spirit),
           Sustatined_skill.mean = mean(Sustatined_Skill),
           Knowledge_General.mean = mean(Knowledge_General),
           Knowledge_Boss.mean = mean(Knowledge_Boss),
           Knowledge_Boss_Lv_reward.mean = mean(Knowledge_Boss_Lv_reward),
           Picture_Book.mean = mean(Picture_Book),
           Character_Lv.mean = mean(Character_Lv)
           )

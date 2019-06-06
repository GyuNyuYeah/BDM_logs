s <- proc.time()

#Magic word!
options(jupyter.plot_mimetypes = 'image/png')
#Load library
targetPackages <- c(
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
  "lubridate",
  "ggplot2",
  "gtable"
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

my_theme_grey <- function(
  base_size = 10.5,
  base_family = "Segoe UI") {
  theme_grey(
    base_size = base_size,
    base_family = base_family
  ) %+replace% theme()
}
theme_set(my_theme_grey())


# Define Export plot size
p.width <- 900
p.height <- 400
g.width <- 18
g.height <- 8
g.scale <- 1.2



dt.cp <- fread("./CP.csv")
dt.cp$Date <- as.Date(dt.cp$Date)
dt.cp <- as.data.table(dt.cp)
ItemList.en <- colnames(dt.cp)[4:20]
ItemList.ja <- c("闇の精霊",
                 "持続スキル",
                 "知識(一般)",
                 "知識(ボス)",
                 "知識(ボスLv報酬)",
                 "ペット収集",
                 "図鑑",
                 "キャラLv",
                 "スキル修練Lv",
                 "衣装",
                 "装備",
                 "アクセ",
                 "遺物",
                 "水晶",
                 "魔力刻印",
                 "光原石",
                 "成長ボーナス")
ItemList <- data.table(en = ItemList.en,
                       ja = ItemList.ja)

dt.cp.temp <- dt.cp[, Total_CP.trunc:=trunc(Total_CP/100)*100]
dt.cp.trunc.user <- dt.cp.temp %>%
  group_by(Player,Total_CP.trunc) %>%
  summarize(mean(get(unname(unlist(ItemList[1,"en"])))),
            mean(get(unname(unlist(ItemList[2,"en"])))),
            mean(get(unname(unlist(ItemList[3,"en"])))),
            mean(get(unname(unlist(ItemList[4,"en"])))),
            mean(get(unname(unlist(ItemList[5,"en"])))),
            mean(get(unname(unlist(ItemList[6,"en"])))),
            mean(get(unname(unlist(ItemList[7,"en"])))),
            mean(get(unname(unlist(ItemList[8,"en"])))),
            mean(get(unname(unlist(ItemList[9,"en"])))),
            mean(get(unname(unlist(ItemList[10,"en"])))),
            mean(get(unname(unlist(ItemList[11,"en"])))),
            mean(get(unname(unlist(ItemList[12,"en"])))),
            mean(get(unname(unlist(ItemList[13,"en"])))),
            mean(get(unname(unlist(ItemList[14,"en"])))),
            mean(get(unname(unlist(ItemList[15,"en"])))),
            mean(get(unname(unlist(ItemList[16,"en"])))),
            mean(get(unname(unlist(ItemList[17,"en"]))))
  )
setnames(dt.cp.trunc.user, c("Player","Total_CP.trunc", ItemList$ja))
dt.cp.trunc.user <- setorder(dt.cp.trunc.user, Total_CP.trunc)
rm(dt.cp.temp)

dt.cp.mean <- dt.cp.trunc.user %>%
  group_by(Total_CP.trunc) %>%
  summarize(length(Player),
            mean(get(unname(unlist(ItemList[1,"ja"])))),
            mean(get(unname(unlist(ItemList[2,"ja"])))),
            mean(get(unname(unlist(ItemList[3,"ja"])))),
            mean(get(unname(unlist(ItemList[4,"ja"])))),
            mean(get(unname(unlist(ItemList[5,"ja"])))),
            mean(get(unname(unlist(ItemList[6,"ja"])))),
            mean(get(unname(unlist(ItemList[7,"ja"])))),
            mean(get(unname(unlist(ItemList[8,"ja"])))),
            mean(get(unname(unlist(ItemList[9,"ja"])))),
            mean(get(unname(unlist(ItemList[10,"ja"])))),
            mean(get(unname(unlist(ItemList[11,"ja"])))),
            mean(get(unname(unlist(ItemList[12,"ja"])))),
            mean(get(unname(unlist(ItemList[13,"ja"])))),
            mean(get(unname(unlist(ItemList[14,"ja"])))),
            mean(get(unname(unlist(ItemList[15,"ja"])))),
            mean(get(unname(unlist(ItemList[16,"ja"])))),
            mean(get(unname(unlist(ItemList[17,"ja"]))))
  )
setnames(dt.cp.mean, c("DataName", "Player.count", ItemList$ja))
dt.cp.mean <- setorder(dt.cp.mean, Total_CP)
rm(dt.cp.trunc.user)

t <- dt.cp.mean
for (i in 1:nrow(t)) {
  for (j in 1:nrow(ItemList)) {
    temp1 <- data.table(DataName = paste(t[i]$DataName, " Avg.\n(n=", t[i]$Player.count, ")", sep = ""),
                        Item = ItemList[j,"ja"],
                        Value = t[i,get(unname(unlist(ItemList[j,"ja"])))])
    if (j == 1) {
      temp2 <- temp1
    } else {
      temp2 <- rbind(temp2,temp1)
    }
  }
  if (i == 1) {
    dt.cp.mean.ByItem <- temp2
  } else {
    dt.cp.mean.ByItem <- rbind(dt.cp.mean.ByItem, temp2)
  }
}

if (dir.exists("./Plot") == FALSE) {
  dir.create("./Plot")
}
setwd("./Plot")

player.list <- c("牛乳家", "ホトトギス","銀さんのミット","Zacky", "Libby")

for (i in 1:length(player.list)) {
  crtPlayer <- player.list[i]
  crtDate <- max(dt.cp[Player== crtPlayer]$Date)
  t <- dt.cp[Player == crtPlayer & Date == max(dt.cp[Player== crtPlayer]$Date)]
  for (i in 1:nrow(t)) {
    for (j in 1:nrow(ItemList)) {
      temp1 <- data.table(DataName = paste(t[i]$Player,"\n(",t[i]$Date, ")", sep=""),
                          Item = ItemList[j,"ja"],
                          Value = t[i,get(unname(unlist(ItemList[j,"en"])))])
      if (j == 1) {
        temp2 <- temp1
        } else {
          temp2 <- rbind(temp2,temp1)
        }
      }
    if (i == 1) {
      dt.cp.ByItem <- temp2
      } else {
        dt.cp.ByItem <- rbind(dt.cp.ByItem, temp2)
      }
    }
  rm(temp1,temp2)
  t <- rbind(dt.cp.ByItem, dt.cp.mean.ByItem)
  t$Item.ja <- factor(t$Item.ja, levels = ItemList.ja)
  FileName <- paste(crtPlayer, " (", crtDate, ")", sep="")
  txtAnnotate <- paste("Player Name: ",crtPlayer,", Date: ",crtDate, "\n", sep ="")
  txtAnnotate <- paste(txtAnnotate, "Date aqcuisition: From ", min(dt.cp$Date), " to ", max(dt.cp$Date), "\n", sep ="")
  txtAnnotate <- paste(txtAnnotate, "Total data count: ", nrow(dt.cp), " (provided by ",nlevels(as.factor(dt.cp$Player)) , " players)\n\n", sep ="")
  txtAnnotate <- paste(txtAnnotate, "Created by: @GyuNyuYeah_BDM (", now(), ")", sep ="")
  g <- ggplot(t, aes(x = DataName, y = Value, fill = DataName)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Value,1)), size = 3) +
    facet_wrap(~ Item.ja, scales = "free") +
    ggtitle(FileName) +
    theme(title=element_text(size=16,face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = 'none')
  gp <- ggplotGrob(g)
  empty.area <- gtable_filter(gp, "panel", trim = F)
  empty.area <- empty.area$layout[sapply(empty.area$grob,
                                         function(x){class(x)[[1]]=="zeroGrob"}),]
  empty.area$t <- empty.area$t - 1 #extend up by 1 cell to cover facet header
  empty.area$b <- empty.area$b + 1 #extend down by 1 cell to cover x-axis
  gp0 <- gtable_add_grob(x = gp,
                         grobs = tableGrob(txtAnnotate,
                                           theme = ttheme_minimal(base_family = "Segoe Condensed",
                                                                  base_size = 14)),
                         t = min(empty.area$t), #16 in this case
                         l = min(empty.area$l), #8
                         b = max(empty.area$b), #18
                         r = max(empty.area$r), #12
                         name = "textbox")
  # grid::grid.draw(gp0)
  ggsave(plot = grid::grid.draw(gp0),
         paste(FileName,".png", sep = ""),
         width = g.width,
         height = g.height*1.4,
         unit = "cm",
         dpi = 600,
         scale = 2.5,
         limitsize = F)
  }
setwd("../")
print(proc.time()-s)
rm(list = ls(all =T))
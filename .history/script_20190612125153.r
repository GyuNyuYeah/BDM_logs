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
  "gtable",
  "corrplot",
  "ggExtra"
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
  base_size = 12,
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
dt.cp.mean <- setorder(dt.cp.mean, DataName)

t <- dt.cp.mean
for (i in 1:nrow(t)) {
  for (j in 1:nrow(ItemList)) {
    temp1 <- data.table(DataName = paste(t[i]$DataName, "\n", t[i]$Player.count, " player(s)", sep = ""),
                        Item = ItemList[j,"ja"],
                        Value = t[i,get(unname(unlist(ItemList[j,"ja"])))],
                        data.type = 0.5)
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
rm(t, temp1, temp2)
t <- dt.cp[,4:20, with =F]
for(i in 1:ncol(t)) {
  temp <- data.table(DataName = "Maximum value \nof acquired data",
                     Item = ItemList[i,"ja"],
                     Value = max(t[,i, with = F]),
                     data.type = 0.5)
  if (i == 1) {
    dt.cp.max.ByItem <- temp
  } else {
    dt.cp.max.ByItem <- rbind(dt.cp.max.ByItem, temp)
  }
}

dt.cp.mean.ByItem <- rbind(dt.cp.mean.ByItem, dt.cp.max.ByItem)

if (dir.exists("./Plot") == FALSE) {
  dir.create("./Plot")
}
setwd("./Plot")

#追加分だけ
player.list <- tail(dt.cp$Player,4)

#全員
# player.list <- as.character(levels(as.factor(dt.cp$Player)))
# player.list <- "牛乳家"
for (i in 1:length(player.list)) {
  crtPlayer <- player.list[i]
  crtDate <- max(dt.cp[Player== crtPlayer]$Date)
  crtDate <- tail(dt.cp[Player== crtPlayer]$Date,1)
  t <- dt.cp[Player == crtPlayer & Date %in% crtDate]
  for (i in 1:nrow(t)) {
    for (j in 1:nrow(ItemList)) {
      temp1 <- data.table(DataName = paste(t[i]$Total_CP,"\n",t[i]$Player, " ",t[i]$Date, sep=""),
                          Item = ItemList[j,"ja"],
                          Value = t[i,get(unname(unlist(ItemList[j,"en"])))],
                          data.type = 1)
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
  FileName <- paste(crtPlayer, " (", tail(crtDate,1), ")", sep="")
  txtAnnotate <- paste("Player Name: ",crtPlayer, "\n", sep ="")
  txtAnnotate <- paste(txtAnnotate, "Date acquisition: From ", min(dt.cp$Date), " to ", max(dt.cp$Date), "\n", sep ="")
  txtAnnotate <- paste(txtAnnotate, "Total data count: ", nrow(dt.cp), " (provided by ",nlevels(as.factor(dt.cp$Player)) , " players)\n\n", sep ="")
  txtAnnotate <- paste(txtAnnotate, "Created by: @GyuNyuYeah_BDM (", now(), ")", sep ="")

  g <- ggplot(t, aes(x = DataName, y = Value, fill = DataName, color = DataName)) +
    geom_bar(stat = "identity",aes(alpha = data.type)) +
    geom_text(aes(label = round(Value,1)), size = 2.5, color = "Black") +
    facet_wrap(~ Item.ja, scales = "free") +
    ggtitle(FileName) +
    scale_alpha(range = c(0.3, 1)) +
    theme(title=element_text(size=20,face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
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
                                                                  base_size = 16)),
                         t = min(empty.area$t), #16 in this case
                         l = min(empty.area$l), #8
                         b = max(empty.area$b), #18
                         r = max(empty.area$r), #12
                         name = "textbox")
  png(paste(FileName,".png", sep = ""),
      width = 6000,
      height = 3500,
      res = 250
      )
  grid::grid.draw(gp0)
  dev.off()
}
t <- dt.cp[,3:20, with = F]
setnames(t,c("戦闘力",ItemList.ja))
s <- cor(t)
s <- data.table(s)
s <- s[,1]
s <- s[,Item:=c("戦闘力",ItemList.ja)]
setnames(s,c("cor", "Item"))
s <- s[Item != "戦闘力"]
s <- setorder(s,cor)
s$Item <- factor(s$Item, levels = rev(s$Item))
txtAnnotate <- NULL
txtAnnotate <- paste(txtAnnotate, "Date acquisition: From ", min(dt.cp$Date), " to ", max(dt.cp$Date), "  \n", sep ="")
txtAnnotate <- paste(txtAnnotate, "Total data count: ", nrow(dt.cp), " (provided by ",nlevels(as.factor(dt.cp$Player)) , " players)  \n", sep ="")
txtAnnotate <- paste(txtAnnotate, "CP range: ", min(dt.cp$Total_CP), "~", max(dt.cp$Total_CP), "    \n\n")
txtAnnotate <- paste(txtAnnotate, "Created by: @GyuNyuYeah_BDM (", now(), ")\n", sep ="")
g <- ggplot(s, aes(x = Item, y = cor, fill = cor)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(cor,2)), size = 3.5) +
  ylab("相関係数") +
  xlab("項目") +
  scale_fill_gradientn(colors = rev(colfunc(4))) +
  theme(title=element_text(size=16,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("戦闘力との相関係数 ", now(), sep ="")) +
  annotate("text",
           label = txtAnnotate,
           x=Inf,y=Inf,hjust=1.1,vjust=1.2)
g
ggsave(plot = g,
       paste("Cor.plot ", date(now()),".png", sep = ""),
       width = g.width,
       height = g.height*1.4,
       unit = "cm",
       dpi = 600,
       scale = 1.5,
       limitsize = F)

for (i in 1:nlevels(factor(dt.cp$Player))) {
  crtPlayer <- as.character(levels(factor(dt.cp$Player))[i])
  dt.cp[Player == crtPlayer, Player:=paste("Player",i, sep = "")]
}

for (i in 1:nlevels(factor(dt.cp.trunc.user$Player))) {
  crtPlayer <- as.character(levels(factor(dt.cp.trunc.user$Player))[i])
  dt.cp.trunc.user[Player == crtPlayer, Player:=paste("Player",i, sep = "")]
}

# 全プレイヤーのデータ(名前は隠す)
t <- dt.cp.trunc.user
setnames(t, c("Player", "Total_CP.trunc", ItemList$ja))
for (i in 1:nrow(t)) {
  for (j in 1:nrow(ItemList)) {
    temp1 <- data.table(DataName = paste(t[i]$Player, " (Mean of ", t[i]$Total_CP.trunc,  "s)", sep = ""),
                        Player = t[i]$Player,
                        Total_CP.trunc = t[i]$Total_CP.trunc,
                        Item = ItemList[j,"ja"],
                        Value = t[i,get(unname(unlist(ItemList[j,"ja"])))])
    if (j == 1) {
      temp2 <- temp1
    } else {
      temp2 <- rbind(temp2,temp1)
    }
  }
  if (i == 1) {
    s <- temp2
  } else {
    s <- rbind(s, temp2)
  }
}
rm(temp1,temp2)
FileName <- paste("戦闘力と各項目の散布図と平滑化曲線", "(", date(now()), ")", sep="")
txtAnnotate <- NULL
txtAnnotate <- paste(txtAnnotate, "戦闘力と各プレイヤーの100区切りの項目ごとの平均値の散布図。", "\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "平滑化曲線が線形で黒点と重なりが多い項目(闇の精霊、ボス知識、装備)はやればその分伸びやすい。", "\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "天井が決まってる一般知識、図鑑、キャラLV、衣装、ペットのうち、ペット・衣装以外はパール使わないからそのうち天井で収束するはず。", "\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "天井が決まってないけど平滑化曲線が右上で水平になるやつ(水晶、アクセ)は、4000後半狙うなら必要。それまでは頑張らなくていいと思う。", "\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "やっぱ、討伐回してシルバー稼ぎながら、シャカッて神話2スロ揃えつつ装備を育てるか。。。もう討伐書ないけど。。。", "\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "以上、ぼやっと眺めて思ったこと。", "\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "Date acquisition: From ", min(dt.cp$Date), " to ", max(dt.cp$Date), "\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "Total data count: ", nrow(dt.cp), " (provided by ",nlevels(as.factor(dt.cp$Player)) , " players)\n\n", sep ="")
txtAnnotate <- paste(txtAnnotate, "Created by: @GyuNyuYeah_BDM (", now(), ")", sep ="")
s$Item.ja <- factor(s$Item.ja, levels = ItemList.ja)
g <- ggplot(s, aes(x = Total_CP.trunc, y = Value)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~Item.ja, scales = "free") +
  ggtitle(FileName) +
  theme(title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        legend.position = "none"
        )

gp <- ggplotGrob(g)
empty.area <- gtable_filroupter(gp, "panel", trim = F)
empty.area <- empty.area$layout[sapply(empty.area$grob,
                                       function(x){class(x)[[1]]=="zeroGrob"}),]
empty.area$t <- empty.area$t - 1 #extend up by 1 cell to cover facet header
empty.area$b <- empty.area$b + 1 #extend down by 1 cell to cover x-axis
gp0 <- gtable_add_grob(x = gp,
                       grobs = tableGrob(txtAnnotate,
                                         theme = ttheme_minimal(base_family = "Segoe Condensed",
                                                                base_size = 16)),
                       t = min(empty.area$t), #16 in this case
                       l = min(empty.area$l), #8
                       b = max(empty.area$b), #18
                       r = max(empty.area$r), #12
                       name = "textbox")
png(paste(FileName,".png", sep = ""),
    width = 12000,
    height = 6000,
    res = 300
)
grid::grid.draw(gp0)
dev.off()


setwd("../")

write.csv(dt.cp, "./CP(Masked).csv")
rm(list = ls(all =T))

# # 確率密度分布
# t <- dt.cp,mean[,4:20, with = F]
# for (i in 1:nrow(t)) {
#   for (j in 1:nrow(ItemList)) {
#     temp1 <- data.table(Item = ItemList[j,"ja"],
#                         Value = t[i,get(unname(unlist(ItemList[j,"en"])))])
#     if (j == 1) {
#       temp2 <- temp1
#     } else {
#       temp2 <- rbind(temp2,temp1)
#     }
#   }
#   if (i == 1) {
#     s <- temp2
#   } else {
#     s <- rbind(s, temp2)
#   }
# }
# 
# FileName <- paste("Probability density plot by Item (", date(now()), ")", sep="")
# txtAnnotate <- NULL
# txtAnnotate <- paste(txtAnnotate, "Date acquisition: From ", min(dt.cp$Date), " to ", max(dt.cp$Date), "\n", sep ="")
# txtAnnotate <- paste(txtAnnotate, "Total data count: ", nrow(dt.cp), " (provided by ",nlevels(as.factor(dt.cp$Player)) , " players)\n\n", sep ="")
# txtAnnotate <- paste(txtAnnotate, "Created by: @GyuNyuYeah_BDM (", now(), ")", sep ="")
# 
# g <- ggplot(s, aes(x = Value, y=..density.., color=Item.ja , fill=Item.ja)) +
#   geom_density(stat = "density", position="identity", alpha = 0.3, size = 0.5) +
#   facet_wrap(~ Item.ja, scales = "free") +
#   ggtitle(FileName) +
#   theme(title=element_text(size=20,face="bold"),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         # axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
#         legend.position = 'none')
# gp <- ggplotGrob(g)
# empty.area <- gtable_filter(gp, "panel", trim = F)
# empty.area <- empty.area$layout[sapply(empty.area$grob,
#                                        function(x){class(x)[[1]]=="zeroGrob"}),]
# empty.area$t <- empty.area$t - 1 #extend up by 1 cell to cover facet header
# empty.area$b <- empty.area$b + 1 #extend down by 1 cell to cover x-axis
# gp0 <- gtable_add_grob(x = gp,
#                        grobs = tableGrob(txtAnnotate,
#                                          theme = ttheme_minimal(base_family = "Segoe Condensed",
#                                                                 base_size = 16)),
#                        t = min(empty.area$t), #16 in this case
#                        l = min(empty.area$l), #8
#                        b = max(empty.area$b), #18
#                        r = max(empty.area$r), #12
#                        name = "textbox")
# png(paste(FileName,".png", sep = ""),
#     width = 6000,
#     height = 3500,
#     res = 250
# )
# grid::grid.draw(gp0)
# dev.off()
# g <- 
#   facet
# g

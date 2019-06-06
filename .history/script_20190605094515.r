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
  "ggplot2"
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
ItemList <- colnames(dt.cp)[4:19]
ItemList.ja <- c("�ł̐���",
                 "�����X�L��",
                 "�m��(���)",
                 "�m��(�{�X)",
                 "�m��(�{�XLv��V)",
                 "�}��",
                 "�L����Lv",
                 "�X�L���C��Lv",
                 "�ߑ�",
                 "����",
                 "�A�N�Z",
                 "�╨",
                 "����",
                 "���͍���",
                 "������",
                 "�����{�[�i�X")

dt.cp.temp <- dt.cp[, Total_CP.trunc:=trunc(Total_CP/100)*100]
dt.cp.trunc.user <- dt.cp.temp %>%
  group_by(Player,Total_CP.trunc) %>%
  summarize(mean(get(ItemList[1])),
            mean(get(ItemList[2])),
            mean(get(ItemList[3])),
            mean(get(ItemList[4])),
            mean(get(ItemList[5])),
            mean(get(ItemList[6])),
            mean(get(ItemList[7])),
            mean(get(ItemList[8])),
            mean(get(ItemList[9])),
            mean(get(ItemList[10])),
            mean(get(ItemList[11])),
            mean(get(ItemList[12])),
            mean(get(ItemList[13])),
            mean(get(ItemList[14])),
            mean(get(ItemList[15])),
            mean(get(ItemList[16]))
  )
setnames(dt.cp.trunc.user, c("Player","Total_CP.trunc", ItemList))
dt.cp.trunc <- dt.cp.trunc.user %>%
  group_by(Total_CP.trunc) %>%
  summarize(mean(get(ItemList[1])),
            mean(get(ItemList[2])),
            mean(get(ItemList[3])),
            mean(get(ItemList[4])),
            mean(get(ItemList[5])),
            mean(get(ItemList[6])),
            mean(get(ItemList[7])),
            mean(get(ItemList[8])),
            mean(get(ItemList[9])),
            mean(get(ItemList[10])),
            mean(get(ItemList[11])),
            mean(get(ItemList[12])),
            mean(get(ItemList[13])),
            mean(get(ItemList[14])),
            mean(get(ItemList[15])),
            mean(get(ItemList[16]))
            )
setnames(dt.cp.trunc, c("Total_CP", ItemList))
rm(dt.cp.temp)

p <- plot_ly(
  data = dt.cp.trunc,
  type = "scatterpolar",
  mode = "markers+lines",
  fill = "toself"
)
for (i in 1:nrow(dt.cp.trunc)) {
 # standard.pts <- unname(unlist(c(dt.cp.trunc[Total_CP == min(Total_CP),2:17],dt.cp.trunc[Total_CP == min(Total_CP),2])))
  standard.pts <- c(1026,60,240,120,13,48,342,47,80,1065,350,40,41,30,23,275,1026)
    pts <- unname(unlist(c(dt.cp.trunc[i,2:17],dt.cp.trunc[i,2])))
  p <- add_trace(
    p,
    theta = c(ItemList.ja, ItemList.ja[1]),
    r = pts/standard.pts,
    name = dt.cp.trunc[i,1]
  )
}
p



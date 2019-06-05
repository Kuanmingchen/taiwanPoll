# --- Load Libraries ------
library(rvest)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# --- 2010 Election ------
url <- "https://zh.wikipedia.org/wiki/2010%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%9B%B4%E8%BD%84%E5%B8%82%E5%B8%82%E9%95%B7%E6%9A%A8%E5%B8%82%E8%AD%B0%E5%93%A1%E9%81%B8%E8%88%89"
sample <- url %>%
  read_html() %>%
  html_nodes(css = 'table')
collapseTables <- which(html_attr(sample, "class")=="wikitable collapsible autocollapse")
tpc        <- as.data.table(html_table(sample[collapseTables[1]],header = TRUE,fill = TRUE))
ntpc       <- as.data.table(html_table(sample[collapseTables[2]],header = TRUE,fill = TRUE))
taichung   <- as.data.table(html_table(sample[collapseTables[3]],header = TRUE,fill = TRUE))
tainan     <- as.data.table(html_table(sample[collapseTables[4]],header = TRUE,fill = TRUE))
kaohsiung  <- as.data.table(html_table(sample[collapseTables[5]],header = TRUE,fill = TRUE))


setnames(tpc,c("Institute","Date","郝龍斌","蘇貞昌","NoResponse","NA"))
setnames(ntpc,c("Institute","Date","朱立倫","蔡英文","NoResponse"))
setnames(taichung,c("Institute","Date","胡志強","蘇嘉全","NoResponse"))
setnames(tainan,c("Institute","Date","賴清德","郭添財","NoResponse"))
setnames(kaohsiung,c("Institute","Date","陳菊","楊秋興","黃昭順","NoResponse"))

tpc[,District := "tpc"]
ntpc[,District := "ntpc"]
taichung[,District := "taichung"]
tainan[,District := "tainan"]
kaohsiung[,District := "kaohsiung"]


dt <- lapply(list(tpc,ntpc,taichung,tainan,kaohsiung),function(x){
  x <- melt(x,id.vars = c("Institute","Date","District","NoResponse"),variable.name = "Candidate",value.name = "Vote")
})
dt <- rbindlist(dt)
dt[,Vote := gsub(pattern = "%",replacement = "",x=Vote)]
dt[,Vote := as.numeric(Vote)]
dt <- dt[!is.na(Vote)]
dt[,Date := gsub(pattern = "年",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "月",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "日",replacement = "" ,x=Date)]
dt[,Date := as.POSIXct(Date)]
dt[,Election := "2010 Election"]
dt2010 <- copy(dt)
# --- 2012 Election ------
url <- "https://zh.wikipedia.org/wiki/2012%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%B8%BD%E7%B5%B1%E9%81%B8%E8%88%89%E6%B0%91%E6%84%8F%E8%AA%BF%E6%9F%A5"
sample <- url %>%
  read_html() %>%
  html_nodes(css = 'table')
collapseTables <- which(html_attr(sample, "class")=="wikitable collapsible autocollapse")
president        <- as.data.table(html_table(sample[collapseTables[1]],header = TRUE,fill = TRUE))

setnames(president,c("Institute","Date","馬英九","蔡英文","宋楚瑜","NoResponse"))
president[,District := "nation"]



dt <- lapply(list(president),function(x){
  x <- melt(x,id.vars = c("Institute","Date","District","NoResponse"),variable.name = "Candidate",value.name = "Vote")
})
dt <- rbindlist(dt)
dt[,Vote := gsub(pattern = "%",replacement = "",x=Vote)]
dt[,Vote := as.numeric(Vote)]
dt <- dt[!is.na(Vote)]
dt[,Date := gsub(pattern = "年",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "月",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "日",replacement = "" ,x=Date)]
dt[,Date := as.POSIXct(Date)]
dt[,Election := "2012 Election"]

dt2012 <- copy(dt)


# --- 2014 Election ------
url <- "https://zh.wikipedia.org/wiki/2014%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%9B%B4%E8%BD%84%E5%B8%82%E9%95%B7%E5%8F%8A%E7%B8%A3%E5%B8%82%E9%95%B7%E9%81%B8%E8%88%89"
sample <- url %>%
  read_html() %>%
  html_nodes(css = 'table')
collapseTables <- which(html_attr(sample, "class") =="wikitable collapsible autocollapse sortable")
tpc        <- as.data.table(html_table(sample[collapseTables[1]],header = TRUE,fill = TRUE))
ntpc       <- as.data.table(html_table(sample[collapseTables[2]],header = TRUE,fill = TRUE))
collapseTables <- which(html_attr(sample, "class") =="wikitable collapsible autocollapse")

taoyuan    <- as.data.table(html_table(sample[collapseTables[8]],header = TRUE,fill = TRUE))
taichung   <- as.data.table(html_table(sample[collapseTables[11]],header = TRUE,fill = TRUE))
tainan     <- as.data.table(html_table(sample[collapseTables[14]],header = TRUE,fill = TRUE))
kaohsiung  <- as.data.table(html_table(sample[collapseTables[17]],header = TRUE,fill = TRUE))


setnames(tpc,c("Institute","Date","柯文哲","連勝文","馮光遠","NoResponse","NA"))
setnames(ntpc,c("Institute","Date","朱立倫","游錫堃","NoResponse","NA","NA"))
setnames(taoyuan,c("Institute","Date","吳志揚","鄭文燦","NoResponse","NA","NA"))
setnames(taichung,c("Institute","Date","林佳龍","胡志強","NoResponse","NA","NA"))
setnames(tainan,c("Institute","Date","賴清德","黃秀霜","NoResponse"))
setnames(kaohsiung,c("Institute","Date","陳菊","楊秋興","NoResponse","NA","NA"))

tpc[,District := "tpc"]
ntpc[,District := "ntpc"]
taoyuan[,District := "taoyuan"]

taichung[,District := "taichung"]
tainan[,District := "tainan"]
kaohsiung[,District := "kaohsiung"]


dt <- lapply(list(tpc,ntpc,taoyuan,taichung,tainan,kaohsiung),function(x){
  x <- melt(x,id.vars = c("Institute","Date","District","NoResponse"),variable.name = "Candidate",value.name = "Vote")
})
dt <- rbindlist(dt)
dt[,Vote := gsub(pattern = "%",replacement = "",x=Vote)]
dt[,Vote := as.numeric(Vote)]
dt <- dt[!is.na(Vote)]
dt[,Date := gsub(pattern = "年",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "月",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "日",replacement = "" ,x=Date)]
dt[,Date := as.POSIXct(Date)]
dt[,Institute := gsub(pattern = "\\*",replacement = "",x=Institute)]
dt[,Institute := gsub(pattern = "\\d+",replacement = "",x=Institute)]
dt[,Election := "2014 Election"]

dt2014 <- copy(dt)

# --- 2016 Election ------
url <- "https://zh.wikipedia.org/wiki/2016%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%B8%BD%E7%B5%B1%E9%81%B8%E8%88%89%E6%B0%91%E6%84%8F%E8%AA%BF%E6%9F%A5"
sample <- url %>%
  read_html() %>%
  html_nodes(css = 'table')
president1        <- as.data.table(html_table(sample[1],header = TRUE,fill = TRUE))
president2        <- as.data.table(html_table(sample[2],header = TRUE,fill = TRUE))
president3        <- as.data.table(html_table(sample[3],header = TRUE,fill = TRUE))

setnames(president1,c("Institute","Date","SampleSize","Error","蔡英文","洪秀柱","NoResponse","NA"))
setnames(president2,c("Institute","Date","SampleSize","Error","蔡英文","洪秀柱","宋楚瑜","NoResponse"))
setnames(president3,c("Institute","Date","SampleSize","Error","蔡英文","朱立倫","宋楚瑜","NoResponse"))

#president1[,District := "nation"]
#president2[,District := "nation"]
president3[,District := "nation"]
#president1 <- president1[1:37,]
#president2 <- president2[1:52,]
president3 <- president3[1:79,]

dt <- lapply(list(president3),function(x){
  x <- melt(x,id.vars = c("Institute","Date","District","NoResponse"),variable.name = "Candidate",value.name = "Vote")
})

# dt <- lapply(list(president1,president2,president3),function(x){
#   x <- melt(x,id.vars = c("Institute","Date","District","NoResponse"),variable.name = "Candidate",value.name = "Vote")
# })
dt <- rbindlist(dt)
dt <- dt[Candidate != "SampleSize" & Candidate != "Error"]
dt[,Vote := gsub(pattern = "%",replacement = "",x=Vote)]
dt[,Vote := as.numeric(Vote)]
dt <- dt[!is.na(Vote)]
dt[,Date := gsub(pattern = "年",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "月",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "日",replacement = "" ,x=Date)]
dt[,Date := as.POSIXct(Date)]
dt[,Institute := gsub(pattern = "\\*",replacement = "",x=Institute)]
dt[,Institute := gsub(pattern = "\\d+",replacement = "",x=Institute)]
dt[,Election := "2016 Election"]
dt2016 <- copy(dt)

# --- 2018 Election ------
url <- "https://zh.wikipedia.org/wiki/2018%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%9B%B4%E8%BD%84%E5%B8%82%E9%95%B7%E5%8F%8A%E7%B8%A3%E5%B8%82%E9%95%B7%E9%81%B8%E8%88%89%E6%B0%91%E6%84%8F%E8%AA%BF%E6%9F%A5"
sample <- url %>%
  read_html() %>%
  html_nodes(css = 'table')
collapseTables <- which(html_attr(sample, "class") =="wikitable collapsible autocollapse sortable")
tpc        <- as.data.table(html_table(sample[collapseTables[2]],header = TRUE,fill = TRUE))
collapseTables <- which(html_attr(sample, "class") =="wikitable collapsible autocollapse")
ntpc       <- as.data.table(html_table(sample[collapseTables[3]],header = TRUE,fill = TRUE))
taoyuan    <- as.data.table(html_table(sample[collapseTables[6]],header = TRUE,fill = TRUE))
taichung   <- as.data.table(html_table(sample[collapseTables[8]],header = TRUE,fill = TRUE))
tainan     <- as.data.table(html_table(sample[collapseTables[11]],header = TRUE,fill = TRUE))
kaohsiung  <- as.data.table(html_table(sample[collapseTables[14]],header = TRUE,fill = TRUE))


setnames(tpc,c("Institute","Date","柯文哲","丁守中","姚文智","NoResponse","NA"))
setnames(ntpc,c("Institute","Date","侯友宜","蘇貞昌","NoResponse","NA","NA"))
setnames(taoyuan,c("Institute","Date","鄭文燦","陳學聖","楊麗環","NoResponse","NA"))
setnames(taichung,c("Institute","Date","林佳龍","盧秀燕","NoResponse","NA","NA"))
setnames(tainan,c("Institute","Date","黃偉哲","高思博","林義豐","蘇煥智","許忠信","陳子敬","NoResponse"))
setnames(kaohsiung,c("Institute","Date","陳其邁","韓國瑜","NoResponse","NA","NA"))

tpc[,District := "tpc"]
ntpc[,District := "ntpc"]
taoyuan[,District := "taoyuan"]
taichung[,District := "taichung"]
tainan[,District := "tainan"]
kaohsiung[,District := "kaohsiung"]


dt <- lapply(list(tpc,ntpc,taoyuan,taichung,tainan,kaohsiung),function(x){
  x <- melt(x,id.vars = c("Institute","Date","District","NoResponse"),variable.name = "Candidate",value.name = "Vote")
})
dt <- rbindlist(dt)
dt[,Vote := gsub(pattern = "%",replacement = "",x=Vote)]
dt[,Vote := as.numeric(Vote)]
dt <- dt[!is.na(Vote)]
dt[,Date := gsub(pattern = "年",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "月",replacement = "/",x=Date)]
dt[,Date := gsub(pattern = "日",replacement = "" ,x=Date)]
dt[,Date := as.POSIXct(Date)]
dt[,Institute := gsub(pattern = "\\*",replacement = "",x=Institute)]
dt[,Institute := gsub(pattern = "\\d+",replacement = "",x=Institute)]
dt[,Election := "2018 Election"]

dt2018 <- copy(dt)

dt <- rbindlist(list(dt2010,dt2012,dt2014,dt2016,dt2018))

dt[,NoResponse := gsub(pattern = "%",replacement = "",x=NoResponse)]
dt[,NoResponse := as.numeric(NoResponse)]
dt[,VoteCorrected := Vote/ ((100-NoResponse)*0.01)]

#------
library(gdata)
library(tidyr)

raw2016 <- as.data.table(read.xls("Data/ElectionOutcome/data_20190604102233.xls"))
raw2014 <- as.data.table(read.xls("Data/ElectionOutcome/data_20190604102200.xls"))
raw2018 <- as.data.table(read.xls("Data/ElectionOutcome/data_20190604102210.xls"))
raw2012 <- as.data.table(read.xls("Data/ElectionOutcome/data_20190604102221.xls"))
raw2010 <- as.data.table(read.xls("Data/ElectionOutcome/data_20190604102149.xls"))

raw2010[,Election := "2010 Election"]
raw2012[,Election := "2012 Election"]
raw2014[,Election := "2014 Election"]
raw2016[,Election := "2016 Election"]
raw2018[,Election := "2018 Election"]

raw2010[,ElectionDate := as.POSIXct("2010-11-27")]
raw2012[,ElectionDate := as.POSIXct("2012-01-14")]
raw2014[,ElectionDate := as.POSIXct("2014-11-29")]
raw2016[,ElectionDate := as.POSIXct("2016-01-16")]
raw2018[,ElectionDate := as.POSIXct("2018-11-24")]

raw <- rbindlist(list(raw2010,raw2012,raw2014,raw2016,raw2018))
setnames(raw,c("zhDistrict","Candidate","No","zhGender","BirthYear","zhParty","nVotes",
                   "voteShare","win","zhIncumbent","Election","ElectionDate"))

raw[zhDistrict == "",zhDistrict := NA]

raw <- fill(raw,zhDistrict)
raw <- raw[!is.na(No)]
raw[,voteShare:= as.character(voteShare)]
raw[,voteShare := gsub(pattern = "%",replacement = "",x=voteShare)]
raw[,voteShare := as.numeric(voteShare)]

unique(raw$zhDistrict)
raw[zhDistrict == "臺北市",District := "tpc"     ]
raw[zhDistrict == "新北市",District := "ntpc" ]
raw[zhDistrict == "臺中市",District := "taichung" ]
raw[zhDistrict == "臺南市",District := "tainan" ]
raw[zhDistrict == "桃園市",District := "taoyuan"]
raw[zhDistrict == "高雄市",District := "kaohsiung"]
raw[zhDistrict == "全國",District := "nation"]
raw[,win := (win != "")]
raw[,Incumbent := (zhIncumbent == "是")]


dt[raw, on = c("Candidate","Election"), c("voteShare","zhParty","ElectionDate") := list(i.voteShare,i.zhParty,i.ElectionDate)]
    
dt[,Error := voteShare - VoteCorrected]
 

dt[,preDate := as.numeric(Date - ElectionDate,units = "days")]

# --- Merge the discrepency of institute names ------
sort(unique(dt$Institute))
dt[Institute == "世新大學[]", Institute := "世新大學" ]
dt[Institute == "台湾指标", Institute := "台灣指標" ]
dt[Institute == "大社會民調", Institute := "大社會" ]
dt[Institute == "旺旺中時", Institute := "中國時報" ]
dt[Institute == "旺旺中時媒體集團", Institute := "中國時報" ]
dt[Institute == "東森新聞", Institute := "東森" ]
dt[Institute == "決策調查", Institute := "決策" ]
dt[Institute == "聯合報系", Institute := "聯合報" ]
dt[Institute == "台灣世代智庫", Institute := "臺灣世代智庫"  ]
dt[Institute == "蘋果民調", Institute := "蘋果日報"  ]
dt[Institute == "台灣世代智庫", Institute := "臺灣世代智庫"  ]
dt[Institute == "民進黨", Institute := "民主進步黨"  ]
dt[Institute == "國民黨", Institute := "中國國民黨"  ]

saveRDS(dt,"dt.rds")


#dt

# qplot(data = dt, x= Date, y = Error, color = zhParty) + 
#   theme(text = element_text(family = "Heiti TC Light")) +
#   facet_grid(Election~.,space = "free_x")
# qplot(data = dt[preDate >= -50 & Institute == "TVBS"], x= preDate, y = Error, color = zhParty) + 
#   theme(text = element_text(family = "Heiti TC Light")) +
#   facet_grid(Election~.,space = "free_x") + 
#   geom_smooth()
# 
# 
# dt[preDate >= -50,mean(Error^2,na.rm=TRUE), by = Institute]

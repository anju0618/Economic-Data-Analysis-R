library(mFilter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)
library(reshape2)
library(openxlsx)
library(zoo)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, rio)
library(seasonal)
library(forecast)

#unemployment of JPN (since 1953-1-1)
x=read.xlsx("UNEMPLOYJPN.xlsx")#https://www.stat.go.jp/data/roudou/sokuhou/tsuki/index.html
xun=select(x,20)
xunrate=xun$X20[8:874]
x=as.numeric(xunrate)
JPNUN=x
#unemployment of USA
y=read.xlsx("UNRATE-USA.xlsx",sheet=2)#https://fred.stlouisfed.org/series/UNRATE
USAUN=y$UNRATE[61:927]
USAUN
JPNUN
#date frame
# date=data_frame(
# Monthly_Timeline= seq(as.Date("1953-01-01"),
# length=867,
# by="month"),
# )
# x=cbind(date,JPN,USA)
# date=t(date)
# rownames(x)=date
# x=melt(x)
# colnames(x)=c("Monthly_Timeline","Nation","Unemployment_Rate")

# Date sequence
date = seq(as.Date("1953-01-01"), by = "month", length.out = 867)
df = data.frame(
 Monthly_Timeline = rep(date, 2),
 Nation = rep(c("JPN", "USA"), each = length(date)),
 Unemployment_Rate = c(JPNUN, USAUN)
)
df
g=ggplot(df, aes(x = Monthly_Timeline, y = Unemployment_Rate, color=Nation))
g=g + geom_line()
g = g + scale_color_nejm()
plot(g)
#UN cycle
#because I use monthly data,λ = 1600*(3^2)
JPNUN_HP = hpfilter(JPNUN, freq = 1600*(3^2))
USAUN_HP = hpfilter(USAUN, freq = 1600*(3^2))
# USAUN_HP = hpfilter(USAUN, freq = )
Trend_un_JPN=JPNUN_HP$trend
Trend_un_USA=USAUN_HP$trend
Cycle_un_JPN=JPNUN_HP$cycle
Cycle_un_USA=USAUN_HP$cycle
df2 = data.frame(
 Monthly_Timeline = rep(date, 2),
 Nation = rep(c("JPN", "USA"), each = length(date)),
 UnemploymentRate_Cycle = c(Cycle_un_JPN,Cycle_un_USA)
)
df2
g=ggplot(df2, aes(x = Monthly_Timeline, y = UnemploymentRate_Cycle, color=Nation))
g=g + geom_line()
g = g + scale_color_nejm()
plot(g)

#trend
df3 = data.frame(
 Monthly_Timeline = rep(date, 2),
 Nation = rep(c("JPN", "USA"), each = length(date)),
 UnemploymentRate_Trend = c(Trend_un_JPN,Trend_un_USA)
)
df3
g=ggplot(df3, aes(x = Monthly_Timeline, y = UnemploymentRate_Trend, color=Nation))
g=g + geom_line()
g = g + scale_color_nejm()
plot(g)
#共分散
sd(Cycle_un_JPN)
sd(Cycle_un_USA)
#相関関係
cor(Cycle_un_JPN, Cycle_un_USA)
#persistence:cor( a[2:Length],a[1:(Length-1)] )
Length=867
cor( JPNUN[2:Length],JPNUN[1:(Length-1)] )
cor(USAUN[2:Length],USAUN[1:(Length-1)])
#Percentage Standard Deviation = (sd(a)/mean(a))*100 HP 前のデータを使う
CV_JPN=(sd(JPNUN)/mean(JPNUN))*100
CV_JPN
CV_USA=(sd(USAUN)/mean(USAUN))*100
CV_USA
#GDP
x=read.xlsx("mpd2018.xlsx",sheet=3)#https://www.rug.nl/ggdc/historicaldevelopment/ma
ddison/releases/maddison-project-database-2018
GDP_JPN=x$Japan
GDP_JPN=GDP_JPN[596:742]#1870 年から
GDP_JPN=as.numeric(GDP_JPN)
HP_GDP_JPN=hpfilter(GDP_JPN,freq=400)
Trend_GDP_JPN=HP_GDP_JPN$trend
Cycle_GDP_JPN=HP_GDP_JPN$cycle
GDP_USA=x$United.States
GDP_USA=GDP_USA[596:742]#1870 年から
GDP_USA=as.numeric(GDP_USA)
HP_GDP_USA=hpfilter(GDP_USA,freq=400)
Trend_GDP_USA=HP_GDP_USA$trend
Cycle_GDP_USA=HP_GDP_USA$cycle
#residental price for JPN and USA
x=read.xlsx("residenatal-jpbis.xlsx",sheet=2)#https://data.bis.org/topics/RPP/BIS%2CWS_SPP%2C1.0/Q.JP.R.628?
view=chart
rp_JPN=x$`OBS_VALUE:Value`#1955-4-1 から 2025-1-1 まで、四半期
plot(rp_JPN)
# rp_JPN = stl(rp_JPN, s.window="periodic") #季節調整時系列データ作成
rp_JPN_ts = ts(rp_JPN, start = c(1956, 1), frequency = 4)
rp_JPN_decomp = stl(rp_JPN_ts, s.window = "periodic")
rp_JPN_adj = seasadj(rp_JPN_decomp)
HP_rp_JPN=hpfilter(rp_JPN_adj, freq = 1600)
plot(HP_rp_JPN$trend)
Trend_rp_JPN=HP_rp_JPN$trend
Cycle_rp_JPN=HP_rp_JPN$cycle
y=read.xlsx("real-residentalUSA.xlsx",sheet=2)#https://fred.stlouisfed.org/series/QUSR628BIS
rp_USA=y$QUSR628BIS#1970-1-1 から 2024-10-1 まで、四半期,2010 年を 100 とした場
合
# rp_USA=stl(rp_USA, s.window="periodic")
rp_USA_ts = ts(rp_USA, start = c(1970, 1), frequency = 4)
rp_USA_decomp = stl(rp_USA_ts, s.window = "periodic")
rp_USA_adj = seasadj(rp_USA_decomp)
plot(rp_USA_adj)
HP_rp_USA = hpfilter(rp_USA_adj, freq = 1600)
plot(HP_rp_USA$trend)
Trend_rp_USA=HP_rp_USA$trend
Cycle_rp_USA=HP_rp_USA$cycle
#Total Credit to Private Non-Financial Sector, Adjusted for Breaks, for JPN and USA
#JPN
x=read.xlsx("credit-privateJPN.xlsx",sheet=2)#https://fred.stlouisfed.org/series/QJPPAM770A
tc_JPN=x$QJPPAM770A#1964-10-1 から 2024-7-1 まで、四半期
tc_JPN_ts=ts(tc_JPN,start=c(1964,10),frequency = 4)
tc_JPN_decomp = stl(tc_JPN_ts, s.window = "periodic")
tc_JPN_adj = seasadj(tc_JPN_decomp)
Hp_tc_JPN=hpfilter(tc_JPN_adj,freq=1600)
Trend_tc_JPN=Hp_tc_JPN$trend
x=read.xlsx("credit-privateUSA.xlsx",sheet=2)#https://fred.stlouisfed.org/series/QUSPAM770A
tc_USA=x$QUSPAM770A#1947-10-1 から 2024-7-1 まで、四半期
tc_USA_ts=ts(tc_USA,start=c(1947,10),frequency = 4)
tc_USA_decomp = stl(tc_USA_ts, s.window = "periodic")
tc_USA_adj = seasadj(tc_USA_decomp)
Hp_tc_USA=hpfilter(tc_USA_adj,freq=1600)
Trend_tc_USA=Hp_tc_USA$trend
#民間非金融部門向け総信用
x=read.xlsx("credit_JPN.xlsx",sheet=2)#https://fred.stlouisfed.org/series/QJPPAM770A
ct_JPN=x$QJPPAM770A#1964-10-1 から 2024-07-1、四半期
ct_JPN_ts = ts(ct_JPN, start = c(1964, 10), frequency = 4)#季節 adjust
ct_JPN_decomp = stl(ct_JPN_ts, s.window = "periodic")
ct_JPN_adj = seasadj(ct_JPN_decomp)
HP_ct_JPN=hpfilter(ct_JPN_adj,freq = 1600)
Trend_ct_JPN=HP_ct_JPN$trend
Cycle_ct_JPN=HP_ct_JPN$cycle
x=read.xlsx("credit_USA.xlsx",sheet=2)#https://fred.stlouisfed.org/series/QUSPAM770A
ct_USA=x$QUSPAM770A#1947-10-1 から 2024-07-01 まで、四半期
ct_USA_ts = ts(ct_USA, start = c(1947, 10), frequency = 4)
ct_USA_decomp = stl(ct_USA_ts, s.window = "periodic")
ct_USA_adj = seasadj(ct_USA_decomp)
HP_ct_USA=hpfilter(ct_USA_adj,freq=1600)
Trend_ct_USA=HP_ct_USA$trend
Cycle_ct_USA=HP_ct_USA$cycle
#---------------------------------------------------------------------------------7
#グラフ,1970 から 2016 まで
2016-1970+1
year=seq(as.Date("1970-01-01"), by = "year", length.out=47)
#GDP
147-47+1
Trend_GDP_JPN1970 = Trend_GDP_JPN[101:147]
Trend_GDP_JPN1970
Trend_GDP_USA1970 = Trend_GDP_USA[101:147]
df=data.frame(
 Anual_Timeline=rep(year,2),
 Nation=rep(c("JPN","USA"),each=length(year)),
 Trend_GDP=c(Trend_GDP_JPN1970,Trend_GDP_USA1970)
)
g=ggplot(df, aes(x = Anual_Timeline, y = Trend_GDP, color=Nation))
g=g + geom_line()
g = g + scale_color_nejm()
plot(g)
Cycle_GDP_JPN1970 = Cycle_GDP_JPN[101:147]
Cycle_GDP_USA1970 = Cycle_GDP_USA[101:147]
df=data.frame( Anual_Timeline=rep(year,2),
 Nation=rep(c("JPN","USA"),each=length(year)),
 Cycle_GDP=c(Cycle_GDP_JPN1970,Cycle_GDP_USA1970)
)
g=ggplot(df, aes(x = Anual_Timeline, y = Cycle_GDP, color=Nation))
g=g + geom_line()
g = g + scale_color_nejm()
plot(g)
#RP 原データ 1970-
rp_JPN
rp_date = seq(as.Date("1970-01-01"), by = "quarter", length.out = 192)
rp_JPN1970=rp_JPN[59:250]
rp_USA1970=rp_USA[1:192]
str(rp_JPN1970)
str(rp_USA1970)
df_rp = data.frame(
 Date = rep(rp_date, 2),
 Country = rep(c("JPN", "USA"), each = length(rp_date)),
 Real_Residential_Price = c(rp_JPN1970, rp_USA1970)
)
g = ggplot(df_rp, aes(x = Date, y = Real_Residential_Price))
g = ggplot(df_rp, aes(x = Date, y = Real_Residential_Price, color = Country)) +
 geom_line(linewidth = 1) +
 labs(title = "Real Residential Price JPN vs USA",
 x = "Year", y = "Index 2010=100") +
 scale_color_nejm() +
 theme_minimal()
plot(g)
#RP_Trend
rp_date = seq(as.Date("1970-01-01"), by = "quarter", length.out = 192)
(1970-1955)*4+1
(2016-1955+1)*4
Trend_rp_JPN1970 = Trend_rp_JPN[59:250]
date_quarter = seq(as.Date("1956-01-01"), by = "quarter", length.out =
length(Trend_rp_JPN))
date_1970_2016 = date_quarter[59:250]
(2016-1970+1)*4
Trend_rp_USA1970= Trend_rp_USA[1:192]
df=data.frame(
 Anual_Timeline=rep(rp_date,2),
 Nation=rep(c("JPN","USA"),each=length(rp_date)),
 Trend_RP=c(Trend_rp_JPN1970,Trend_rp_USA1970)
)
g=ggplot(df, aes(x = Anual_Timeline, y = Trend_RP, color=Nation))
g=g + geom_line(linewidth = 1)
g = g + scale_color_nejm()
plot(g)
#RP_Cycle
Cycle_rp_JPN1970 = Cycle_rp_JPN[59:250]
Cycle_rp_USA1970 = Cycle_rp_USA[1:192]
df=data.frame(
 Anual_Timeline=rep(rp_date,2),
 Nation=rep(c("JPN","USA"),each=length(rp_date)),
 Cycle_RP=c(Cycle_rp_JPN1970,Cycle_rp_USA1970)
)
g=ggplot(df, aes(x = Anual_Timeline, y = Cycle_RP, color=Nation))
g=g + geom_line(linewidth = 1)
g = g + scale_color_nejm()+
 labs(title = "Real Residential Price Cycle JPN vs USA",
 x = "Year", y = "Cycle")
plot(g)
#CT 源データ
ct_date = seq(as.Date("1970-01-01"), by = "quarter", length.out = (2017 - 1970 + 1) * 4)
ct_JPN1970 = ct_JPN_adj[16:207]
ct_USA1970 = ct_USA_adj[84:275]
str(ct_USA1970)
str(ct_JPN1970)
df_ct = data.frame(
 Date = rep(ct_date, 2),
 Country = rep(c("JPN", "USA"), each = length(ct_date)),
 Credit = c(ct_JPN1970, ct_USA1970)
)
g = ggplot(df_ct, aes(x = Date, y = Credit, color = Country)) +
 geom_line(linewidth = 1) +
 labs(title = "Total Credit to Private Non-Financial Sector (Adjusted)",
 x = "Year", y = "Credit Level") +
 scale_color_nejm() +
 theme_minimal()
plot(g)
#Trend_ct グラフ
ct_date = seq(as.Date("1970-01-01"), by = "quarter", length.out = (2017 - 1970 + 1) * 4)
Trend_ct_USA[272]
Trend_ct_USA_1970
4*(1970-1950)+3
84+4*(2017-1970)
str(Trend_ct_JPN_1970)
str(Trend_ct_USA_1970)
Trend_ct_JPN
Trend_ct_JPN[210]
Trend_ct_USA[275]
Trend_ct_JPN1970 = Trend_ct_JPN[16:207]#192
Trend_ct_USA1970 = Trend_ct_USA[84:275]
df_trend_credit = data.frame(
 Date = rep(ct_date, 2),
 Country = rep(c("JPN", "USA"), each = 192),
 Trend_Credit = c(Trend_ct_JPN1970, Trend_ct_USA1970)
)
g = ggplot(df_trend_credit, aes(x = Date, y = Trend_Credit, color = Country)) +
 geom_line(linewidth = 1) +
 labs(title = "Trend of Credit to Private Non-Financial Sector (1970–2016)",
 x = "Year", y = "Credit (Adjusted)") +
 scale_color_nejm() +
 theme_minimal()
plot(g)
#Cycle_ct グラフ
Cycle_ct_JPN
Cycle_ct_JPN1970 = Cycle_ct_JPN[16:207]
Cycle_ct_USA1970 = Cycle_ct_USA[84:275]
df_cycle_credit = data.frame(
 Date = rep(ct_date, 2),
 Country = rep(c("JPN", "USA"), each = 192),
 Cycle_Credit = c(Cycle_ct_JPN1970, Cycle_ct_USA1970)
)
g = ggplot(df_cycle_credit, aes(x = Date, y = Cycle_Credit, color = Country)) +
 geom_line(linewidth = 1) +
 labs(title = "Credit Cycle to Private Non-Financial Sector (1970–2016)",
 x = "Year", y = "Cycle Component") +
 scale_color_nejm() +
 theme_minimal()
plot(g)
#アメリカの住宅価格サイクルと CT サイクルのグラフ
str(credit_date)
df_cycle_compare = data.frame(
 Date = rep(ct_date, 2),
 Indicator = rep(c("Credit", "Real Residental Price"), each = 192),
Cycle = c(Cycle_ct_USA_1970, Cycle_rp_USA1970)
)
g = ggplot(df_cycle_compare, aes(x = Date, y = Cycle, color = Indicator)) +
 geom_line(linewidth = 1) +
 labs(title = "Cycle Comparison: Credit vs Real Residential Price (USA, 1970–2016)",
 x = "Year", y = "Cycle Component") +
 scale_color_nejm() +
 theme_minimal()
plot(g)
#相関係数
(cor(Cycle_ct_USA1970,Cycle_rp_USA1970))
ccf(Cycle_rp_USA1970, Cycle_ct_USA1970, lag.max = 20)
Cycle_ct_USA_1970
Cycle_rp_USA1970
#2000 まで
rp_x <- Cycle_rp_USA1970[1:124]
ct_x <- Cycle_ct_USA1970[1:124]
(cor(rp_sub, ct_sub, use = "complete.obs"))
rp_2000_2017 <- Cycle_rp_USA1970[121:192]
ct_2000_2017 <- Cycle_ct_USA1970[121:192]
(cor(rp_2000_2017, ct_2000_2017, use = "complete.obs"))
#共分散
cov(Cycle_ct_USA1970, Cycle_rp_USA1970)
cov(rp_x, ct_x)
#PSD
sd(Cycle_ct_USA1970)/mean(ct_USA1970)*100
sd(Cycle_rp_USA1970)/mean(rp_USA1970)*100
#日本の住宅価格サイクルと CT サイクルのグラフ
df_cycle_compare_JPN = data.frame(
 Date = rep(ct_date, 2),
 Indicator = rep(c("Credit", "Real Residental Price"), each = 192),
 Cycle = c(Cycle_ct_JPN1970, Cycle_rp_JPN1970)
)
g = ggplot(df_cycle_compare_JPN, aes(x = Date, y = Cycle, color = Indicator)) +
geom_line(linewidth = 1) +
 labs(title = "Cycle Comparison: Credit vs Real Residential Price (JPN, 1970–2016)",
 x = "Year", y = "Cycle Component") +
 scale_color_nejm() +
 theme_minimal()
plot(g)
# 相関係数（日本）
(cor(Cycle_ct_JPN_1970, Cycle_rp_JPN1970))
#PSD
sd(Cycle_ct_JPN1970)/mean(ct_JPN1970)*100
sd(Cycle_rp_JPN1970)/mean(rp_JPN1970)*100
#失業率 1970
Trend_un_JPN1970 = Trend_un_JPN[205:396]
Trend_un_USA1970 = Trend_un_USA[205:396]
df_trend_comparison = data.frame(
 Date = rep(ct_date, 4),
 Indicator = rep(c("Unemployment Rate", "Residential Price"), each = 192*2),
 Country = rep(c("JPN", "USA"), times = 2, each = 192),
 Trend_Value = c(Trend_un_JPN1970, Trend_un_USA1970, Trend_rp_JPN1970,
Trend_rp_USA1970)
)
g = ggplot(df_trend_comparison, aes(x = Date, y = Trend_Value, color = Country)) +
 geom_line(linewidth = 1) +
 facet_wrap(~Indicator, scales = "free_y", ncol = 1) +
 labs(title = "Trend Comparison: Unemployment Rate vs Residential Price (1970–2016)",
 x = "Year", y = "Trend Value") +
 scale_color_nejm() +
 theme_minimal()
plot(g)
#アメリカの失業率と RP の比較
Trend_un_USA_1970=Trend_un_USA[205:(205+192*3-1)]
Trend_un_USA_q = tapply(Trend_un_USA_1970, rep(1:192,each = 3), mean)
Trend_rp_USA_q = Trend_rp_USA[1:192]
date_seq = seq(as.Date("1970-01-01"),by="quarter",length.out=192)
df = data.frame(
 Date = rep(date_seq, 2),
 Value = c(Trend_un_USA_q, Trend_rp_USA_q),
 Indicator = rep(c("Unemployment Rate", "RP"),each=192)
)
g=ggplot(df, aes(x = Date, y = Value, color = Indicator)) +
 geom_line(size = 1)
plot(g)
#失業率と RP 比較
Cycle_un_JPN_qtr <- aggregate(zoo(Cycle_un_JPN, order.by =
as.yearmon(seq(as.Date("1970-01-01"), as.Date("2016-03-01"), by = "month"))),
 as.yearqtr, mean)
Cycle_un_USA_qtr <- aggregate(zoo(Cycle_un_USA, order.by =
as.yearmon(seq(as.Date("1970-01-01"), as.Date("2016-03-01"), by = "month"))),
 as.yearqtr, mean)
str(Cycle_un_JPN_qtr)
str(Cycle_un_USA_qtr)
Cycle_un_JPN_qtr = as.numeric(Cycle_un_JPN_qtr)
Cycle_un_USA_qtr = as.numeric(Cycle_un_USA_qtr)
cor(Cycle_un_JPN_qtr, Cycle_rp_JPN1970)
cor(Cycle_un_USA_qtr, Cycle_rp_USA1970)
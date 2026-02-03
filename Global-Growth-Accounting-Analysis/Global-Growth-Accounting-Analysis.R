setwd("C:/under my control/明治大学/大学資料3ｰ1/5金曜日/1経済変動論")
library(openxlsx)
library(ggplot2)
library(dplyr)
pwt = read.xlsx("pwt1001.xlsx", sheet = "Data")

target_countries = c("JPN", "KOR", "DEU")
start_year = 1970
end_year = 2019

D_raw = pwt[pwt$countrycode %in% target_countries & pwt$year >= start_year & pwt$year <= end_year, ]

#(b) 
D_final = data.frame()

for (country_code in target_countries) {
  E = D_raw[D_raw$countrycode == country_code, ]
  E$g_y = (E$rgdpo / lag(E$rgdpo, 1)) - 1
  E$g_k = (E$cn / lag(E$cn, 1)) - 1
  E$g_l = (E$emp / lag(E$emp, 1)) - 1
  E$g_hc = (E$hc / lag(E$hc, 1)) - 1
  E$g_pop = (E$pop / lag(E$pop, 1)) - 1
  E$alpha = 1 - E$labsh
  E$g_a = E$g_y - (E$alpha * E$g_k) - ((1 - E$alpha) * E$g_l)
  D_final = rbind(D_final, E)
}
D_final = na.omit(D_final[, c("countrycode", "year", "g_y", "g_k", "g_l", "g_a", "alpha", "labsh", "g_hc", "g_pop")])

ggplot(D_final, aes(x = year, y = g_a, color = countrycode)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "ソロー残差（TFP成長率）の推移",
       x = "年",
       y = "TFP成長率",
       color = "国") +
  theme_bw()

# (e) 資本ストック成長率の推移
ggplot(D_final, aes(x = year, y = g_k, color = countrycode)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "資本ストック成長率の推移",
       x = "年",
       y = "資本成長率",
       color = "国")

#(f)労働投入量成長率
ggplot(D_final, aes(x = year, y = g_l, color = countrycode)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "労働投入量成長率の推移",
       x = "年",
       y = "労働成長率",
       color = "国")

#(g)実質GDP成長率の推移
ggplot(D_final, aes(x = year, y = g_y, color = countrycode)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "実質GDP成長率の推移",
       x = "年",
       y = "GDP成長率",
       color = "国")

#(h)労働分配率の推移
ggplot(D_final, aes(x = year, y = labsh, color = countrycode)) +
  geom_line(linewidth = 1) +
  labs(title = "労働分配率の推移",
       x = "年",
       y = "労働分配率",
       color = "国") +
  theme_bw()

#(i)人的資本成長率の推移
ggplot(D_final, aes(x = year, y = g_hc, color = countrycode)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "人的資本成長率の推移",
       x = "年",
       y = "人的資本成長率",
       color = "国") +
  theme_bw()

#(j)人口成長率の推移
ggplot(D_final, aes(x = year, y = g_pop, color = countrycode)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "人口成長率の推移",
       x = "年",
       y = "人口成長率",
       color = "国") +
  theme_bw()


#(k)寄与度分解
D_final$k_contribution = D_final$alpha * D_final$g_k
D_final$l_contribution = (1 - D_final$alpha) * D_final$g_l
D_final$a_contribution = D_final$g_a

D_final$period = cut(D_final$year, 
                     breaks = c(1970, 1990, 2008, 2019), 
                     labels = c("1971-1990", "1991-2008", "2009-2019"), 
                     right = TRUE)

summary_data = aggregate(cbind(g_y, k_contribution, l_contribution, a_contribution) ~ countrycode + period, 
                         data = D_final, 
                         FUN = mean)
library(reshape2)
summary_melt = melt(summary_data, id.vars = c("countrycode", "period", "g_y"))

ggplot(summary_melt, aes(x = countrycode, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_point(aes(y = g_y), color = "black", size = 3) + 
  facet_wrap(~ period) +
  labs(title = "経済成長率への寄与度分解",
       x = "国",
       y = "平均成長率",
       fill = "寄与要因") +
  scale_fill_discrete(labels = c("資本", "労働", "TFP")) +
  theme_bw()

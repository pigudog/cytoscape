if (!require("igraph"))
  BiocManager::install("igraph")
if (!require("ggnewscale"))
  install.packages("ggnewscale")
if (!require("tidyverse"))
  install.packages("tidyverse")
if (!require("ggraph"))
  install.packages("ggraph")
library(igraph)
library(ggraph)
library(tidyverse)
library(ggnewscale)

ppidf<-read.table("string_interactions_short.tsv",header = F)
nodes<-read.csv("geneInfo.CSV",header = T)

#保留string_interactions_short.tsv的第1，2，10列，分别是节点的起点、终点，和试验验证的可信度
nodelink<-ppidf[,c(1,2,10)] %>%
  rename_with(~c("from","to","experimentally_determined_interaction"),c(1,2,3))

#对可信度分组，大于0.5为已验证，否则为非验证
nodelink$experimentally_group <- ifelse(nodelink$experimentally_determined_interaction>0.5,
                                        "validation","no validation")
# 创造igraph图形
mygraph<-graph_from_data_frame(nodelink,vertices = nodes)
plot(mygraph)

ov = c('#A499CC',
      '#5E4D9A',
      '#EF7B77',
      '#A56BA7',
      '#E0A7C8',
      '#E069A6',
      '#941456',
      '#01A0A7',
      '#75C8CC',
      '#279AD7',
      '#1F577B',
      '#78C2ED',
      '#F0D7BC',
      '#FCBC10',
      '#EAEFC5',
      '#D5B26C',
      '#D5DA48',
      '#B6B812',
      '#9DC3C3',
      '#A89C92',
      '#FEE00C',
      '#FEF2A1',
      '#7CBB5F',
      '#368650',
      '#866017',
      '#9F987F',
      '#E0DFED',
      '#F0EEF0')

ggraph(mygraph,layout = "linear",circular=T)+
  geom_edge_bend(aes(edge_width=experimentally_determined_interaction,edge_colour=experimentally_group),
                 strength = 0.02, #strength参数后接数值，在0-1之间，越大，代表曲线的弯曲程度越大
                 alpha=0.6)+ 
  scale_edge_width_continuous(range = c(0.5,1.2))+ #设置连线的粗细范围
  scale_edge_color_manual(values = c("skyblue","#fe817d"))+ #设置连线的颜色
  geom_node_point(aes(colour=Sample,size=logFC))+ #添加第一层散点，映射样本信息和logFC
  scale_size_continuous(range =c(3,15))+ #设置散点大小范围
  scale_colour_manual(values = ov[1:10])+ #设置散点颜色
  new_scale_colour()+ #添加新的scale
  geom_node_point(aes(colour=DEG),alpha=0.8,size=2)+ #添加第二层散点，映射差异表达基因信息
  scale_colour_manual(values = c("black","#FFEB3B"))+ #设置散点颜色
  theme_void()

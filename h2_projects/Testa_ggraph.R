library(tidygraph)
library(tidygraph)
library(ggraph)
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(visNetwork)


ss <- "https://docs.google.com/spreadsheets/d/1RefL2DMmCoOjCcIiaYgSnuDF34ORQFAZKu3nkaoS6o8/edit?gid=1138676736#gid=1138676736" 
# Read project sheet and clean 
df<- read_sheet(ss, sheet = "main_projects") 
comp<-read_sheet(ss,sheet="collaborations")

gs_input <- df %>%
  select(date_online, product)%>%
  filter(date_online != "N/A")%>%
  group_by(date_online,product)%>%
  summarise(n = n())


g <- gs_input%>%
  as_tbl_graph(directed=FALSE) %N>%
  mutate(group = ifelse(name %in% gs_input$date_online, "yy","pp" ))%>%
  mutate(degree = centrality_degree(mode="all"))

nodes <- g %>% activate(nodes) %>% as_tibble()
edges <- g %>% activate(edges) %>% as_tibble()

g %>%ggraph(layout = "nicely") + 
  geom_edge_link(alpha=0.8,color="grey")+
  geom_node_point(aes(color=group, shape=group,size=degree))+
  geom_node_text(aes(label=name),repel=TRUE,size=3)+
  theme_graph()+
  theme(legend.position = "none")

nodes <- g %>% activate(nodes) %>% as_tibble() %>% rowid_to_column("id")
edges <- g %>% activate(edges) %>% as_tibble()




newtest <-comp %>%
  select(id,Comp_name)
  filter(date_online != "N/A")%>%
  
newtest2 <- df%>%
  select(id,country)%>%
  left_join(newtest,by="id")%>%
  group_by(country, Comp_name)%>%
  summarise(n = n())

g <- newtest2%>%
  as_tbl_graph(directed=FALSE) %N>%
  mutate(group = ifelse(name %in% newtest2$country, "yy","pp" ))%>%
  mutate(degree = centrality_degree(mode="all"))

nodes <- g %>% activate(nodes) %>% as_tibble()
edges <- g %>% activate(edges) %>% as_tibble()

g %>%ggraph(layout = "nicely") + 
  geom_edge_link(alpha=0.8,color="grey")+
  geom_node_point(aes(color=group, shape=group,size=degree))+
  geom_node_text(aes(label=name),repel=TRUE,size=3)+
  theme_graph()+
  theme(legend.position = "none")



##############################################

com <-comp %>%
  select(id,Comp_name)
#  filter(date_online != "N/A")%>%

cont <- df%>%
  select(id,country, product)%>%
  left_join(com,by="id")%>%
  select(-id,-country)
  #group_by(country,Comp_name,product)%>%
  #summarise(n = n())

number<- cont%>%
  select(-Comp_name)

net <- as.matrix(cont)
class(net)
g <- graph.edgelist(net , directed = FALSE)
plot(g)
library(igraph)
V(g)
E(g)


g<- set_vertex_attr(g, "count",value=n)
head(vertex_attr(g))
library(igraph)

################ Product-companies-network ###############################

g <- graph_from_data_frame(cont, directed = FALSE)
print(g)
plot(g, vertex.color = "lightblue", vertex.size = 30, edge.color = "gray",
     main = "Product-Companies network", vertex.label.color = "black")

# Set labels: Only show labels for nodes in column "from"
V(g)$label <- ifelse(V(g)$name %in% cont$product, V(g)$name, NA)
V(g)$size <- ifelse(V(g)$name %in% cont$product, 10, 5)   # Större noder för namn i kolumn 1
V(g)$color <- ifelse(V(g)$name %in% cont$product, "orange", "lightblue")  # Färg för noder i kolumn 1

plot(g, vertex.size = V(g)$size, vertex.color = V(g)$color, edge.color = "gray",
     main = "Product-Companies network", vertex.label = V(g)$label,
     vertex.label.color = "black", vertex.frame.color = "white")

################# Country-companies-network ######################################

contr <- df%>%
  select(id,country)%>%
  left_join(com,by="id")%>%
  select(-id)

g <- graph_from_data_frame(contr, directed = FALSE)
print(g)
plot(g, vertex.color = "lightblue", vertex.size = 30, edge.color = "gray",
     main = "Product-Companies network", vertex.label.color = "black")

# Set labels: Only show labels for nodes in column "from"
V(g)$label <- ifelse(V(g)$name %in% contr$country, V(g)$name, NA)
V(g)$size <- ifelse(V(g)$name %in% contr$country, 10, 5)   # Större noder för namn i kolumn 1
V(g)$color <- ifelse(V(g)$name %in% contr$country, "orange", "lightblue")  # Färg för noder i kolumn 1

plot(g, vertex.size = V(g)$size, vertex.color = V(g)$color, edge.color = "gray",
     main = "Product-Companies network", vertex.label = V(g)$label,
     vertex.label.color = "black", vertex.frame.color = "white")

###########################################################
prod<-df%>%
  select(country,product)

  g <- graph_from_data_frame(prod, directed = FALSE)
print(g)
plot(g, vertex.color = "lightblue", vertex.size = 30, edge.color = "gray",
     main = "Product-Companies network", vertex.label.color = "black")

# Set labels: Only show labels for nodes in column "from"
#V(g)$label <- ifelse(V(g)$name %in% prod$country, V(g)$name, NA)
V(g)$size <- ifelse(V(g)$name %in% prod$country, 10, 5)   # Större noder för namn i kolumn 1
V(g)$color <- ifelse(V(g)$name %in% prod$country, "orange", "lightblue")  # Färg för noder i kolumn 1

plot(g, vertex.size = V(g)$size, vertex.color = V(g)$color, edge.color = "gray",
     main = "Product-Countries network", vertex.label = V(g)$label,
     vertex.label.color = "black", vertex.frame.color = "white")

################################################################
prod<-df%>%
  select(product,country)

g <- graph_from_data_frame(prod, directed = FALSE)
print(g)
plot(g, vertex.color = "lightblue", vertex.size = 30, edge.color = "gray",
     main = "Product-Companies network", vertex.label.color = "black")

# Set labels: Only show labels for nodes in column "from"
#V(g)$label <- ifelse(V(g)$name %in% prod$country, V(g)$name, NA)
V(g)$size <- ifelse(V(g)$name %in% prod$product, 10, 5)   # Större noder för namn i kolumn 1
V(g)$color <- ifelse(V(g)$name %in% prod$product, "orange", "lightblue")  # Färg för noder i kolumn 1

plot(g, vertex.size = V(g)$size, vertex.color = V(g)$color, edge.color = "gray",
     main = "Product-Countries network", vertex.label = V(g)$label,
     vertex.label.color = "black", vertex.frame.color = "white")




# Load igraph
library(igraph)
data(friends)
class(friends)

# Inspect the first few rows of the dataframe 'friends'
head(friends)

# Convert friends dataframe to a matrix
friends.mat <- as.matrix(friends)
class(friends.mat)
# Convert friends matrix to an igraph object

g <- graph.edgelist(friends.mat , directed = FALSE)


# Make a very basic plot of the network
plot(g)

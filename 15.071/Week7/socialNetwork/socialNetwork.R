edges = read.csv("edges.csv")
users = read.csv("users.csv")

146*2/59

table(subset(users, !is.na(school))$locale)

table(users$school, users$gender)

library(igraph)

# undirected graph
g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)

table(degree(g))

# make the nodes with bigger network bigger in size
V(g)$size = degree(g)/2+2

plot(g, vertex.label=NA)

# assign red to gender 'A' and gray to gender 'B'
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label = NA)

# assign red to school 'A' and gray to shcool 'AB'
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label = NA)

# assign red to locale 'A' and gray to locale 'AB'
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label = NA)

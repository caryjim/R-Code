# Sankey 
#install.packages('networkD3')

library(networkD3)

# Define nodes
nodes <- data.frame(name = c(
  "First-Time Freshman", "Ongoing Year 1", "Did not Return", "Ongoing Year 2",
  "Transfer to 2yrs (Y1)", "Transfer to 4yrs (Y1)", "Award (F2019)", "Readmit",
  "Transfer to 2yrs (Y2)", "Transfer to 4yrs (Y2)", "Award (F2020)",
  "Ongoing Year 3", "Transfer to 2yrs (Y3)", "Transfer to 4yrs (Y3)", "Award (F2021)",
  "Ongoing Year 4", "Transfer to 2yrs (Y4)", "Transfer to 4yrs (Y4)", "Award (F2022)",
  "Ongoing Year 5", "Transfer to 2yrs (Y5)", "Transfer to 4yrs (Y5)"
))

# Define links
links <- data.frame(
  source = c(0, 0, 0, 0, 0, 0,
             1, 1, 1, 1,
             3, 3, 3,
             11, 11, 11,
             15, 15, 15),
  target = c(1, 2, 4, 5, 6, 7,
             3, 8, 9, 10,
             11, 12, 13,
             15, 16, 17,
             19, 20, 21),
  value = c(3588, 2166, 49, 67, 4, 1023,
            1990, 54, 247, 50,
            1424, 59, 387,
            1002, 54, 384,
            675, 54, 216)
)

# Create Sankey diagram
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

Brief description of the codes:

45_functions.R: All functions necessary for 45_maincode.R

45_maincode.R: Code to plot the railway networks and analyse the cascade with the Motter-Lai model over that network

45_shape_to_csv.R: Code necessary to convert the shapefiles into .csv files. Output: edges.csv : nodeID_from, nodeID_to stations.csv : nodeID, latitude, longitude, nodeLabel, country_ISO3 (nodeLabel and country_ISO3 are NA for nodes that are not stations). NOTE: nodes in stations.csv are the endpoints of all railways' partitions, therefore not all nodes are stations but all stations are nodes

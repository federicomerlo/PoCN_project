# PoCN_project
Project for Physics of Complex System

Project chosen:
project 11: Growth-Shrink models. SCORE: 0.6;
project 45: European transportation networks I. SCORE: 1.0;



11_functions.R: All the fucntions necessary for 11_maincode.R


11_maincode.R: 


45_functions.R: All functions necessary for 45_maincode.R


45_maincode.R: Code to plot the railway networks and analyse the cascade with the Motter-Lai model over that network


45_shape_to_csv.R: Code necessary to convert the shapefiles into .csv files. Output:
edges.csv : nodeID_from, nodeID_to
stations.csv : nodeID, latitude, longitude, nodeLabel, country_ISO3 (nodeLabel and country_ISO3 are NA for nodes that are not stations).
NOTE: nodes in stations.csv are the endpoints of all railways' partitions, therefore not all nodes are stations but all stations are nodes



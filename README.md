# chord-map

A small Haskell tool that parses a list of songs and their chord progressions and outputs a matrix representing a weighted undirected graph of all the progressions in the songs, each weight representing the similarity of the two progressions.
The said graph can then be viewed in a program like Gephi to get visual representation of the data and draw conclusions.

An example graph created using Gephi with the "ForceAtlas 2" layout:

![graph](https://github.com/mcxim/chord-map/blob/master/gephi-graph.jpg?raw=true)

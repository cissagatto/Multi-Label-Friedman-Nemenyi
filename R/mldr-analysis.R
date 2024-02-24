library("mldr")

#mldrGUI()

arff= "/home/biomal/Multi-Label-Friedman-Nemenyi/Data/EukaryotePseAAC"
xml = "/home/biomal/Multi-Label-Friedman-Nemenyi/Data/EukaryotePseAAC.xml"

mymldr <-mldr(filename = arff , xml_file = xml)

mymldr$attributesIndexes
mymldr$labelsets
mymldr$attributes
mymldr$labels
mymldr$measures

summary(mymldr)
res = concurrenceReport(mymldr)
labelInteractions(mymldr, labelProportion=1)
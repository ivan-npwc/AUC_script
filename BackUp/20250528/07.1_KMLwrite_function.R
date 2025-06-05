#################################################################################### KML WRITE 
KMLwrite=function(Img3,kmlPathSave) {
write.csv(Img3, gsub("kml","csv",kmlPathSave))
  sink(kmlPathSave)
  cat(paste0("<?xml version='1.0' encoding='UTF-8'?>
             <kml xmlns='UIAautoCount'>
             <Document>
             <Style id='default'>
             <LineStyle>
             <color>7F0000FF</color>
             <width>3</width>
             </LineStyle>
             <PolyStyle>
             <color>990000FF</color>
             <fill>false</fill>
             </PolyStyle>
             </Style>
             <Folder>
             <name>Point Features</name>
             <description>Point Features</description>"))
  
  for (i in 1:length(Img3[,1])) {
    if (i==1) {
      a=paste0("<Placemark>
               <description>",
               Img3$age[i],
               "</description>
               <Point>
               <coordinates>",
               Img3$lon[i], ",",
               Img3$lat[i],
               "</coordinates>
               </Point>
               <styleUrl>#default</styleUrl>
               </Placemark>")
    } else {
      b=paste0("<Placemark>
               <description>",
               Img3$age[i],
               "</description>
               <Point>
               <coordinates>",
               Img3$lon[i], ",",
               Img3$lat[i],
               "</coordinates>
               </Point>
               <styleUrl>#default</styleUrl>
               </Placemark>") 
      a=paste0(a,b)
    }}
  cat(a)
  
  cat(paste0("  
             </Folder>
             </Document>
             </kml>"))
  sink()
}

# PIEROutil
A R client library for PIERO SPARQL Endpoint

Installation
------------
```R
install.packages("devtools")
install.packages("SPARQL")
install.packages("hash")
library(devtools)
install_github("kozo2/PIEROutil")
```

Examples
--------
```R
library(PIEROutil)
getInfo("ec:3.2.1.18")
getInfo("reaction:RN00004")
getTrans("transformation:TR00216")
getInfo("kegg:R00414")
getInfo("kegg:RP00020")
getTransFromReact("kegg:R00001")
getReactFromTrans("cyclization")
getReactSameTrans("kegg:R02110")
getReactFromRpair("kegg:RP00020")
getReactFromRpair("kegg:RP00020 kegg:RP00021")
getEcFromRpair("kegg:RP00020")
getEcFromCpdpair("kegg:C00015", "kegg:C00043")
```

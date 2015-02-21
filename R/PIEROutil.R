#library(SPARQL)
#library(hash)



getInfo <- function(exfactor, limit = 0, endpoint="http://www.genome.jp/sparql/reactionontology/"){

    limitC = ""
    if (limit != 0)
    if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
    limitC = paste( " limit " , limit)
    else
    warning ("limit should be an integer, limit omitted from the query")

    query <- paste( "SELECT DISTINCT *\n",
                    "WHERE { \n",
                        exfactor, " ?p ?o . \n",
                    "}  \n",
             limitC )

    message("Performing query please wait...")

    res <- tryCatch({
                SPARQL(url=endpoint,query)
            },
            error = function(err){
                message("an error occured when trying to query for ensembl genes ", err)
            })#end tryCatch
    
    #res<-SPARQL(url=endpoint,query)
    return (res$results)    
}

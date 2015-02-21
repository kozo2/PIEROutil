#library(SPARQL)
#library(hash)

getInfo <- function(exfactor, inference = FALSE, limit = 0, endpoint="http://www.genome.jp/sparql/reactionontology/"){

  limitC = ""
  if (limit != 0)
  if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
  limitC = paste( " limit " , limit)
  else
  warning ("limit should be an integer, limit omitted from the query")
  
  sparql_base <- paste( "PREFIX transformation: <http://reactionontology.org/piero/transformation/> \n",
                        "SELECT DISTINCT *\n",
                        "WHERE { \n",
                        exfactor, " ?p ?o . \n",
                        "}  \n",
                 limitC )

  if(inference) {
    query <- paste( "DEFINE input:inference 'http://reactionontology.org/inference' \n", sparql_base)
  }
  else {
    query <- sparql_base
  }

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

getTrans <- function(reactionID, inference = FALSE, limit=0, endpoint="http://www.genome.jp/sparql/reactionontology/"){
  
  limitC = ""
  if (limit != 0)
  if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
    limitC = paste( " limit " , limit)
  else
    warning ("limit should be an integer, limit omitted from the query")
    
  sparql_base <- paste( "PREFIX transformation: <http://reactionontology.org/piero/transformation/> \n",
                        "SELECT DISTINCT *\n",
                        "WHERE { \n",
                        reactionID, " piero:hasTransformation ?transformation . \n",
                        "?transformation rdfs:label ?transformation_name . \n",
                        "}  \n",
                 limitC )

  if(inference) {
    query <- paste( "DEFINE input:inference 'http://reactionontology.org/inference' \n", sparql_base)
  }
  else {
    query <- sparql_base
  }

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

getReactFromTrans <- function(transformation_name, inference = FALSE, limit=0, endpoint="http://www.genome.jp/sparql/reactionontology/"){
  
  limitC = ""
  if (limit != 0)
  if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
    limitC = paste( " limit " , limit)
  else
    warning ("limit should be an integer, limit omitted from the query")
  
  sparql_base <- paste( "PREFIX transformation: <http://reactionontology.org/piero/transformation/> \n",
                        "SELECT DISTINCT ?kegg_reaction \n",
                        "WHERE { \n",
                        "?transformation rdfs:label '", transformation_name, "' . \n",
                        "?kegg_reaction piero:hasTransformation ?transformation . \n",
                        "}  \n",
                        limitC, sep = "" )
  
  if(inference) {
    query <- paste( "DEFINE input:inference 'http://reactionontology.org/inference' \n", sparql_base)
  }
  else {
    query <- sparql_base
  }
  
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
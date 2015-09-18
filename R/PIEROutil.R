#library(SPARQL)
#library(hash)

getPath <- function(sourcecpd, targetcpd, inference = FALSE, limit = 0, endpoint = "http://www.genome.jp/sparql/reactionontology/"){
  
  limitC = ""
  
  sparql_base <- paste( "PREFIX transformation: <http://reactionontology.org/piero/transformation/> \n",
                        "SELECT DISTINCT ?o3 \n",
                        "WHERE { \n",
                        sourcecpd, " ?p1 ?rp1 . \n",
                        "?rp1 rdf:type kegg:rpair . \n",
                        "?rp1 ?p3 ?o3 . \n",
                        "?o3 rdf:type kegg:compound . \n",
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
  
  foo <- res$results
  buf <- c()
  
  if (length(grep(targetcpd, foo)) == 0) {
    breaddfs <- data.frame()
    for (i in 1:length(foo)) {
      cpdid <- str_extract(foo[1,i], "C[0-9]{5}")
      keggid <- paste("kegg:", cpdid, sep = "")
      print(keggid)
      getPath(keggid, targetcpd)
    }
  }
  else {
    print("reached")
    #return(foo)
  }
}

#nextPath <- function(sourcecpd, targetcpd, inference = FALSE, limit = 0, endpoint = "http://www.genome.jp/sparql/reactionontology/"){
  
  limitC = ""
  
  sparql_base <- paste( "PREFIX transformation: <http://reactionontology.org/piero/transformation/> \n",
                        "SELECT DISTINCT ?o3 \n",
                        "WHERE { \n",
                        sourcecpd, " ?p1 ?rp1 . \n",
                        "?rp1 rdf:type kegg:rpair . \n",
                        "?rp1 ?p3 ?o3 . \n",
                        "?o3 rdf:type kegg:compound . \n",
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
  
  return(res$result)
}
  

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
                        reactionID, " ?p ?o . \n",
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

getTransFromReact <- function(reaction, inference = FALSE, limit=0, endpoint="http://www.genome.jp/sparql/reactionontology/"){
  limitC = ""
  if (limit != 0)
    if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
      limitC = paste( " limit " , limit)
    else
      warning ("limit should be an integer, limit omitted from the query")
    
    sparql_base <- paste( "PREFIX transformation: <http://reactionontology.org/piero/transformation/> \n",
                          "SELECT DISTINCT * \n",
                          "WHERE { \n",
                          reaction, "piero:hasTransformation ?transformation . \n",
                          "?transformation rdfs:label ?transformation_name . \n",
                          "}  \n",
                          limitC, sep = " " )
    
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
  
  sparql_base <- paste( "SELECT DISTINCT ?kegg_reaction \n",
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

getReactSameTrans <- function(reaction,inference = FALSE, limit=0, endpoint="http://www.genome.jp/sparql/reactionontology/"){
  
  limitC = ""
  if (limit != 0)
  if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
    limitC = paste( " limit " , limit)
  else
    warning ("limit should be an integer, limit omitted from the query")
  
  sparql_base <- paste( "SELECT DISTINCT ?transformation_label ?kegg_reaction \n",
                        "WHERE { \n",
                        reactionID, "piero:hasTransformation ?transformation . \n",
                        "?transformation piero:hasKeggReaction ?kegg_reaction . \n",
                        "?transformation rdfs:label ?transformation_label . \n",
                        "} ORDER BY ?transformation_label ?kegg_reaction \n",
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

getReactFromRpair <- function(rpair, inference = FALSE, limit=0, endpoint="http://www.genome.jp/sparql/reactionontology/"){
  
  limitC = ""
  if (limit != 0)
    if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
      limitC = paste( " limit " , limit)
    else
      warning ("limit should be an integer, limit omitted from the query")
    
    sparql_base <- paste( "DEFINE input:inference 'http://reactionontology.org/inference' \n",
                          "SELECT DISTINCT ?kegg_rpair ?kegg_reaction \n",
                          "WHERE { \n",
                          "VALUES ?kegg_rpair {", rpair, "} . \n",
                          "?kegg_rpair ?p ?kegg_reaction . \n",
                          "?kegg_reaction rdf:type kegg:reaction . \n",
                          "} ORDER BY ?kegg_rpair ?kegg_reaction \n",
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

getEcFromRpair <- function(rpair, inference = FALSE, limit=0, endpoint="http://www.genome.jp/sparql/reactionontology/"){
  
  limitC = ""
  if (limit != 0)
    if (is.numeric(limit) && ! is.null (grep ("/.",limit)))
      limitC = paste( " limit " , limit)
    else
      warning ("limit should be an integer, limit omitted from the query")
    
    sparql_base <- paste( "DEFINE input:inference 'http://reactionontology.org/inference' \n",
                          "SELECT DISTINCT ?kegg_reaction, ?ec_number \n",
                          "WHERE { \n",
                          rpair, "piero:inReaction ?kegg_reaction . \n",
                          "?kegg_reaction piero:catalyzedBy ?gene_product . \n",
                          "?gene_product piero:mediates ?ec . \n",
                          "?ec rdfs:label ?ec_number . \n",
                          "} ORDER BY ?kegg_reaction ?ec_number \n",
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

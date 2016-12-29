# dependency
library(SPARQL)
library(ggplot2)
library(ggrepel)
library(plotly)
library(cluster)
library(ggfortify)

endpoint <- "http://localhost:3030/pi/query"
query <-
  "
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbp: <http://dbpedia.org/property/>
PREFIX dbr: <http://dbpedia.org/resource/>
PREFIX dbc: <http://dbpedia.org/resource/Category:>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX wil: <http://example.com/jumlahdanrasiodokterperawatterhadappuskesmas2015#>
PREFIX wi: <http://purl.org/ontology/wi/core#>
PREFIX db: <http://dbpedia.org/>

SELECT ?name ?puskes ?doktergigi ?dokterumum ?perawat ?bidan ?lat ?long ?area ?pop
{
  ?temp a wil:wilayah .
  ?temp owl:sameAs ?out .
  ?temp wil:label ?name .
  ?temp wil:jumlah_puskesmas ?puskes .
  ?temp wil:dokter_umum_puskesmas ?dokterumum .
  ?temp wil:dokter_gigi_puskesmas ?doktergigi .
  ?temp wil:perawat_puskesmas ?perawat .
  ?temp wil:bidan_puskesmas ?bidan .
  SERVICE <http://dbpedia.org/sparql>
  {SELECT * WHERE{
  ?out dbo:populationTotal ?pop .
  ?out dbp:areaTotalKm ?area .
  ?out geo:lat ?lat .
  ?out geo:long ?long .
  }}
}
"

# query from sparql endpoint
quedata <- SPARQL(endpoint,query)
# parsing result
puskesmas = quedata$results
df=puskesmas
puskesmasMatrix=as.matrix(cbind(df$puskes, df$dokterumum, df$doktergigi, df$perawat, df$bidan, df$pop, df$area),ncol=8)
puskesmasData = data.frame(puskesmasMatrix)
# execute
fit <- kmeans(puskesmasData, 5)

autoplot(fit, data = puskesmasData, label = TRUE)
ggplotly(label = puskesmas$name)

fit
fit$size
fit$withinss


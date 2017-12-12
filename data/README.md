
## Query for taxonomy

The `queryResults_phenoscape_taxonomy.csv` file was generated with this query:

```
PREFIX obo: <http://purl.obolibrary.org/obo/> PREFIX ps: <http://purl.org/phenoscape/vocab.owl#> PREFIX rdf: 
<http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX owl: 
<http://www.w3.org/2002/07/owl#> PREFIX BFO: <http://purl.obolibrary.org/obo/BFO_> PREFIX fin: 
<http://purl.obolibrary.org/obo/UBERON_0008897> PREFIX dc: <http://purl.org/dc/elements/1.1/> PREFIX hint: 
<http://www.bigdata.com/queryHints#> PREFIX pedal_digit_1: <http://purl.obolibrary.org/obo/UBERON_0003631> PREFIX 
pectoral_fin: <http://purl.obolibrary.org/obo/UBERON_0000151> PREFIX prehallux: 
<http://purl.obolibrary.org/obo/UBERON_0012136> PREFIX Anura: <http://purl.obolibrary.org/obo/VTO_0002210> PREFIX length: 
<http://purl.obolibrary.org/obo/PATO_0000122> PREFIX dwc: <http://rs.tdwg.org/dwc/terms/> PREFIX has_external_reference: 
<http://purl.obolibrary.org/obo/CDAO_0000164> SELECT DISTINCT ?matrix_taxon ?vto ?vto_label WHERE { ?otu rdfs:label 
?matrix_taxon . ?otu has_external_reference: ?vto . ?vto rdfs:label ?vto_label .
}
```

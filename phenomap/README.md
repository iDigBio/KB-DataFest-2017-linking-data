# Phenomap

Phenomap is an approach to present the current knowledge available In the 
Phenoscape Knowledgebase. It connects the available phenotypic information with
specimens from IDigBio and plots them in various ways to show the availability 
of knowledge and how it is distributed. This allows also  to visualize where 
there are gaps in our knowledge.

Currently, this is a demo mode. First, taxa need to be selected and then a trait
associated with them. The main part of the app shows three plots. The map 
presents the distribution of available specimens from IdigBio. The bar charts 
represent the range of specimens available based on genus level followed by the 
different traits and the trait value associated with it. The phylogeny is based 
on the current Open Tree of Life for the taxa available with their plotted trait
values at the tips.

For the future we aim to expand the shiny app in various ways. The app will 
hopefully presented on the Phenoscape Knowledgebase to give users the possibility 
to easily explore what is available. The selection of taxa and traits, as well as 
the connection to IDigBio is currently based on static files. We want to 
implement API queries to keep it up to date. Instead of selecting a taxon first, 
it shall be possible to select the trait first.
The information of what is available will be expanded in general. It would be 
nice to present the distribution range of the taxa in comparison to the specimen 
data, which will allow to compare the sampling of the species to the range. Based 
on the species there shall be hyper links to the IDigBio specimens and 
information where the specimens can be found in case people want to further 
analyze the data.

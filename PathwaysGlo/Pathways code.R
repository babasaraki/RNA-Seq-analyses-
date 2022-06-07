##DESeq results to pathways in 60 Seconds with the fgsea package

#Load results from DESeq2
library(tidyverse)
res <- read_csv("diffexpr-results.csv")
res

#Map Ensembl gene IDs to symbol. First create a mapping table.
library(org.Hs.eg.db)
ens2symbol <- AnnotationDbi::select(org.Hs.eg.db,
                                    key=res$row, 
                                    columns="SYMBOL",
                                    keytype="ENSEMBL")
ens2symbol <- as_tibble(ens2symbol)
ens2symbol

#Now join them
res <- inner_join(res, ens2symbol, by=c("row"="ENSEMBL"))
res

##Get gene symbol and the test statistic, remove the NAs and avarage multiple test statistics for the same symbol, 
res2 <- res %>% 
  dplyr::select(SYMBOL, stat) %>% 
  na.omit() %>% 
  distinct() %>% 
  group_by(SYMBOL) %>% 
  summarize(stat=mean(stat))
res2

#Using the fgsea package
library(fgsea)
#create a vector for test statistics 
ranks <- deframe(res2)
head(ranks, 20)

#Get the hallmark gene set from MSigDB into your project library.
# Load the pathways into a named list
pathways.hallmark <- gmtPathways("h.all.v6.2.symbols.gmt.txt")

# Look at them all if you want (uncomment)
# pathways.hallmark
# Show the first few pathways, and within those, show only the first few genes. 
pathways.hallmark %>% 
  head() %>% 
  lapply(head)

#Now, run the fgsea algorithm with 1000 permutations
fgseaRes <- fgsea(pathways=pathways.hallmark, stats=ranks, nperm=1000)

#Tidy the results
fgseaResTidy <- fgseaRes %>%
  as_tibble() %>%
  arrange(desc(NES))

# Show in a nice table:
fgseaResTidy %>% 
  dplyr::select(-leadingEdge, -ES, -nMoreExtreme) %>% 
  arrange(padj) %>% 
  DT::datatable()

#Plot the normalized enrichment scores. 
#Color the bar indicating whether or not the pathway was significant.
ggplot(fgseaResTidy, aes(reorder(pathway, NES), NES)) +
  geom_col(aes(fill=padj<0.05)) +
  coord_flip() +
  labs(x="Pathway", y="Normalized Enrichment Score",
       title="Hallmark pathways NES from GSEA") + 
  theme_minimal()

#Get the genes in each of these pathways 
pathways.hallmark %>% 
  enframe("pathway", "SYMBOL") %>% 
  unnest() %>% 
  inner_join(res, by="SYMBOL")

#GO annotations
fgsea(pathways=gmtPathways("c5.all.v6.2.symbols.gmt.txt"), ranks, nperm=1000) %>% 
  as_tibble() %>% 
  arrange(padj)
  
write.table(x = as.data.frame(fgseaRes), 
            file = "pathways-list.cvs", 
            sep = '\t', 
            quote = F,
            col.names = FALSE)



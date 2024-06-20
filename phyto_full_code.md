phyto_full_code
================
KB
2024-06-20

    ## Warning: package 'knitr' was built under R version 4.2.3

Rarefaction curves to determine subsampling level. 30000 reads chosen.
All samples used for analysis.

<img src="phyto_full_code_files/figure-gfm/rarefaction-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/rarefaction-2.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/rarefaction-3.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/Calculate alpha diversity [Richness, Shannon, Evenness]-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/Calculate alpha diversity [Richness, Shannon, Evenness]-2.png" style="display: block; margin: auto;" />

NMDS ordinations based on 16s data ANOSIM and ADONIS results shown below
done by Culture_type

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1829904 
    ## Run 1 stress 0.1830075 
    ## ... Procrustes: rmse 0.001247055  max resid 0.006856106 
    ## ... Similar to previous best
    ## Run 2 stress 0.1829904 
    ## ... Procrustes: rmse 1.626101e-05  max resid 8.719097e-05 
    ## ... Similar to previous best
    ## Run 3 stress 0.1829904 
    ## ... New best solution
    ## ... Procrustes: rmse 8.733041e-06  max resid 2.608909e-05 
    ## ... Similar to previous best
    ## Run 4 stress 0.1829904 
    ## ... New best solution
    ## ... Procrustes: rmse 4.661422e-06  max resid 2.551961e-05 
    ## ... Similar to previous best
    ## Run 5 stress 0.1857731 
    ## Run 6 stress 0.1830075 
    ## ... Procrustes: rmse 0.001247982  max resid 0.006853457 
    ## ... Similar to previous best
    ## Run 7 stress 0.1829904 
    ## ... Procrustes: rmse 2.317635e-06  max resid 9.845759e-06 
    ## ... Similar to previous best
    ## Run 8 stress 0.1829904 
    ## ... Procrustes: rmse 4.912041e-06  max resid 2.585871e-05 
    ## ... Similar to previous best
    ## Run 9 stress 0.1830075 
    ## ... Procrustes: rmse 0.001248131  max resid 0.006854391 
    ## ... Similar to previous best
    ## Run 10 stress 0.185915 
    ## Run 11 stress 0.1829904 
    ## ... Procrustes: rmse 5.688224e-06  max resid 3.10201e-05 
    ## ... Similar to previous best
    ## Run 12 stress 0.1829904 
    ## ... Procrustes: rmse 1.143114e-05  max resid 3.571795e-05 
    ## ... Similar to previous best
    ## Run 13 stress 0.185915 
    ## Run 14 stress 0.1829904 
    ## ... Procrustes: rmse 3.816719e-06  max resid 1.872174e-05 
    ## ... Similar to previous best
    ## Run 15 stress 0.1829904 
    ## ... New best solution
    ## ... Procrustes: rmse 2.841904e-06  max resid 1.075637e-05 
    ## ... Similar to previous best
    ## Run 16 stress 0.185915 
    ## Run 17 stress 0.185915 
    ## Run 18 stress 0.1829904 
    ## ... Procrustes: rmse 1.41393e-05  max resid 7.598458e-05 
    ## ... Similar to previous best
    ## Run 19 stress 0.1830075 
    ## ... Procrustes: rmse 0.001247245  max resid 0.00685251 
    ## ... Similar to previous best
    ## Run 20 stress 0.185915 
    ## *** Best solution repeated 3 times

    ## 
    ## Call:
    ## metaMDS(comm = veganifyOTU(physeq), distance = distance) 
    ## 
    ## global Multidimensional Scaling using monoMDS
    ## 
    ## Data:     wisconsin(sqrt(veganifyOTU(physeq))) 
    ## Distance: bray 
    ## 
    ## Dimensions: 2 
    ## Stress:     0.1829904 
    ## Stress type 1, weak ties
    ## Best solution was repeated 3 times in 20 tries
    ## The best solution was from try 15 (random start)
    ## Scaling: centring, PC rotation, halfchange scaling 
    ## Species: expanded scores based on 'wisconsin(sqrt(veganifyOTU(physeq)))'

<img src="phyto_full_code_files/figure-gfm/Compare beta diversity-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/Compare beta diversity-2.png" style="display: block; margin: auto;" />

    ## [1] 0.001

    ## [1] 0.4876747

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = Culture_type_ado ~ Culture_type, data = df_ado)
    ##              Df SumOfSqs      R2      F Pr(>F)    
    ## Culture_type  4   5.6698 0.26229 4.8888  0.001 ***
    ## Residual     55  15.9466 0.73771                  
    ## Total        59  21.6164 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## [1] 0.001

    ## [1] 0.452097

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = Cultivation_Stage_ado ~ Culture_type, data = df_ado)
    ##              Df SumOfSqs      R2      F Pr(>F)    
    ## Culture_type  4   5.6698 0.26229 4.8888  0.001 ***
    ## Residual     55  15.9466 0.73771                  
    ## Total        59  21.6164 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## phyloseq-class experiment-level object
    ## otu_table()   OTU Table:         [ 3487 taxa and 15 samples ]
    ## sample_data() Sample Data:       [ 15 samples by 20 sample variables ]
    ## tax_table()   Taxonomy Table:    [ 3487 taxa by 6 taxonomic ranks ]
    ## refseq()      DNAStringSet:      [ 3487 reference sequences ]

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1257047 
    ## Run 1 stress 0.1298163 
    ## Run 2 stress 0.1257047 
    ## ... Procrustes: rmse 5.120515e-06  max resid 9.083175e-06 
    ## ... Similar to previous best
    ## Run 3 stress 0.1848047 
    ## Run 4 stress 0.1257047 
    ## ... Procrustes: rmse 4.490904e-06  max resid 7.494049e-06 
    ## ... Similar to previous best
    ## Run 5 stress 0.1298163 
    ## Run 6 stress 0.1298163 
    ## Run 7 stress 0.1938036 
    ## Run 8 stress 0.1298163 
    ## Run 9 stress 0.1257047 
    ## ... Procrustes: rmse 9.337857e-06  max resid 1.548054e-05 
    ## ... Similar to previous best
    ## Run 10 stress 0.1298163 
    ## Run 11 stress 0.3312718 
    ## Run 12 stress 0.1257047 
    ## ... Procrustes: rmse 1.327703e-06  max resid 2.302098e-06 
    ## ... Similar to previous best
    ## Run 13 stress 0.1257047 
    ## ... Procrustes: rmse 1.589118e-06  max resid 3.218158e-06 
    ## ... Similar to previous best
    ## Run 14 stress 0.1257047 
    ## ... Procrustes: rmse 2.133494e-05  max resid 3.98379e-05 
    ## ... Similar to previous best
    ## Run 15 stress 0.1257047 
    ## ... Procrustes: rmse 1.293149e-05  max resid 2.849245e-05 
    ## ... Similar to previous best
    ## Run 16 stress 0.1298163 
    ## Run 17 stress 0.1298163 
    ## Run 18 stress 0.1871934 
    ## Run 19 stress 0.1298163 
    ## Run 20 stress 0.1298163 
    ## *** Best solution repeated 7 times

    ## 
    ## Call:
    ## metaMDS(comm = veganifyOTU(physeq), distance = distance) 
    ## 
    ## global Multidimensional Scaling using monoMDS
    ## 
    ## Data:     wisconsin(sqrt(veganifyOTU(physeq))) 
    ## Distance: bray 
    ## 
    ## Dimensions: 2 
    ## Stress:     0.1257047 
    ## Stress type 1, weak ties
    ## Best solution was repeated 7 times in 20 tries
    ## The best solution was from try 0 (metric scaling or null solution)
    ## Scaling: centring, PC rotation, halfchange scaling 
    ## Species: expanded scores based on 'wisconsin(sqrt(veganifyOTU(physeq)))'

<img src="phyto_full_code_files/figure-gfm/Compare beta diversity Lag-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/Compare beta diversity Lag-2.png" style="display: block; margin: auto;" />

    ## [1] 0.001

    ## [1] 0.9112426

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = Culture_type_ado ~ Culture_type, data = df_ado)
    ##              Df SumOfSqs      R2      F Pr(>F)    
    ## Culture_type  4  0.97484 0.83773 11.616  0.001 ***
    ## Residual      9  0.18883 0.16227                  
    ## Total        13  1.16367 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## phyloseq-class experiment-level object
    ## otu_table()   OTU Table:         [ 3487 taxa and 15 samples ]
    ## sample_data() Sample Data:       [ 15 samples by 20 sample variables ]
    ## tax_table()   Taxonomy Table:    [ 3487 taxa by 6 taxonomic ranks ]
    ## refseq()      DNAStringSet:      [ 3487 reference sequences ]

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1203087 
    ## Run 1 stress 0.1618468 
    ## Run 2 stress 0.1203087 
    ## ... Procrustes: rmse 8.802202e-07  max resid 1.88394e-06 
    ## ... Similar to previous best
    ## Run 3 stress 0.1615754 
    ## Run 4 stress 0.1202427 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0237894  max resid 0.06341365 
    ## Run 5 stress 0.1202427 
    ## ... New best solution
    ## ... Procrustes: rmse 7.459615e-07  max resid 1.411457e-06 
    ## ... Similar to previous best
    ## Run 6 stress 0.1599023 
    ## Run 7 stress 0.1599023 
    ## Run 8 stress 0.1596523 
    ## Run 9 stress 0.1202427 
    ## ... Procrustes: rmse 4.956447e-06  max resid 1.013371e-05 
    ## ... Similar to previous best
    ## Run 10 stress 0.1202427 
    ## ... New best solution
    ## ... Procrustes: rmse 2.861137e-06  max resid 5.979226e-06 
    ## ... Similar to previous best
    ## Run 11 stress 0.1596794 
    ## Run 12 stress 0.1203087 
    ## ... Procrustes: rmse 0.02378788  max resid 0.06317246 
    ## Run 13 stress 0.1203087 
    ## ... Procrustes: rmse 0.0237878  max resid 0.06317167 
    ## Run 14 stress 0.1599022 
    ## Run 15 stress 0.1596794 
    ## Run 16 stress 0.1596794 
    ## Run 17 stress 0.1638513 
    ## Run 18 stress 0.1203087 
    ## ... Procrustes: rmse 0.02378794  max resid 0.06317163 
    ## Run 19 stress 0.1202427 
    ## ... New best solution
    ## ... Procrustes: rmse 1.222053e-06  max resid 2.262283e-06 
    ## ... Similar to previous best
    ## Run 20 stress 0.1203087 
    ## ... Procrustes: rmse 0.023788  max resid 0.06317378 
    ## *** Best solution repeated 1 times

    ## 
    ## Call:
    ## metaMDS(comm = veganifyOTU(physeq), distance = distance) 
    ## 
    ## global Multidimensional Scaling using monoMDS
    ## 
    ## Data:     wisconsin(sqrt(veganifyOTU(physeq))) 
    ## Distance: bray 
    ## 
    ## Dimensions: 2 
    ## Stress:     0.1202427 
    ## Stress type 1, weak ties
    ## Best solution was repeated 1 time in 20 tries
    ## The best solution was from try 19 (random start)
    ## Scaling: centring, PC rotation, halfchange scaling 
    ## Species: expanded scores based on 'wisconsin(sqrt(veganifyOTU(physeq)))'

<img src="phyto_full_code_files/figure-gfm/Compare beta diversity Exponential-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/Compare beta diversity Exponential-2.png" style="display: block; margin: auto;" />

    ## [1] 0.001

    ## [1] 0.9274074

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = Culture_type_ado ~ Culture_type, data = df_ado)
    ##              Df SumOfSqs      R2      F Pr(>F)    
    ## Culture_type  4   3.5232 0.77584 8.6525  0.001 ***
    ## Residual     10   1.0180 0.22416                  
    ## Total        14   4.5411 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## phyloseq-class experiment-level object
    ## otu_table()   OTU Table:         [ 3487 taxa and 15 samples ]
    ## sample_data() Sample Data:       [ 15 samples by 20 sample variables ]
    ## tax_table()   Taxonomy Table:    [ 3487 taxa by 6 taxonomic ranks ]
    ## refseq()      DNAStringSet:      [ 3487 reference sequences ]

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1625297 
    ## Run 1 stress 0.1803734 
    ## Run 2 stress 0.1962825 
    ## Run 3 stress 0.1692352 
    ## Run 4 stress 0.1625299 
    ## ... Procrustes: rmse 0.0005136513  max resid 0.001165333 
    ## ... Similar to previous best
    ## Run 5 stress 0.1625297 
    ## ... Procrustes: rmse 0.0001229774  max resid 0.0002824029 
    ## ... Similar to previous best
    ## Run 6 stress 0.2087158 
    ## Run 7 stress 0.2087153 
    ## Run 8 stress 0.1625297 
    ## ... Procrustes: rmse 8.0126e-05  max resid 0.0001846096 
    ## ... Similar to previous best
    ## Run 9 stress 0.2087154 
    ## Run 10 stress 0.1625296 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0001472935  max resid 0.0003331948 
    ## ... Similar to previous best
    ## Run 11 stress 0.1692353 
    ## Run 12 stress 0.1625298 
    ## ... Procrustes: rmse 0.0003366188  max resid 0.0007604344 
    ## ... Similar to previous best
    ## Run 13 stress 0.1625296 
    ## ... Procrustes: rmse 5.946303e-05  max resid 0.0001318217 
    ## ... Similar to previous best
    ## Run 14 stress 0.1625299 
    ## ... Procrustes: rmse 0.0003761753  max resid 0.0008578681 
    ## ... Similar to previous best
    ## Run 15 stress 0.1625297 
    ## ... Procrustes: rmse 0.0002041567  max resid 0.000461203 
    ## ... Similar to previous best
    ## Run 16 stress 0.1625297 
    ## ... Procrustes: rmse 0.0002676634  max resid 0.0006066426 
    ## ... Similar to previous best
    ## Run 17 stress 0.1692352 
    ## Run 18 stress 0.254755 
    ## Run 19 stress 0.1625298 
    ## ... Procrustes: rmse 0.0002492477  max resid 0.0005753141 
    ## ... Similar to previous best
    ## Run 20 stress 0.1692352 
    ## *** Best solution repeated 7 times

    ## 
    ## Call:
    ## metaMDS(comm = veganifyOTU(physeq), distance = distance) 
    ## 
    ## global Multidimensional Scaling using monoMDS
    ## 
    ## Data:     wisconsin(sqrt(veganifyOTU(physeq))) 
    ## Distance: bray 
    ## 
    ## Dimensions: 2 
    ## Stress:     0.1625296 
    ## Stress type 1, weak ties
    ## Best solution was repeated 7 times in 20 tries
    ## The best solution was from try 10 (random start)
    ## Scaling: centring, PC rotation, halfchange scaling 
    ## Species: expanded scores based on 'wisconsin(sqrt(veganifyOTU(physeq)))'

<img src="phyto_full_code_files/figure-gfm/Compare beta diversity Stationary-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/Compare beta diversity Stationary-2.png" style="display: block; margin: auto;" />

    ## [1] 0.001

    ## [1] 0.7437037

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = Culture_type_ado ~ Culture_type, data = df_ado)
    ##              Df SumOfSqs      R2   F Pr(>F)    
    ## Culture_type  4   3.0669 0.59678 3.7  0.001 ***
    ## Residual     10   2.0722 0.40322               
    ## Total        14   5.1392 1.00000               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## phyloseq-class experiment-level object
    ## otu_table()   OTU Table:         [ 3487 taxa and 15 samples ]
    ## sample_data() Sample Data:       [ 15 samples by 20 sample variables ]
    ## tax_table()   Taxonomy Table:    [ 3487 taxa by 6 taxonomic ranks ]
    ## refseq()      DNAStringSet:      [ 3487 reference sequences ]

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1536091 
    ## Run 1 stress 0.2005711 
    ## Run 2 stress 0.154246 
    ## Run 3 stress 0.2035061 
    ## Run 4 stress 0.1901275 
    ## Run 5 stress 0.1536091 
    ## ... Procrustes: rmse 8.15616e-06  max resid 1.524019e-05 
    ## ... Similar to previous best
    ## Run 6 stress 0.1536091 
    ## ... New best solution
    ## ... Procrustes: rmse 9.725472e-06  max resid 1.897952e-05 
    ## ... Similar to previous best
    ## Run 7 stress 0.2368596 
    ## Run 8 stress 0.154246 
    ## Run 9 stress 0.1536091 
    ## ... Procrustes: rmse 1.101181e-05  max resid 1.974409e-05 
    ## ... Similar to previous best
    ## Run 10 stress 0.1901275 
    ## Run 11 stress 0.1536091 
    ## ... Procrustes: rmse 3.158383e-06  max resid 6.116807e-06 
    ## ... Similar to previous best
    ## Run 12 stress 0.1654816 
    ## Run 13 stress 0.1536091 
    ## ... Procrustes: rmse 7.193429e-06  max resid 1.366659e-05 
    ## ... Similar to previous best
    ## Run 14 stress 0.1654816 
    ## Run 15 stress 0.1536091 
    ## ... Procrustes: rmse 1.093595e-05  max resid 2.106068e-05 
    ## ... Similar to previous best
    ## Run 16 stress 0.1536091 
    ## ... Procrustes: rmse 1.706916e-05  max resid 3.446483e-05 
    ## ... Similar to previous best
    ## Run 17 stress 0.1823011 
    ## Run 18 stress 0.1823011 
    ## Run 19 stress 0.1536091 
    ## ... Procrustes: rmse 2.391025e-05  max resid 4.849492e-05 
    ## ... Similar to previous best
    ## Run 20 stress 0.168745 
    ## *** Best solution repeated 7 times

    ## 
    ## Call:
    ## metaMDS(comm = veganifyOTU(physeq), distance = distance) 
    ## 
    ## global Multidimensional Scaling using monoMDS
    ## 
    ## Data:     wisconsin(sqrt(veganifyOTU(physeq))) 
    ## Distance: bray 
    ## 
    ## Dimensions: 2 
    ## Stress:     0.1536091 
    ## Stress type 1, weak ties
    ## Best solution was repeated 7 times in 20 tries
    ## The best solution was from try 6 (random start)
    ## Scaling: centring, PC rotation, halfchange scaling 
    ## Species: expanded scores based on 'wisconsin(sqrt(veganifyOTU(physeq)))'

<img src="phyto_full_code_files/figure-gfm/Compare beta diversity Death-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/Compare beta diversity Death-2.png" style="display: block; margin: auto;" />

    ## [1] 0.001

    ## [1] 0.8325926

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = Culture_type_ado ~ Culture_type, data = df_ado)
    ##              Df SumOfSqs     R2      F Pr(>F)    
    ## Culture_type  4   2.5962 0.5939 3.6561  0.001 ***
    ## Residual     10   1.7753 0.4061                  
    ## Total        14   4.3715 1.0000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

<img src="phyto_full_code_files/figure-gfm/distance comparisons Lag-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/distance comparisons Exponential-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/distance comparisons Stationary-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/distance comparisons Death-1.png" style="display: block; margin: auto;" />

Stacked bar plot based on phylum (draft and clean version)

<img src="phyto_full_code_files/figure-gfm/stack bar phylum-1.png" style="display: block; margin: auto;" />

Stacked bar plot based on Order (clean version)

<img src="phyto_full_code_files/figure-gfm/stack bar Order-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/All significantly affected taxa plot Phylum-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/All significantly affected taxa plot Order-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/All significantly affected taxa plot Family-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/All significantly affected taxa plot Genus-1.png" style="display: block; margin: auto;" />

<img src="phyto_full_code_files/figure-gfm/old taxa plot code-1.png" style="display: block; margin: auto;" /><img src="phyto_full_code_files/figure-gfm/old taxa plot code-2.png" style="display: block; margin: auto;" />

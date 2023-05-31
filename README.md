# Census of research integrity advisors in Australia

Analysis of the questions sent to Research Integrity Advisors in Australia in early 2023.

The main folder contains the R and Rmarkdown code needed to read in the raw data and run the analyses.

The `rdata` folder contains:
* 0_emails.RData 
* 3_AnalysisReady.RData 
* flow_numbers.RData 

The `figures` folder contains the key figures used in the paper.

The `results` folder contains the modelling results for x and y.

The `checklist` folder contains the EQUATOR reporting checklists.

The protocol, questions, and pre-print are available [here](https://osf.io/sptcg/) on the Open Science Framework (DOI 10.17605/OSF.IO/SPTCG).

### R and package versions

```{r}
> sessionInfo()
R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
[3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] egg_0.4.5         gridExtra_2.3     ggplot2_3.3.6     MultinomialCI_1.2
 [5] diagram_1.6.5     shape_1.4.6       naniar_0.6.1      visdat_0.5.3     
 [9] flextable_0.7.0   stringr_1.4.0     janitor_2.1.0     tidyr_1.2.0      
[13] dplyr_1.0.9      

loaded via a namespace (and not attached):
 [1] tidyselect_1.1.2   xfun_0.31          purrr_0.3.4        snakecase_0.11.0  
 [5] colorspace_2.0-3   vctrs_0.4.1        generics_0.1.2     htmltools_0.5.2   
 [9] base64enc_0.1-3    utf8_1.2.2         rlang_1.0.2        pillar_1.7.0      
[13] glue_1.6.2         withr_2.5.0        DBI_1.1.2          gdtools_0.2.4     
[17] uuid_1.1-0         lifecycle_1.0.1    munsell_0.5.0      gtable_0.3.0      
[21] ragg_1.2.2         zip_2.2.0          evaluate_0.15      labeling_0.4.2    
[25] knitr_1.39         fastmap_1.1.0      fansi_1.0.3        Rcpp_1.0.8.3      
[29] scales_1.2.0       farver_2.1.0       systemfonts_1.0.4  textshaping_0.3.6 
[33] TeachingDemos_2.12 digest_0.6.29      stringi_1.7.8      grid_4.2.1        
[37] cli_3.3.0          tools_4.2.1        magrittr_2.0.3     tibble_3.1.7      
[41] crayon_1.5.1       pkgconfig_2.0.3    ellipsis_0.3.2     data.table_1.14.2 
[45] xml2_1.3.3         lubridate_1.8.0    assertthat_0.2.1   rmarkdown_2.14    
[49] officer_0.4.2      rstudioapi_0.13    R6_2.5.1           compiler_4.2.1  
```

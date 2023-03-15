setwd('D:\\Thesissen 2020\\Mathijs\\Thesis_finaal')

load('TrainingBasetableFinal.Rdata')

Basetable2 %>% skim()

Basetable_tussen <- do.call(data.frame,
                      lapply(Basetable2,
                             function(x) replace(x, is.infinite(x), NA)))

rider <- Basetable_tussen$Rider
Basetable_tussen$Rider <- NULL

Basetable_tussen <- Basetable_tussen %>% mutate_if(is.factor, as.character) %>% mutate_if(is.character, as.numeric)
Basetable_tussen$Rider <- rider

Basetable_tussen %>% skim()

Basetable <- Basetable_tussen

save(Basetable, file = 'TrainingBasetableWithoutInfiniteValues.Rdata')

load('SampleForDeploymentFinal.Rdata')  

Basetable3 %>% skim()

deployment_set <- do.call(data.frame,
                            lapply(Basetable3,
                                   function(x) replace(x, is.infinite(x), NA)))
deployment_set %>% skim()

save(deployment_set, file = 'SampleForDeploymentWithoutInfiniteValues.Rdata')

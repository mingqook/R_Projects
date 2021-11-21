
#기존 119개 변수에서 필터링을 거쳐 35개로 축소된 변수를 'phenotype'으로 저장(weigh.exposure, age_at_index.demgrapic 제거)
#데이터를 필터링 할떄 빈칸,'not reported'도 NA로 처리함
require(mice)
pheno <- read.delim2('TCGA-LIHC.GDC_phenotype.tsv', header = T, na.strings = c('','NA','not reported'))
attach(pheno)
pheno_1 <- subset(pheno, select = c(adjacent_hepatic_tissue_inflammation_extent_type,
                                    age_at_initial_pathologic_diagnosis, albumin_result_lower_limit,albumin_result_specified_value,
                                    albumin_result_upper_limit, bilirubin_lower_limit, bilirubin_upper_limit, child_pugh_classification_grade, 
                                    creatinine_lower_level, creatinine_upper_limit, creatinine_value_in_mg_dl, fetoprotein_outcome_lower_limit,
                                    fetoprotein_outcome_upper_limit,fetoprotein_outcome_value,fibrosis_ishak_score,inter_norm_ratio_lower_limit,
                                    intern_norm_ratio_upper_limit,neoplasm_histologic_grade,platelet_result_count,platelet_result_lower_limit,
                                    platelet_result_upper_limit,post_op_ablation_embolization_tx,postoperative_rx_tx,
                                    prothrombin_time_result_value,radiation_therapy,total_bilirubin_upper_limit,vascular_tumor_cell_type,
                                    ethnicity.demographic,gender.demographic,race.demographic,age_at_diagnosis.diagnoses,bmi.exposures,
                                    height.exposures,weight,tumor_stage.diagnoses
                                    ))
write.table(pheno_1,'pheno_1')
pt<- read.table('pheno_1', header = T, na.strings = c('','NA','not reported'))
detach(pheno)
#outliar 처리
pt$albumin_result_lower_limit <- ifelse(pt$albumin_result_lower_limit >= 30, NA, pt$albumin_result_lower_limit)
pt$albumin_result_lower_limit <- ifelse(pt$albumin_result_lower_limit <= 0.5, NA, pt$albumin_result_lower_limit)
pt$albumin_result_upper_limit <- ifelse(pt$albumin_result_upper_limit >= 10, NA, pt$albumin_result_upper_limit)
pt$albumin_result_upper_limit <- ifelse(pt$albumin_result_upper_limit <= 1, NA, pt$albumin_result_upper_limit)
pt$albumin_result_specified_value <- ifelse(pt$albumin_result_specified_value >= 30, NA, pt$albumin_result_specified_value)
pt$bilirubin_lower_limit <- ifelse(pt$bilirubin_lower_limit >= 1, NA, pt$bilirubin_lower_limit)
pt$bilirubin_upper_limit <- ifelse(pt$bilirubin_upper_limit >= 5, NA, pt$bilirubin_upper_limit)
pt$creatinine_lower_level <- ifelse(pt$creatinine_lower_level >=3 , NA, pt$creatinine_lower_level)
pt$creatinine_upper_limit <- ifelse(pt$creatinine_upper_limit >=4 , NA, pt$creatinine_upper_limit)
pt$creatinine_value_in_mg_dl <- ifelse(pt$creatinine_value_in_mg_dl >=4 , NA, pt$creatinine_value_in_mg_dl)
pt$fetoprotein_outcome_lower_limit<- ifelse(pt$fetoprotein_outcome_lower_limit >=5 , NA, pt$fetoprotein_outcome_lower_limit)
pt$fetoprotein_outcome_upper_limit <- ifelse(pt$fetoprotein_outcome_upper_limit >=30 , NA, pt$fetoprotein_outcome_upper_limit)
pt$fetoprotein_outcome_value <- ifelse(pt$fetoprotein_outcome_value >=10000 , NA, pt$fetoprotein_outcome_value)
pt$platelet_result_count <- ifelse(pt$platelet_result_count >=50000 , NA, pt$platelet_result_count)
pt$platelet_result_lower_limit <- ifelse(pt$platelet_result_lower_limit >=100000 , NA, pt$platelet_result_lower_limit)
pt$platelet_result_lower_limit <- ifelse(pt$platelet_result_lower_limit <=1 , NA, pt$platelet_result_lower_limit)
pt$platelet_result_upper_limit <- ifelse(pt$platelet_result_upper_limit >=200000 , NA, pt$platelet_result_upper_limit)
pt$platelet_result_upper_limit <- ifelse(pt$platelet_result_upper_limit <=10 , NA, pt$platelet_result_upper_limit)
pt$total_bilirubin_upper_limit <- ifelse(pt$total_bilirubin_upper_limit >=15 , NA, pt$total_bilirubin_upper_limit)
pt$bmi.exposures <- ifelse(pt$bmi.exposures >=100 , NA, pt$bmi.exposures)
#전처리 전후 NA값 비교(263개)
raw_pt<- read.table('pheno_1', header = T, na.strings = c('','NA','not reported'))
D <- sum(is.na(pt)) -sum(is.na(raw_pt))
#box plot으로 outliar를 확인
par(mfrow = c(4,4))
boxplot(pt$albumin_result_lower_limit, xlab="albumin_result_lower_limit") 
boxplot(pt$albumin_result_specified_value, xlab="albumin_result_specified_value")
boxplot(pt$albumin_result_upper_limit, xlab="albumin_result_upper_limit")
boxplot(pt$bilirubin_lower_limit, xlab="bilirubin_lower_limit")
boxplot(pt$bilirubin_upper_limit, xlab="bilirubin_upper_limit")
boxplot(pt$creatinine_lower_level, xlab="creatinine_lower_level")
boxplot(pt$creatinine_upper_limit, xlab="creatinine_upper_limit")
boxplot(pt$creatinine_value_in_mg_dl, xlab="creatinine_value_in_mg_dl")
boxplot(pt$fetoprotein_outcome_lower_limit, xlab="fetoprotein_outcome_lower_limit")
boxplot(pt$fetoprotein_outcome_upper_limit, xlab="fetoprotein_outcome_upper_limit")
boxplot(pt$fetoprotein_outcome_value, xlab="fetoprotein_outcome_value")
boxplot(pt$platelet_result_count, xlab="platelet_result_count")
boxplot(pt$platelet_result_lower_limit, xlab="platelet_result_lower_limit")
boxplot(pt$platelet_result_upper_limit, xlab="platelet_result_upper_limit")
boxplot(pt$total_bilirubin_upper_limit, xlab="total_bilirubin_upper_limit")
boxplot(pt$bmi.exposures, xlab="bmi.exposures")
#아웃라이어를 제거해보니 fetoprotein group이 이상해서 2가지로 multivarate imputation chain equation 진행
#multivariate imputaion by chain equatic : mice(데이터, m = 출력하고 싶은 imputation dataset 갯수, maxit = 최대 반복횟수)
pt_1 <- subset(pt, select = -fetoprotein_outcome_lower_limit)
pt_2 <- subset(pt, select = c(-fetoprotein_outcome_lower_limit, -fetoprotein_outcome_upper_limit,-fetoprotein_outcome_value))
imp_1 <- mice(pt_1, m=5, maxit = 50)
imp_2 <- mice(pt_2, m=5, maxit = 50)
#imputation dataset export : complete(mids인자, 5개 imputation datatset중 export하고 싶은 dataset 번호)
feto_data <- complete(imp_1, 1) 
nofeto_data <- complete(imp_2, 1)
write.table(feto_data, 'feto_data')
write.table(feto_data, 'nofeto_data')
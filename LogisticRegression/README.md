## 로지스틱 회귀분석

### cleansing.r

* 분석에 필요 없는 feature 제거

```r
# 의미 없는 값이거나 (예: 년도나 ID) 
# 분석이 어려운, 패턴이 없는 문자열 값인 경우 (예: BM14_2, 구강 진료를 받지 못한 상세 이유) 제거

# 결측치가 포함된 행 제거

# 결과
dim(cleaned_data)
#[1] 2699  586
```



### feature_engineering.r

* 앞서 한 차례 feature를 제거했음에도 feature가 너무 많음(600개 가량)

* 차원의 저주 현상이 일어날 수 있으므로 feature selection이 필요함

  

* 로지스틱 회귀 모델(glm())로 Wrapper 방법 중 하나인 Forward Selection(전진 선택) 진행

```r
# 결과

dim(selected_data)
# [1] 2699   65
```

* 설문조사 결과가 "모름" 인 경우를 missing value로 간주하고 최빈값으로 대체

  

* 다중공선성이 높은 feature들 제거

  ```r
   vif(glm_fit)
  
  # LQ4_07 2821.131467 활동제한 사유: 당뇨병 -> 제거
  # LQ4_05 2358.953265 활동제한 사유: 호흡 문제 -> 제거
  # LQ4_13 2550.799553 활동제한 사유: 청각 문제
  # DH4_dg 22.327465 중이염 의사진단 여부
  # DH4_ag 22.283937 중이염 진단시기 -> 제거
  # DE1_pt 1372.532445 당뇨병 치료
  # DE1_ag 1373.196667 당뇨병 진단시기 -> 제거
  # MO4_11 164.399678 진료항목: 발치 또는 구강내수술 -> 제거
  # MO4_12 164.006320 진로항목: 다쳐서 빠지거나 부러진 치아 치료
  # DH3_ag 73.950212 녹내장 진단시기 -> 제거
  # DH3_pt 73.703535 녹내장 치료
  # DF2_pr 210.309755 우울증 현재 유병 여부
  # DF2_ag 211.212132 우울증 진단 시기 -> 제거
  ```

  

* 클래스 불균형 데이터 문제 해결 위해 언더 샘플링 진행

  ```r
  table(selected_data$danger)
  #    0    1 
  # 2212  487 
  
  # ...
  
  # 결과
  table(selected_data$danger)
  #   0   1 
  # 480 480 
  dim(selected_data)
  # [1] 960  58
  ```



### modeling.r

* 분석에 앞서 train, test 데이터세트 나눔

  ```r
  dim(train_dataset)
  # [1] 768  65
  dim(test_dataset)
  # [1] 192  65
  ```

* embedded 기법인 lasso 모델링

  * 교차검증 실시, alpha =1, nfolds =5
  * cv.glmnet()은 교차검증을 할때 오차를 다양하게 정할 수 있음(auc, mse, misclassification error ...)
  * auc를 확인하기 위해 type measure(오차)를 `auc`, `mse`로 둬서 두 번 돌렸음.

  ```r
  # 결과
  
  table(real = test_dataset[,length(test_dataset)],pred= factor((lasso_pred), levels = c(1,0)))
  
  #     pred
  # real  1  0
  #    1 64 32
  #    0 29 67
  
  confusionMatrix(table(factor(test_dataset[,length(test_dataset)], levels=c(1,0)),factor((lasso_pred), levels = c(1,0))))
  
  #                Accuracy : 0.6406          
  #                  95% CI : (0.5684, 0.7084)
  #     No Information Rate : 0.5156 
  #     P-Value [Acc > NIR] : 0.0003158       
                                            
  #                   Kappa : 0.2812          
                                            
  #  Mcnemar's Test P-Value : 0.8097321       
                                            
  #             Sensitivity : 0.6364          
  #             Specificity : 0.6452          
  #          Pos Pred Value : 0.6562          
  #          Neg Pred Value : 0.6250          
  #              Prevalence : 0.5156          
  #          Detection Rate : 0.3281          
  #    Detection Prevalence : 0.5000          
  #       Balanced Accuracy : 0.6408          
                                            
  #        'Positive' Class : 1
  ```

  
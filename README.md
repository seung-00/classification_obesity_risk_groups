# Classification Obesity Risk Groups

## 개요

*  경희대 데이터마이닝 텀프로젝트 

   *  [제출한 보고서](https://drive.google.com/file/d/1VnCOygnr5cw5ArcJROciQdSaEbkZEDpT/view?usp=sharing)

*  “체중이 증가하는 사람들의 특징들을 파악해 이를 바탕으로 사전에 비만 위험군을 분류할 수 있다” 라는 가설을 세우고 이를 증명하는 분석을 수행함

* 국민건강영양조사 데이터의 "모름" 응답 값을 대체하는 경우, 성능에 어떤 영향을 주는지 두 번의 실험을 통해 비교함

  

## EDA

* 사용 데이터: 국민건강영양조사(2016, 2017, 2018)

* 전국민을 대상으로한 설문조사 및 검사 결과로 numerical variable과 categorical variable이 섞여 있음.

  ```r
  > dim(dm_df)
  [1] 7992  737
  ```

* 국민건강영양조사 데이터에는 "모름" 응답 값이 존재하며 변수에 따라 값이 달라짐.
* 본 분석에서는 2016~2018 년도 데이터의 공통 변수들만을 분석에 사용함
  
  * [integration.r](https://github.com/seung-00/classification_obesity_risk_groups/blob/master/LogisticRegression/integration.r)

## 과정

### 전처리

* 반응 변수 danger 생성

  * 본 분석의 반응 변수는 정상 체중을 가진 사람이 1년 사이 비만이 되었다면 위험군, 정상 체중을 유지했다면 정상군으로 분류
  * 현재 체중 변수(HE_wt)와 1년간 체중 증가량 변수(B01_3)로 비만 표본들의 과거 체중 변수(past_wt)를 생성 
  * 이를 바탕으로 과거에 정상군이었지만 현재 비만인 경우를 위험군(danger=1) 
  * 현재와 과거 모두 정상 체중인 표본을 정상군(dagner=0)으로 정의

* "모름" 데이터 처리

  * "모름" 변수 값을 조사함
  * 범주형인 경우에는 최빈값으로, 수치형인 경우에는 평균값으로 대체

* 혼동 변수 제거

  * 해당 값이 일정한 패턴이 없는 문자열 값인 경우 (예: 구강 진료를 받지 못한 상세 이유)
  * 비만 유병여부, BMI, 혈당과 같은 신체 상태와 관련된 변수들(변수명에 "HE" 포함됨)
  * 설문조사 가중치 값(변수명에 "wt"가 포함됨)
  * 결측치가 2000개 이상 존재하는 변수

  ```r
  dim(cleaned_data)
  [1] 7626  477
  ```

* 랜덤 언더샘플링, 628개로 클래스 불균형 해결

### 모델링 및 평가

* 8:2로 훈련, 테스트 데이터 세트 구성
* 전진 선택
  * 선택된 변수들로 VIF 계산, 다중공선성 확인
* lasso 로지스틱 회귀 모형
* 10-fold 교차검증
  * loss: AUC
  * 1-표준편차 범위에서 최적의 lambda 선택
* confusion matrix
  * 정확도, 민감도, 특이도 계산
* ROC Curve
  * AUC 계산



## 결과

### "모름" 응답 값 대체 이전

* 전진선택 이후 22개 변수에서 다중공선성이 나타남

* Confusion Matrix

  |                  | Predictive  Positive | Predictive  Negative |
  | ---------------- | -------------------- | -------------------- |
  | Actual  Positive | 73                   | 52                   |
  | Actual  Negative | 44                   | 81                   |
  * 정확도: 0.616, 민감도: 0.624, 특이도: 0.610

* ROC Curve

  <img src="https://user-images.githubusercontent.com/46865281/86350194-4c0c8e00-bc9d-11ea-966d-74e83ccfec2e.png" width="250" height="250">

  

  * AUC: 0.703

    

### "모름" 응답 값 대체 이후

* "모름 데이터를 처리한 경우 전진선택을 했을 때 VIF 값이 10을 넘는 변수가 없었음.

* Confusion Matrix

  |                  | Predictive  Positive | Predictive  Negative |
  | ---------------- | -------------------- | -------------------- |
  | Actual  Positive | 81                   | 44                   |
  | Actual  Negative | 41                   | 84                   |
  * 정확도: 0.66, 민감도: 0.664, 특이도: 0.656

* ROC Curve

  <img src="https://user-images.githubusercontent.com/46865281/86350751-1c11ba80-bc9e-11ea-9d4a-6b12807851f6.png" width="250" height="250">
  * AUC: 0.722
# Classification Obesity Risk Groups

Kyung-Hee Univ. Dataminig Term-Project



### EDA

* 사용 데이터: 국민건강영양조사(2018)

  ```r
  > dim(dm_df)
  [1] 7992  737
  ```

  

### feature generation

* 비만 데이터 중(`HE_obe>2`) 중 최근 1년 간 체중 증가가 있었던 데이터(`BO1_1 == 3`) 비만 위험군으로 가정하고 새로운 binary feature `danger` 생성
  * 이를 종속 변수로 사용
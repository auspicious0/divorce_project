install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)


# 데이터 불러오기
df <- read_excel("시도_일반이혼율_20230501170138.xlsx", sheet = "데이터")
df<-as_tibble(df,locale=locale("ko,encoding=EUC_KR"),na=".")

# 데이터 프레임 재구성
df_new <- df %>% 
  pivot_longer(cols = -시도별, names_to = "성별", values_to = "이혼률") %>% 
  mutate(성별 = if_else(성별 == "2022...2", "남편", "아내"))

# 그래프 그리기
ggplot(df_new, aes(x=시도별, y=이혼률, fill=성별)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("#f8766d", "#00ba38")) +
  ggtitle("도시별, 남편, 아내 별 이혼률") +
  labs(x="시도별", y="이혼률(%)")


#-------------------------------------------아래부터 추가된 코드

print(df_new)
# 결측치와 무한대 값 제거
df_new <- na.omit(df_new)

#필요 없는 값 제거 ex 전국이란 텍스트는 숫자를 비교할 때 오류가 된다.
df_new <- df_new[5:nrow(df_new), ]
df_new <- df_new %>%
  group_by(시도별) %>%
  summarise(이혼률 = mean(as.numeric(이혼률)))
#귀무가설 귀무가설(H0): 시에 산다면 이혼률이 낮지 않다를 선형 회귀 분석하기 위하여 이혼률을 작은 순서에서 정렬하였다. 작은 순서에서 차례대로 적용하여 시는 더 작은 순서에 배정하고 도는 그 뒤 순서에 배정하여 실제로 시에 산다면 도에 사는 것 보다 이혼률이 높은지 낮은지 판단하기 위함이다.

df_new <-df_new %>% 
  arrange(이혼률)

# 시로 끝나는 시도에 1부터 순서대로 적용합니다.
df_modified$시도별[str_detect(df_modified$시도별, "시$")] <- 1:8

# 도로 끝나는 시도에 11부터 순서대로 적용합니다.
df_modified$시도별[str_detect(df_modified$시도별, "도$")] <- 11:19

# 수정된 tibble 출력
df_modified
df_modified$시도별 <- as.integer(df_modified$시도별)

# lm 모델을 사용하여 가설 검정 수행
model <- lm(이혼률 ~ 시도별, data = df_modified)

# 검정 결과 출력
summary(model)



# 데이터 탐색
plot(df_modified$시도별, df_modified$이혼률, main = "이혼률과 시도별 관계", xlab = "시도별", ylab = "이혼률")

# 상관 관계 분석
correlation <- cor(df_modified$시도별, df_modified$이혼률)
print(correlation)

# 시각화
plot(df_modified$시도별, df_modified$이혼률, main = "이혼률과 시도별 관계", xlab = "시도별", ylab = "이혼률")
abline(lm(df_modified$이혼률 ~ df_modified$시도별), col = "red")
















# read_excel() 함수를 사용하여 엑셀 파일을 불러옵니다.
library(readxl)
data <- read_excel("시도_성_연령별_이혼율_20230502000730.xlsx")

# 데이터프레임으로 변환합니다.
df <- as.data.frame(data)
df
# 필요 없는 열을 제거합니다.
df <- df[, c(1, 2, 3, 4)]
df
# 열 이름을 변경합니다.
colnames(df) <- c("시도별", "연령별", "남편", "아내")

# 계 열을 제거합니다.
df <- df[df$시도별 != "계", ]
df
# 남편과 아내 열의 값을 천명당 건수에서 건수로 변경합니다.
df$남편 <- df$남편 * 10
df$아내 <- df$아내 * 10

# 결과를 출력합니다.
print(df)

data<-head(data,14)
data


# 나이별 이혼율을 데이터프레임으로 만들기
df_age <- data.frame(
  연령별 = data$연령별,
  이혼율 = c(data$남편, data$아내),
  성별 = rep(c("남편", "아내"), each = nrow(data))
)

# 원 그래프 그리기
ggplot(df_age, aes(x = "", y = 이혼율, fill = 성별)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ 연령별, ncol = 3) +
  labs(title = "연령별 이혼율", x = NULL, y = NULL) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) +
  theme_void()

#-------------------------------------------------아래부터 추가된 코드드

# 연령별 이혼율 계산
for (i in 1:nrow(df_age)) {
  age <- df_age$연령별[i]
  df_age$이혼율[i] <- mean((df$남편[df$연령별 == age] + df$아내[df$연령별 == age]) / 2)
}

# 나이별 값을 int 형으로 변경합니다. 가설 검정을 위하여.
df_age$연령별 <- as.integer(gsub("[^0-9]+", "", df_age$연령별))

# 연령별 값이 55와 75인 행 제거 55세 이상인 행과 75세 이상인 행은 유의미한 값이 아니기 때문에 제거합니다.
df_age <- df_age[!(df_age$연령별 %in% c(55, 75)), ]

# 1행부터 12행까지 추출 13행부터 24행까지는 같은 내용이 반복 되기 때문입니다. 왜냐하면 아내와 남편 값을 연령별로 더하여 2를 나눈 값으로 초기화 했기 때문입니다.
df_age <- df_age[1:12, ]

#성별 열 삭제제
df_age <- df_age[, c("연령별", "이혼율")]

# 45세까지 이혼율 증가 가설에 대한 lm 모델링
model1 <- lm(이혼율 ~ 연령별, data = df_age[df_age$연령별 <= 4044, ])

# 45세 이후 이혼율 감소 가설에 대한 lm 모델링
model2 <- lm(이혼율 ~ 연령별, data = df_age[df_age$연령별 > 4044, ])

# 모델링 결과 분석
summary(model1)  # 45세까지 이혼율 증가 가설에 대한 결과 분석
summary(model2)  # 45세 이후 이혼율 감소 가설에 대한 결과 분석

# 데이터 탐색
plot(df_age$연령별, df_age$이혼율, main = "이혼율과 연령별 관계", xlab = "연령별", ylab = "이혼율")

# 모델1 시각화
abline(model1, col = "red")

# 모델2 시각화
abline(model2, col = "blue")









library(tidyverse)
tb<-read_csv("시도_직업별_2008__이혼_20230508094338.csv",locale=locale("ko",encoding="EUC-KR"),na=".")
tb

# 데이터 불러오기
data <- tb
data

# 첫 번째 행 제거
data <- data[-1,]
data

# 첫 번째 행 제거거
data<- data[-1,]
data

# 변수명 변경
names(data) <- c("시도", "직업", "남자_이혼", "여자_이혼")

# 시도 변수 제거
data <- data %>% select(-시도)

# 숫자형으로 변환
data <- data %>% 
  mutate(across(2:3, ~as.numeric(str_replace(., ",", ""))))


# 연봉 변수 추가 (연봉이 낮을수록 상위로 배치)
data$연봉 <- rep(c(2000, 2500, 3500, 4000, 3000, 4500, 6000, 7000, 8000, 5500), length.out = nrow(data))

# 직업별 연봉에 따른 이혼율 계산
divorce_by_income <- data %>%
  group_by(직업, 연봉) %>%
  summarize(mean_divorce = mean(남자_이혼 + 여자_이혼)) %>% 
  arrange(desc(연봉))

# 시각화
ggplot(divorce_by_income, aes(x = 연봉, y = mean_divorce, group = 직업, color = 직업)) + 
  geom_line() + 
  labs(title = "직업별 연간 소득(만원)에 따른 이혼율", x = "연간 소득(만원)")

# 필요한 패키지 로드
library(ggplot2)
library(dplyr)
library(tidyr)

# 데이터 불러오기
tb<-read_csv("시도_직업별_2008__이혼_20230508094338.csv",locale=locale("ko",encoding="EUC-KR"),na=".")
df<-tb[-2,]  # 행 제거

# 행 이름 변경
df <- rename(df, "husband" = `2022...3`, "wife" = `2022...4`)

# 시도별 제거
df <- df %>% filter(시도별 != "시도별")

# 숫자형으로 변환
df$husband <- as.numeric(df$husband)
df$wife <- as.numeric(df$wife)

# 데이터 가공
df <- pivot_longer(df, cols=c("husband", "wife"), names_to = "gender", values_to = "divorce_rate")
df <- df[order(df$직업별), ]  # 직업별 오름차순으로 정렬
df <- df %>% filter(직업별 != "계")


# 시각화
ggplot(df, aes(x=reorder(직업별, divorce_rate), y=divorce_rate, fill=gender)) +
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# 직업별 평균 연봉 데이터 추가
avg_salary <- c(60000000, 50000000, 40000000, 30000000, 20000000, 35000000, 25000000, 15000000, 10000000, 12000000)

# 데이터 프레임의 행 개수와 평균 연봉 데이터 개수 일치시키기
avg_salary <- rep(avg_salary, each = nrow(df) / length(avg_salary))

# 평균 연봉 열 추가
df$avg_salary <- avg_salary

# 가설검정 
# 직업군별 평균 연봉과 이혼률 간에 상관관계가 있는지 검정
correlation <- cor.test(df$avg_salary, df$divorce_rate)
print(correlation)

# 선형회귀
# 직업군별 평균 연봉을 기준으로 이혼률을 예측하는 선형회귀 모델 구축
model <- lm(divorce_rate ~ avg_salary, data = df)
summary(model)

# 모델 시각화
# 회귀선 그리기
plot(df$avg_salary, df$divorce_rate, xlab = "평균 연봉", ylab = "이혼률")
abline(model, col = "red")

# 데이터 탐색
# 산점도 그리기
plot(df$avg_salary, df$divorce_rate, xlab = "평균 연봉", ylab = "이혼률")



# 모델링과 예측 가능성 판단
# 예측 모델의 성능을 평가하고 예측 가능성을 판단하는 작업 수행
predictions <- predict(model, newdata = df)








# 필요한 패키지 로드
library(ggplot2)
library(dplyr)
library(tidyr)

# 데이터 불러오기
tb<-read_csv("시도_직업별_2008__이혼_20230508094338.csv",locale=locale("ko",encoding="EUC-KR"),na=".")
df<-tb[-2,]  # 행 제거
df<-head(df,10)
df
df %>% 
  select(-시도별) %>% 
  rename(남편 = `2022...3`, 아내 = `2022...4`) %>% 

  pivot_longer(cols = -직업별, names_to = "성별", values_to = "인원") %>%
  ggplot(aes(x = "", y = 인원, fill = 직업별)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  facet_wrap(~성별) + 
  theme_void() + 
  labs(title = "2023년 직업별 이혼률 분포", fill = "직업별") 











# 필요한 라이브러리 로드
library(tidyverse)
library(ggplot2)
library(dplyr)

# 데이터 취득 및 정제
data <- read_csv("연령_5세___혼인지속기간_동거기간_별_이혼_20230517123720.csv",locale=locale("ko",encoding="EUC-KR"),na=".")
data
data <- data[-1, ]  # 첫 번째 행 제거 (헤더 제거)
data$동거기간별 <- gsub("년 미만", "", data$동거기간별)  # "년 미만" 문자열 삭제
data$동거기간별 <- gsub("∼", "-", data$동거기간별)  # "∼" 문자열을 "-"로 변경
data$동거기간별 <- gsub("년 이상", "+", data$동거기간별)  # "년 이상" 문자열을 "+"로 변경
data$동거기간별 <- as.character(data$동거기간별)  # 동거기간별을 문자열로 변환

# 데이터 가공
data <- data %>%
  mutate(이혼건수 = as.integer(이혼건수))  # 이혼건수를 정수로 변환

# 데이터 시각화
data <- data %>%
  mutate(동거기간별 = reorder(동거기간별, -이혼건수))  # 동거기간별을 이혼건수 기준으로 내림차순 정렬

ggplot(data, aes(x = 동거기간별, y = 이혼건수, fill = 동거기간별)) +
  geom_bar(stat = "identity") +
  labs(x = "동거기간별", y = "이혼건수", fill = "동거기간별") +
  ggtitle("동거기간별 이혼건수") +
  theme_minimal()



# 동거기간별을 정수로 변환
data$동거기간별 <- gsub("\\+","",data$동거기간별)
data$동거기간별 <- gsub("\\-","",data$동거기간별)
data$동거기간별 <- gsub("년","",data$동거기간별)
data$동거기간별 <- gsub("1014","10",data$동거기간별)
data$동거기간별 <- gsub("1519","15",data$동거기간별)
data$동거기간별 <- as.integer(data$동거기간별)


# 선형 회귀 모델링
model <- lm(이혼건수 ~ 동거기간별, data = data)

# 모델 요약
summary(model)


# 예측 가능성 평가
prediction <- predict(model, newdata = data)
accuracy <- cor(data$이혼건수, prediction)
accuracy

# 시각화 및 회귀 직선 그리기
ggplot(data, aes(x = 동거기간별, y = 이혼건수)) +
  geom_point() +
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "red") +
  labs(x = "동거기간별", y = "이혼건수") +
  ggtitle("동거기간별 이혼건수") +
  theme_minimal()




df <- read.csv('C:/football_rating/csv/preprocess.csv')

head(df) # 데이터 확인
summary(df) # 요약 통계량
#####################################################################################################################################
cor(df, method='pearson') # 전체 변수간 상관계수 보기

(corrmatrix <- cor(df))

# 강한 양의 상관관계, 강한 음의 상관관계
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5]

library(corrplot)
# 변수간의 상관관계를 heatmap 시각화
corrplot(cor(df), method="circle",tl.srt = 90)
#####################################################################################################################################
# 단순회귀분석
# 연봉과 시즌
# R-squared값이 전체 변동량의 약 5%정도를 이 모형을 통해 설명 가능.
# R-squared값이 비교적 낮게 나타난 이유는 시즌이 연봉외에 영향을 받는 요인이 있을 수 있음을 설명.
model1 <- lm(formula = salary ~ Season, data=df)
summary(model1)

#####################################################################################################################################
# 다중회귀분석
model <- lm(overall_rating ~., data = df)
summary(model)
# R-squared = 85.31%

cor(df, method='pearson') # 전체 변수간 다중공선성 파악

# 후진제거법
model_ <- lm(overall_rating ~ .-volleys-preferred_foot_left-preferred_foot_right, data = df)
summary(model_)
# R-squared = 85.31%
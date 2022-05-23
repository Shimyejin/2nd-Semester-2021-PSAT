# 경로설정
setwd('C:\\Users\\User\\Desktop\\주제분석\\카트')

# 각자 날짜 설정
date = "20211010"

# 아래 실행
match_code_df = getAllMatchCode(date, 1, 1)
name_lst = c("스피드 개인전", "스피드 팀전", "아이템 개인전", "아이템 팀전")
new_match_code_df = match_code_df %>%
  filter(name %in% name_lst)
n=nrow(new_match_code_df)
partition = n %/% 500


for (i in 1:(partition+1)){
  
  cat("============================\n")
  start = lubridate::now(); cat(str_c(start,"\n"))
  cat(str_c(i, "번째\n"))
  if (i == partition+1) data = getAllMatchInfo(new_match_code_df[500*(i-1)+1:n])
  else data = getAllMatchInfo(new_match_code_df[500*(i-1)+1:500*i])
  
  cat("저장 중....\n")
  write.csv(data, paste0(date,"-", i,".csv"), fileEncoding = "cp949", row.names = F)
  cat("저장 완료!\n")
  
  end = lubridate::now(); cat(str_c(end,"\n"))
  cat(str_c(end-start,"\n"))
}

data1 = fread('20211010-1.csv')
data2 = fread('20211010-2.csv')
data3 = fread('20211010-3.csv')
data4 = fread('20211010-4.csv')
data5 = fread('20211010-5.csv')
data6 = fread('20211010-6.csv')
data7 = fread('20211010-7.csv')
data8 = fread('20211010-8.csv')
data9 = fread('20211010-9.csv')
data10 = fread('20211010-10.csv')
data11 = fread('20211010-11.csv')
data12 = fread('20211010-12.csv')

q = rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)
write.csv(q,'20211010.csv',fileEncoding = "cp949", row.names = F)

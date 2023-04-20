# Packages use:
library(ggplot2)


#Bộ dữ liệu "Concrete Compressive Strength": giá trị đo lường độ bền nén
#của bê tông

# a) Tìm hiểu thông tin về tập dữ liệu

# b) Đọc dữ liệu và đổi tên các cột

    # Luu file xls -> csv
filepath = "C:/Users/DELL/Documents/SP_Assignment/Concrete_Data.csv"
    # Doc file
data<-read.csv(filepath, check.names = F)

# c) Tinh trung binh, trung vi, do lech chuan, min, max cho cac bien

summary(data)

# d) Ve scatter plot va histogram

plot1 = ggplot(data = data) + geom_boxplot(data = data$cememt)
View(plot1)


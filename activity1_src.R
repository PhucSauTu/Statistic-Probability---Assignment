# Packages use:
library(ggplot2)  
library(skimr)
library(Rmisc)

  #Bộ dữ liệu "Concrete Compressive Strength": giá trị đo lường độ bền nén
  #của bê tông

  # a) Tìm hiểu thông tin về tập dữ liệu
  # b) Đọc dữ liệu và đổi tên các cột

    # Lưu file xls -> csv
filepath = "C:/Users/DELL/Documents/SP_Assignment/Concrete_Data.csv"
    # Đọc file
df<-read.csv(filepath, check.names = F)

  # c) Tính trung bình, trung vị, đọ lệch chuẩn, min, max cho các biến
  #Option 1(default):
summary(df)
  #Option 2(skimr):
    #Lý do sử dụng skimr:
    # + Cung cấp đầy đủ thông tin từ data frame
    # + So với summary, skimr có thêm standard deviation
options(pillar.sigfig = 6)  #format 6 số
skim(df)

  # d) Vẽ scatter plot và histogram

#Histogram
cementPlot<-ggplot(data = df, mapping = aes(x = `Cement`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (",expression(kg/m^3), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "yellow", high = "red") +
  ggtitle(substitute(paste(bold("Xi măng"))))
#show(cementPlot)

bfsPlot<-ggplot(data = df, mapping = aes(x = `Blast Furnace Slag`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (",expression(kg/m^3), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "yellow", high = "red") +
  ggtitle(substitute(paste(bold("Xỉ lò cao"))))
#show(bfsPlot)

flyashPlot<-ggplot(data = df, mapping = aes(x = `Fly Ash`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (",expression(kg/m^3), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "yellow", high = "red") +
  ggtitle(substitute(paste(bold("Tro bay"))))
#show(flyashPlot)

waterPlot<-ggplot(data = df, mapping = aes(x = `Water`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (",expression(kg/m^3), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "cyan", high = "green") +
  ggtitle(substitute(paste(bold("Nước"))))
#show(waterPlot)

superplasticizerPlot<-ggplot(data = df, mapping = aes(x = `Superplasticizer`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 5) +
  scale_x_continuous(name = paste("Giá trị (",expression(kg/m^3), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "cyan", high = "green") +
  ggtitle(substitute(paste(bold("Phụ gia siêu dẻo"))))
#show(superplasticizerPlot)

caPlot<-ggplot(data = df, mapping = aes(x = `Coarse Aggregate`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (",expression(kg/m^3), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "cyan", high = "green") +
  ggtitle(substitute(paste(bold("Đá"))))
#show(caPlot)

faPlot<-ggplot(data = df, mapping = aes(x = `Fine Aggregate`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (",expression(kg/m^3), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "purple", high = "cyan") +
  ggtitle(substitute(paste(bold("Cát"))))
#show(faPlot)

agePlot<-ggplot(data = df, mapping = aes(x = `Age`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (","ngày", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "purple", high = "cyan") +
  ggtitle(substitute(paste(bold("Ngày"))))
#show(agePlot)

ccsPlot<-ggplot(data = df, mapping = aes(x = `Concrete compressive strength`)) +
  geom_histogram(aes(fill = ..count..), col = "black", bins = 30, binwidth = 5) +
  scale_x_continuous(name = paste("Giá trị (","MPA", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "purple", high = "cyan") +
  ggtitle(substitute(paste(bold("Cường độ nén bê tông"))))
#show(ccsPlot)

layout<-matrix(c(1:9), nrow = 3, byrow = TRUE)
multiplot(cementPlot, bfsPlot, flyashPlot,
          waterPlot, superplasticizerPlot, caPlot,
          faPlot, agePlot, ccsPlot
          ,layout = layout)

#Box Plot & Scatter Plot


# Packages use:
library(ggplot2)  
library(skimr)
library(Rmisc)

  #Bộ dữ liệu 

  # a) Tìm hiểu thông tin về tập dữ liệu
  # b) Đọc dữ liệu và đổi tên các cột

    # Lưu file xls -> csv
filepath = "C:/Users/DELL/Documents/SP_Assignment/energydata_complete.csv"
    # Đọc file
    # Xoa cot date, rv1, rv2
df<-read.csv(filepath, check.names = F)
df<-df[c(-29,-28,-1)]

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
aplPlot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `Appliances`, fill = ..x..), col = "black", bins = 50, binwidth = 10) +
  scale_x_continuous(name = paste("Giá trị (",expression(Wh), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "yellow", high = "red") +
  ggtitle(substitute(paste(bold("Appliances"))))
show(aplPlot)

lgtPlot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `lights`, fill = ..x..), col = "black", bins = 30, binwidth = 5) +
  scale_x_continuous(name = paste("Giá trị (",expression(Wh), ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradient("Tần số", low = "yellow", high = "red") +
  ggtitle(substitute(paste(bold("Lights"))))
show(lgtPlot)

layout<-matrix(c(1,2), nrow = 1, byrow = TRUE)
view1<-multiplot(aplPlot, lgtPlot, layout = layout)

T1Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T1`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 1"))))
show(T1Plot)

RH1Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_1`, fill = ..x..), col = "black", bins = 30, binwidth = 1) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 1"))))
show(RH1Plot)

T2Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T2`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 2"))))
show(T2Plot)

RH2Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_2`, fill = ..x..), col = "black", bins = 30, binwidth = 1) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 2"))))
show(RH2Plot)

T3Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T3`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 3"))))
show(T3Plot)

RH3Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_3`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 3"))))
show(RH3Plot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view2<-multiplot(T1Plot, RH1Plot,
                 T2Plot, RH2Plot,
                 T3Plot, RH3Plot,
                 layout = layout)

T4Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T4`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 4"))))
show(T4Plot)

RH4Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_4`, fill = ..x..), col = "black", bins = 30, binwidth = 1) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 4"))))
show(RH4Plot)

T5Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T5`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 5"))))
show(T5Plot)

RH5Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_5`, fill = ..x..), col = "black", bins = 30, binwidth = 1) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 5"))))
show(RH5Plot)

T6Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T6`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(-10,35), colours = c("purple","blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 6"))))
show(T6Plot)

RH6Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_6`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 6"))))
show(RH6Plot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view3<-multiplot(T4Plot, RH4Plot,
                 T5Plot, RH5Plot,
                 T6Plot, RH6Plot,
                 layout = layout)

T7Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T7`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 7"))))
show(T7Plot)

RH7Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_7`, fill = ..x..), col = "black", bins = 30, binwidth = 1) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 7"))))
show(RH7Plot)

T8Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T8`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 8"))))
show(T8Plot)

RH8Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_8`, fill = ..x..), col = "black", bins = 30, binwidth = 1) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 8"))))
show(RH8Plot)

T9Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T9`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(0,35), colours = c("blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ 9"))))
show(T9Plot)

RH9Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_9`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm 9"))))
show(RH9Plot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view4<-multiplot(T7Plot, RH7Plot,
                 T8Plot, RH8Plot,
                 T9Plot, RH9Plot,
                 layout = layout)

T0Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `T_out`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = paste("Giá trị (","\u00B0C", ")", sep = "")) +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(-10,35), colours = c("purple","blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Nhiệt độ ngoài"))))
show(T0Plot)

RH0Plot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `RH_out`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = "Giá trị (%)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Độ ẩm (%)", limits = c(0,100), colours = c("red","orange","cyan","blue")) +
  ggtitle(substitute(paste(bold("Độ ẩm ngoài"))))
show(RH0Plot)

pmhPlot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `Press_mm_hg`), col = "black", fill = "cyan", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = "Giá trị (mmHg)") +
  scale_y_continuous(name = "Tần số") +
  ggtitle(substitute(paste(bold("Khí áp"))))
show(pmhPlot)

wdsPlot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `Windspeed`), col = "black", fill = "green", bins = 30, binwidth = 0.25) +
  scale_x_continuous(name = "Giá trị (m/s)") +
  scale_y_continuous(name = "Tần số") +
  ggtitle(substitute(paste(bold("Tốc độ gió"))))
show(wdsPlot)

vsbPlot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `Visibility`), col = "black", fill = "grey", bins = 30, binwidth = 1) +
  scale_x_continuous(name = "Giá trị (km)") +
  scale_y_continuous(name = "Tần số") +
  ggtitle(substitute(paste(bold("Tầm nhìn"))))
show(vsbPlot)

tdpPlot<-ggplot(data = df) +
  geom_histogram(mapping = aes(x = `Tdewpoint`, fill = ..x..), col = "black", bins = 30, binwidth = 0.5) +
  scale_x_continuous(name = "Giá trị (\u00B0C)") +
  scale_y_continuous(name = "Tần số") +
  scale_fill_gradientn("Nhiệt độ (\u00B0C)", limits = c(-10,35), colours = c("purple","blue","cyan","green","yellow","red")) +
  ggtitle(substitute(paste(bold("Điểm ngưng sương"))))
show(tdpPlot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view5<-multiplot(wdsPlot, T0Plot,
                 pmhPlot, RH0Plot,
                 vsbPlot, tdpPlot,
                 layout = layout)

#Box Plot & Scatter Plot
cementPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Cement`, y = `Concrete compressive strength`), color = "red", size = 1) +
  scale_x_continuous(name = paste("Xi măng (",expression(kg/m^3),")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo xi măng"))))
#show(cementPlot)

bfsPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Blast Furnace Slag`, y = `Concrete compressive strength`), color = "blue", size = 1) +
  scale_x_continuous(name = paste("Xỉ lò cao (",expression(kg/m^3),")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo xỉ lò cao"))))
#show(bfsPlot)

flyashPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Fly Ash`, y = `Concrete compressive strength`), color = "red", size = 1) +
  scale_x_continuous(name = paste("Tro bay (",expression(kg/m^3),")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo tro bay"))))
#show(flyashPlot)

waterPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Water`, y = `Concrete compressive strength`), color = "blue", size = 1) +
  scale_x_continuous(name = paste("Nước (",expression(kg/m^3),")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo nước"))))
#show(waterPlot)

superplasticizerPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Superplasticizer`, y = `Concrete compressive strength`), color = "red", size = 1) +
  scale_x_continuous(name = paste("Phụ gia siêu dẻo (",expression(kg/m^3),")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo phụ gia siêu dẻo"))))
#show(superplasticizerPlot)

caPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Coarse Aggregate`, y = `Concrete compressive strength`), color = "blue", size = 1) +
  scale_x_continuous(name = paste("Đá (",expression(kg/m^3),")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo đá"))))
#show(caPlot)

faPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Fine Aggregate`, y = `Concrete compressive strength`), color = "red", size = 1) +
  scale_x_continuous(name = paste("Cát (",expression(kg/m^3),")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo cát"))))
#show(faPlot)

agePlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Age`, y = `Concrete compressive strength`), color = "blue", size = 1) +
  scale_x_continuous(name = paste("Tuổi (","ngày",")", sep = "")) +
  scale_y_continuous(name = paste("Cường độ nén bê tông (","MPA",")",sep = "")) +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán cường độ nén bê tông theo tuổi"))))
#show(agePlot)

layout<-matrix(c(1:8), nrow = 4, byrow = TRUE)
multiplot(cementPlot, bfsPlot, flyashPlot, waterPlot,
          superplasticizerPlot, caPlot, faPlot, agePlot
          ,layout = layout)
  
  # d) Tính ma trận hệ số tương quan

cor(df, method = "pearson")

# Packages use:
library(ggplot2)  
library(skimr)
library(Rmisc)

  #Bộ dữ liệu "Appliances energy prediction Data Set":
        # Appliances: năng lượng tiêu thụ của vật dụng (Wh)
        # lights    : năng lượng tiêu thụ cho chiếu sáng(Wh)
        # T1        : nhiệt độ nhà bếp (Celcius)
        # RH_1      : độ ẩm nhà bếp (%)
        # T2        : nhiệt độ phòng khách (Celcius)
        # RH_2      : độ ẩm phòng khách (%)
        # T3        : nhiệt độ phòng giặt (Celcius)
        # RH_3      : độ ẩm phòng giặt (%)
        # T4        : nhiệt độ văn phòng (Celcius)
        # RH_4      : độ ẩm văn phòng (%)
        # T5        : nhiệt độ phòng tắm (Celcius)
        # RH_5      : độ ẩm phòng tắm (%)
        # T6        : nhiệt độ ngoài trời (Celcius)
        # RH_6      : độ ẩm ngoài trời (%)
        # T7        : nhiệt độ phòng ủi (Celcius)
        # RH_7      : độ ẩm phòng ủi (%)
        # T8        : nhiệt độ phòng ngủ 1 (Celcius)
        # RH_8      : độ ẩm phòng ngủ 1 (%)
        # T9        : nhiệt độ phòng ngủ 2 (Celcius)
        # RH_9      : độ ẩm phòng ngủ 2 (%)
        # T_out     : nhiệt độ trạm Chievres (Celcius)
        # Press_mm_hg: khí áp trạm Chievres (mmHg)
        # RH_out    : độ ẩm trạm Chievres (%)
        # Windspeed : tốc độ gió trạm Chievres (m/s)
        # Visibility: tầm nhìn (km)
        # Tdewpoint : điểm sương (Celcius)

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

# Scatter Plot
lgtPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `lights`, y = `Appliances`), color = "black", size = 1) +
  scale_x_continuous(name = "lights (Wh)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ cho chiếu sáng"))))
#show(lgtPlot)

T1Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T1`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature1 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 1"))))
#show(T1Plot)

RH1Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_1`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity1 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 1"))))
#show(RH1Plot)

T2Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T2`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature2 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 2"))))
#show(T2Plot)

RH2Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_2`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity2 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 2"))))
#show(RH2Plot)

T3Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T3`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature3 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 3"))))
#show(T3Plot)

RH3Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_3`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity3 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 3"))))
#show(RH3Plot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view5<-multiplot(T1Plot, RH1Plot,
                 T2Plot, RH2Plot,
                 T3Plot, RH3Plot,
                 layout = layout)

T4Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T4`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature4 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 4"))))
#show(T4Plot)

RH4Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_4`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity4 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 4"))))
#show(RH4Plot)

T5Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T5`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature5 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 5"))))
#show(T5Plot)

RH5Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_5`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity5 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 5"))))
#show(RH5Plot)

T6Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T6`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature6 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 6"))))
#show(T6Plot)

RH6Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_6`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity6 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 6"))))
#show(RH6Plot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view5<-multiplot(T4Plot, RH4Plot,
                 T5Plot, RH5Plot,
                 T6Plot, RH6Plot,
                 layout = layout)

T7Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T7`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature7 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 7"))))
#show(T7Plot)

RH7Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_7`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity7 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 7"))))
#show(RH7Plot)

T8Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T8`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature8 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiêt độ 8"))))
#show(T8Plot)

RH8Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_8`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity8 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 8"))))
#show(RH8Plot)

T9Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T9`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature9 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ 9"))))
#show(T9Plot)

RH9Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_9`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity9 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm 9"))))
#show(RH9Plot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view5<-multiplot(T7Plot, RH7Plot,
                 T8Plot, RH8Plot,
                 T9Plot, RH9Plot,
                 layout = layout)

T0Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `T_out`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temporature0 (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo nhiệt độ trạm"))))
#show(T0Plot)

RH0Plot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `RH_out`, y = `Appliances`), color = "blue", size = 1) +
  scale_x_continuous(name = "Rate of Humidity0 (%)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo độ ẩm trạm"))))
#show(RH0Plot)

pmhPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Press_mm_hg`, y = `Appliances`), color = "black", size = 1) +
  scale_x_continuous(name = "Pressure (mmHg)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lương tiêu thụ theo khí áp"))))
#show(pmhPlot)

wdsPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Windspeed`, y = `Appliances`), color = "green", size = 1) +
  scale_x_continuous(name = "Windspeed (m/s)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo tốc độ gió"))))
#show(wdsPlot)

vsbPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Visibility`, y = `Appliances`), color = "black", size = 1) +
  scale_x_continuous(name = "Visibility (km)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo tầm nhìn"))))
#show(vsbPlot)

tdpPlot<-ggplot(data = df) +
  geom_point(mapping = aes(x = `Tdewpoint`, y = `Appliances`), color = "red", size = 1) +
  scale_x_continuous(name = "Temperature (\u00B0C)") +
  scale_y_continuous(name = "Appliances (Wh)") +
  ggtitle(substitute(paste(bold("Biểu đồ phân tán năng lượng tiêu thụ theo điểm sương"))))
#show(tdpPlot)

layout<-matrix(c(1:6), nrow = 3, byrow = TRUE)
view5<-multiplot(T0Plot, RH0Plot,
                 pmhPlot, wdsPlot,
                 vsbPlot, tdpPlot,
                 layout = layout)






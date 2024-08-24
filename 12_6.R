# HOẠT ĐỘNG 1
#1. Đọc dữ liệu từ file CSV và hiển thị 6 hàng đầu của dữ liệu
auto_mpgData <- read.csv("C:/Users/Admin/Desktop/auto_mpg.csv", sep=";")
head(auto_mpgData)

#2.1a. Thay thế giá trị "?" bằng NA và kiểm tra số lượng giá trị NA trong mỗi cột
auto_mpgData[auto_mpgData == "?"] <- NA
apply(is.na(auto_mpgData), 2, sum)  # Tính tổng số giá trị NA trong mỗi cột

#2.1b. Loại bỏ các hàng chứa giá trị NA và kiểm tra lại số lượng giá trị NA
auto_mpgData <- na.omit(auto_mpgData)
apply(is.na(auto_mpgData), 2, sum)  # Kiểm tra lại số lượng giá trị NA sau khi loại bỏ

#2.2a. Kiểm tra kiểu dữ liệu của các cột
flagMpg <- is.numeric(auto_mpgData$mgp)
flagCylinders <- is.numeric(auto_mpgData$cylinders)
flagDisplacement <- is.numeric(auto_mpgData$displacement)
flagHorsePower <- is.numeric(auto_mpgData$horsepower)
flagWeight <- is.numeric(auto_mpgData$weight)
flagAcceleration <- is.numeric(auto_mpgData$acceleration)
flagYear <- is.numeric(auto_mpgData$model_year)
flagOrigin <- is.numeric(auto_mpgData$origin)
# Tạo bảng hiển thị kết quả kiểm tra kiểu dữ liệu
data.frame(flagMpg, flagCylinders, flagDisplacement,
           flagHorsePower, flagWeight, flagAcceleration,
           flagYear, flagOrigin)

#2.2b. Chuyển cột 'horsepower' thành kiểu số và kiểm tra lại
auto_mpgData$horsepower <- as.numeric(auto_mpgData$horsepower)
is.numeric(auto_mpgData$horsepower)

#2.2c. Thay thế các giá trị số trong cột 'origin' bằng tên khu vực
auto_mpgData$origin[auto_mpgData$origin == 1] <- "North American"
auto_mpgData$origin[auto_mpgData$origin == 2] <- "Europe"
auto_mpgData$origin[auto_mpgData$origin == 3] <- "Asia"

#2.3. Vẽ biểu đồ hộp cho cột 'horsepower', tìm và loại bỏ các giá trị ngoại lệ
boxplot(auto_mpgData$horsepower, ylab = "Horsepower", main = "Biểu đồ hộp của horsepower")
out <- boxplot.stats(auto_mpgData$horsepower)$out  # Xác định các giá trị ngoại lệ
out_ind <- which(auto_mpgData$horsepower %in% c(out))  # Tìm chỉ số của các giá trị ngoại lệ
clearData <- auto_mpgData[-out_ind, ]  # Loại bỏ các hàng chứa giá trị ngoại lệ
head(clearData, 10)  # Hiển thị 10 hàng đầu của dữ liệu đã loại bỏ giá trị ngoại lệ


#3.1. Tính toán các thống kê mô tả cho các biến số
continuousVar <- clearData[sapply(clearData, is.numeric)]  # Chọn các cột số
trung_binh <- apply(continuousVar, 2, mean)  # Tính trung bình
do_lech_chuan <- apply(continuousVar, 2, sd)  # Tính độ lệch chuẩn
GTLN <- apply(continuousVar, 2, max)  # Tính giá trị lớn nhất
GTNN <- apply(continuousVar, 2, min)  # Tính giá trị nhỏ nhất
trung_vi <- apply(continuousVar, 2, median)  # Tính trung vị
phan_vi1 <- apply(continuousVar, 2, quantile, probs = 0.25)  # Tính phân vị 25%
phan_vi3 <- apply(continuousVar, 2, quantile, probs = 0.75)  # Tính phân vị 75%

# Tạo bảng hiển thị các thống kê mô tả
t(data.frame(trung_binh, do_lech_chuan, GTLN, GTNN, trung_vi, phan_vi1, phan_vi3))

#3.2. Vẽ các biểu đồ phân phối và mối quan hệ giữa các biến
hist(clearData$mgp, main = "Biểu đồ phân phối của MPG", xlab = "MPG",
     col = heat.colors(9), labels = TRUE, ylim = c(0, 100))

# Biểu đồ hộp cho MPG theo từng khu vực
boxplot(clearData$mgp ~ clearData$origin, main = "Biểu đồ hộp của MPG theo khu vực", 
        ylab = "MPG", xlab = "Khu vực", col = c(2, 3, 4))

# Biểu đồ hộp cho MPG theo năm sản xuất
boxplot(clearData$mgp ~ clearData$model_year, main = "Biểu đồ hộp của MPG theo năm sản xuất", 
        ylab = "MPG", xlab = "Năm sản xuất", col = c(2, 3, 4, 5, 6))

# Biểu đồ hộp cho MPG theo số xy-lanh
boxplot(clearData$mgp ~ clearData$cylinders, main = "Biểu đồ hộp của MPG theo số xy-lanh", 
        ylab = "MPG", xlab = "Số xy-lanh", col = c(2, 3, 4, 5, 6))

# Biểu đồ phân tán giữa MPG và các biến liên tục
plot(clearData$displacement, clearData$mgp, main = "Biểu đồ phân tán của MPG và displacement", 
     ylab = "MPG", xlab = "Displacement", col = c(3))

plot(clearData$horsepower, clearData$mgp, main = "Biểu đồ phân tán của MPG và horsepower", 
     ylab = "MPG", xlab = "Horsepower", col = c(2))

plot(clearData$weight, clearData$mgp, main = "Biểu đồ phân tán của MPG và weight", 
     ylab = "MPG", xlab = "Weight", col = c(4))

plot(clearData$acceleration, clearData$mgp, main = "Biểu đồ phân tán của MPG và acceleration", 
     ylab = "MPG", xlab = "Acceleration", col = c(6))

dim(auto_mpgData)  # Kiểm tra số lượng hàng và cột

#4.1 Tạo mẫu huấn luyện và mẫu kiểm tra
auto_mpg1 <- auto_mpgData[1:200, ] # mẫu huấn luyện gồm 200 quan trắc 
auto_mpg1$origin <- as.numeric(factor(auto_mpg1$origin)) #chuyển các biến origin và car_name sang định tính
auto_mpg1$car_name <- as.numeric(factor(auto_mpg1$car_name))
head(auto_mpg1) #kiểm tra lại

remaining_indices <- setdiff(seq_len(nrow(auto_mpgData)), 1:200) #mẫu kiểm tra gồm các quan trắc còn lại đã được làm sạch
auto_mpg2 <- clearData[remaining_indices, ] 
auto_mpg2$origin <- as.numeric(factor(auto_mpg2$origin)) #chuyển các biến origin và car_name sang định tính
auto_mpg2$car_name <- as.numeric(factor(auto_mpg2$car_name))
head(auto_mpg2) #kiểm tra lại

#4.2 Mô hình hồi quy tuyến tính của biến phụ thuộc mpg 
#sử dụng mẫu huấn luyện
model_1 <- lm(mgp~., data=auto_mgp1)
summary(model_1)
model_2 <- lm(mgp~weight+model_year+origin, data=auto_mgp1) #Loại bỏ các biến độc lập không có ý nghĩa thống kê
summary(model_2)
confint(model_2) #tìm khoảng tin cậy
plot(model_2,1) 
plot(model_2,2)
plot(model_2,3)
plot(model_2,5)
#sử dụng mẫu kiểm tra
predict_mpg <- lm(mgp~weight+model_year+origin, data=auto_mgp2)
summary(predict_mpg)
confint(predict_mpg)
plot(predict_mpg,1) 
plot(predict_mpg,2)
plot(predict_mpg,3)
plot(predict_mpg,5)

# HOẠT ĐỘNG 2

#Đọc dữ liệu
library(readr)
starcraft <-read.csv("C:/Users/phamt/Downloads/skillcraft1+master+table+dataset/SkillCraft1_Dataset.csv")

#Tạo data frame với cột là các biến muốn khảo sát
starcraft <-starcraft[,c("LeagueIndex", "Age", "TotalHours", "TotalMapExplored")]

#Tìm và xử lý các giá trị NA trong dữ liệu
starcraft[starcraft == "?"] <- NA
apply(is.na(starcraft), 2, sum)
starcraft <- na.omit(starcraft)
apply(is.na(starcraft), 2, sum)

#Kiểm tra kiểu dữ liệu của các cộtcột
flagLeagueIndex <- is.numeric(starcraft$LeagueIndex)
flagAge <- is.numeric(starcraft$Age)
flagTotalHours <- is.numeric(starcraft$TotalHours)
flagTotalMapExplored <- is.numeric(starcraft$TotalMapExplored)
data.frame(flagLeagueIndex, flagAge, flagTotalHours, flagTotalMapExplored)

#Chuyển các dữ liệu chưa phải dạng số về dạng số và kiểm tra lại
starcraft$Age <- as.numeric(starcraft$Age)
is.numeric(starcraft$Age)
starcraft$TotalHours <- as.numeric(starcraft$TotalHours)
is.numeric(starcraft$TotalHours)

#Vẽ biểu đồ hộp cho các cột để kiểm tra các giá trị ngoại lai
boxplot(starcraft$LeagueIndex, main = "Bậc xếp hạng")
boxplot(starcraft$Age, main = "Tuổi người chơi")
boxplot(starcraft$TotalMapExplored, main = "Tổng số bản đồ đã khám phá")
boxplot(starcraft$TotalHours, main = "Tổng thời gian chơi")

#Tìm và loại bỏ các giá trị ngoại lai của cột TotalHours bằng cách dùng tứ phân vị
q25 <- quantile(starcraft$TotalHours, 0.25)
q75 <- quantile(starcraft$TotalHours, 0.75)
iqr <- q75 - q25
limit_iqr = 1.5*iqr
lower_iqr = q25 - limit_iqr
upper_iqr = q75 + limit_iqr
ouliers_index_iqr <- which(starcraft$TotalHours > upper_iqr | starcraft$TotalHours < lower_iqr)
starcraft <- subset(starcraft, TotalHours < 1550)

#Dùng lệnh summary để mô tả tổng quát dữ liệu
summary(starcraft)
hist(starcraft$LeagueIndex, main = "Biều đồ phân phối bậc xếp hạng")
hist(starcraft$Age, main = "Biều đồ phân phối độ tuổi")
hist(starcraft$TotalHours, main = "Biều đồ phân phối thời gian chơi")
hist(starcraft$TotalMapExplored, main = "Biều đồ phân phối số bản đồ đã khám phá")

hist(data$ActionLatency, main = "Histogram of ActionLatency", beark = 30, xlim = c(24, 180))
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored", beark = 30, xlim = c(0, 70))
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
library(readr)
data <- read.csv("D:/xstk-2.0/BTL/skillcraft1+master+table+dataset/SkillCraft1_Dataset.csv")
View(data)
head(data, 10)
# chọn các biến quan tâm
data <- data[,c(2,3, 4,5,6, 9, 14, 16)]
# Kiểm tra sự hiện diện của "?" trong từng cột
has_question_mark <- sapply(data, function(x) sum(x == "?", na.rm = TRUE))
print(has_question_mark)
# Thay thế "?" bằng NA trong toàn bộ data frame
data[data == "?"] <- NA
# Chuyển đổi các cột từ kiểu ký tự sang kiểu số
data$Age <- as.numeric(as.character(data$Age))
data$HoursPerWeek <- as.numeric(as.character(data$HoursPerWeek))
data$TotalHours <- as.numeric(as.character(data$TotalHours))
# Tính toán trung bình của một cột và thay thế các giá trị "NA"
mean_value <- mean(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] <- mean_value
mean_value <- mean(data$HoursPerWeek, na.rm = TRUE)
data$HoursPerWeek[is.na(data$HoursPerWeek)] <- mean_value
mean_value <- mean(data$TotalHours, na.rm = TRUE)
data$TotalHours[is.na(data$TotalHours)] <- mean_value
# tổng quan dữ liệu
summary(data)
# vẽ biểu đồ
par(mfrow = c(2, 1))
hist(data$LeagueIndex, main = "Histogram of LeagueIndex", xlim = c(1,8))
boxplot(data$LeagueIndex, main = "Histogram of LeagueIndex")
par(mfrow = c(2, 1))
hist(data$Age, main = "Histogram of Age")
boxplot(data$Age, main = "Histogram of Age")
par(mfrow = c(2, 1))
hist(data$HoursPerWeek, main = "Histogram of HoursPerWeek" )
boxplot(data$HoursPerWeek, main = "Histogram of HoursPerWeek")
par(mfrow = c(2, 1))
hist(data$TotalHours, main = "Histogram of TotalHours")
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
par(mfrow = c(2, 1))
hist(data$LeagueIndex, main = "Histogram of LeagueIndex", xlim = c(1,8))
boxplot(data$LeagueIndex, main = "Histogram of LeagueIndex")
par(mfrow = c(2, 1))
hist(data$Age, main = "Histogram of Age")
boxplot(data$Age, main = "Histogram of Age")
par(mfrow = c(2, 1))
hist(data$HoursPerWeek, main = "Histogram of HoursPerWeek" )
boxplot(data$HoursPerWeek, main = "Histogram of HoursPerWeek")
par(mfrow = c(2, 1))
hist(data$TotalHours, main = "Histogram of TotalHours")
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
par(mfrow = c(2, 1))
hist(data$HoursPerWeek,
breaks = seq(0, 168, by = 12),  # Chia bins mỗi 12 giờ (nửa ngày)
xlim = c(0, 168),               # Giới hạn trục x từ 0 đến 168
ylim = c(0, max(table(data$HoursPerWeek)) + 10),  # Giới hạn trục y dựa trên dữ liệu
xlab = "Hours Per Week",        # Nhãn trục x
ylab = "Frequency",             # Nhãn trục y
main = "Histogram of Hours Per Week")  # Tiêu đề biểu đồ
par(mfrow = c(2, 1))
hist(data$TotalHours, main = "Histogram of TotalHours")
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
source("D:/xstk-2.0/BTL/codeR.R", echo=TRUE)
axis(1, at = seq(0, 168, by = 10), labels = seq(0, 168, by = 10))
par(mfrow = c(2, 1))
hist(data$TotalHours, main = "Histogram of TotalHours")
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
#par(mfrow = c(2, 1))
hist(data$HoursPerWeek,
breaks = seq(0, 168, by = 12),  # Chia bins mỗi 12 giờ (nửa ngày)
xlim = c(0, 168),               # Giới hạn trục x từ 0 đến 168
ylim = c(0, max(table(data$HoursPerWeek)) + 10),  # Giới hạn trục y dựa trên dữ liệu
xlab = "Hours Per Week",        # Nhãn trục x
ylab = "Frequency",             # Nhãn trục y
main = "Histogram of Hours Per Week")  # Tiêu đề biểu đồ
axis(1, at = seq(0, 168, by = 10), labels = seq(0, 168, by = 10))
par(mfrow = c(2, 1))
hist(data$TotalHours, main = "Histogram of TotalHours")
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
#par(mfrow = c(2, 1))
hist(data$HoursPerWeek,
breaks = seq(0, 168, by = 12),  # Chia bins mỗi 12 giờ (nửa ngày)
xlim = c(0, 168),               # Giới hạn trục x từ 0 đến 168
#ylim = c(0, max(table(data$HoursPerWeek)) + 10),  # Giới hạn trục y dựa trên dữ liệu
xlab = "Hours Per Week",        # Nhãn trục x
ylab = "Frequency",             # Nhãn trục y
main = "Histogram of Hours Per Week")  # Tiêu đề biểu đồ
axis(1, at = seq(0, 168, by = 10), labels = seq(0, 168, by = 10))
par(mfrow = c(2, 1))
hist(data$TotalHours, main = "Histogram of TotalHours")
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
par(mfrow = c(2, 1))
hist(data$HoursPerWeek,
breaks = seq(0, 168, by = 12),  # Chia bins mỗi 12 giờ (nửa ngày)
xlim = c(0, 168),               # Giới hạn trục x từ 0 đến 168
#ylim = c(0, max(table(data$HoursPerWeek)) + 10),  # Giới hạn trục y dựa trên dữ liệu
xlab = "Hours Per Week",        # Nhãn trục x
ylab = "Frequency",             # Nhãn trục y
main = "Histogram of Hours Per Week")  # Tiêu đề biểu đồ
axis(1, at = seq(0, 168, by = 10), labels = seq(0, 168, by = 10))
boxplot(data$HoursPerWeek, main = "Histogram of HoursPerWeek")
par(mfrow = c(2, 1))
hist(data$TotalHours, main = "Histogram of TotalHours")
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
hist(data$TotalHours, main = "Histogram of TotalHours", xlim = c(0, 25000))
boxplot(data$TotalHours, main = "Histogram of TotalHours")
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
source("D:/xstk-2.0/BTL/codeR.R", echo=TRUE)
boxplot(data$APM, main = "Histogram of Action per minute")
source("D:/xstk-2.0/BTL/codeR.R", echo=TRUE)
specific_value <- 4
frequency <- sum(data$LeagueIndex == specific_value, na.rm = TRUE)
print(frequency)
specific_value <- 5
frequency <- sum(data$LeagueIndex == specific_value, na.rm = TRUE)
print(frequency)
specific_value <- 16
frequency <- sum(data$Age == specific_value, na.rm = TRUE)
print(frequency)
#đếm tần suất của tất cả các giá trị trong cột
frequency_table <- table(data$HoursPerWeek)
print(frequency_table)
par(mfrow = c(2, 1))
hist(data$APM, main = "Histogram of Action per minute")
boxplot(data$APM, main = "Histogram of Action per minute")
par(mfrow = c(2, 1))
hist(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
boxplot(data$UniqueHotkeys, main = "Histogram of UniqueHotkeys")
par(mfrow = c(2, 1))
hist(data$ActionLatency, main = "Histogram of ActionLatency")
boxplot(data$ActionLatency, main = "Histogram of ActionLatency")
par(mfrow = c(2, 1))
hist(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
boxplot(data$TotalMapExplored, main = "Histogram of TotalMapExplored")
# đếm số lần xuất hiện của 1 giá trị cụ thể trong cột
specific_value <- 16
frequency <- sum(data$Age == specific_value, na.rm = TRUE)
print(frequency)
#đếm tần suất của tất cả các giá trị trong cột
frequency_table <- table(data$HoursPerWeek)
print(frequency_table)
# đếm số lần xuất hiện của 1 giá trị cụ thể trong cột
specific_value <- 5
frequency <- sum(data$UniqueHotkeys == specific_value, na.rm = TRUE)
print(frequency)
#đếm tần suất của tất cả các giá trị trong cột
frequency_table <- table(data$HoursPerWeek)
print(frequency_table)
library(readr)
data <- read.csv("D:/xstk-2.0/BTL/skillcraft1+master+table+dataset/SkillCraft1_Dataset.csv")
View(data)
head(data, 10)
# chọn các biến quan tâm
data <- data[,c(2,3, 4,5,6, 9, 14, 16)]
# Kiểm tra sự hiện diện của "?" trong từng cột
has_question_mark <- sapply(data, function(x) sum(x == "?", na.rm = TRUE))
print(has_question_mark)
# Thay thế "?" bằng NA trong toàn bộ data frame
data[data == "?"] <- NA
# Chuyển đổi các cột từ kiểu ký tự sang kiểu số
data$Age <- as.numeric(as.character(data$Age))
data$HoursPerWeek <- as.numeric(as.character(data$HoursPerWeek))
data$TotalHours <- as.numeric(as.character(data$TotalHours))
# Tính toán trung bình của một cột và thay thế các giá trị "NA"
mean_value <- mean(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] <- mean_value
mean_value <- mean(data$HoursPerWeek, na.rm = TRUE)
data$HoursPerWeek[is.na(data$HoursPerWeek)] <- mean_value
mean_value <- mean(data$TotalHours, na.rm = TRUE)
data$TotalHours[is.na(data$TotalHours)] <- mean_value
# tổng quan dữ liệu
summary(data)
# vẽ biểu đồ
par(mfrow = c(2, 1))
hist(data$LeagueIndex, main = "Histogram of LeagueIndex", xlim = c(1,8))
boxplot(data$LeagueIndex, main = "Histogram of LeagueIndex")
par(mfrow = c(2, 1))
hist(data$Age, main = "Histogram of Age")
boxplot(data$Age, main = "Histogram of Age")
par(mfrow = c(2, 1))
hist(data$HoursPerWeek,
breaks = seq(0, 168, by = 12),  # Chia bins mỗi 12 giờ (nửa ngày)
xlim = c(0, 168),               # Giới hạn trục x từ 0 đến 168
#ylim = c(0, max(table(data$HoursPerWeek)) + 10),  # Giới hạn trục y dựa trên dữ liệu
xlab = "Hours Per Week",        # Nhãn trục x
ylab = "Frequency",             # Nhãn trục y
main = "Histogram of Hours Per Week")  # Tiêu đề biểu đồ
boxplot(data$HoursPerWeek, main = "Histogram of HoursPerWeek")
par(mfrow = c(2, 1))
hist(data$TotalHours, breaks = seq(0, 25000, by = 1000),  # Chia bins mỗi 1000 giờ
xlim = c(0, 25000),                 # Giới hạn trục x từ 0 đến 25.000
ylim = c(0, 600))
# đếm số lần xuất hiện của 1 giá trị cụ thể trong cột
specific_value <- 21
frequency <- sum(data$TotalHours == specific_value, na.rm = TRUE)
print(frequency)
# đếm số lần xuất hiện của 1 giá trị cụ thể trong cột
specific_value <- 21
frequency <- sum(data$TotalMapExplored == specific_value, na.rm = TRUE)
print(frequency)
specific_value <- 22
frequency <- sum(data$TotalMapExplored == specific_value, na.rm = TRUE)
print(frequency)
specific_value <- 23
frequency <- sum(data$TotalMapExplored == specific_value, na.rm = TRUE)
print(frequency)
specific_value <- 24
frequency <- sum(data$TotalMapExplored == specific_value, na.rm = TRUE)
print(frequency)
# Vẽ biểu đồ heatmap cho ma trận tương quan
corrplot(cor_matrix, method = "color", type = "lower",
tl.col = "black", tl.srt = 45,
addCoef.col = "black",
number.cex = 0.7, # Độ lớn của hệ số tương quan
col = colorRampPalette(c("red", "white", "blue"))(200),
title = "Correlation Matrix", mar = c(0, 0, 1, 0))
# Tính ma trận tương quan cho các biến số trong dataframe
cor_matrix <- cor(data[, c("LeagueIndex", "Age", "HoursPerWeek", "TotalHours")], use = "complete.obs")
# Vẽ biểu đồ heatmap cho ma trận tương quan
corrplot(cor_matrix, method = "color", type = "lower",
tl.col = "black", tl.srt = 45,
addCoef.col = "black",
number.cex = 0.7, # Độ lớn của hệ số tương quan
col = colorRampPalette(c("red", "white", "blue"))(200),
title = "Correlation Matrix", mar = c(0, 0, 1, 0))
# Tính ma trận tương quan cho các biến số trong dataframe
cor_matrix <- cor(data[, c("LeagueIndex", "Age", "HoursPerWeek", "TotalHours"
, "APM", "UniqueHotkeys", "ActionLatency", "TotalMapExplored")], use = "complete.obs")
# Vẽ biểu đồ heatmap cho ma trận tương quan
corrplot(cor_matrix, method = "color", type = "lower",
tl.col = "black", tl.srt = 45,
addCoef.col = "black",
number.cex = 0.7, # Độ lớn của hệ số tương quan
col = colorRampPalette(c("red", "white", "blue"))(200),
title = "Correlation Matrix", mar = c(0, 0, 1, 0))
# Tính ma trận tương quan cho các biến số trong dataframe
cor_matrix <- cor(data[, c("LeagueIndex", "Age", "HoursPerWeek", "TotalHours"
, "APM", "UniqueHotkeys", "ActionLatency", "TotalMapExplored")], use = "complete.obs")
# Vẽ biểu đồ heatmap cho ma trận tương quan
corrplot(cor_matrix, method = "color", type = "lower",
tl.col = "black", tl.srt = 45,
addCoef.col = "black",
number.cex = 0.7, # Độ lớn của hệ số tương quan
col = colorRampPalette(c("red", "white", "blue"))(200),
title = "Correlation Matrix", mar = c(0, 0, 1, 0))
source("D:/xstk-2.0/BTL/codeR.R", echo=TRUE)
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
library(ggplot2)
ggplot(mtcars, aes(x = drat, y = mpg)) +
geom_point()
# Tính ma trận tương quan cho các biến số trong dataframe
cor_matrix <- cor(data[, c("LeagueIndex", "Age", "HoursPerWeek", "TotalHours"
, "APM", "UniqueHotkeys", "ActionLatency", "TotalMapExplored")], use = "complete.obs")
# Vẽ biểu đồ heatmap cho ma trận tương quan
corrplot(cor_matrix, method = "color", type = "lower",
tl.col = "black", tl.srt = 45,
addCoef.col = "black",
number.cex = 0.7, # Độ lớn của hệ số tương quan
col = colorRampPalette(c("red", "white", "blue"))(200),
title = "Correlation Matrix", mar = c(0, 0, 1, 0))
# Tính ma trận tương quan cho các biến số trong dataframe
cor_matrix <- cor(data[, c("LeagueIndex", "Age", "HoursPerWeek", "TotalHours"
, "APM", "UniqueHotkeys", "ActionLatency", "TotalMapExplored")], use = "complete.obs")
# Vẽ biểu đồ heatmap cho ma trận tương quan
corrplot(cor_matrix, method = "color", type = "lower",
tl.col = "black", tl.srt = 45,
addCoef.col = "black",
number.cex = 0.7, # Độ lớn của hệ số tương quan
col = colorRampPalette(c("red", "white", "blue"))(200),
title = "Correlation Matrix", mar = c(0, 0, 1, 0))
source("D:/xstk-2.0/BTL/codeR.R", echo=TRUE)
corrplot(corr = cor(new_data[3:13]),
method = 'color',
order = 'FPC',
title = "correlation plot",
tl.cex = 0.7,
addCoef.col = 'red',
number.cex = 0.7,
addgrid.col = 0.1,
)
title = "correlation plot",
title = "correlation plot",
setwd("F:/R")
#setwd("F:/R")
m  = cor(data)
summary(m)
corrplot(m, method = "number")
#setwd("F:/R")
m  = cor(data)
summary(m)
corrplot(m, method = "number")
#setwd("F:/R")
m  = cor(data)
summary(m)
corrplot(m, method = "number")
#load the dataset
data(data)
#fit a regression model
model <- lm(mpg~disp+hp, data=mtcars)
#get list of residuals
res <- resid(model)
#load the dataset
data(data)
#fit a regression model
model <- lm(mpg~disp+hp, data=mtcars)
#get list of residuals
res <- resid(model)
#load the dataset
data(data)
#fit a regression model
model <- lm(mpg~disp+hp, data=mtcars)
#get list of residuals
res <- resid(model)
model <- lm(mpg ~ disp + hp, data = mtcars)
#load the dataset
data(mtcars)
#fit a regression model
model <- lm(mpg~disp+hp, data=mtcars)
#get list of residuals
res <- resid(model)
#-- Biểu đồ phần dư
fitted(formula)
#-- Biểu đồ phần dư
fitted(formula)
#-- Biểu đồ phần dư
# Load the dataset
data(mtcars)
# Fit a regression model
model <- lm(mpg ~ disp + hp, data = mtcars)
# Get the residuals
res <- resid(model)
# Produce residual vs. fitted plot
plot(fitted(model), res)
abline(0, 0)  # Add a horizontal line at 0
#-- Biểu đồ phần dư
# Load the dataset
data(mtcars)
# Fit a regression model
model <- lm(mpg ~ disp + hp, data = mtcars)
# Get the residuals
res <- resid(model)
# Produce residual vs. fitted plot
plot(fitted(model), res)
abline(0, 0)  # Add a horizontal line at 0
# Create Q-Q plot for residuals
qqnorm(res)
qqline(res)  # Add a straight diagonal line to the plot
# Create density plot of residuals
plot(density(res))
#-- Biểu đồ phần dư
# Load the dataset
data(mtcars)
# Fit a regression model
model <- lm(mpg ~ disp + hp, data = mtcars)
# ve 4 biểu đồ
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)
# Residuals vs Fitted
plot(model$fitted.values, resid(model),
main = "Residuals vs Fitted",
xlab = "Fitted values",
ylab = "Residuals")
abline(h = 0, col = "red")
# Q-Q Plot
qqnorm(resid(model))
qqline(resid(model), col = "red")
# Scale-Location Plot
plot(model$fitted.values, sqrt(abs(resid(model))),
main = "Scale-Location",
xlab = "Fitted values",
ylab = "Square Root of |Residuals|")
abline(h = 0, col = "red")
# Residuals vs Leverage
plot(model, which = 5)
summary(model)
#-- Biểu đồ phần dư
# Ví dụ mô hình hồi quy tuyến tính
model <- lm(LeagueIndex ~ Age + HoursPerWeek + TotalHours + APM + UniqueHotkeys + ActionLatency + TotalMapExplored, data = data)
# Vẽ các biểu đồ chẩn đoán
par(mfrow = c(2, 2)) # Cài đặt layout 2x2 cho 4 biểu đồ
plot(model)
summary(model)
#hồi quy tuyến tính
predictions <- predict(model, newdata = data)
#hồi quy tuyến tính
predictions <- predict(model, newdata = data)
# Vẽ các biểu đồ chẩn đoán
par(mfrow = c(2, 2)) # Chia cửa sổ đồ họa thành 4 phần
plot(model)
# Dự đoán giá trị LeagueIndex dựa trên dữ liệu hiện tại
predictions <- predict(model, newdata = data)
# Dự đoán giá trị LeagueIndex dựa trên dữ liệu hiện tại
predictions <- predict(model, newdata = data)
head(predictions)
# dự đoán
# Dự đoán giá trị LeagueIndex dựa trên dữ liệu hiện tại
predictions <- predict(model, newdata = data)
head(predictions)
# Ví dụ về dữ liệu mới
new_data <- data.frame(
Age = c(25, 30, 22),
HoursPerWeek = c(15, 20, 35),
TotalHours = c(2000, 5000, 300),
APM = c(100, 120, 85),
UniqueHotkeys = c(5, 7, 4),
ActionLatency = c(50, 45, 60),
TotalMapExplored = c(20, 25, 18)
)
# Dự đoán giá trị LeagueIndex cho dữ liệu mới
new_predictions <- predict(model, newdata = new_data)
# Hiển thị các dự đoán
new_predictions

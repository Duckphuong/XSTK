#1
#1.1 Đọc dữ liệu từ file CSV và hiển thị 6 hàng đầu của dữ liệu
auto_mpgData <- read.csv("~C:\HCMUT\233\XSTK\auto_mpg.csv",sep=";")
head(auto_mpgData)

#1.2a. Thay thế giá trị "?" bằng NA và kiểm tra số lượng giá trị NA trong mỗi cột
auto_mpgData[auto_mpgData == "?"] <- NA
apply(is.na(auto_mpgData), 2, sum)  # Tính tổng số giá trị NA trong mỗi cột

#1.2b. Loại bỏ các hàng chứa giá trị NA và kiểm tra lại số lượng giá trị NA
auto_mpgData <- na.omit(auto_mpgData)
apply(is.na(auto_mpgData), 2, sum)  # Kiểm tra lại số lượng giá trị NA sau khi loại bỏ

#2.2a. Kiểm tra kiểu dữ liệu của các cột
flagMpg <- is.numeric(auto_mpgData$mpg)
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
hist(clearData$mpg, ồmain = "Biểu đ phân phối của MPG", xlab = "MPG",
     col = heat.colors(9), labels = TRUE, ylim = c(0, 100))

# Biểu đồ hộp cho MPG theo từng khu vực
boxplot(clearData$mpg ~ clearData$origin, main = "Biểu đồ hộp của MPG theo khu vực", 
        ylab = "MPG", xlab = "Khu vực", col = c(2, 3, 4))

# Biểu đồ hộp cho MPG theo năm sản xuất
boxplot(clearData$mpg ~ clearData$model_year, main = "Biểu đồ hộp của MPG theo năm sản xuất", 
        ylab = "MPG", xlab = "Năm sản xuất", col = c(2, 3, 4, 5, 6))

# Biểu đồ hộp cho MPG theo số xy-lanh
boxplot(clearData$mpg ~ clearData$cylinders, main = "Biểu đồ hộp của MPG theo số xy-lanh", 
        ylab = "MPG", xlab = "Số xy-lanh", col = c(2, 3, 4, 5, 6))

# Biểu đồ phân tán giữa MPG và các biến liên tục
plot(clearData$displacement, clearData$mpg, main = "Biểu đồ phân tán của MPG và displacement", 
     ylab = "MPG", xlab = "Displacement", col = c(3))

plot(clearData$horsepower, clearData$mpg, main = "Biểu đồ phân tán của MPG và horsepower", 
     ylab = "MPG", xlab = "Horsepower", col = c(2))

plot(clearData$weight, clearData$mpg, main = "Biểu đồ phân tán của MPG và weight", 
     ylab = "MPG", xlab = "Weight", col = c(4))

plot(clearData$acceleration, clearData$mpg, main = "Biểu đồ phân tán của MPG và acceleration", 
     ylab = "MPG", xlab = "Acceleration", col = c(6))
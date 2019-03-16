# Ex_1.1

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Thao phD/1.csv')
data
# sub_1 cho Nam song o Hai Dao

sub_1 = subset(data, (data$GioiTinh == 'Nam') & (data$KhuVuc == 'HaiDao'))
sub_1

# sub_2 cho Nu song o Nong Thon

sub_2 = subset(data, (data$GioiTinh == 'Nu') & (data$KhuVuc == 'NongThon'))
sub_2

# Tinh tong Nam o Hai Dao va Nu o Nong Thon

length(sub_1$GioiTinh)
length(sub_2$GioiTinh)

# Ti le Nu song o Thanh Pho

sub_3 = subset(data, (data$GioiTinh == 'Nu') & (data$KhuVuc == 'ThanhPho'))
sub_Nu = subset(data, data$GioiTinh == 'Nu')
length(sub_3$GioiTinh)/length(sub_Nu$GioiTinh)

# Ti le Nu song o Mien Nui

sub_4 = subset(data, (data$GioiTinh == 'Nu') & (data$KhuVuc == 'MienNui'))
length(sub_4$GioiTinh)/length(sub_Nu$GioiTinh)

# Phan to Du lieu

nhom_1 = subset(data, (data$Tuoi >= 20) & (data$Tuoi <30))
nhom_2 = subset(data, (data$Tuoi >= 30) & (data$Tuoi < 40))
nhom_3 = subset(data, (data$Tuoi >= 30) & (data$Tuoi <40))
nhom_4 = subset(data, (data$Tuoi >= 40) & (data$Tuoi <50))
nhom_5 = subset(data, (data$Tuoi >= 50) & (data$Tuoi <60))

# Tinh ty le

length(nhom_1$Tuoi)/length(data$Tuoi)

# Phan to thu nhap

nhomtn_1 = subset(data, (data$ThuNhap >= 20) & (data$ThuNhap < 40))
nhomtn_2 = subset(data, (data$ThuNhap >= 60) & (data$ThuNhap < 80))
nhomtn_3 = subset(data, (data$ThuNhap >= 80) & (data$ThuNhap < 100))

# Ti le nguoi phai dong thue thu nhap
nguoi_dong_thue = subset(data, data$ThuNhap > 60)
nguoi_dong_thue
length(nguoi_dong_thue)/length(data$ThuNhap)

# Ti le nguoi co thu nhap > 80 trieu trong do tuoi (40, 50)

dong_thue_2 = subset(data, (data$ThuNhap > 80) & (data$Tuoi >= 40) & (data$Tuoi < 50))
dong_thue_2
length(dong_thue_2$ThuNhap)/length(data$ThuNhap)

# Ex_1.2

x = c(61, 27 ,26, 37, 30, 47, 63, 46, 67, 19, 81, 47,
      45, 60, 65, 53, 35, 28, 57, 37, 45, 25, 48, 60,
      30, 47, 60, 61, 55, 48)
mean(x)
median(x)
mode(x)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(x)

quantile(x, probs = c(0.1, 0.6, 0.9))
boxplot(x)

# Do phan tan

sigma = ((length(x) - 1)/length(x)) * (sd(x))^2
sigma
var(x)
sd(x)


# Ex_1.3

x = c(61.4, 27.3, 26.4, 37.4, 30.4, 47.5,
      63.9, 46.8, 67.9, 19.1, 81.6, 47.9, 73.4, 54.6, 65.1, 53.3, 71.6, 58.6,
      57.3, 87.8, 71.1, 74.1, 48.9, 60.2, 54.8, 60.5, 32.5, 61.7, 55.1, 48.2,
      56.8, 60.1, 52.9, 60.5, 55.6, 38.1, 76.4, 46.8, 19.9, 27.3, 77.4, 58.1,
      32.1, 54.9, 32.7, 40.1, 52.7, 32.5, 35.3, 39.1
)

range(x)
length(x)
to = seq(19.1, 90, by = 10)
to

bieudo = hist(x, breaks = to, labels = TRUE, xlim = c(19.1, 100), ylim = c(0, 15), col = rainbow(7))
bieudo

hoanhdo = c(min(bieudo$breaks), bieudo$mids, max(bieudo$breaks))
tungdo = c(0, bieudo$count, 0)

lines(hoanhdo, tungdo)
  
mean(x)
sd(x)

# Ex-1.6

diem_chia = seq(0, 7.5, by = 1.5)
diem_chia = c(diem_chia, 9.5)
diem_chia

tanso = c(115, 170, 168, 127, 68, 18)

bieu_do = barplot(tanso, width = 1.5, space = 0, col = "blue")
bieu_do

# Ex 1_7

x = c(2656, 2301, 2975, 3002, 2468, 2742, 2830, 2405, 2677, 2990,
2200, 2764, 2337, 2961, 3010, 2976, 2375, 2602, 2670, 2922,
2344, 2760, 2555, 2524, 2814, 2996, 2437, 2268, 2448, 2460)

sort(x)

stem.leaf(x, depths = FALSE)
boxplot(x)
hist(x)

# EX 1_8

theloai = c('R&B', 'Rock', 'Rap', 'dongque', 'codien', 'latin')
soluong = c(146.4, 102.6, 73.7, 64.5, 14.8, 14.5)

pie(soluong, labels = theloai)

# Ex 1_10

thunhap = c(30, 20, 35, 27, 37, 30, 38, 40, 42, 35, 38, 44,
41, 42, 47, 48, 43, 50, 55, 60, 70, 45, 47, 49,
78, 55, 60, 85, 90, 100, 75, 80, 85, 95, 105, 47)

mean(thunhap)
median(thunhap)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(thunhap)

hist(thunhap)
boxplot(thunhap)
stem.leaf(thunhap, depths = FALSE)


# Ex_11

x = c(5, 30, 100, 150, 600, 210, 85, 95, 105, 120, 185, 250,
      310, 1500, 500, 425, 480, 450, 550, 175, 180, 285, 350, 450, 750, 65, 55, 880, 1350, 270)

range(x)
q = quantile(x, c(0.25, 0.5, 0.75))
sd(x)
q[3] - q[1]
boxplot((x))

# EX_12

tsum.test(mean.x = 125, s.x = 12, n.x = 100, conf.level = 0.68)

# EX_13

2*pnorm(2, mean = 0, sd = 1)- 1
2*pnorm(4, mean = 0, sd = 1) - 1
pnorm(50, mean = 38, sd = 6)
zsum.test(mean.x = 38, sigma.x = 6, n.x = 100, conf.level = 0.89)




source("read_data.R")

coupon1_mean = mean(coupon1Used)
coupon1_error = sum((abs(coupon1Used - coupon1_pred) / coupon1_mean)^2)

coupon2_mean = mean(coupon2Used)
coupon2_error = sum((abs(coupon2Used - coupon2_pred) / coupon2_mean)^2)

coupon3_mean = mean(coupon3Used)
coupon3_error = sum((abs(coupon3Used - coupon3_pred) / coupon3_mean)^2)

basketValue_mean = mean(basketValue)
basketValue_error = sum((abs(basketValue - basketValue_pred) / 
												 basketValue_mean)^2)

error = coupon1_error + coupon2_error + coupon3_error + basketValue_error

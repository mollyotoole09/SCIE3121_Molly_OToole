##27/02 - First attempt at analysing BioTIME dataset ---- 
rainbow_bay <- read.csv("~/SCIE3121_Molly_OToole/data/raw_data_328.csv")
rainbow_bay

#Could have sorted by GENUS_SPECIES 
y_vals_ao <- rainbow_bay[1:30, 1] 
x_vals_ao <- rainbow_bay[1:30, 10]
plot(x_vals_ao, y_vals_ao, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Ambystoma opacum')

y_vals_at <- rainbow_bay[31:60, 1]
x_vals_at <- rainbow_bay[31:60, 10]
plot(x_vals_at, y_vals_at, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Ambystoma talpoideum')

y_vals_ati <- rainbow_bay[61:91, 1]
x_vals_ati <- rainbow_bay[61:91, 10]
plot(x_vals_ati, y_vals_ati, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Ambystoma tigrinum')

y_vals_as <- rainbow_bay[92:121, 1]
x_vals_as <- rainbow_bay[92:121, 10]
plot(x_vals_as, y_vals_as, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Anaxyrus sp')

y_vals_eq <- rainbow_bay[122:151, 1]
x_vals_eq <- rainbow_bay[122:151, 10]
plot(x_vals_eq, y_vals_eq, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Eurycea quadridigitatta')

y_vals_gc <- rainbow_bay[152:181, 1]
x_vals_gc <- rainbow_bay[152:181, 10]
plot(x_vals_gc, y_vals_gc, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Gastrophryne carolinensis')

y_vals_ls <- rainbow_bay[182:211, 1]
x_vals_ls <- rainbow_bay[182:211, 10]
plot(x_vals_ls, y_vals_ls, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Lithobates sp')

y_vals_pc <- rainbow_bay[212:241, 1]
x_vals_pc <- rainbow_bay[212:241, 10]
plot(x_vals_pc, y_vals_pc, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Pseudacris crucifer')

y_vals_po <- rainbow_bay[242:271, 1]
x_vals_po <- rainbow_bay[242:271, 10]
plot(x_vals_po, y_vals_po, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Pseudacris ornata')

y_vals_sh <- rainbow_bay[272:301, 1]
x_vals_sh <- rainbow_bay[272:301, 10]
plot(x_vals_sh, y_vals_sh, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Scaphiopus holbrookii')
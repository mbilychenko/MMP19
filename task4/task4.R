v = 1.5 #m/sec
H = 2 #m
W = 10 #m
max_period = 360 #number of periods

#paramenters - ochistka
point_clean_factor = 0 #pollution cleaning adjustment 0- nothing, 1 - 50%,2 - 100%, - for zavof 
point_clean_factor_2 = 2 #pollution cleaning adjustment 0- nothing, 1 - 50%,2 - 100%,  - for ferma
adj_pollution_polya_lis = 1 #lisonasadgenya, 0 - no, 1 - 1 linua, 2 - 2 linii, 3 -3 linii
adj_pollution_polya_aero = 1 #aeranka 0 - no, 1 yes
adj_pollution_polya_oranka = 1 #oranka
# paramenters - dobriva real
dobr_az = 10 #kg/gektar - azotni dobriva
dobr_ka = 10
dobr_fo = 10
dobr_org = 10000 #kg/ga
dobr_vapno = 1000 #kg/ga
dobr_metafos = 10 #kg/ga
dobr_hvorob = 1.5 #kg/ga
dobr_byrian = 2 
#parametrs  - gektary for tovary (max gektar - 300 #gektar)
zito_plosha = 10
kykyr_plosha = 10
yachmin_plosha = 10
pshen_plosha = 10
kart_plosha = 10



#dobriva max
dobr_az_max = 50 #kg/gektar - azotni dobriva
dobr_ka_max = 50
dobr_fo_max = 50
dobr_org_max = 20000 #kg/ga
dobr_vapno_max = 2000 #kg/ga
dobr_metafos_max = 30 #kg/ga
dobr_hvorob_max = 3.5 #kg/ga
dobr_byrian_max = 6 


#prices
price_1_good = 1000
svin_price = 500 #ymovnih odiniz
korov_price = 1000 #ymovnih odiniz
kart_price = 7 #ym odiniz za kg
pshen_price = 2
yachmin_price = 5
zito_price = 2
kykyr_price = 5.5
cost_per_1perc_pollution = 500 #ymovnih odiniz
per_1_perc_pollution_desease = 500

L_1 = 5000 #m - rastoyanie zavoda do mesta zbora
L_2 = 1000#m - rastoyanie fermy do mesta zbora
n_svin = 500
n_korov = 500 
Y = 50 #-proizvodstvo tovarov



#initialization
total_cost = 0
total_revenue = 0
econom_cost = 0
cost_mas = rep(NA, max_period)
ec_cost_mas = rep(NA, max_period)
revenue_mas = rep(NA, max_period)

t=0
for (t in 1:max_period) {
  
  #rashod vody vsego
  Q = v*(H*W) #m3/sec
  if (t%%360 %in% 45:90) {Q = 1.1*Q} #pavodok (45-90) dney s nachala goda
    
  ######    zavod     ######

  

  #norma
  c_post_1 = 2000 #mg/l
  
  #rashod vody zavod
  q_1 = Y*0.1/86400 #m3/sec 
  #koef turbulentnosti
  E = v*H/90
  gamma_1 = 1*1*(E/q_1)^(1/3)
  #koef smesheniya
  a_1 = (1-exp(-gamma_1*L_1^(1/3)))/(1+Q/q_1*exp(-gamma_1*L_1^(1/3)))
  #koef rasbavleniya stochih vod
  n_1 = (a_1*Q+q_1)/q_1
  
  #bpk norm
  c_pdk = 2.0 #gr/m3 - 2 dlya pitiya, 4 - dlya hoziastva
  c_pv = 1.9 #gr/m3
  c_f_0 = 0 #gr/m3 - base level
  k0 = 0.065/86400
  t0_1 = L_1/v
  
  c_pds_1 = n_1*((c_pdk - c_pv)*exp(k0*t0_1)-c_f_0) + c_f_0 #gr/m3 = mgr/l
  if ((c_pds_1 < c_post_1) && (c_pds_1 > 0)){
    print("Nado ochistka 1")
    print(c_pds_1)
    c_adjust_1 = (c_post_1 - c_pds_1)/c_post_1*100
    c_f_1 = c_post_1*q_1
  } else {
    print("ochistka 1 - ne nado")
    print(c_pds_1)
    c_adjust_1 = 0 
    c_f_1 = c_post_1*q_1
  }
  
  #choose percent to clean from factory
  if (point_clean_factor == 1) {
    perc_pollution_clean = c_adjust_1/2
  } else if (point_clean_factor == 2) {
    perc_pollution_clean = c_adjust_1
  } else {
    perc_pollution_clean = 0
  }
  
  #calculate costs/revenue/ec zbitok
  cost = perc_pollution_clean*cost_per_1perc_pollution
  revenue = Y*price_1_good
  ec_cost = (c_adjust_1 - perc_pollution_clean)*per_1_perc_pollution_desease
  total_cost = total_cost + cost
  total_revenue = total_revenue + revenue
  econom_cost = econom_cost + ec_cost 
  
  
  
  
  
  
  
  ######    ferma   #######
  
  
  #rashod vody 
  q_2 = (n_svin*4.5+n_korov*14)/1000/864 #m3/sec 
  #koef turbulentnosti
  E = v*H/90
  gamma_2 = 1*1*(E/q_2)^(1/3)
  #koef smesheniya
  a_2 = (1-exp(-gamma_2*L_2^(1/3)))/(1+Q/q_2*exp(-gamma_2*L_2^(1/3)))
  #koef rasbavleniya stochih vod
  n_2 = (a_2*Q+q_2)/q_2
  
  #bpk norm
  c_pdk = 2.0 #gr/m3 - 2 dlya pitiya, 4 - dlya hoziastva
  c_pv = 1.9 #gr/m3
  k0 = 0.065/86400
  t0_2 = L_2/v
  
  c_post_2 = (6000*4.5*n_svin + 8000*14*n_korov)/(4.5*n_svin+14*n_korov) #mg/litr
  c_f_1 = 0
  
  c_pds_2 = n_2*((c_pdk - c_pv)*exp(k0*t0_2)-c_f_1) + c_f_1 #gr/m3 = mgr/l
  if ((c_pds_2 < c_post_2) && (c_pds_2 > 0)){
    print("Nado ochistka 1")
    print(c_pds_2)
    c_adjust_2 = (c_post_2 - c_pds_2)/c_post_2*100
  } else {
    print("ochistka 1 - ne nado")
    print(c_pds_2)
    c_f_2 = c_post_2*q_2
    c_adjust_2 = 0
  }
  
  
  #choose percent to clean from factory
  if (point_clean_factor_2 == 1) {
    perc_pollution_clean_2 = c_adjust_2/2
  } else if (point_clean_factor_2 == 2) {
    perc_pollution_clean_2 = c_adjust_2
  } else {
    perc_pollution_clean_2 = 0
  }
  
  
  #calculate costs/revenue/ec zbitok
  cost_2 = perc_pollution_clean_2*cost_per_1perc_pollution
  revenue_2 = svin_price*ifelse(n_svin >= 1, 1, 0)+ korov_price*ifelse(n_korov >= 1, 1, 0)
  ec_cost_2 = (c_adjust_2 - perc_pollution_clean_2)*per_1_perc_pollution_desease
  
  total_cost = total_cost + cost_2
  total_revenue = total_revenue + revenue_2
  econom_cost = econom_cost + ec_cost_2 
  n_svin = n_svin - 1 + ifelse(t%%70 == 0, 30, 0)
  n_korov = n_korov - 1  + ifelse(t%%150 == 0, 10, 0)
  
  
  
  ######    polya   #######
  
  total_plosha = kart_plosha + pshen_plosha + yachmin_plosha + zito_plosha + kykyr_plosha
  ## dobryva
  
  
  urozay_pers_adjust = (dobr_az/dobr_az_max + dobr_ka/dobr_ka_max + 
      dobr_fo/dobr_fo_max + dobr_org/dobr_org_max + 
      dobr_vapno/dobr_vapno_max + dobr_metafos/dobr_metafos_max + 
      dobr_hvorob/dobr_hvorob_max + dobr_byrian/dobr_byrian_max)/8
  cost_dobriva = (dobr_az*total_plosha*35 + dobr_ka*total_plosha*32 + 
    dobr_fo*total_plosha*37 + dobr_org*total_plosha*2 + dobr_vapno*total_plosha*10 +
    dobr_metafos*total_plosha*50 + dobr_hvorob*total_plosha*100 + dobr_byrian*total_plosha*85)/360
    
  ## gektar polya
    
  revenue_kart = (1000*kart_price*kart_plosha*(10+40*urozay_pers_adjust))/360 
  revenue_pshen = (1000*pshen_price*pshen_plosha*(2+5*urozay_pers_adjust))/360 
  revenue_yachmin = (1000*yachmin_price*yachmin_plosha*(2+4*urozay_pers_adjust))/360 
  revenue_zito = (1000*zito_price*zito_plosha*(1+5*urozay_pers_adjust))/360 
  revenue_kykyr = (1000*kykyr_price*kykyr_plosha*(2+5*urozay_pers_adjust))/360 
  revenue_polya_total = revenue_kart + revenue_pshen + revenue_yachmin + revenue_zito + revenue_kykyr
  
  cost_polya_urozay = (kart_plosha*kart_price/2 + pshen_plosha*pshen_price/2 +yachmin_plosha*yachmin_price/2 +
    zito_plosha*zito_price/2 + kykyr_plosha*kykyr_price/2)*1000/360
  cost_polya_total = cost_polya_urozay + adj_pollution_polya_lis*50000/360 + 
    adj_pollution_polya_aero*100000/360 + adj_pollution_polya_oranka*75000/360
  
  
  #economic cost
  perc_pollution_desease_polya = urozay_pers_adjust*
    ifelse(adj_pollution_polya_lis > 0, 1-0.2*adj_pollution_polya_lis, 1)*
    ifelse(adj_pollution_polya_aero > 0, 0.7, 1)*
    ifelse(adj_pollution_polya_oranka > 0, 0.7, 1)
  econom_cost_polya = perc_pollution_desease_polya*100*per_1_perc_pollution_desease
  
  total_cost = total_cost + cost_dobriva + cost_polya_total
  total_revenue = total_revenue + revenue_polya_total
  econom_cost = econom_cost + econom_cost_polya
  
  
  cost_mas[t] = total_cost
  revenue_mas[t] = total_revenue
  ec_cost_mas[t] = econom_cost
  
  t=t+1
}
 

df <- data.frame(periods = 1:max_period,cost_mas, revenue_mas, ec_cost_mas)
df
 




library(ggplot2)
g1 <- ggplot(df, aes(x =periods, y = revenue_mas - cost_mas))
g1 + geom_line() + theme_classic() +
    labs(x = "Період", y = "Умовні одиниці") 

g1 + geom_line(size = 2) + 
    labs(x = "Період", y = "Населення країни", col='Частка витрат\nна дослідження, %') + 
    theme_classic() +# + annotate("rect", xmin=0, xmax=10, ymin=0, ymax=Inf, alpha=0.2, fill="red") 
    #theme(legend.position = c(0.8, 0.8)) + 
    scale_colour_gradient(low="blue", high="red")

plot(df$periods, df$revenue_mas - df$cost_mas, type = "l", ylim = c(0,max(df$ec_cost_mas, df$revenue_mas - df$cost_mas)))  
lines(df$ec_cost_mas, col = "red")





  #https://www.chem-astu.ru/chair/study/engmet-ooc/?p=193
  #http://lib.ssga.ru/fulltext/umk/auumk/ecology/rab5_2.htm
  #https://studopedia.info/4-109769.html
  #https://studfiles.net/preview/5615547/page:4/
  
  

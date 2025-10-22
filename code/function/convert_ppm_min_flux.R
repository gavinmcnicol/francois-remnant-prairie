convert_ppm_min_flux <- function(gas, estimate, total_vol_m3, average_temp_kelvin){
  
  if(gas == "co2"){
    estimate * ( (1.01*10^5*total_vol_m3) / (8.3145*average_temp_kelvin) ) / (0.05*60)
    # umol/mol min-1 * (P (J/m2) * V (m3) / r (J/molK) * K) (mol) / (m2 * 60 s)
    # results in flux umol_m2_s1
  } else {
    estimate * ( (1.01*10^5*total_vol_m3) / (8.3145*average_temp_kelvin) ) / (0.05*60) * 1000
    # umol/mol min-1 * (P (J/m2) * V (m3) / r (J/molK) * K) (mol) / (m2 * 60 s) * 1000 nm/um
    # results in flux nmol_m2_s1
  }
  
}
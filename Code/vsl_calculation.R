
# input parameters
vsl_epa = 11500000 # source = EPA (https://www.epa.gov/environmental-economics/mortality-risk-valuation), inflating 2006 USD to 2024 using BLS inflator
inc_e = 1 # income elasticity: percent change in VSL for a percent change in income (see Viscusi, 2015 for review of this value https://doi.org/10.1162/ajhe_a_00002)

# GDP per capita, PPP adjusted: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
# Data obtained for year 2023 for US and global average 
# inflation adjusted for 2024 USD using FRED deflator
inc_usa = 82223 * (139.716/136.417)
inc_global = 22850.4*(139.716/136.417) 

# global vsl
vsl_global = (inc_global/inc_usa)*inc_e*vsl_epa
vsl_global

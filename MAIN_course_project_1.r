library("dplyr")
source("functions_lib.r")
#-----------------------------------------------------------------------------
my_path =  getwd() ; 
file_name = "household_power_consumption.txt" ; 
complete_path = paste0( my_path, "/", file_name );
#--------------------------- load entire dataset -----------------------------
my_ds = read.delim( file = complete_path, sep = ";", header = TRUE  );
#--------------------------- extract only the two days -----------------------
HHPC_2d = filter( .data = my_ds, Date == "1/2/2007" | Date == "2/2/2007" ); 
vec_GAP_kW = as.double( as.matrix ( HHPC_2d$Global_active_power ) [ , 1 ] )   ;
vec_GRP_kW = as.double( as.matrix ( HHPC_2d$Global_reactive_power ) [ , 1 ] )   ;
vec_Voltage = as.double( as.matrix ( HHPC_2d$Voltage ) [ , 1 ] )   ;
vec_Subm_1 = as.double( as.matrix ( HHPC_2d$Sub_metering_1 ) [ , 1 ] )   ;
vec_Subm_2 = as.double( as.matrix ( HHPC_2d$Sub_metering_2 ) [ , 1 ] )   ;
vec_Subm_3 = as.double( as.matrix ( HHPC_2d$Sub_metering_3 ) [ , 1 ] )   ;

Sys.setlocale( category = "LC_ALL",locale = "UK" );
DateTime = strptime( paste( HHPC_2d$Date,  HHPC_2d$Time ) , "%d/%m/%Y %H:%M:%S") 

# ------------------ clean the variables   -----------------------------------
rm( list = c("my_ds", "my_path", "complete_path") ) ;

#------------------------ PLOT 1 ------------------------------------------------------------------
plot1( vec_GAP_kW, "Global active power (kilowatts)" );

#------------------------ PLOT 2 ------------------------------------------------------------------
plot2( DateTime, vec_GAP_kW , "Global active power (kilowatts)" );

#------------------------ PLOT 3 ------------------------------------------------------------------
plot3( DateTime , vec_Subm_1 , vec_Subm_2 , vec_Subm_3 );

#------------------------ PLOT 4 ------------------------------------------------------------------
plot4( DateTime, vec_GAP_kW, vec_Voltage, vec_Subm_1, vec_Subm_2, vec_Subm_3, vec_GRP_kW );


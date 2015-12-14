#------------------------ PLOT 1 ------------------------------------------------------------------
fct_1_plot1 <- function( vec_GAP_kW, my_xlabel )
{
  my_hist_GAP = hist( x = vec_GAP_kW , plot = FALSE   )  # ... extract histogram specific data
  min_y = min( my_hist_GAP$counts );
  max_y = max( my_hist_GAP$counts ); 
  
  hist( x = vec_GAP_kW , col = "red" 
        , xlab = my_xlabel  
        , ylab = "Frequency"
        , main = my_xlabel );
  abline( h = seq(from = min_y, to = max_y, by = 200 ), lty = 3, col = "gray");
}    

#------------------------ PLOT 2 ------------------------------------------------------------------
fct_1_plot2 <- function( DateTime, vec_TimeSeries,  my_ylabel )
{ 
  times_txt = c("midnight", "6 am",  "midday", "6 pm"  );
  times_seq_graph = seq(from = min( DateTime ), to = max( DateTime ) + 3 * 3600, by = 3 * 3600 );
  times_seq_lab = seq(from = min( DateTime ), to = max( DateTime ) + 6 * 3600, by = 6 * 3600 );
  
  plot( DateTime, vec_TimeSeries, type = "l", col = "black"
        , ylab = my_ylabel 
        , xlab = "Date and Time");
  abline( v = times_seq_graph , lty = 3, lwd = 1 , col = "gray" );
  abline( v = seq(from = min(DateTime), to = max( DateTime ) + 24 * 3600, by = 24 * 3600 ), lty = 3, lwd = 2 , col = "darkred" );
  
  text(   x = times_seq_lab
          , y = 0.9 * max( vec_TimeSeries )
          , labels = times_txt, col = "darkred"
          , pos = 2, cex = 1.15, srt = 90 );
  points( x = times_seq_lab
          , y = rep( 0.9 * max( vec_TimeSeries ), length( times_seq_lab ))
          , pch = 20, cex = 1.1, col = "darkred" );
}


#------------------------ PLOT 3 ------------------------------------------------------------------
fct_1_plot3 <- function( GAP_DateTime, vec_Subm_1, vec_Subm_2, vec_Subm_3 )
{ 
  times_txt = c("midnight", "6 am",  "midday", "6 pm"  );
  times_seq_graph = seq( from = min(GAP_DateTime), to = max( GAP_DateTime ) + 3 * 3600, by = 3 * 3600 );
  times_seq_lab = seq( from = min(GAP_DateTime), to = max( GAP_DateTime ) + 6 * 3600, by = 6 * 3600 );
  
  plot( GAP_DateTime, vec_Subm_1, type = "l", col = "black"
        , ylab = "Energy sub metering" 
        , xlab = "Date and Time");
  lines(x = GAP_DateTime, y = vec_Subm_2, lty = 1, col = "red");
  lines(x = GAP_DateTime, y = vec_Subm_3, lty = 1, col = "blue");
  
  abline( v = times_seq_graph , lty = 3, lwd = 1 , col = "gray" );
  abline( v = seq( from = min( GAP_DateTime ), to = max( GAP_DateTime ) + 24 * 3600, by = 24 * 3600 ), lty = 3, lwd = 2 , col = "darkred" );
  
  text(   x = times_seq_lab
          , y = 0.9 * max( vec_Subm_1 )
          , labels = times_txt, col = "darkred"
          , pos = 2, cex = 1.15, srt = 90 );
  points( x = times_seq_lab
          , y = rep( 0.9 * max( vec_Subm_1 ), length( times_seq_lab ))
          , pch = 20, cex = 1.1, col = "darkred" );
}

#------------------------ PLOT 4 ------------------------------------------------------------------
fct_1_plot4 <- function( DateTime
                         , vec_GAP_kW
                         , vec_Voltage
                         , vec_Subm_1, vec_Subm_2, vec_Subm_3
                         , vec_GRP_kW )
{ 
  
  
  par( mfrow = c( 2 , 2 ));
  #------------------------ Global active power ----------------
  fct_1_plot2( DateTime, vec_GAP_kW, "Global active power (kilowatts)" );
  #------------------------ Voltage ----------------------------
  fct_1_plot2( DateTime, vec_Voltage, "Voltage" );
  #------------------------ Submetering-------------------------
  fct_1_plot3( DateTime, vec_Subm_1 , vec_Subm_2 , vec_Subm_3 );
  #------------------------ Global reactive power ----------------
  fct_1_plot2( DateTime, vec_GRP_kW, "Global reactive power (kilowatts)" );
}

#------------------------ PLOT 1 PNG  ----------------------------------------------------------
plot1 <- function( vec_GAP_kW, my_xlabel )
{
  png(filename = "Plot1.png",
      width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white");
  
  fct_1_plot1( vec_GAP_kW, my_xlabel );
  dev.off()
}

#------------------------ PLOT 2 PNG  ----------------------------------------------------------
plot2 <- function( DateTime , vec_TimeSeries, my_ylabel )
{
  png(filename = "Plot2.png",
      width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white");
  
  fct_1_plot2( DateTime , vec_TimeSeries, my_ylabel );
  dev.off()
}

#------------------------ PLOT 3 PNG  ----------------------------------------------------------
plot3 <- function( DateTime , vec_Subm_1, vec_Subm_2, vec_Subm_3 )
{
  png(filename = "Plot3.png",
      width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white");
  
  fct_1_plot3( DateTime , vec_Subm_1, vec_Subm_2, vec_Subm_3 );
  dev.off()
}

#------------------------ PLOT 4 PNG  ----------------------------------------------------------
plot4 <- function( DateTime, vec_GAP_kW, vec_Voltage , vec_Subm_1, vec_Subm_2, vec_Subm_3, vec_GRP_kW )
{
  png(filename = "Plot4.png",
      width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white");
  
  fct_1_plot4( DateTime, vec_GAP_kW, vec_Voltage , vec_Subm_1, vec_Subm_2, vec_Subm_3, vec_GRP_kW );
  dev.off()
}

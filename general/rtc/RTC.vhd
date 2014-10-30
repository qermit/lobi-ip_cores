----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:14:14 09/02/2014 
-- Design Name: 
-- Module Name:    RTC - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;
--use IEEE.STD_LOGIC_ARITH.ALL;
--use IEEE.STD_LOGIC_UNSIGNED.ALL;
library work;
use work.func.all;

entity RTC is
    GENERIC(
			ZeroYear  : std_logic_vector(15 downto 0):= x"2008";
			g_CLK_FREQ     : natural := 50000000;  -- in Hz
         g_REFRESH_RATE : natural :=   1   -- in Hz
	 );
    Port ( clk : in  STD_LOGIC;
	        rst : in  STD_LOGIC;
           sync : in  STD_LOGIC;
           utc_timestamp : in  STD_LOGIC_VECTOR (31 downto 0);
           synchronized : out  STD_LOGIC;
           bcd_year_o : out  STD_LOGIC_VECTOR (15 downto 0);
           bcd_doy_o : out  STD_LOGIC_VECTOR (11 downto 0);
           bcd_moy_o : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_dom_o : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_hour_o : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_minute_o : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_second_o : out  STD_LOGIC_VECTOR (7 downto 0)
           );
end RTC;

architecture Behavioral of RTC is

	--CONSTANT	CLK_in_ps			: INTEGER	:= (1000000000 / (g_CLK_FREQ / 1000 / 2)); -- muss eigentlich clk-halbe sein
  constant c_REFRESH_CNT_INIT     : natural := natural(g_CLK_FREQ/(2 * g_REFRESH_RATE)) - 1;
  constant c_REFRESH_CNT_NB_BITS  : natural := log2_ceil(c_REFRESH_CNT_INIT);

    COMPONENT counter_year_day
    PORT(
         clk_i : IN  std_logic;
         rst_i : IN  std_logic;
         inc_i : IN  std_logic;
         dec_i : IN  std_logic;
         bcd_year_o : OUT  std_logic_vector(15 downto 0);
			bcd_doy_o : OUT std_logic_vector(11 downto 0);
         leap_year_o : OUT  std_logic;
         leap_year_next_o : OUT  std_logic;
         leap_year_prev_o : OUT  std_logic
        );
    END COMPONENT;
	 
	 
	 COMPONENT counter_param
	 GENERIC(
			ResetVal  : std_logic_vector(7 downto 0):= x"00";
			MaxVal  : std_logic_vector(7 downto 0):= x"59"
	 );
    PORT(
         clk_i : IN  std_logic;
         rst_i : IN  std_logic;
         inc_i : IN  std_logic;
         dec_i : IN  std_logic;
         bcd_value_o : OUT  std_logic_vector(7 downto 0);
         high_inc_o : OUT  std_logic;
         high_dec_o : OUT  std_logic
        );
    END COMPONENT;
	 
  signal r_utc_time: STD_LOGIC_VECTOR(31 downto 0);
  signal r_utc_time_bcd: STD_LOGIC_VECTOR(31 downto 0);
  signal r_utc_time_diff: STD_LOGIC_VECTOR(31 downto 0);
  
  signal r_bcd_year : STD_LOGIC_VECTOR (15 downto 0);
  signal r_bcd_doy : STD_LOGIC_VECTOR (11 downto 0);
  signal r_bcd_moy : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_dom : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_hour : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_minute : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_second : STD_LOGIC_VECTOR (7 downto 0);
  
  signal s_1pps : std_logic;
  signal s_synchronized: std_logic;
  signal refresh_rate_cnt   : unsigned(c_REFRESH_CNT_NB_BITS - 1 downto 0);
  
  
  signal cmp_inc, cmp_dec: std_logic;
  
  signal s_second_dec: std_logic_vector(1 downto 0):= "00";
  signal s_minute_dec: std_logic_vector(1 downto 0):= "00";
  signal s_hour_dec: std_logic_vector(1 downto 0):= "00";
  signal s_day_dec: std_logic_vector(1 downto 0):= "00";
  signal s_minute_dec_sync: std_logic := '0';
  signal s_hour_dec_sync: std_logic := '0';
  signal s_day_dec_sync: std_logic := '0';
	
  signal s_second_inc: std_logic_vector(1 downto 0):= "00";
  signal s_minute_inc: std_logic_vector(1 downto 0):= "00";
  signal s_hour_inc: std_logic_vector(1 downto 0):= "00";
  signal s_day_inc: std_logic_vector(1 downto 0):= "00";
  signal s_minute_inc_sync: std_logic:= '0';
  signal s_hour_inc_sync: std_logic:= '0';
  signal s_day_inc_sync: std_logic:= '0';

  signal s_year_leap_next, s_year_leap_prev, s_year_leap: std_logic;
  
  TYPE STATE_CLK_INCDEC IS (clk_none_incdec, clk_second_inc, clk_second_dec, clk_minute_inc, clk_minute_dec, clk_hour_inc, clk_hour_dec, clk_day_inc,clk_day_dec);
  signal clk_action: STATE_CLK_INCDEC;
begin

proc_1pps: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' or sync = '1' then
      refresh_rate_cnt <= to_unsigned(c_REFRESH_CNT_INIT-1, c_REFRESH_CNT_NB_BITS);
      s_1pps     <= '0';
    elsif refresh_rate_cnt = 0 then
      refresh_rate_cnt <= to_unsigned(c_REFRESH_CNT_INIT, c_REFRESH_CNT_NB_BITS);
      s_1pps     <= '1';
    else
      refresh_rate_cnt <= refresh_rate_cnt - 1;
      s_1pps     <= '0';
    end if;
  end if;
end process;

 
proc_timestamp: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' then
	   r_utc_time <= std_logic_vector(to_unsigned(0,32));
--		r_utc_inc_diff <= std_logic_vector(to_unsigned(0,32));
--      r_utc_dec_diff <= std_logic_vector(to_unsigned(0,32)) ;
	 elsif sync = '1' then
	   r_utc_time <= utc_timestamp;
	--	r_utc_inc_diff <= std_logic_vector(signed(utc_timestamp) - signed(r_utc_time_bcd));
      --r_utc_dec_diff <= std_logic_vector(unsigned(r_utc_time_bcd) - unsigned(utc_timestamp)) ;
--		if (utc_timestamp > 
	 elsif s_1pps = '1' then
	   r_utc_time <= std_logic_vector(unsigned(r_utc_time) + 1);
	 end if;
  end if;
end process proc_timestamp;

proc_timestamp_bcd: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' then
	   r_utc_time_bcd <= std_logic_vector(to_signed(0,32));
		s_synchronized <= '1';
		r_utc_time_diff <= std_logic_vector(to_signed(0,32));
		clk_action <= clk_none_incdec;
	elsif sync='1' then
	   s_synchronized <='0';
	   clk_action <= clk_none_incdec;
		r_utc_time_diff <= std_logic_vector(signed(utc_timestamp) - signed(r_utc_time_bcd));
	elsif s_synchronized = '1' then
	   if s_1pps = '1' then
		  r_utc_time_bcd <= std_logic_vector(unsigned(r_utc_time_bcd) + 1);
		  clk_action <= clk_second_inc;
		else
		  clk_action <= clk_none_incdec;
		end if;
	else
     if s_1pps = '1' then
		  r_utc_time_diff <= std_logic_vector(signed(r_utc_time_diff) + 1);
		  clk_action <= clk_none_incdec;
	  elsif signed(r_utc_time_diff) >=0 then
	    if signed(r_utc_time_diff) = 0 then
	      s_synchronized <= '1';
		   clk_action <= clk_none_incdec;
		   --r_utc_time_bcd  <= std_logic_vector(signed(r_utc_time_bcd) + 1);
		 elsif signed(r_utc_time_diff) >=	to_signed(86400, 32) then
		   clk_action <= clk_day_inc;
		   r_utc_time_bcd  <= std_logic_vector(signed(r_utc_time_bcd) + 86400);
		   r_utc_time_diff <= std_logic_vector(signed(r_utc_time_diff) - 86400);
		 elsif signed(r_utc_time_diff) >=	to_signed(3600, 32) then
		   clk_action <= clk_hour_inc;
		   r_utc_time_bcd  <= std_logic_vector(signed(r_utc_time_bcd) + 3600);
		   r_utc_time_diff <= std_logic_vector(signed(r_utc_time_diff) - 3600);
	    elsif signed(r_utc_time_diff) > to_signed(0, 32) then
		   clk_action <= clk_second_inc;
		   r_utc_time_bcd  <= std_logic_vector(signed(r_utc_time_bcd) + 1);
		   r_utc_time_diff <= std_logic_vector(signed(r_utc_time_diff) - 1);
	    end if;
	  else
       if signed(r_utc_time_diff) < to_signed(0, 32) then
 	      clk_action <= clk_second_dec;
		   r_utc_time_bcd  <= std_logic_vector(signed(r_utc_time_bcd) - 1);
		   r_utc_time_diff <= std_logic_vector(signed(r_utc_time_diff) + 1);
	    end if;
	    
	  end if;
--	 elsif cmp_gt = '1' then
--	   r_utc_time_bcd <= std_logic_vector(unsigned(r_utc_time_bcd) + 1);
--		synchronized <= '0';
--	 elsif cmp_lt = '1' then
--	   r_utc_time_bcd <= std_logic_vector(unsigned(r_utc_time_bcd) - 1);
--		synchronized <= '0';
--	 else
--	   synchronized <= '1';
	 end if;
  end if;
end process;


	s_minute_dec(0) <= s_second_dec(1) or s_minute_dec_sync;
   s_hour_dec(0) <= s_minute_dec(1) or s_hour_dec_sync;
   s_day_dec(0) <= s_hour_dec(1) or s_day_dec_sync;
	
	s_minute_inc(0) <= s_second_inc(1) or s_minute_inc_sync;
   s_hour_inc(0) <= s_minute_inc(1) or s_hour_inc_sync;
   s_day_inc(0) <= s_hour_inc(1) or s_day_inc_sync;

  PROCESS (clk_action)
   BEGIN
      CASE clk_action IS
         WHEN clk_none_incdec =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '0';
         WHEN clk_second_inc =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '1';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '0';
         WHEN clk_second_dec =>
			  s_second_dec(0) <= '1';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '0';
			WHEN clk_minute_inc =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '1';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '0';
			WHEN clk_minute_dec =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '1';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '0';
			WHEN clk_hour_dec =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '0';
			WHEN clk_hour_inc =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '1';
			  s_day_inc_sync <= '0';
			WHEN clk_day_inc =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '0';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '1';
			WHEN clk_day_dec =>
			  s_second_dec(0) <= '0';
           s_minute_dec_sync  <= '0';
	        s_hour_dec_sync  <= '0';
			  s_day_dec_sync  <= '1';
			  s_second_inc(0) <= '0';
			  s_minute_inc_sync <= '0';
			  s_hour_inc_sync <= '0';
			  s_day_inc_sync <= '0';
	  END CASE;
   END PROCESS;
	
	
	   u_counter_sec: counter_param 
	  GENERIC MAP(
			MaxVal => x"59"
	  )
	  PORT MAP (
          clk_i => clk,
          rst_i => rst,
          inc_i => s_second_inc(0),
          dec_i => s_second_dec(0),
          bcd_value_o => r_bcd_second,
          high_inc_o => s_second_inc(1),
          high_dec_o => s_second_dec(1)
        );
     u_counter_min: counter_param 
		 GENERIC MAP(
			MaxVal => x"59"
	  )
	  PORT MAP (
          clk_i => clk,
          rst_i => rst,
          inc_i => s_minute_inc(0),
          dec_i => s_minute_dec(0),
          bcd_value_o => r_bcd_minute,
          high_inc_o => s_minute_inc(1),
          high_dec_o => s_minute_dec(1)
        );
     u_counter_hour: counter_param 
		 GENERIC MAP(
			MaxVal => x"23"
	   )
	  PORT MAP (
          clk_i => clk,
          rst_i => rst,
          inc_i => s_hour_inc(0),
          dec_i => s_hour_dec(0),
          bcd_value_o => r_bcd_hour,
          high_inc_o => s_hour_inc(1),
          high_dec_o => s_hour_dec(1)
        );
	
	
   year_day : counter_year_day PORT MAP (
          clk_i => clk,
          rst_i => rst,
          inc_i => s_day_inc(0),
          dec_i => s_day_dec(0),
          bcd_year_o => r_bcd_year,
			 bcd_doy_o => r_bcd_doy,
          leap_year_o => s_year_leap,
          leap_year_next_o => s_year_leap_next,
          leap_year_prev_o => s_year_leap_prev
        );

  bcd_year_o <= r_bcd_year;
  bcd_doy_o <= r_bcd_doy;
  bcd_moy_o <= r_bcd_moy;
  bcd_dom_o <= r_bcd_dom;
  bcd_hour_o <= r_bcd_hour;
  bcd_minute_o <= r_bcd_minute;
  bcd_second_o <= r_bcd_second;
  
  
--timestamp_cmp: process(r_utc_time, r_utc_time_bcd,rst)
--begin
--  r_utc_inc_diff <= std_logic_vector(signed(r_utc_time) - signed(r_utc_time_bcd));
--  cmp_inc <= '1';
--
----    
----    
--end process;
--
--diff_proc:process(r_utc_inc_diff, r_utc_inc_diff,cmp_inc, cmp_dec)
--begin
--  if cmp_inc = '1' then
--    if signed(r_utc_inc_diff) > to_signed(86400,32) then
--	   s_day_inc_sync <= '1';
--		s_hour_inc_sync <= '0';
--		s_minute_inc_sync <= '0';
--		s_second_inc(0) <= '0';
--	 elsif signed(r_utc_inc_diff) > to_signed(3600, 32) then
--	   s_day_inc_sync <= '0';
--		s_hour_inc_sync <= '1';
--		s_second_inc(0) <= '0';
--		s_minute_inc_sync <= '0';
--	 elsif signed(r_utc_inc_diff) > to_signed(60, 32) then
--	   s_day_inc_sync <= '0';
--		s_hour_inc_sync <= '0';
--		s_minute_inc_sync <= '1';
--		s_second_inc(0) <= '0';
--	 else
--	   s_day_inc_sync <= '0';
--		s_hour_inc_sync <= '0';
--		s_minute_inc_sync <= '0';
--	   s_second_inc(0) <= '1';
--	 end if;
--  else
--     s_day_inc_sync <= '0';
--	  s_hour_inc_sync <= '0';
--	  s_minute_inc_sync <= '0';
--	  s_second_inc(0) <= '0';
--  end if;
--end process;

synchronized <= s_synchronized;
  
end Behavioral;



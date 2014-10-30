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
			CLK_in_Hz		: INTEGER := 50000000;
			ZeroYear  : std_logic_vector(15 downto 0):= x"2008";
			g_CLK_FREQ     : natural := 50000000;  -- in Hz
         g_REFRESH_RATE : natural :=   1   -- in Hz
    
	 );
    Port ( clk : in  STD_LOGIC;
	        rst : in  STD_LOGIC;
           sync : in  STD_LOGIC;
           utc_timestamp : in  STD_LOGIC_VECTOR (31 downto 0);
           synchronized : out  STD_LOGIC;
           bcd_year : out  STD_LOGIC_VECTOR (15 downto 0);
           bcd_doy : out  STD_LOGIC_VECTOR (11 downto 0);
           bcd_moy : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_dom : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_hour : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_minute : out  STD_LOGIC_VECTOR (7 downto 0);
           bcd_second : out  STD_LOGIC_VECTOR (7 downto 0)
           );
end RTC;

architecture Behavioral of RTC is

	--CONSTANT	CLK_in_ps			: INTEGER	:= (1000000000 / (CLK_in_Hz / 1000 / 2)); -- muss eigentlich clk-halbe sein
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
  
  signal r_bcd_year : STD_LOGIC_VECTOR (15 downto 0);
  signal r_bcd_doy : STD_LOGIC_VECTOR (11 downto 0);
  signal r_bcd_moy : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_dom : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_hour : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_minute : STD_LOGIC_VECTOR (7 downto 0);
  signal r_bcd_second : STD_LOGIC_VECTOR (7 downto 0);
  
  signal s_1pps : std_logic;
  signal refresh_rate_cnt   : unsigned(c_REFRESH_CNT_NB_BITS - 1 downto 0);


  signal s_year_inc, s_year_dec: std_logic;
  signal s_day_inc, s_day_dec: std_logic;
  signal s_hour_inc, s_hour_dec: std_logic;
  signal s_minute_inc, s_minute_dec: std_logic;
  
  
  signal cmp_gt, cmp_lt: std_logic;
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
	 elsif sync = '1' then
	   r_utc_time <= utc_timestamp;
	 elsif s_1pps = '1' then
	   r_utc_time <= std_logic_vector(unsigned(r_utc_time) + 1);
	 end if;
  end if;
end process proc_timestamp;

proc_timestamp_bcd: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' then
	   r_utc_time_bcd <= std_logic_vector(to_unsigned(0,32));
		synchronized <= '0';
	 elsif cmp_gt = '1' then
	   r_utc_time_bcd <= std_logic_vector(unsigned(r_utc_time_bcd) + 1);
		synchronized <= '0';
	 elsif cmp_lt = '1' then
	   r_utc_time_bcd <= std_logic_vector(unsigned(r_utc_time_bcd) - 1);
		synchronized <= '0';
	 else
	   synchronized <= '1';
	 end if;
  end if;
end process;

proc_bcd_second: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' then
      r_bcd_second <= x"00";
	 elsif cmp_gt = '1' then
	   if r_bcd_second = x"59" then
		  r_bcd_second <= x"00";
		elsif r_bcd_second(3 downto 0) = x"9" then
		  r_bcd_second(7 downto 4) <= std_logic_vector(unsigned(r_bcd_second(7 downto 4)) + 1);
		  r_bcd_second(3 downto 0) <= x"0";
		else
		  r_bcd_second(3 downto 0) <= std_logic_vector(unsigned(r_bcd_second(3 downto 0)) + 1);
		end if;
 	 elsif cmp_lt = '1' then
	 	if r_bcd_second = x"00" then
		  r_bcd_second <= x"59";
		elsif r_bcd_second(3 downto 0) = x"0" then
		  r_bcd_second(7 downto 4) <= std_logic_vector(unsigned(r_bcd_second(7 downto 4)) - 1);
		  r_bcd_second(3 downto 0) <= x"9";
		else
		  r_bcd_second(3 downto 0) <= std_logic_vector(unsigned(r_bcd_second(3 downto 0)) - 1);
		end if;
	 end if;
  end if;
end process;
bcd_second <= r_bcd_second;

proc_bcd_second_carry: process(cmp_lt, cmp_gt, r_bcd_second)
begin
  if cmp_lt = '1' and r_bcd_second = x"00" then
    s_minute_inc <= '0';
	 s_minute_dec <= '1';
  elsif cmp_gt = '1' and r_bcd_second = x"59" then
    s_minute_inc <= '1';
	 s_minute_dec <= '0';
  else
    s_minute_inc <= '0';
	 s_minute_dec <= '0';
  end if;
end process;

proc_bcd_minute: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' then
      r_bcd_minute <= x"00";
	 elsif s_minute_inc = '1' then
	   if r_bcd_minute = x"59" then
		  r_bcd_minute <= x"00";
		elsif r_bcd_minute(3 downto 0) = x"9" then
		  r_bcd_minute(7 downto 4) <= std_logic_vector(unsigned(r_bcd_minute(7 downto 4)) + 1);
		  r_bcd_minute(3 downto 0) <= x"0";
		else
		  r_bcd_minute(3 downto 0) <= std_logic_vector(unsigned(r_bcd_minute(3 downto 0)) + 1);
		end if;
 	 elsif s_minute_dec = '1' then
	 	if r_bcd_minute = x"00" then
		  r_bcd_minute <= x"59";
		elsif r_bcd_minute(3 downto 0) = x"0" then
		  r_bcd_minute(7 downto 4) <= std_logic_vector(unsigned(r_bcd_minute(7 downto 4)) - 1);
		  r_bcd_minute(3 downto 0) <= x"9";
		else
		  r_bcd_minute(3 downto 0) <= std_logic_vector(unsigned(r_bcd_minute(3 downto 0)) - 1);
		end if;
	 end if;
  end if;
end process;
bcd_minute <= r_bcd_minute;

proc_bcd_minute_carry: process(s_minute_dec, s_minute_inc, r_bcd_minute)
begin
  if s_minute_dec = '1' and r_bcd_minute = x"00" then
    s_hour_inc <= '0';
	 s_hour_dec <= '1';
  elsif s_minute_inc = '1' and r_bcd_minute = x"59" then
    s_hour_inc <= '1';
	 s_hour_dec <= '0';
  else
    s_hour_inc <= '0';
	 s_hour_dec <= '0';
  end if;
end process;


proc_bcd_hour: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' then
      r_bcd_hour <= x"00";
	 elsif s_hour_inc = '1' then
	   if r_bcd_hour = x"23" then
		  r_bcd_hour <= x"00";
		elsif r_bcd_hour(3 downto 0) = x"9" then
		  r_bcd_hour(7 downto 4) <= std_logic_vector(unsigned(r_bcd_hour(7 downto 4)) + 1);
		  r_bcd_hour(3 downto 0) <= x"0";
		else
		  r_bcd_hour(3 downto 0) <= std_logic_vector(unsigned(r_bcd_hour(3 downto 0)) + 1);
		end if;
 	 elsif s_hour_dec = '1' then
	 	if r_bcd_hour = x"00" then
		  r_bcd_hour <= x"23";
		elsif r_bcd_hour(3 downto 0) = x"0" then
		  r_bcd_hour(7 downto 4) <= std_logic_vector(unsigned(r_bcd_hour(7 downto 4)) - 1);
		  r_bcd_hour(3 downto 0) <= x"9";
		else
		  r_bcd_hour(3 downto 0) <= std_logic_vector(unsigned(r_bcd_hour(3 downto 0)) - 1);
		end if;
	 end if;
  end if;
end process;
bcd_hour <= r_bcd_hour;

proc_bcd_hour_carry: process(s_hour_dec, s_hour_inc, r_bcd_hour)
begin
  if s_hour_dec = '1' and r_bcd_hour = x"00" then
    s_day_inc <= '0';
	 s_day_dec <= '1';
  elsif s_hour_inc = '1' and r_bcd_hour = x"23" then
    s_day_inc <= '1';
	 s_day_dec <= '0';
  else
    s_day_inc <= '0';
	 s_day_dec <= '0';
  end if;
end process;


proc_bcd_doy: process(clk)
begin
  if rising_edge(clk) then
    if rst = '1' then
      r_bcd_doy <= x"000";
	 elsif s_day_inc = '1' then
	   if r_bcd_doy = x"355" then
		  r_bcd_doy <= x"000";
		elsif r_bcd_doy(7 downto 0) = x"99" then
		  r_bcd_doy(11 downto 8) <= std_logic_vector(unsigned(r_bcd_doy(11 downto 8)) + 1);
		  r_bcd_doy(7 downto 0) <= x"00";
		elsif r_bcd_doy(3 downto 0) = x"9" then
		  r_bcd_doy(7 downto 4) <= std_logic_vector(unsigned(r_bcd_doy(7 downto 4)) + 1);
		  r_bcd_doy(3 downto 0) <= x"0";
		else
		  r_bcd_doy(3 downto 0) <= std_logic_vector(unsigned(r_bcd_doy(3 downto 0)) + 1);
		end if;
 	 elsif s_day_dec = '1' then
	 	if r_bcd_doy = x"000" then
		  r_bcd_doy <= x"356";
		elsif r_bcd_doy(7 downto 0) = x"00" then
		  r_bcd_doy(11 downto 8) <= std_logic_vector(unsigned(r_bcd_doy(11 downto 8)) - 1);
		  r_bcd_doy(7 downto 0) <= x"99";
		elsif r_bcd_doy(3 downto 0) = x"0" then
		  r_bcd_doy(7 downto 4) <= std_logic_vector(unsigned(r_bcd_doy(7 downto 4)) - 1);
		  r_bcd_doy(3 downto 0) <= x"9";
		else
		  r_bcd_doy(3 downto 0) <= std_logic_vector(unsigned(r_bcd_doy(3 downto 0)) - 1);
		end if;
	 end if;
  end if;
end process;
bcd_doy <= r_bcd_doy;





timestamp_cmp: process(r_utc_time, r_utc_time_bcd,rst)
begin
  if r_utc_time = r_utc_time_bcd then
    cmp_gt <= '0';
	 cmp_lt <= '0';
  elsif unsigned(r_utc_time) < unsigned(r_utc_time_bcd) then
    cmp_gt <= '0';
	 cmp_lt <= '1';
  elsif unsigned(r_utc_time) > unsigned(r_utc_time_bcd) then
    cmp_gt <= '1';
	 cmp_lt <= '0';
  else
    cmp_gt <= '0';
	 cmp_lt <= '0';
  end if;
end process;

end Behavioral;



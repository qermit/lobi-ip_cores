----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    14:07:55 09/03/2014 
-- Design Name: 
-- Module Name:    counter_year_day - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity counter_year_day is
    GENERIC (
	   StartYear : std_logic_vector(15 downto 0):= x"2008"
	 );
    Port ( clk_i : in  STD_LOGIC;
           rst_i : in  STD_LOGIC;
			  inc_i : in  STD_LOGIC;
           dec_i : in  STD_LOGIC;
           bcd_year_o : out  STD_LOGIC_VECTOR (15 downto 0);
			  bcd_doy_o : OUT std_logic_vector(11 downto 0);
           leap_year_o : out  STD_LOGIC;
           leap_year_next_o : out  STD_LOGIC;
           leap_year_prev_o : out  STD_LOGIC);
end counter_year_day;

architecture Behavioral of counter_year_day is
	COMPONENT counter_year
	 GENERIC (
	   StartYear : std_logic_vector(15 downto 0):= x"2008"
	 );
	PORT(
		clk_i : IN std_logic;
		rst_i : IN std_logic;
		inc_i : IN std_logic;
		dec_i : IN std_logic;          
		leap_year_o : OUT std_logic;
		bcd_year_o : OUT std_logic_vector(15 downto 0)
		);
	END COMPONENT;
	
   signal r_bcd_doy: std_logic_vector(11 downto 0);
  	COMPONENT bcd_digit
	PORT(
		inc_i : IN std_logic;
		dec_i : IN std_logic;
		rst_i : IN std_logic;
		clk_i : IN std_logic;
		load_i : IN std_logic;
		value_i : IN std_logic_vector(3 downto 0);          
		value_o : OUT std_logic_vector(3 downto 0);
		high_inc_o : out  STD_LOGIC;
		high_dec_o : out  STD_LOGIC
		);
	END COMPONENT; 
	
   signal day_inc: std_logic_vector(3 downto 0);
	signal day_dec: std_logic_vector(3 downto 0);
	signal day_reset: std_logic_vector(3 downto 0);
	signal day_load: std_logic_vector(3 downto 0) := x"0";
	
	signal doy_to_load: std_logic_vector(11 downto 0);
	
	signal s_leap_year : STD_LOGIC;
	signal s_leap_year_next : STD_LOGIC;
	signal s_leap_year_prev : STD_LOGIC;
begin

	bcd_digit_0: bcd_digit PORT MAP(
		inc_i => day_inc(0),
		dec_i => day_dec(0),
		rst_i => day_reset(0),
		clk_i => clk_i,
		load_i => day_load(0),
		value_i => doy_to_load(3 downto 0),
		value_o => r_bcd_doy(3 downto 0),
		high_inc_o => day_inc(1),
		high_dec_o => day_dec(1)
	);
   bcd_digit_1: bcd_digit PORT MAP(
		inc_i => day_inc(1),
		dec_i => day_dec(1),
		rst_i => day_reset(1),
		clk_i => clk_i,
      load_i => day_load(0),
		value_i => doy_to_load(3 downto 0),
		value_o => r_bcd_doy(7 downto 4),
		high_inc_o => day_inc(2),
		high_dec_o => day_dec(2)
	);
	bcd_digit_2: bcd_digit PORT MAP(
		inc_i => day_inc(2),
		dec_i => day_dec(2),
		rst_i => day_reset(2),
		clk_i => clk_i,
      load_i => day_load(0),
		value_i => doy_to_load(3 downto 0),
		value_o => r_bcd_doy(11 downto 8)
	);

   day_reset <= (others => rst_i);
   bcd_doy_o <= r_bcd_doy;
	day_inc(0)<= inc_i;
	day_dec(0)<= dec_i;

proc_bcd_carry: process(rst_i, dec_i, inc_i, r_bcd_doy, s_leap_year, s_leap_year_prev)
begin
  if dec_i = '1' and r_bcd_doy = x"000" then
    if s_leap_year_prev = '1' then
	   doy_to_load <= x"365";
	 else
	   doy_to_load <= x"364";
	 end if;
	 day_load(0) <= '1';
    day_inc(3) <= '0';
	 day_dec(3) <= '1';
  elsif inc_i = '1' and r_bcd_doy = x"365" and s_leap_year='1' then
    day_inc(3) <= '1';
	 day_dec(3) <= '0';
	 doy_to_load <= x"000";
	 day_load(0) <= '1';
  elsif inc_i = '1' and r_bcd_doy = x"364" and s_leap_year='0' then
    day_inc(3) <= '1';
	 doy_to_load <= x"000";
	 day_load(0) <= '1';
	 day_dec(3) <= '0';
  else
  	 doy_to_load <= x"000";
  	 day_load(0) <= '0';
    day_inc(3) <= '0';
	 day_dec(3) <= '0';
  end if;
end process;



	year_prev: counter_year
	GENERIC MAP (
	  StartYear => x"2007"
	)
	PORT MAP(
		clk_i => clk_i,
		rst_i => rst_i,
		inc_i => day_inc(3),
		dec_i => day_dec(3),
		leap_year_o => s_leap_year_prev
	);

   year_next: counter_year 
	GENERIC MAP (
	  StartYear => x"2009"
	)
	PORT MAP(
		clk_i => clk_i,
		rst_i => rst_i,
		inc_i => day_inc(3),
		dec_i => day_inc(3),
		leap_year_o => leap_year_next_o
	);
	
   year: counter_year 
	GENERIC MAP (
	  StartYear => StartYear
	)
	PORT MAP(
		clk_i => clk_i,
		rst_i => rst_i,
		inc_i => day_inc(3),
		dec_i => day_dec(3),
		leap_year_o => s_leap_year,
		bcd_year_o => bcd_year_o
	);

leap_year_o <= s_leap_year ;
leap_year_prev_o <= s_leap_year_prev;

end Behavioral;


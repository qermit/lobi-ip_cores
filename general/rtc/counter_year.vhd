----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    11:31:44 09/03/2014 
-- Design Name: 
-- Module Name:    counter_year - Behavioral 
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

entity counter_year is
    GENERIC (
	   StartYear : std_logic_vector(15 downto 0):= x"2008"
	 );
    Port ( clk_i : in  STD_LOGIC;
           rst_i : in  STD_LOGIC;
           inc_i : in  STD_LOGIC;
           dec_i : in  STD_LOGIC;
           leap_year_o : out  STD_LOGIC;
           bcd_year_o : out  STD_LOGIC_VECTOR (15 downto 0));
end counter_year;

architecture Behavioral of counter_year is
  signal r_bcd_value: std_logic_vector(15 downto 0);
  
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
	
	signal local_inc: std_logic_vector(4 downto 0);
	signal local_dec: std_logic_vector(4 downto 0);
	signal local_reset: std_logic_vector(3 downto 0);
	
	signal div_by_4, div_by_100, div_by_400: std_logic;

   signal div_by_4c, div_by_100c, div_by_400c: std_logic;
	
	signal counter_4: std_logic_vector(1 downto 0);
   signal counter_100: std_logic_vector (6 downto 0);
	signal counter_400: std_logic_vector (8 downto 0);

begin

	bcd_digit_0: bcd_digit PORT MAP(
		inc_i => local_inc(0),
		dec_i => local_dec(0),
		rst_i => local_reset(0),
		clk_i => clk_i,
		load_i => rst_i,
		value_i => StartYear(3 downto 0),
		value_o => r_bcd_value(3 downto 0),
		high_inc_o => local_inc(1),
		high_dec_o => local_dec(1)
		);

   bcd_digit_1: bcd_digit PORT MAP(
		inc_i => local_inc(1),
		dec_i => local_dec(1),
		rst_i => local_reset(1),
		clk_i => clk_i,
		load_i => rst_i,
		value_i => StartYear(7 downto 4),
		value_o => r_bcd_value(7 downto 4),
		high_inc_o => local_inc(2),
		high_dec_o => local_dec(2)
	);
	bcd_digit_2: bcd_digit PORT MAP(
		inc_i => local_inc(2),
		dec_i => local_dec(2),
		rst_i => local_reset(2),
		clk_i => clk_i,
		load_i => rst_i,
		value_i => StartYear(11 downto 8),
		value_o => r_bcd_value(11 downto 8),
		high_inc_o => local_inc(3),
		high_dec_o => local_dec(3)
	);
	
	bcd_digit_3: bcd_digit PORT MAP(
		inc_i => local_inc(3),
		dec_i => local_dec(3),
		rst_i => local_reset(3),
		clk_i => clk_i,
		load_i => rst_i,
		value_i => StartYear(15 downto 12),
		value_o => r_bcd_value(15 downto 12),
		high_inc_o => local_inc(4),
		high_dec_o => local_dec(4)
	);

  local_reset(1) <= '0';
  local_reset(0) <= '0';
  local_reset(2) <= '0';
  local_reset(3) <= '0';
  bcd_year_o <= r_bcd_value;
  local_inc(0) <= inc_i;
  local_dec(0) <= dec_i;
  
proc_div: process(rst_i, r_bcd_value)
begin
  if r_bcd_value(4 downto 0) = "00000" or
     r_bcd_value(4 downto 0) = "00100" or
	  r_bcd_value(4 downto 0) = "01000" or
	  r_bcd_value(4 downto 0) = "10010" or
	  r_bcd_value(4 downto 0) = "10110" then
	  div_by_4 <= '1';
	else
	  div_by_4 <= '0';
	end if;
	
	if r_bcd_value(7 downto 0) = x"00" then
	  div_by_100 <= '1';
	  if r_bcd_value(12 downto 8) = "00000" or
       r_bcd_value(12 downto 8) = "00100" or
	    r_bcd_value(12 downto 8) = "01000" or
	    r_bcd_value(12 downto 8) = "10010" or
       r_bcd_value(12 downto 8) = "10110" then
	    div_by_400 <= '1';
	  else
	    div_by_400 <= '0';
	  end if;
	  
	else 
	  div_by_100 <= '0';
	  div_by_400 <= '0';
	end if;
end process;

proc_leap: process(rst_i, div_by_4, div_by_100, div_by_400)
begin
  if div_by_4 = '0' then
    leap_year_o <= '0';
  elsif div_by_100 = '0' then
    leap_year_o <= '1';
  elsif div_by_400 = '0' then
    leap_year_o <= '0';
  else
    leap_year_o <= '1';
  end if;
  
end process;
  

end Behavioral;


----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:11:08 09/03/2014 
-- Design Name: 
-- Module Name:    bcd_digit - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity bcd_digit is
    Port ( inc_i : in  STD_LOGIC;
           dec_i : in  STD_LOGIC;
           rst_i : in  STD_LOGIC;
           clk_i : in  STD_LOGIC;
			  load_i  : in STD_LOGIC;
			  value_i : in  STD_LOGIC_VECTOR (3 downto 0);
           value_o : out  STD_LOGIC_VECTOR (3 downto 0);
           high_inc_o : out  STD_LOGIC;
			  high_dec_o : out  STD_LOGIC);
end bcd_digit;

architecture Behavioral of bcd_digit is
 signal r_value :std_logic_vector(3 downto 0);
 signal s_high_inc : std_logic;
 signal s_high_dec : std_logic;
begin

process (clk_i)
begin
  if rising_edge(clk_i) then
    if rst_i = '1' then
	   r_value <= x"0";
	 elsif load_i = '1' then
	   r_value <= value_i;
	 elsif s_high_inc = '1' then
	   r_value <= x"0";
	 elsif s_high_dec = '1' then
	   r_value <= x"9";
	 elsif inc_i = '1' then
	   r_value <= std_logic_vector(unsigned(r_value) + 1);
	 elsif dec_i = '1' then
	   r_value <= std_logic_vector(unsigned(r_value) - 1);
	 end if;
 end if;
end process;	 


proc_bcd_carry: process(dec_i, inc_i, r_value)
begin
  if dec_i = '1' and r_value = x"0" then
    s_high_inc <= '0';
	 s_high_dec <= '1';
  elsif inc_i = '1' and r_value = x"9" then
    s_high_inc <= '1';
	 s_high_dec <= '0';
  else
    s_high_inc <= '0';
	 s_high_dec <= '0';
  end if;
end process;

value_o <= r_value;

high_inc_o <= s_high_inc;
high_dec_o <= s_high_dec;

end Behavioral;


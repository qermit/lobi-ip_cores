----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:28:49 09/03/2014 
-- Design Name: 
-- Module Name:    counter_param - Behavioral 
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

entity counter_param is
    GENERIC(
			ResetVal  : std_logic_vector(7 downto 0):= x"00";
			MaxVal  : std_logic_vector(7 downto 0):= x"59"
	 );
    Port ( clk_i : in  STD_LOGIC;
           rst_i : in  STD_LOGIC;
           inc_i : in  STD_LOGIC;
           dec_i : in  STD_LOGIC;
			  bcd_value_o : out  STD_LOGIC_VECTOR (7 downto 0);
           high_inc_o : out  STD_LOGIC;
           high_dec_o : out  STD_LOGIC);
end counter_param;

architecture Behavioral of counter_param is
  signal r_bcd_value: std_logic_vector(7 downto 0);
  
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
	
	signal load_val: std_logic := '0';
	signal local_inc: std_logic_vector(1 downto 0);
	signal local_dec: std_logic_vector(1 downto 0);
	signal local_reset: std_logic_vector(1 downto 0);
	
	signal s_high_inc : std_logic;
   signal s_high_dec : std_logic;
	
begin

  

	bcd_digit_low: bcd_digit PORT MAP(
		inc_i => local_inc(0),
		dec_i => local_dec(0),
		rst_i => local_reset(0),
		clk_i => clk_i,
		load_i => s_high_dec,
		value_i => MaxVal(3 downto 0),
		value_o => r_bcd_value(3 downto 0),
		high_inc_o => local_inc(1),
		high_dec_o => local_dec(1)
		);

   bcd_digit_high: bcd_digit PORT MAP(
		inc_i => local_inc(1),
		dec_i => local_dec(1),
		rst_i => local_reset(1),
		clk_i => clk_i,
		load_i => s_high_dec,
		value_i => MaxVal(7 downto 4),
		value_o => r_bcd_value(7 downto 4)
	);

   local_inc(0) <= inc_i;
	local_dec(0) <= dec_i;
	
	load_val <= '0';
	
	local_reset(0) <= rst_i or s_high_inc;
	local_reset(1) <= rst_i or s_high_inc;
--proc_bcd: process(clk_i)
--begin
--  if rising_edge(clk_i) then
--    if rst_i = '1' then
--      r_bcd_value <= ResetVal;
--	 elsif inc_i = '1' then
--	   if r_bcd_value = MaxVal then
--		  r_bcd_value <= x"00";
--		elsif r_bcd_value(3 downto 0) = x"9" then
--		  r_bcd_value(7 downto 4) <= std_logic_vector(unsigned(r_bcd_value(7 downto 4)) + 1);
--		  r_bcd_value(3 downto 0) <= x"0";
--		else
--		  r_bcd_value(3 downto 0) <= std_logic_vector(unsigned(r_bcd_value(3 downto 0)) + 1);
--		end if;
-- 	 elsif dec_i = '1' then
--	 	if r_bcd_value = x"00" then
--		  r_bcd_value <= MaxVal;
--		elsif r_bcd_value(3 downto 0) = x"0" then
--		  r_bcd_value(7 downto 4) <= std_logic_vector(unsigned(r_bcd_value(7 downto 4)) - 1);
--		  r_bcd_value(3 downto 0) <= x"9";
--		else
--		  r_bcd_value(3 downto 0) <= std_logic_vector(unsigned(r_bcd_value(3 downto 0)) - 1);
--		end if;
--	 end if;
--  end if;
--end process;
--
--proc_internal: process(rst_i, dec_i, inc_i, r_bcd_value)
--begin
--  if rst_i = '1' then
--    local_reset <= "11";
--	 local_inc <= "00";
--	 local_dec <= "00";
--  elsif inc_i = '1' then
--    local_dec <= "00";	
--    if r_bcd_value = MaxVal then
--      local_reset <= "11";
--		local_inc <= "00";
--    elsif r_bcd_value(3 downto 0) = x"9" then
--      local_reset <= "01";
--		local_inc <= "10";
--	 else
--	   local_reset <= "00";
--	   local_inc <= "01";	
--	 end if;
--  elsif dec_i = '1' then
--    local_inc <= "00";	
--    if r_bcd_value = x"00" then
--      local_reset <= "11";
--		local_dec <= "00";
--    elsif r_bcd_value(3 downto 0) = x"0" then
--      local_reset <= "01";
--		local_dec <= "10";
--	 else
--	   local_reset <= "00";
--	   local_dec <= "01";	
--	 end if;
--  else
--    local_reset <= "00";
--	 local_inc <= "00";
--	 local_dec <= "00";
--  end if;
--end process;

bcd_value_o <= r_bcd_value;

proc_bcd_carry: process(dec_i, inc_i, r_bcd_value)
begin
  if dec_i = '1' and r_bcd_value = x"00" then
    s_high_inc <= '0';
	 s_high_dec <= '1';
  elsif inc_i = '1' and r_bcd_value = MaxVal then
    s_high_inc <= '1';
	 s_high_dec <= '0';
  else
    s_high_inc <= '0';
	 s_high_dec <= '0';
  end if;
end process;

high_inc_o <= s_high_inc;
high_dec_o <= s_high_dec;

end Behavioral;


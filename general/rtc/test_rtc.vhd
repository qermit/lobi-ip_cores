----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:20:58 09/29/2014 
-- Design Name: 
-- Module Name:    test_rtc - Behavioral 
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
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
library UNISIM;
use UNISIM.Vcomponents.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity test_rtc is
    	GENERIC(
			CLK_in_Hz		: INTEGER := 50000000
			);
    Port ( clk_i : in  STD_LOGIC;
           rst_i : in  STD_LOGIC;
           sync_i : in  STD_LOGIC;
           time_i : in  STD_LOGIC_VECTOR (39 downto 0);
           time_o : out  STD_LOGIC_VECTOR (39 downto 0)
			  );
end test_rtc;

architecture Behavioral of test_rtc is
	CONSTANT	tick_per_1ms			: INTEGER	:= (CLK_in_Hz / 1000);
	CONSTANT tick_per_1ms_bits		: INTEGER   := 16; -- todo poprawic


signal s_counter_1hz: std_logic_vector(29 downto 0);
signal s_counter_1khz: std_logic_vector(9 downto 0);

signal s_pulse_1khz: std_logic;
signal s_tick_counter: std_logic_vector(tick_per_1ms_bits - 1 downto 0);

begin

tick_1khz:process(clk_i)
begin
  if rising_edge(clk_i) then
    if rst_i = '1' then
	   s_tick_counter <= std_logic_vector(to_unsigned((tick_per_1ms - 1), tick_per_1ms_bits));
		s_pulse_1khz <= '0';
	 else
	   if unsigned(s_tick_counter) = 0 then
		  s_tick_counter <= std_logic_vector(to_unsigned((tick_per_1ms - 1), tick_per_1ms_bits));
		  s_pulse_1khz <= '1';
		else
		  s_pulse_1khz <= '0';
		  s_tick_counter <= std_logic_vector(unsigned(s_tick_counter) - 1);
		end if;
	 end if;
	 
  end if;
end process;

time_counters: process(clk_i)
begin
  if rising_edge(clk_i) then
    if rst_i = '1' then
	   s_counter_1hz <= (others => '0');
		s_counter_1khz <= (others => '0');
	 elsif sync_i = '1' then
	   s_counter_1hz <= time_i(29 downto 0);
		s_counter_1khz <= time_i(39 downto 30);
	 elsif s_pulse_1khz = '1' then
--	   s_counter_1khz <= std_logic_vector(unsigned(s_counter_1khz) + 1);
	   if unsigned(s_counter_1khz) = 999 then
		  s_counter_1khz <= std_logic_vector(to_unsigned(0, 10));
		  s_counter_1hz <= std_logic_vector(unsigned(s_counter_1hz) + 1);
		else
		  s_counter_1khz <= std_logic_vector(unsigned(s_counter_1khz) + 1);
		end if;
	 end if;
  end if;
end process;

time_o <= s_counter_1khz & s_counter_1hz;

end Behavioral;


----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:17:49 02/04/2016 
-- Design Name: 
-- Module Name:    scaler_counter - Behavioral 
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity scaler_counter is
  port(
-- System reset, active low
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;
	 
	 scaler_reset_int: in std_logic;
	 latch_int: in std_logic;
	 counter_inc : in std_logic;
	 scaler_enable_i: in std_logic;
	 
	 counter_val_out: out std_logic_vector(31 downto 0);
	 counter_ovf: out std_logic
	 );
end scaler_counter;

architecture Behavioral of scaler_counter is
  signal counter_val_in : std_logic_vector(31 downto 0);	
  signal s_counter_ovf : std_logic;
begin

counter_proc: process(clk_sys_i)
begin
	if rising_edge(clk_sys_i) then
	  if rst_n_i = '0' then
	    counter_val_in <= (others => '0');
		 s_counter_ovf <= '0';
     else
	    if scaler_reset_int = '1' then
         counter_val_in <= (others => '0');
			s_counter_ovf <= '0';
		 elsif latch_int = '1' then
         counter_val_in <= (others => '0');
			--s_counter_ovf <= '0';		   
   	 elsif counter_inc = '1' then
		   counter_val_in <= std_logic_vector(unsigned(counter_val_in) + 1);
			if (counter_val_in = x"FFFFFFFF") then
			  s_counter_ovf <= '1';
			end if;
		 end if;
	  end if;
	end if;
end process;

counter_val_out <= counter_val_in;
counter_ovf <= s_counter_ovf;
end Behavioral;


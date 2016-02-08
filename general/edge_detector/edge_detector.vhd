----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:37:10 02/04/2016 
-- Design Name: 
-- Module Name:    edge_detector - Behavioral 
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

entity edge_detector is
    generic (
	    g_num_pins: natural := 8
	 );
    Port ( clk_sys_i : in  STD_LOGIC;
				rst_n_i : in std_logic;
           async_i : in  std_logic_vector(g_num_pins-1 downto 0);
           sync_o : out  std_logic_vector(g_num_pins-1 downto 0);
           sync_fall_o : out  std_logic_vector(g_num_pins-1 downto 0);
           sync_rise_o : out  std_logic_vector(g_num_pins-1 downto 0);
			  sync_edge_o : out std_logic_vector(g_num_pins-1 downto 0));
end edge_detector;

architecture Behavioral of edge_detector is

signal r0, r1, r2: std_logic_vector(g_num_pins-1 downto 0);
begin

process(clk_sys_i)
begin
  if rising_edge(clk_sys_i) then
    if rst_n_i = '0' then
	   r0 <= (others => '0');
		r1 <= (others => '0');
		r2 <= (others => '0');
	 else
		r0 <= async_i;
		r1 <= r0;
		r2 <= r1;
	 end if;
  end if;
end process;

sync_o <= r1;
sync_rise_o <= r1 and (not r2);
sync_fall_o <= (not r1) and r2;
sync_edge_o <= r1 xor r2;


end Behavioral;


----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:11:01 09/29/2014 
-- Design Name: 
-- Module Name:    time_mux - Behavioral 
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

entity time_mux is
    Port ( gsi_time_i : in  STD_LOGIC_VECTOR (39 downto 0);
           gsi_sync_i : in  STD_LOGIC;
           user_time_i : in  STD_LOGIC_VECTOR (39 downto 0);
           user_sync_i : in  STD_LOGIC;
           sel_i : in  STD_LOGIC;
           time_o : out  STD_LOGIC_VECTOR (39 downto 0);
           sync_o : out  STD_LOGIC);
end time_mux;

architecture Behavioral of time_mux is

begin

process (gsi_time_i, gsi_sync_i, user_time_i, user_sync_i, sel_i)
begin
  if sel_i = '0' then
    time_o <= gsi_time_i;
	 sync_o <= gsi_sync_i;
  else
    time_o <= user_time_i;
	 sync_o <= user_sync_i;
  end if;
end process;

end Behavioral;


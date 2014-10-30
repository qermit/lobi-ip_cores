--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   14:15:48 09/02/2014
-- Design Name:   
-- Module Name:   D:/Devel/projekty/GSI-CERN-Spliter/IP_lib/tb_RTC.vhd
-- Project Name:  Spliter
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: RTC
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY tb_RTC IS
END tb_RTC;
 
ARCHITECTURE behavior OF tb_RTC IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT RTC
	 GENERIC (
			ZeroYear  : std_logic_vector(15 downto 0):= x"2008";
			g_CLK_FREQ     : natural := 50000000;  -- in Hz
         g_REFRESH_RATE : natural :=   1   -- in Hz
	 );
    PORT(
         clk : IN  std_logic;
         rst : IN  std_logic;
         sync : IN  std_logic;
         utc_timestamp : IN  std_logic_vector(31 downto 0);
         synchronized : OUT  std_logic;
         bcd_year_o : OUT  std_logic_vector(15 downto 0);
         bcd_doy_o : OUT  std_logic_vector(11 downto 0);
         bcd_moy_o : OUT  std_logic_vector(7 downto 0);
         bcd_dom_o : OUT  std_logic_vector(7 downto 0);
         bcd_hour_o : OUT  std_logic_vector(7 downto 0);
         bcd_minute_o : OUT  std_logic_vector(7 downto 0);
         bcd_second_o : OUT  std_logic_vector(7 downto 0)
        );
    END COMPONENT;
    

   --Inputs
   signal clk : std_logic := '0';
   signal rst : std_logic := '0';
   signal sync : std_logic := '0';
   signal utc_timestamp : std_logic_vector(31 downto 0) := (others => '0');

 	--Outputs
   signal synchronized : std_logic;
   signal bcd_year : std_logic_vector(15 downto 0);
   signal bcd_doy : std_logic_vector(11 downto 0);
   signal bcd_moy : std_logic_vector(7 downto 0);
   signal bcd_dom : std_logic_vector(7 downto 0);
   signal bcd_hour : std_logic_vector(7 downto 0);
   signal bcd_minute : std_logic_vector(7 downto 0);
   signal bcd_second : std_logic_vector(7 downto 0);

   -- Clock period definitions
   constant clk_period : time := 20 ns;
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: RTC 
	GENERIC MAP (
		g_REFRESH_RATE => 600000
	)
	PORT MAP (
          clk => clk,
          rst => rst,
          sync => sync,
          utc_timestamp => utc_timestamp,
          synchronized => synchronized,
          bcd_year_o => bcd_year,
          bcd_doy_o => bcd_doy,
          bcd_moy_o => bcd_moy,
          bcd_dom_o => bcd_dom,
          bcd_hour_o => bcd_hour,
          bcd_minute_o => bcd_minute,
          bcd_second_o => bcd_second
        );

   -- Clock process definitions
   clk_process :process
   begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
		rst <= '1';
      wait for 100 ns;	
		rst <= '0';
      wait for clk_period*10;
		
		utc_timestamp <= std_logic_vector(to_unsigned(86400,32));

		wait until clk = '0';
		sync <= '1';
		wait until clk = '1';
		wait until clk = '0';
		sync <= '0';
		wait for 2us;
		
--      utc_timestamp <= std_logic_vector(to_unsigned(4,32));
--		wait until clk = '0';
--		sync <= '1';
--		wait until clk = '1';
--		wait until clk = '0';
--		sync <= '0';

	   utc_timestamp <= std_logic_vector(to_unsigned(1409913933 -  1199145600,32));
		wait until clk = '0';
		sync <= '1';
		wait until clk = '1';
		wait until clk = '0';
		sync <= '0';
		
      -- insert stimulus here 

      wait;
   end process;

END;

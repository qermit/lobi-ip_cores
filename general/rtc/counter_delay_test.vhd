----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:34:41 09/25/2014 
-- Design Name: 
-- Module Name:    counter_delay_test - Behavioral 
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
----------------------------------

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
library UNISIM;
use UNISIM.Vcomponents.ALL;


use work.vme_timer_pkg.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity counter_delay_test is
    	GENERIC(
			CLK_in_Hz		: INTEGER := 50000000
			);
    Port ( clk : in  STD_LOGIC;
	        reset: in STD_LOGIC;
           gsi_strobe : in  STD_LOGIC;
           gsi_data : in  STD_LOGIC_VECTOR (15 downto 0);
			  time_i: in std_logic_vector(39 downto 0);
			  gsi_time_o: out std_logic_vector(39 downto 0);
			  gsi_sync_o: out std_logic);
end counter_delay_test;

architecture Behavioral of counter_delay_test is
  attribute keep : string;  

  signal s_ovf_1ms: std_logic;
  signal s_ovf_1s: std_logic;


  	signal gsi_time: std_logic_vector(39 downto 0);
	signal gsi_time_fraction: std_logic_vector(9 downto 0);
	signal gsi_time_sec: std_logic_vector(31 downto 0);
	signal gsi_time_sync: std_logic;
	signal gsi_time_cnt_ovf: std_logic;
	
	signal gsi_1ms_counter: std_logic_vector(9 downto 0);
	signal gsi_1ms_ovf: std_logic;
	signal gsi_frac_cnt: std_logic_vector(18 downto 0);
	attribute keep of gsi_time_sec: signal is "true";  
	attribute keep of gsi_time_fraction: signal is "true";  
	attribute keep of gsi_time_sync: signal is "true"; 
	attribute keep of gsi_1ms_counter: signal is "true";  
	attribute keep of gsi_frac_cnt: signal is "true";  	
	attribute keep of gsi_time_cnt_ovf: signal is "true";  	
  



  signal read_addr_counter: std_logic_vector(8 downto 0);
  signal events_read_log: std_logic_vector(63 downto 0);
  signal s_events_write_log: std_logic_vector(63 downto 0);
  attribute keep of events_read_log: signal is "true";  	
  attribute keep of s_events_write_log: signal is "true";  	


  signal tmp_s_rmb_addr_ovf: std_logic;
  attribute keep of tmp_s_rmb_addr_ovf: signal is "true";  	
  
  signal s_rama_en: std_logic;
  signal s_rama_we: std_logic_vector(0 downto 0);
  signal s_rama_data: std_logic_vector(63 downto 0);
--------------------------------------------------------------------
-- Registers
----------------------------------------------------------------------------------------------------
  signal r_levent_stamp:std_logic_vector(31 downto 0); -- last time when new event was seen
  signal r_lrevent_stamp:std_logic_vector(31 downto 0); -- last time when event was recorded
  signal r_lrevent_ptr:std_logic_vector(8 downto 0); -- last recorded event pointer;
  
  signal r_levent_start_ptr: std_logic_vector(8 downto 0); -- last start cycle event
  signal r_levent_start_stamp: std_logic_vector(8 downto 0); -- last time when cycle start event was seen
  signal r_levent_end_ptr: std_logic_vector(8 downto 0); -- last  start cycle event
  signal r_levent_end_stamp: std_logic_vector(8 downto 0); -- last time when cycle end event was seen
  
  signal s_current_timestamp: std_logic_vector(39 downto 0);
----------------------------------------------------------------------------------------------------
begin

   inc_40: process(clk)
	begin
	  if rising_edge(clk) then
	    if reset = '1' then
		   gsi_frac_cnt <= std_logic_vector(to_unsigned(49999, 19));
		   s_ovf_1ms <= '1';	
		 elsif unsigned(gsi_frac_cnt) = 0 then
		   gsi_frac_cnt <= std_logic_vector(to_unsigned(49999, 19));
			s_ovf_1ms <= not s_ovf_1ms;
		 else
		   gsi_frac_cnt <= std_logic_vector(unsigned(gsi_frac_cnt) - 1);
		 end if;
	  end if;
	end process;

	inc_1ms: process(clk)
	begin
	  if rising_edge(clk) then
		  if reset = '1' then
			 gsi_1ms_counter <= std_logic_vector(to_unsigned(999,10));
			 s_ovf_1s <= '1';
		  elsif unsigned(gsi_frac_cnt) = 0 then
			  if unsigned(gsi_1ms_counter) = 0 then
				 gsi_1ms_counter <= std_logic_vector(to_unsigned(999,10));
				 s_ovf_1s <= not s_ovf_1s;
			  else
				 gsi_1ms_counter <= std_logic_vector(unsigned(gsi_1ms_counter) - 1);
			  end if;
		  end if;
	  end if;
	end process;

	gsi_sync_o <= gsi_time_sync ;
	--	
	
	gsi_time_proc: process(clk)
	begin
	  if rising_edge(clk) then
	    if reset = '1' then
		   gsi_time_sync <= '0';
			gsi_time <= (others => '0');
		 elsif gsi_strobe = '1' then
		   if gsi_data( 7 downto 0) = x"E4" then
			  s_current_timestamp <= gsi_time(39 downto 8) & gsi_data(15 downto 8);
			  gsi_time_sync <= '1';
			else
			  gsi_time_sync <= '0';
			end if;
			
			if gsi_data(7 downto 0) = x"E4" then
			  gsi_time(7 downto 0) <= gsi_data(15 downto 8);
			elsif gsi_data(7 downto 0) = x"E3" then
			  gsi_time(15 downto 8) <= gsi_data(15 downto 8);
			elsif gsi_data(7 downto 0) = x"E2" then
			  gsi_time(23 downto 16) <= gsi_data(15 downto 8);
			elsif gsi_data(7 downto 0) = x"E1" then
			  gsi_time(31 downto 24) <= gsi_data(15 downto 8);
			elsif gsi_data(7 downto 0) = x"E0" then
			  gsi_time(39 downto 32) <= gsi_data(15 downto 8);	  
			end if;
		 else
		   gsi_time_sync <= '0';
		 end if;
		 
	  end if;
	end process;
	
	gsi_time_sec(31 downto 30) <= "00";
	gsi_time_sec(29 downto 0) <= s_current_timestamp(29 downto 0);
	gsi_time_fraction(9 downto 0) <= s_current_timestamp(39 downto 30);
	gsi_time_o <= s_current_timestamp;
					 
					 
   ram_logger_proc: process(clk)
	
	begin
	  if rising_edge(clk) then
	    if reset = '1' then
		   s_rama_en <= '0';
         s_rama_we <= "0";
			r_lrevent_ptr <= "111111111";			
		 elsif gsi_strobe = '1' then
		   r_levent_stamp <= s_current_timestamp(31 downto 0);
		   s_rama_we <= "1";
			r_lrevent_ptr <= std_logic_vector(unsigned(r_lrevent_ptr) + 1);
			r_lrevent_stamp <= s_current_timestamp(31 downto 0);
			--s_events_write_log <= time_i & 
			if gsi_data( 7 downto 0) = x"E4" then
			  s_events_write_log <= time_i & "1" & "0000000" & gsi_data;	
			else 
			  s_events_write_log <= time_i & "0" & "0000000" & gsi_data;	
			end if;
			s_rama_en <= '1';
		 else
 		   s_rama_we <= "0";
			s_rama_en <= '0';
		 end if;
	  end if;
	end process;
  
  ram_logger_inst : ram_logger
  PORT MAP (
    clka => clk,
    rsta => reset,
    ena => s_rama_en,
    wea => s_rama_we,
    addra => r_lrevent_ptr,
    dina => s_events_write_log ,
    douta => open,
	 
    clkb => clk,
    rstb => reset,
    enb => not reset,
    web => "0",
    addrb => read_addr_counter,
    dinb => (others => '0'),
    doutb => events_read_log
  );
	
	
   read_process: process(clk)
	begin
	  if rising_edge(clk) then
	    if reset = '1' then
		   read_addr_counter <= "000000000";
		 else
		   read_addr_counter <= std_logic_vector(unsigned(read_addr_counter) + 1);
			if read_addr_counter= "111111111" then
			  tmp_s_rmb_addr_ovf <= '1';
			else
			  tmp_s_rmb_addr_ovf <= '0';
			end if;
		 end if;
	  end if;
	end process;
	
end Behavioral;


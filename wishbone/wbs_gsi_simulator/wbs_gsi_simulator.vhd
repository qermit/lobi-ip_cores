--_________________________________________________________________________________________________
--                                                                                                |
--                                           |SVEC PTS|                                           |
--                                                                                                |
--                                         CERN,BE/CO-HT                                          |
--________________________________________________________________________________________________|

---------------------------------------------------------------------------------------------------
--                                                                                                |
--                                        fp_info_wb_slave                                        |
--                                                                                                |
---------------------------------------------------------------------------------------------------
-- File         fp_event_logger_wb_slave.vhd                                                              |
--                                                                                                |
-- Description  Wishbone slave that receives/ transmits information from/ to the SN74VMEH22500    |
--              tranceiver. Five 32 bits long registers are used:                                 |
--                o reg in address x0 (reg 0) bit 0: output of  FP_GPIO1                          |
--                o reg in address x0 (reg 0) bit 1: output of  FP_GPIO2                          |
--                o reg in address x1 (reg 1) bit 0: output of  FP_GPIO1_A2B                      |
--                o reg in address x1 (reg 1) bit 1: output of  FP_GPIO2_A2B                      |
--                o reg in address x2 (reg 2) bit 0: input from FP_GPIO3                          |
--                o reg in address x2 (reg 2) bit 1: input from FP_GPIO4                          |
--                o reg in address x3 (reg 3) bit 0: output of  FP_GPIO3_4_DIR                    |
--                o reg in address x4 (reg 4) bit 0: output of  FP_TERM_EN_1                      |
--                o reg in address x4 (reg 4) bit 1: output of  FP_TERM_EN_2                      |
--                o reg in address x4 (reg 4) bit 2: output of  FP_TERM_EN_3                      |
--                o reg in address x4 (reg 4) bit 3: output of  FP_TERM_EN_4                      |
--                                                                                                |
-- Authors      Evangelia Gousiou (Evangelia.Gousiou@cern.ch)                                     |
-- Date         07/2012                                                                           |
-- Version      v1                                                                                |
-- Depends on                                                                                     |
--                                                                                                |
----------------                                                                                  |
-- Last changes                                                                                   |
--     07/2012  v1  EG  First version                                                             |
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
--                               GNU LESSER GENERAL PUBLIC LICENSE                                |
--                              ------------------------------------                              |
-- This source file is free software; you can redistribute it and/or modify it under the terms of |
-- the GNU Lesser General Public License as published by the Free Software Foundation; either     |
-- version 2.1 of the License, or (at your option) any later version.                             |
-- This source is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       |
-- without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.      |
-- See the GNU Lesser General Public License for more details.                                    |
-- You should have received a copy of the GNU Lesser General Public License along with this       |
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html                     |
---------------------------------------------------------------------------------------------------


--=================================================================================================
--                                       Libraries & Packages
--=================================================================================================
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.vme_timer_pkg.all;
--library work;
--use work.bicolor_led_ctrl_pkg.all;
library UNISIM;
use UNISIM.Vcomponents.ALL;
--=================================================================================================
--                              Entity declaration for fp_info_wb_slave
--=================================================================================================
entity wbs_gsi_simulator is
  generic(
    g_CLK_FREQ     : natural := 125000000;  -- in Hz
    g_REFRESH_RATE : natural :=   2000000   -- in Hz
    );
  port
    -- WISHBONE slave signals
    (wb_clk_i           : in  std_logic;
     rst_i              : in  std_logic;
     wb_cyc_i           : in  std_logic;
     wb_stb_i           : in  std_logic;
     wb_addr_i          : in  std_logic_vector(31 downto 0);
     wb_data_i          : in  std_logic_vector(31 downto 0);
     wb_we_i            : in  std_logic;
     wb_data_o          : out std_logic_vector(31 downto 0);
     wb_ack_o           : out std_logic;
     
	  
	  current_timestamp_i : in std_logic_vector(39 downto 0);
	  simulation_i			: in std_logic;
	  
     mil_rdy_o				: out  std_logic;
	  mil_data_o		   : out  std_logic_vector (15 downto 0);
	  mil_serialized_o	: out  std_logic;
	  mil_gate_o	      : out  std_logic
	  
	  );
end wbs_gsi_simulator;


--=================================================================================================
--                                    architecture declaration
--=================================================================================================
architecture behavioral of wbs_gsi_simulator is

  ------------------------------------------------------------------------------
  -- Constants declaration
  ------------------------------------------------------------------------------
  constant c_REFRESH_CNT_INIT     : natural := natural(g_CLK_FREQ/(2 * g_REFRESH_RATE)) - 1;
  constant c_REFRESH_CNT_NB_BITS  : natural := log2_ceil(c_REFRESH_CNT_INIT);
  constant c_CMPCLK_CNT_INIT     : natural := natural(g_CLK_FREQ/(2 * 1)) - 1;
  constant c_CMPCLK_CNT_NB_BITS  : natural := log2_ceil(c_CMPCLK_CNT_INIT);
  constant c_EVT_START_CYCLE      : natural := 16#0020#;
  constant c_EVT_END_CYCLE        : natural := 16#0037#;
  constant c_EVT_START_CYCLE_MASK      : natural := 16#00FF#;
  constant c_EVT_END_CYCLE_MASK        : natural := 16#00FF#;
  
  constant c_2MHz_PULSE_INIT: natural := natural(g_CLK_FREQ/(2 * 1000000)) -1;
  constant c_2MHz_PULSE_BITS: natural := log2_ceil(c_2MHz_PULSE_INIT);
  signal s_counter_1mhz: std_logic_vector(c_2MHz_PULSE_BITS-1 downto 0);
  signal s_pulse_1mhz: std_logic;

  
  constant c_1MS_PULSE_INIT: natural := natural(g_CLK_FREQ/1000) -1;
  constant c_1MS_PULSE_BITS: natural := log2_ceil(c_1MS_PULSE_INIT);
  signal s_counter_1ms: std_logic_vector(c_1MS_PULSE_BITS-1 downto 0);
  signal s_pulse_1ms: std_logic;

  constant c_PULSE_CNT_INIT     : natural := 10000;

  signal dummy_reg                       : std_logic_vector(31 downto 0);
  
    
  signal s_rama_en: std_logic;
  signal s_rama_we: std_logic_vector(0 downto 0);
  signal s_rama_event_read: std_logic_vector(63 downto 0);
  signal s_rama_event_write: std_logic_vector(63 downto 0);
  signal s_event_ptr :std_logic_vector(8 downto 0);
  
  signal s_ramb_en: std_logic;
  signal s_ramb_we: std_logic_vector(0 downto 0);
  signal s_ramb_rforward		: std_logic_vector(31 downto 0); -- register used to store BRAM value
  signal s_ramb_data				: std_logic_vector(31 downto 0); -- register used to store BRAM value
  
  signal s_current_timestamp: std_logic_vector(39 downto 0);

  signal r_status_reg: std_logic_vector(31 downto 0);
  signal r_status_reg_set: std_logic_vector(31 downto 0);
  signal r_status_reg_clr: std_logic_vector(31 downto 0);


  signal s_enable_simulation: std_logic;
  signal s_local_rst: std_logic;
  
  signal s_tie_to_gound: std_logic_vector(31 downto 0);

  COMPONENT ram_dual
  PORT (
    clka : IN STD_LOGIC;
    rsta : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(63 DOWNTO 0);
    clkb : IN STD_LOGIC;
    rstb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
  END COMPONENT;
  
  COMPONENT MIL_TRANSMITTER
  GENERIC(
			CLK_in_Hz		: INTEGER := 40000000		
			);
  PORT(
         clk : IN  std_logic;
				        pulse_clk: in std_logic;

			reset : IN STD_LOGIC;
         data : IN  std_logic_vector(15 downto 0);
         stb : IN  std_logic;
         serdata : OUT  std_logic;
         gate : OUT  std_logic;
         milidle : OUT  std_logic
        );
    END COMPONENT;
	 
	 signal s_mil_event: std_logic_vector(15 downto 0);
	 
	 signal s_mil_stb: std_logic;
	 signal s_mil_gate: std_logic;
	 signal s_mil_serialized: std_logic;
	 
	 
	 signal s_mil_delay_next: std_logic_vector(31 downto 0);
	 signal s_mil_event_next: std_logic_vector(15 downto 0);
	 signal s_mil_delay_left: std_logic_vector(31 downto 0);
	 signal s_mil_search_next: std_logic;
--=================================================================================================
--                                       architecture begin
--=================================================================================================
begin
  s_current_timestamp <= current_timestamp_i;
  
  s_tie_to_gound <= x"00000000";
  
  
  ram_dual_inst : ram_dual
  PORT MAP (
    --- ram a - data for simulation
    clka => wb_clk_i,
    rsta => rst_i,
    ena => s_rama_en,
    wea => s_rama_we,
    addra => s_event_ptr,
    dina => s_rama_event_write,
    douta => s_rama_event_read,
	 
	 --- ram b - read/write wishbone interface
    clkb => wb_clk_i,
    rstb => rst_i,
    enb => s_ramb_en,
    web => s_ramb_we,
    addrb => wb_addr_i(9 downto 0),
    dinb => s_ramb_data,
    doutb => s_ramb_rforward
  );

  s_rama_we <= "0";
  s_rama_en <= '1';
  
  mil_transmiter_inst: MIL_TRANSMITTER 
  generic map (
    CLK_in_Hz => g_CLK_FREQ
  )
  port map (
		clk => wb_clk_i,
		reset => rst_i,
      pulse_clk => s_pulse_1mhz,
		data => s_mil_event,
		stb => s_mil_stb,
		serdata => s_mil_serialized,
		gate => s_mil_gate,
		milidle => open
  );
  
  mil_data_o <= s_mil_event;
  mil_serialized_o <= s_mil_serialized;
  mil_gate_o <= s_mil_gate;
  s_ramb_en <= '1';
---------------------------------------------------------------------------------------------------
--                                         DATA IN/ OUT                                          --
---------------------------------------------------------------------------------------------------
  data_out: process (wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then   
      if rst_i = '1' then
        wb_data_o           <= (others => '0');
        dummy_reg           <= x"C200FFEE";
		  r_status_reg			 <= x"00000000";
      else
        if (wb_cyc_i = '1') and (wb_stb_i = '1') and (wb_we_i = '0') then   -- WISHBONE reads
          if (wb_addr_i = x"00000000") then                                 -- status register
			   wb_data_o       <= r_status_reg;
			 elsif (wb_addr_i = x"00000001") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= dummy_reg;
			 elsif (wb_addr_i = x"00000002") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= dummy_reg;
			 elsif (wb_addr_i = x"00000003") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= x"0000" & s_mil_event;
			elsif (wb_addr_i = x"00000004") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= s_mil_delay_left;
			elsif (wb_addr_i = x"00000005") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= x"0000" & s_mil_event_next;
			elsif (wb_addr_i = x"00000006") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= s_mil_delay_next;
			elsif (wb_addr_i = x"00000007") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= s_tie_to_gound(31 downto 9) & s_event_ptr;
				
			 elsif (wb_addr_i(31 downto 12) = x"00001") then -- Address space for bram (allows to use memcpy without rewinding)
			   wb_data_o <= s_ramb_rforward;
          else
            wb_data_o       <= x"00000000";                         
          end if;
	     elsif (wb_cyc_i = '1') and (wb_stb_i = '1') and (wb_we_i = '1') then-- WISHBONE writes
          if (wb_addr_i = x"00000000") then                                 -- dummy reg read write
            dummy_reg       <= wb_data_i;
			 elsif (wb_addr_i = x"00000001") then                                 -- dummy reg read write
            r_status_reg       <= r_status_reg or wb_data_i;
			 elsif (wb_addr_i = x"00000002") then                                 -- dummy reg read write
            r_status_reg       <= r_status_reg and (not wb_data_i);
			 end if;
        end if;
      end if;
    end if;
  end process;


  s_enable_simulation <= r_status_reg(0);
  s_local_rst <= rst_i or (not  s_enable_simulation);

  s_ramb_data <= wb_data_i;

  ramb_we_proc: process (wb_clk_i, wb_cyc_i, wb_stb_i, wb_we_i, wb_addr_i(31 downto 12))
  begin
    if wb_cyc_i = '1' and wb_stb_i = '1' and wb_we_i = '1' and (wb_addr_i(31 downto 12) = x"00001") then   
        s_ramb_we <= "1";
	 else
	     s_ramb_we <= "0";
    end if;
  end process;
---------------------------------------------------------------------------------------------------
--                                        ACK generation                                         --
---------------------------------------------------------------------------------------------------
  ack_generator: process (wb_clk_i)
  begin
    if rising_edge (wb_clk_i) then
      if rst_i = '1' then
        wb_ack_o <= '0';
      else
        wb_ack_o <= wb_stb_i and wb_cyc_i;
      end if;
    end if;
  end process;

---------------------------------------------------------------------------------------------------
--                                   1Mhz pulse for mil transmiter                               --
---------------------------------------------------------------------------------------------------

pulse_1mhz_proc: process(wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then
	   if rst_i = '1' then
		  s_counter_1mhz <= std_logic_vector(to_unsigned(c_2MHz_PULSE_INIT, c_2MHz_PULSE_BITS));
		  s_pulse_1mhz <= '0';
		elsif unsigned(s_counter_1mhz) = 0 or unsigned(s_counter_1ms) = 0 then
		  s_counter_1mhz <= std_logic_vector(to_unsigned(c_2MHz_PULSE_INIT, c_2MHz_PULSE_BITS));
		  s_pulse_1mhz <= '1';
		else
		  s_counter_1mhz <= std_logic_vector(unsigned(s_counter_1mhz) - 1);
		  s_pulse_1mhz <= '0';
		end if;
	 end if;
  end process;


---------------------------------------------------------------------------------------------------
--                               1khz pulse for event timer                                      --
---------------------------------------------------------------------------------------------------
  pulse_1ms_proc: process(wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then
	   if s_local_rst = '1' then
		  s_counter_1ms <= std_logic_vector(to_unsigned(c_1MS_PULSE_INIT, c_1MS_PULSE_BITS));
		  s_pulse_1ms <= '0';
		elsif unsigned(s_counter_1ms) = 0 then
		  s_counter_1ms <= std_logic_vector(to_unsigned(c_1MS_PULSE_INIT, c_1MS_PULSE_BITS));
		  s_pulse_1ms <= '1';
		else
		  s_counter_1ms <= std_logic_vector(unsigned(s_counter_1ms) - 1);
		  s_pulse_1ms <= '0';
		end if;
	 end if;
  end process;
  
  
  delay_counter_proc: process(wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then
	   if s_local_rst = '1' then
		  s_mil_delay_left <= std_logic_vector(to_unsigned(0, 32));
		  s_mil_event <= x"0000";
		  s_mil_stb <= '0';
		elsif s_pulse_1ms = '1' then
		  if unsigned(s_mil_delay_left) = 0 then
		    if unsigned(s_mil_delay_next) = 0 then
		      s_mil_stb <= '0';
		    else
		      s_mil_stb <= '1';
			   s_mil_event <= s_mil_event_next;
			   s_mil_delay_left <= s_mil_delay_next;
		    end if;
		  else
		    s_mil_delay_left <= std_logic_vector(unsigned(s_mil_delay_left) - 1);
		    s_mil_stb <= '0';
		  end if;
		else
		  s_mil_stb <= '0';
		end if;
	 end if;
  end process;
  mil_rdy_o <= s_mil_stb;
  
  
  
  
  
  find_next_event: process(wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then
	   if s_local_rst = '1' then
	    s_event_ptr <= (others => '0');
		 s_mil_search_next <= '0';
		 s_mil_event_next <= x"0000";
		 s_mil_delay_next <= std_logic_vector(to_unsigned(0, 32));
		elsif s_mil_stb = '1' then
		  s_mil_search_next <= '1';
		else
		  s_mil_event_next <= s_rama_event_read(15 downto 0);
		  s_mil_delay_next <= std_logic_vector(unsigned(s_rama_event_read(63 downto 32)) - 1);
		  if s_rama_event_read(15 downto 0) = x"0000" then
		    s_event_ptr <= (others => '0');
			 s_mil_search_next <= '0';
		  elsif s_mil_search_next = '1' then
		    s_event_ptr <= std_logic_vector(unsigned(s_event_ptr) + 1);
		    s_mil_search_next <= '0';
		  end if;
		end if;
	 end if;
  end process; 
  

end behavioral;

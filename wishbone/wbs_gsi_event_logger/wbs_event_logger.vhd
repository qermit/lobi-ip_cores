--=================================================================================================
--                                       Libraries & Packages
--=================================================================================================
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

library work;
use work.lobi_general_pkg.all;

--=================================================================================================
--                              Entity declaration for fp_info_wb_slave
--=================================================================================================
entity wbs_event_logger is
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

     -- GSI Timing Input
	  mil_rdy_i				: in  std_logic;
	  mil_data_i		   : in  std_logic_vector (15 downto 0);
	  simulation_o			: out std_logic;
	  
	  time_i             : in std_logic_vector(39 downto 0);
     gsi_time_o         : out std_logic_vector(39 downto 0);
     gsi_sync_o         : out std_logic;
	  
	  refresh_cnt_init_o : out std_logic_vector(31 downto 0);
	  gate_start_o       : out std_logic;
     gate_end_o         : out std_logic
	  );
end wbs_event_logger;


--=================================================================================================
--                                    architecture declaration
--=================================================================================================
architecture behavioral of wbs_event_logger is

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

  constant c_PULSE_CNT_INIT     : natural := 10000;
  --natural((g_CLK_FREQ * 100) / 1000000) - 1; --- 
  
  signal r_event_start_cycle :std_logic_vector(15 downto 0);
  signal r_event_start_cycle_mask :std_logic_vector(15 downto 0);
  signal r_event_end_cycle :std_logic_vector(15 downto 0);
  signal r_event_end_cycle_mask:std_logic_vector(15 downto 0);
  

  signal dummy_reg                       : std_logic_vector(31 downto 0);
  
  signal r_bram_rforward						: std_logic_vector(63 downto 0); -- register used to store BRAM value
  
  signal gsi_time: std_logic_vector(39 downto 0);
  signal gsi_time_sync: std_logic;  
  signal r_time_cache						: std_logic_vector(9 downto 0);
  
  
  
  signal s_rama_en: std_logic;
  signal s_rama_we: std_logic_vector(0 downto 0);
  signal s_rama_data: std_logic_vector(63 downto 0);
  signal s_events_write_log: std_logic_vector(63 downto 0);

  signal s_current_timestamp: std_logic_vector(39 downto 0);
  
  signal r_levent_stamp:std_logic_vector(31 downto 0); -- last time when new event was seen
  signal r_lrevent_stamp:std_logic_vector(31 downto 0); -- last time when event was recorded
  signal r_lrevent_stamp_ms:std_logic_vector(9 downto 0);
  signal r_lrevent_stamp_cache:std_logic_vector(9 downto 0);
  signal r_lrevent_ptr:std_logic_vector(8 downto 0); -- last recorded event pointer;
  signal r_lrevent_event:std_logic_vector(15 downto 0); -- last recorded event value;
  signal r_nevent_ptr:std_logic_vector(8 downto 0); 
  
  
  signal r_levent_start_ptr: std_logic_vector(8 downto 0); -- last start cycle event
  signal r_levent_start_event:std_logic_vector(15 downto 0); -- last recorded event value;
  signal r_levent_start_stamp: std_logic_vector(31 downto 0); -- last time when cycle start event was seen
  signal r_levent_end_ptr: std_logic_vector(8 downto 0); -- last  start cycle event
  signal r_levent_end_event:std_logic_vector(15 downto 0); -- last recorded event value;
  signal r_levent_end_stamp: std_logic_vector(31 downto 0); -- last time when cycle end event was seen
  
  signal s_gate_start: std_logic;
  signal s_gate_end: std_logic;
  
  signal r_refresh_cnt_init : std_logic_vector(31 downto 0);
  
  signal s_simulation: std_logic;

	COMPONENT ram_logger
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
		 addrb : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
		 dinb : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
		 doutb : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
	  );
	  
	  END COMPONENT;

--=================================================================================================
--                                       architecture begin
--=================================================================================================
begin
  ram_logger_inst : ram_logger
  PORT MAP (
    clka => wb_clk_i,
    rsta => rst_i,
    ena => s_rama_en,
    wea => s_rama_we,
    addra => r_lrevent_ptr,
    dina => s_events_write_log ,
    douta => open,
	 
    clkb => wb_clk_i,
    rstb => rst_i,
    enb => not rst_i,
    web => "0",
    addrb => wb_addr_i(9 downto 1),
    dinb => (others => '0'),
    doutb => r_bram_rforward
  );

--  ------------------------------------------------------------------------------
--  -- Refresh rate counter
--  ------------------------------------------------------------------------------
--  p_refresh_rate_cnt : process (wb_clk_i)
--  begin
--    if rising_edge(wb_clk_i) then
--      if rst_i = '1' then
--        refresh_rate_cnt <= (others => '0');
--        refresh_rate     <= '0';
--      elsif refresh_rate_cnt = 0 then
--        refresh_rate_cnt <= to_unsigned(c_REFRESH_CNT_INIT, c_REFRESH_CNT_NB_BITS);
--        refresh_rate     <= '1';
--      else
--        refresh_rate_cnt <= refresh_rate_cnt - 1;
--        refresh_rate     <= '0';
--      end if;
--    end if;
--  end process p_refresh_rate_cnt;


---------------------------------------------------------------------------------------------------
--                                         DATA IN/ OUT                                          --
---------------------------------------------------------------------------------------------------
  data_out: process (wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then   
      if rst_i = '1' then
        wb_data_o           <= (others => '0');
        dummy_reg           <= x"C000FFEE";
        r_time_cache        <= (others => '0');
		  r_refresh_cnt_init  <= std_logic_vector(to_unsigned(c_PULSE_CNT_INIT,32));
		  
		  r_event_start_cycle_mask <= std_logic_vector(to_unsigned(c_EVT_START_CYCLE_MASK,16));
		  r_event_start_cycle <= std_logic_vector(to_unsigned(c_EVT_START_CYCLE,16));
		  r_event_end_cycle_mask <= std_logic_vector(to_unsigned(c_EVT_END_CYCLE_MASK,16));
		  r_event_end_cycle <= std_logic_vector(to_unsigned(c_EVT_END_CYCLE,16));
		  
		  s_simulation <= '0';
		  
      else

        if (wb_cyc_i = '1') and (wb_stb_i = '1') and (wb_we_i = '0') then   -- WISHBONE reads

           if (wb_addr_i = x"00000000") then                                 -- status register
			   wb_data_o       <= (0=> s_simulation, others=>'0');
			 elsif (wb_addr_i = x"00000001") then                              -- reg 0 (read/ write): FPGPIO2, FPGPIO1
            wb_data_o       <= dummy_reg;
			 elsif (wb_addr_i = x"00000002") then
			   wb_data_o       <= "00" & time_i(29 downto 0);
				r_time_cache <= time_i(39 downto 30);
			 elsif (wb_addr_i = x"00000003") then
			   wb_data_o       <= "0000000000000000000000" & r_time_cache;
				
			 elsif (wb_addr_i = x"00000004") then                              -- Last recorded event timestamp(sec)
			   wb_data_o       <= r_lrevent_stamp;
				r_lrevent_stamp_cache <= r_lrevent_stamp_ms;
			 elsif (wb_addr_i = x"00000005") then                              -- Last recorded event timestamp(msec)
			   wb_data_o       <= "0000000000000000000000" & r_lrevent_stamp_cache;
				
			elsif (wb_addr_i = x"00000006") then
			   wb_data_o       <= r_lrevent_event & "0000000" & r_nevent_ptr;
			
         elsif (wb_addr_i = x"00000008") then
			   wb_data_o       <= r_levent_start_stamp;
			--elsif (wb_addr_i = x"00000009") then
			--   wb_data_o       <= "0000000000000000000000" & r_levent_start_stamp;
				
			elsif (wb_addr_i = x"0000000A") then
			   wb_data_o       <= r_levent_start_event & "0000000" & r_levent_start_ptr;
			
			elsif (wb_addr_i = x"0000000C") then
			   wb_data_o       <= r_levent_end_stamp;
			--elsif (wb_addr_i = x"0000000D") then
			--   wb_data_o       <= "0000000000000000000000" & r_levent_end_stamp;
				
			elsif (wb_addr_i = x"0000000E") then
			   wb_data_o       <= r_levent_end_event & "0000000" & r_levent_end_ptr;
				
			elsif (wb_addr_i = x"00000010") then
			   wb_data_o       <= r_event_start_cycle_mask & r_event_start_cycle;
			elsif (wb_addr_i = x"00000011") then
			   wb_data_o       <= r_event_end_cycle_mask & r_event_end_cycle;
			elsif (wb_addr_i = x"00000012") then                                 -- status register
			   wb_data_o       <= r_refresh_cnt_init;
			
			 elsif (wb_addr_i(31 downto 12) = x"00001") then -- Address space for bram (allows to use memcpy without rewinding)
			   if (wb_addr_i(0) = '0') then
				  wb_data_o <= r_bram_rforward(63 downto 32);
				else
				  wb_data_o <= r_bram_rforward(31 downto 0);
				end if;
          else
            wb_data_o       <= x"00000000";                         
          end if;

	    elsif (wb_cyc_i = '1') and (wb_stb_i = '1') and (wb_we_i = '1') then-- WISHBONE writes

          if (wb_addr_i = x"00000000") then                                 -- dummy reg read write
            --r_refresh_cnt_init       <= wb_data_i;
				s_simulation <= wb_data_i(0);
			 elsif (wb_addr_i = x"00000001") then                                 -- dummy reg read write
            dummy_reg       <= wb_data_i;
			elsif (wb_addr_i = x"00000010") then
			   r_event_start_cycle_mask <= wb_data_i(31 downto 16);
				r_event_start_cycle <= wb_data_i(15 downto 0);
			elsif (wb_addr_i = x"00000011") then
			   r_event_end_cycle_mask <= wb_data_i(31 downto 16);
				r_event_end_cycle <= wb_data_i(15 downto 0);
			elsif (wb_addr_i = x"00000012") then                                 -- dummy reg read write
            r_refresh_cnt_init       <= wb_data_i;
			 
			 end if;

        end if;
      end if;
    end if;
  end process;

simulation_o <= s_simulation;
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

   ram_logger_proc: process(wb_clk_i)
	
	begin
	  if rising_edge(wb_clk_i) then
	    if rst_i = '1' then
		   s_rama_en <= '0';
         s_rama_we <= "0";
			r_nevent_ptr <= (others => '0');	
			r_lrevent_ptr <= (others => '0');	
         r_levent_stamp <= (others => '0');
			r_levent_start_stamp <= (others => '0');
			r_levent_start_ptr <= (others => '0');
			r_levent_end_stamp <= (others => '0');
			r_levent_end_ptr <= (others => '0');
			
		 elsif mil_rdy_i = '1' then
		   r_levent_stamp <= s_current_timestamp(31 downto 0);
		   s_rama_we <= "1";
			
			r_nevent_ptr <= std_logic_vector(unsigned(r_nevent_ptr) + 1);
			
			r_lrevent_ptr <= r_nevent_ptr;
			r_lrevent_stamp <= "00" &  time_i(29 downto 0);
			r_lrevent_stamp_ms <= time_i(39 downto 30);
			r_lrevent_event <= mil_data_i;
			
			if (mil_data_i( 15 downto 0) and  r_event_start_cycle_mask) = r_event_start_cycle then
			  r_levent_start_ptr <= r_nevent_ptr;
			  r_levent_start_stamp <= "00" & time_i(29 downto 0);
			  r_levent_start_event <= mil_data_i;
			end if;
			
			if (mil_data_i( 15 downto 0) and  r_event_end_cycle_mask) = r_event_end_cycle then
			  r_levent_end_ptr <= r_nevent_ptr;
			  r_levent_end_stamp <= "00" &  time_i(29 downto 0);
			  r_levent_end_event <= mil_data_i;
			end if;
			
			--s_events_write_log <= time_i & 
			if mil_data_i( 7 downto 0) = x"E4" then
			  s_events_write_log <= time_i & "1" & "0000000" & mil_data_i;	
			else 
			  s_events_write_log <= time_i & "0" & "0000000" & mil_data_i;	
			end if;
			s_rama_en <= '1';
		 else
 		   s_rama_we <= "0";
			s_rama_en <= '0';
		 end if;
	  end if;
	end process;


  gate_start_stop_proc: process(wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then
	    if rst_i = '1' then
		   s_gate_start <= '0';
			s_gate_end <= '0';
		 elsif mil_rdy_i = '1' then
		   if (mil_data_i( 15 downto 0) and  r_event_start_cycle_mask) = r_event_start_cycle then
			  s_gate_start <= '1';
			else
			  s_gate_start <= '0';
			end if;

		   if (mil_data_i( 15 downto 0) and  r_event_end_cycle_mask) = r_event_end_cycle then
			  s_gate_end <= '1';
			else
			  s_gate_end <= '0';
			end if;
		 else
		   s_gate_start <= '0';
			s_gate_end <= '0';
		 end if;
	  end if;
  end process;

  gate_start_o <= s_gate_start;
  gate_end_o <= s_gate_end;

  gsi_time_proc: process(wb_clk_i)
  begin
	  if rising_edge(wb_clk_i) then
	    if rst_i = '1' then
		   gsi_time_sync <= '0';
			gsi_time <= (others => '0');
		 elsif mil_rdy_i = '1' then
		   if mil_data_i( 7 downto 0) = x"E4" then
			  s_current_timestamp <= gsi_time(39 downto 8) & mil_data_i(15 downto 8);
			  gsi_time_sync <= '1';
			else
			  gsi_time_sync <= '0';
			end if;
			
			if mil_data_i(7 downto 0) = x"E4" then
			  gsi_time(7 downto 0) <= mil_data_i(15 downto 8);
			elsif mil_data_i(7 downto 0) = x"E3" then
			  gsi_time(15 downto 8) <= mil_data_i(15 downto 8);
			elsif mil_data_i(7 downto 0) = x"E2" then
			  gsi_time(23 downto 16) <= mil_data_i(15 downto 8);
			elsif mil_data_i(7 downto 0) = x"E1" then
			  gsi_time(31 downto 24) <= mil_data_i(15 downto 8);
			elsif mil_data_i(7 downto 0) = x"E0" then
			  gsi_time(39 downto 32) <= mil_data_i(15 downto 8);	  
			end if;
		 else
		   gsi_time_sync <= '0';
		 end if;
		 
	  end if;
	end process;
	gsi_sync_o <= gsi_time_sync;
	gsi_time_o <= s_current_timestamp;
	
--  s_rama_en <= '1' when (wb_cyc_i = '1') and (wb_stb_i = '1') and (wb_we_i = '0') else
--               '0';

  refresh_cnt_init_o <= r_refresh_cnt_init;

end behavioral;

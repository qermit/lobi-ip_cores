
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
library UNISIM;
use UNISIM.Vcomponents.ALL;

use work.vme64x_pack.all;
use work.gencores_pkg.all;
use work.wishbone_pkg.all;

use work.lobi_general_pkg.all; 
use work.dtim_vme_pkg.all; 
use work.dtim_vme_const_pkg.all; 


entity wrapper_top is
   port ( CLK40Ex_PIN      : in    std_logic; 
          DISP_CLK_PIN     : in    std_logic; 
          DSP_PF0_TMR0_PIN : in    std_logic; 
          DSP_PF9_PIN      : in    std_logic; 
          dsp_tif_sck      : in    std_logic; 
          DSP_UART_TX_PIN  : in    std_logic; 
          LVTTLC0_PIN      : in    std_logic; 
          LVTTLC1_PIN      : in    std_logic; 
          LVTTLC2_PIN      : in    std_logic; 
          LVTTLC3_PIN      : in    std_logic; 
          LVTTLC4_PIN      : in    std_logic; 
          LVTTLC5_PIN      : in    std_logic; 
          LVTTLC6_PIN      : in    std_logic; 
          LVTTLC7_PIN      : in    std_logic; 
          MILBUS_PIN       : in    std_logic; 
          RS232_TX_PIN     : in    std_logic; 
          TD0_PRI_PIN      : in    std_logic; 
          TD0_SEC_PIN      : in    std_logic; 
          TD1_PRI_PIN      : in    std_logic; 
          TD1_SEC_PIN      : in    std_logic; 
          T_FS0_PIN        : in    std_logic; 
          T_FS1_PIN        : in    std_logic; 
          T_SCLK0_PIN      : in    std_logic; 
			 --- VME in
			 vme_rst_n_i:in std_logic;
          vme_addr_b          : in    std_logic_vector (31 downto 0); 
          vme_data_b           : in    std_logic_vector (31 downto 0); 
          vme_am_i           : in    std_logic_vector (5 downto 0); 
          vme_as_n_i           : in    std_logic; 
          vme_ds_n_i         : in    std_logic_vector (1 downto 0); 
          vme_iack_n       : in    std_logic; 
			 vme_iackin_n_i   : in std_logic;
          vme_write_n_i           : in    std_logic; 
			 vme_iack_n_i     : in std_logic;
			 --- VME out
          vme_data_dir_o    : out   std_logic_vector(1 downto 0); 
			 
          vme_data_b_o   : out   std_logic_vector (31 downto 0); 
			 vme_berr_o       : out   std_logic;
			 vme_irq_n_o		: out   std_logic_vector(6 downto 0);
			 vme_iackout_n_o  :out std_logic;
			 
			 vme_retry_n_b       : inout   std_logic;
			 vme_lword_n_b       : inout std_logic; 
          vme_dtack_n_b      : inout std_logic;
			 
			 
          DAC_DATA_PIN     : out   std_logic; 
          DAC_SCLK_PIN     : out   std_logic; 
          DAC_SYNC_PIN     : out   std_logic; 
          DBUS             : out   std_logic_vector (7 downto 0); 
          DISP_DATA_PIN    : out   std_logic; 
          DISP_LE_PIN      : out   std_logic; 
          DR0_PRI_PIN      : out   std_logic; 
          DR0_SEC_PIN      : out   std_logic; 
          DR1_PRI_PIN      : out   std_logic; 
          DR1_SEC_PIN      : out   std_logic; 
          DSP_PF8_PIN      : out   std_logic; 
          DSP_UART_RX_PIN  : out   std_logic; 
          LED1_PIN         : out   std_logic; 
          LED2_PIN         : out   std_logic; 
          LED3_PIN         : out   std_logic; 
          LED4_PIN         : out   std_logic; 
          LVTTLA0_PIN      : out   std_logic; 
          LVTTLA1_PIN      : out   std_logic; 
          LVTTLA2_PIN      : out   std_logic; 
          LVTTLA3_PIN      : out   std_logic; 
          LVTTLA4_PIN      : out   std_logic; 
          LVTTLA5_PIN      : out   std_logic; 
          LVTTLA6_PIN      : out   std_logic; 
          LVTTLA7_PIN      : out   std_logic; 
          NIM_OUT          : out   std_logic_vector (7 downto 0); 
			 NIM_IN				: in	  std_logic_vector (7 downto 0);
          RS232_RX_PIN     : out   std_logic; 
          R_FS0_PIN        : out   std_logic; 
          R_FS1_PIN        : out   std_logic; 
          R_SCLK0_PIN      : out   std_logic; 
          R_SCLK1_PIN      : out   std_logic; 
          T_SCLK1_PIN      : out   std_logic; 

			 LVTTLB0_PIN         : in   std_logic; 
          LVTTLB1_PIN         : in   std_logic; 
          LVTTLB2_PIN         : in   std_logic; 
          LVTTLB3_PIN         : in   std_logic; 
          LVTTLB4_PIN         : in   std_logic; 
          LVTTLB5_PIN         : in   std_logic; 
          LVTTLB6_PIN         : in   std_logic; 
          LVTTLB7_PIN         : in   std_logic);
end wrapper_top;

architecture BEHAVIORAL of wrapper_top is
   attribute keep : string;  
	
	CONSTANT	CLK_in_HZ			: INTEGER	:= 40000000;



   signal gaddr: std_logic_vector(4 downto 0); -- geogarphical address emulation
	signal gaddr_par: std_logic; -- parity
	signal vme_ga_i: std_logic_vector(5 downto 0);
	--- DCM
	signal clk_40m_vcxo_buf :std_logic;
	signal sys_clk_40m :std_logic;
	signal sys_clk_80m :std_logic;
	signal clk_locked_out :std_logic;
	signal CLK0_BUF :std_logic;
	signal CLK2X_BUF :std_logic;
	---
	  signal local_rst_n, local_rst, s_rst                 : std_logic;
  -- VME mux
 -- signal s_vme_data_b_o                                : std_logic_vector(31 downto 0);
  signal s_vme_data_dir, s_vme_addr_dir                : std_logic;
  signal s_vme_lword_n_b_o                             : std_logic;
  signal vme_addr_b_o                                : std_logic_vector(31 downto 1);
  
  signal s_vme_retry_n_b_o :std_logic;
  signal s_vme_retry_dir: std_logic;
  
    signal s_vme_dtack_dir:std_logic;
	signal s_vme_dtack_n_b_o :std_logic:='1'; -- @todo: sprawdzic jak to ma sie zachowywac
   
  signal s_vme_berr_n_b: std_logic;
  
  signal s_vme_irq_n_b_o: std_logic_vector(6 downto 0);
  --signal vme_iack_n_i:std_logic:='1';

  signal vme_data_oe_n_o: std_logic;
  signal vme_addr_oe_n_o: std_logic;
  signal vme_retry_oe_o: std_logic;
  -- CSR WISHBONE
  signal wbm_rst, wbm_err, wbm_lock, wbm_rty           : std_logic;
  signal wbm_irq_ack, wbm_irq                          : std_logic;
  signal wbm_cyc, wbm_stb, wbm_ack_decoded             : std_logic;
  signal wbm_stall, wbm_we                             : std_logic;
  signal wbm_dat_in32, wbm_dat_out32                   : std_logic_vector(31 downto 0);
  signal wbm_adr64                                     : std_logic_vector(63 downto 0);
  signal wbm_sel                                       : std_logic_vector(3 downto 0);
  signal wb_sel_decoded                                : std_logic_vector(3 downto 0);
  signal wb_stb_decoded, wb_we_decoded                 : std_logic;
  signal wb_adr_decoded32, wb_dat_wr_decoded32         : std_logic_vector(31 downto 0);
  signal wb_all_cyc_decoded, wb_all_ack                : std_logic_vector(c_CSR_WB_SLAVES_NB-1 downto 0);
  signal wb_all_stall                                  : std_logic_vector(c_CSR_WB_SLAVES_NB-1 downto 0);
  signal wb_all_dat_rdx32                              : std_logic_vector((32*c_CSR_WB_SLAVES_NB)-1 downto 0);


-------------------------	 
  signal mil_rdy_to_rd: std_logic;
  signal mil_rdy: std_logic;
  signal mil_data: std_logic_vector(15 downto 0);
 
   component Mil_dec_edge_timed_vhd
  	GENERIC(
			CLK_in_Hz		: INTEGER := 40000000;			
			Uni_Pol_Neg_In	: INTEGER Range 0 to 1 := 0
			);
      port ( Manchester_In    : in    std_logic; 
             RD_MIL           : in    std_logic; 
             Res              : in    std_logic; 
             High_Speed       : in    std_logic; 
             Clk              : in    std_logic; 
             Rcv_Cmd          : out   std_logic; 
             Rcv_Error        : out   std_logic; 
             Rcv_Rdy          : out   std_logic; 
             Mil_Rcv_Data     : out   std_logic_vector (15 downto 0); 
             Mil_Decoder_Diag : out   std_logic_vector (14 downto 0); 
             Rcv_Rdy_pulse    : out   std_logic);
   end component;
	
	
	component RTC
	GENERIC(
			ZeroYear  : std_logic_vector(15 downto 0):= x"2008";
			g_CLK_FREQ     : natural := 50000000;  -- in Hz
			g_REFRESH_RATE : natural := 1   -- in Hz
	 );
	 Port ( clk : in  STD_LOGIC;
			  rst : in  STD_LOGIC;
			  sync : in  STD_LOGIC;
			  utc_timestamp : in  STD_LOGIC_VECTOR (31 downto 0);
			  synchronized : out  STD_LOGIC;
			  bcd_year_o : out  STD_LOGIC_VECTOR (15 downto 0);
			  bcd_doy_o : out  STD_LOGIC_VECTOR (11 downto 0);
			  bcd_moy_o : out  STD_LOGIC_VECTOR (7 downto 0);
			  bcd_dom_o : out  STD_LOGIC_VECTOR (7 downto 0);
			  bcd_hour_o : out  STD_LOGIC_VECTOR (7 downto 0);
			  bcd_minute_o : out  STD_LOGIC_VECTOR (7 downto 0);
			  bcd_second_o : out  STD_LOGIC_VECTOR (7 downto 0)
			  );
	end component;
	
	signal gsi_time: std_logic_vector(39 downto 0);
	signal gsi_time_fraction: std_logic_vector(9 downto 0);
	signal gsi_time_sec: std_logic_vector(31 downto 0);
	signal gsi_time_sync: std_logic;
	signal gsi_time_cnt_ovf: std_logic;
	signal gsi_1ms_counter: std_logic_vector(15 downto 0);
	signal gsi_frac_cnt: std_logic_vector(18 downto 0);
	attribute keep of gsi_time_sec: signal is "true";  
	attribute keep of gsi_time_fraction: signal is "true";  
	attribute keep of gsi_time_sync: signal is "true"; 
	attribute keep of gsi_1ms_counter: signal is "true";  
	attribute keep of gsi_frac_cnt: signal is "true";  	
	attribute keep of gsi_time_cnt_ovf: signal is "true";  	
-------------------------------------------------------
--- 
-------------------------------------------------------
	COMPONENT test_rtc
		    	GENERIC(
			CLK_in_Hz		: INTEGER := 50000000
			);
	PORT(
		clk_i : IN std_logic;
		rst_i : IN std_logic;
		sync_i : IN std_logic;
		time_i : IN std_logic_vector(39 downto 0);          
		time_o : OUT std_logic_vector(39 downto 0)
		);
	END COMPONENT;

  COMPONENT counter_delay_test
	generic 
    (CLK_in_Hz		: INTEGER := 50000000);
	PORT(
		clk : in  STD_LOGIC;
	        reset: in STD_LOGIC;
           gsi_strobe : in  STD_LOGIC;
           gsi_data : in  STD_LOGIC_VECTOR (15 downto 0);
			  time_i: in std_logic_vector(39 downto 0);
			  gsi_time_o: out std_logic_vector(39 downto 0);
			  gsi_sync_o: out std_logic
		);
	END COMPONENT;
	
	
	signal s_time: std_logic_vector(39 downto 0);
	signal s_gsi_time: std_logic_vector(39 downto 0);
	signal s_gsi_sync: std_logic;
	
	signal gate_event_start: std_logic;
	signal gate_event_end: std_logic;
	signal gate_counter: std_logic_vector(21 downto 0);
	signal gate_counter_end: std_logic_vector(21 downto 0);
	
	signal s_gate_event_start: std_logic;
	signal s_gate_event_end: std_logic;
	
	
	COMPONENT clock_generator
	generic(
      g_CLK_FREQ     : natural := 125000000;  -- in Hz
      g_REFRESH_RATE : natural :=      1000;   -- in Hz
		g_PULSE_WIDTH  : natural :=      100   -- pulse width in uS
   );
   PORT(
         clk_i : IN  std_logic;
         rst_i : IN  std_logic;
         start_i : IN  std_logic;
         stop_i : IN  std_logic;
			refresh_cnt_init_i: in STD_LOGIC_VECTOR(31 downto 0);
         pulse0_o : OUT  std_logic;
         pulse1_o : OUT  std_logic
        );
    END COMPONENT;
	 
	 signal s_pulse0: std_logic;
	 signal s_pulse1: std_logic;
	
	
	signal s_refresh_cnt_init : std_logic_vector(31 downto 0);
	
	signal s_simulation : std_logic;
	signal s_sim_mil_rdy: std_logic;
	signal s_sim_mil_data: std_logic_vector(15 downto 0);
	
	signal s_mil_gate: std_logic;
	
	
	signal s_sim_mil_serialized: std_logic;
	signal s_real_mil_serialized: std_logic;
	signal s_mil_serialized: std_logic;
	
begin

---------------------------------------------------------------------------------------------------
-- Use of 50 MHz VCXO OSC2 to generate a 50 MHz clock with Xilinx internal PLL
  cmp_clk_vcxo : BUFG
  port map
    (O => clk_40m_vcxo_buf,
     I => CLK40Ex_PIN);
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

   DCM_INST : DCM
   generic map( CLK_FEEDBACK => "1X",
            CLKDV_DIVIDE => 2.0,
            CLKFX_DIVIDE => 1,
            CLKFX_MULTIPLY => 4,
            CLKIN_DIVIDE_BY_2 => FALSE,
            CLKIN_PERIOD => 25.000,
            CLKOUT_PHASE_SHIFT => "NONE",
            DESKEW_ADJUST => "SYSTEM_SYNCHRONOUS",
            DFS_FREQUENCY_MODE => "LOW",
            DLL_FREQUENCY_MODE => "LOW",
            DUTY_CYCLE_CORRECTION => TRUE,
            FACTORY_JF => x"C080",
            PHASE_SHIFT => 0,
            STARTUP_WAIT => FALSE)
      port map (CLKFB=>sys_clk_40m,
                CLKIN=>clk_40m_vcxo_buf,
                DSSEN=> '0',
                PSCLK=>'0',
                PSEN=> '0',
                PSINCDEC=>'0',
                RST=>'0',
                CLKDV=>open,
                CLKFX=>open,
                CLKFX180=>open,
                CLK0=>CLK0_BUF,
                CLK2X=>CLK2X_BUF,
                CLK2X180=>open,
                CLK90=>open,
                CLK180=>open,
                CLK270=>open,
                LOCKED=>clk_locked_out,
                PSDONE=>open,
                STATUS=>open);


  dcm_1x_buf : BUFG
  port map
    (O => sys_clk_40m,
     I => CLK0_BUF);

  dcm_2x_buf : BUFG
  port map
    (O => sys_clk_80m,
     I => CLK2X_BUF);

  locked_buf : BUFG
  port map
    (O => local_rst_n,
     I => clk_locked_out);


---------------------------------------------------------------------------------------------------
--                                         Internal Reset                                        --
---------------------------------------------------------------------------------------------------
---- Generation of a reset pulse for the initialization of the logic right after power-up.
--  internal_rst: intern_rst_generator
--  port map
--    (clk_i       => sys_clk_40m,
--     por_n_i     => LVTTLC7_PIN,
--     vme_rst_n_i => vme_rst_n_i,
--     rst_n_o     => local_rst_n);
--
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  local_rst      <= not local_rst_n;




---------------------------------------------------------------------------------------------------
--                                    FAKE Geographic position                                   --
---------------------------------------------------------------------------------------------------
   gaddr(0) <= LVTTLB0_PIN;
   gaddr(1) <= LVTTLB1_PIN;
   gaddr(2) <= LVTTLB2_PIN;
   gaddr(3) <= LVTTLB3_PIN;
   gaddr(4) <= LVTTLB4_PIN;
	gaddr_par <= LVTTLB5_PIN;
	vme_ga_i <= gaddr_par & gaddr;
---------------------------------------------------------------------------------------------------
	

---------------------------------------------------------------------------------------------------
--                                            VME core                                           --
---------------------------------------------------------------------------------------------------
  VMEcore: VME64xCore_Top
  generic map
     (g_clock          => 25,             -- 12.5ns (80MHz clock)
      g_wb_data_width  => 32,
      g_wb_addr_width  => 64,
      g_cram_size      => 1024,
      g_BoardID        => 408,
      g_ManufacturerID => 524336,
      g_RevisionID     => 1,
      g_ProgramID      => 90)
  port map
    (clk_i             => sys_clk_40m,    -- VME signals
     VME_AS_n_i        => vme_as_n_i,
     VME_RST_n_i       => local_rst_n,
     VME_WRITE_n_i     => vme_write_n_i,
     VME_AM_i          => vme_am_i,
     VME_DS_n_i        => vme_ds_n_i,
     VME_GA_i          => vme_ga_i,
     VME_BERR_o        => s_vme_berr_n_b,
     VME_DTACK_n_o     => s_vme_dtack_n_b_o,
     VME_RETRY_n_o     => s_vme_retry_n_b_o,
     VME_LWORD_n_i     => vme_lword_n_b,
     VME_LWORD_n_o     => s_vme_lword_n_b_o,
     VME_ADDR_i        => vme_addr_b(31 downto 1),
     VME_ADDR_o        => vme_addr_b_o,
     VME_DATA_i        => vme_data_b,
     VME_DATA_o        => vme_data_b_o,
     VME_IRQ_o         => s_vme_irq_n_b_o,
     VME_IACKIN_n_i    => vme_iackin_n_i,
     VME_IACK_n_i      => vme_iack_n_i,
     VME_IACKOUT_n_o   => vme_iackout_n_o,
     VME_DTACK_OE_o    => s_vme_dtack_dir, -- Buffer signals
     VME_DATA_DIR_o    => s_vme_data_dir,--
     VME_DATA_OE_N_o   => vme_data_oe_n_o,
     VME_ADDR_DIR_o    => s_vme_addr_dir,
     VME_ADDR_OE_N_o   => vme_addr_oe_n_o,
     VME_RETRY_OE_o    => s_vme_retry_dir,
     DAT_i             => wbm_dat_in32,   -- WISHBONE signals
     DAT_o             => wbm_dat_out32,
     ADR_o             => wbm_adr64,
     CYC_o             => wbm_cyc,
     ERR_i             => wbm_err,
     RTY_i             => wbm_rty,
     SEL_o             => wbm_sel,
     STB_o             => wbm_stb,
     ACK_i             => wbm_ack_decoded,
     WE_o              => wbm_we,
     STALL_i           => wbm_stall,
     IRQ_i             => wbm_irq,        -- IRQ Generator
     INT_ack_o         => wbm_irq_ack,
	  
	  rst_n_i => local_rst_n
	  );

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  wbm_irq            <= '0';

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  -- buffers
  --vme_data_b       <= s_vme_data_b_o    when s_vme_data_dir = '1' else (others => 'Z');
--  vme_addr_b(31 downto 1)       <= s_vme_addr_b_o    when s_vme_addr_dir = '1' else (others => 'Z');
  vme_lword_n_b    <= s_vme_lword_n_b_o when s_vme_addr_dir = '1' else 'Z';
  vme_retry_n_b    <= s_vme_retry_n_b_o when s_vme_retry_dir = '1' else 'Z';
  vme_dtack_n_b    <= s_vme_dtack_n_b_o when s_vme_dtack_dir = '1' else 'Z';
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  -- Outputs:
  --vme_addr_dir_o   <= s_vme_addr_dir;
  vme_data_dir_o(0)   <= not s_vme_data_dir;
  vme_data_dir_o(1)   <= not s_vme_data_dir;
  vme_berr_o <= not s_vme_berr_n_b;
  
  vme_irq_n_o(0) <= '0' when s_vme_irq_n_b_o(0) = '1' else 'Z';
  vme_irq_n_o(1) <= '0' when s_vme_irq_n_b_o(1) = '1' else 'Z';
  vme_irq_n_o(2) <= '0' when s_vme_irq_n_b_o(2) = '1' else 'Z';
  vme_irq_n_o(3) <= '0' when s_vme_irq_n_b_o(3) = '1' else 'Z';
  vme_irq_n_o(4) <= '0' when s_vme_irq_n_b_o(4) = '1' else 'Z';
  vme_irq_n_o(5) <= '0' when s_vme_irq_n_b_o(5) = '1' else 'Z';
  vme_irq_n_o(6) <= '0' when s_vme_irq_n_b_o(6) = '1' else 'Z';

---------------------------------------------------------------------------------------------------
--                                      WISHBONE CSR DECODER                                     --
---------------------------------------------------------------------------------------------------
-- VME base addr |           Unit
-- --------------|--------------------------
--    0x000      | tranceiver info exchange
--    0x100      | push button evaluation

  addr_decoder: wb_addr_decoder
  generic map
    (g_WINDOW_SIZE  => c_BAR0_APERTURE,
     g_WB_SLAVES_NB => c_CSR_WB_SLAVES_NB)
  port map
    (clk_i       => sys_clk_40m,
     rst_n_i     => local_rst_n,
     -- WISHBONE master interface
     wbm_adr_i   => wbm_adr64(31 downto 0),
     wbm_dat_i   => wbm_dat_out32,
     wbm_sel_i   => wbm_sel,
     wbm_stb_i   => wbm_stb,
     wbm_we_i    => wbm_we,
     wbm_cyc_i   => wbm_cyc,
     wbm_ack_o   => wbm_ack_decoded,
     wbm_dat_o   => wbm_dat_in32,
     wbm_stall_o => wbm_stall,
     -- WISHBONE slaves interface
     wb_dat_i    => wb_all_dat_rdx32,
     wb_ack_i    => wb_all_ack,
     wb_stall_i  => wb_all_stall,
     wb_cyc_o    => wb_all_cyc_decoded,
     wb_stb_o    => wb_stb_decoded,
     wb_we_o     => wb_we_decoded,
     wb_sel_o    => wb_sel_decoded,
     wb_adr_o    => wb_adr_decoded32,
     wb_dat_o    => wb_dat_wr_decoded32);

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  wb_all_stall   <= (others => '0');

---------------------------------------------------------------------------------------------------
--                                 LEMO TRANCEIVER INFO EXCHANGE                                 --
---------------------------------------------------------------------------------------------------
-- Note: c_LEMO_TRANCEIV_WB_ADDR = 0

  event_logger: fp_event_logger_wb_slave
  generic map
    (g_CLK_FREQ      => CLK_in_HZ,     -- in Hz
     g_REFRESH_RATE  => 250)          -- in Hz
  port map
    (
	  wb_clk_i         => sys_clk_40m,
     rst_i            => local_rst,
     wb_cyc_i         => wb_all_cyc_decoded(c_LEMO_TRANCEIV_WB_ADDR),
     wb_stb_i         => wb_stb_decoded,
     wb_addr_i        => wb_adr_decoded32,
     wb_data_i        => wb_dat_wr_decoded32,
     wb_we_i          => wb_we_decoded,
     wb_data_o        => wb_all_dat_rdx32(c_LEMO_TRANCEIV_WB_ADDR * 32 + 31 downto 32 * c_LEMO_TRANCEIV_WB_ADDR),
     wb_ack_o         => wb_all_ack(c_LEMO_TRANCEIV_WB_ADDR),

	  mil_data_i       => mil_data,
	  mil_rdy_i        => mil_rdy,
	  simulation_o		 => s_simulation,
	  time_i           => s_time,
     gsi_time_o       => s_gsi_time,
     gsi_sync_o       => s_gsi_sync,
	  refresh_cnt_init_o => s_refresh_cnt_init,
	  gate_start_o     => gate_event_start,
     gate_end_o       => gate_event_end

	 );


  event_simulator: fp_gsi_simulator_wb_slave
  generic map(
    g_CLK_FREQ     => CLK_in_HZ
    )
  port map 
  (
	  wb_clk_i         => sys_clk_40m,
     rst_i            => local_rst,
     wb_cyc_i         => wb_all_cyc_decoded(c_GSI_EVENT_SIM_WB_ADDR),
     wb_stb_i         => wb_stb_decoded,
     wb_addr_i        => wb_adr_decoded32,
     wb_data_i        => wb_dat_wr_decoded32,
     wb_we_i          => wb_we_decoded,
     wb_data_o        => wb_all_dat_rdx32(c_GSI_EVENT_SIM_WB_ADDR * 32 + 31 downto 32 * c_GSI_EVENT_SIM_WB_ADDR),
     wb_ack_o         => wb_all_ack(c_GSI_EVENT_SIM_WB_ADDR),
     
	  simulation_i		=> s_simulation,
	  
     mil_rdy_o			=> s_sim_mil_rdy,
	  mil_data_o		=> s_sim_mil_data,
	  mil_serialized_o		=> s_sim_mil_serialized,
	  mil_gate_o => s_mil_gate
	  
	  );

  LVTTLA0_PIN <= s_mil_gate;
  LVTTLA1_PIN <= s_sim_mil_serialized;

   gate_process: process(sys_clk_40m)
	begin
	  if rising_edge(sys_clk_40m) then
	    if local_rst = '1' then
  	      s_gate_event_start <= '0';
			gate_counter <= std_logic_vector(to_unsigned(39999, 22));
		 elsif gate_event_start = '1' then
		   gate_counter <= std_logic_vector(to_unsigned(39999, 22));
			s_gate_event_start <= '1';
		 elsif unsigned(gate_counter) = 0 then
  	      s_gate_event_start <= '0';
		 else
		   gate_counter <= std_logic_vector(unsigned(gate_counter) - 1);
			s_gate_event_start <= '1';
		 end if;		 
	  end if;
	end process;
	LED1_PIN <= s_gate_event_start;
	NIM_OUT(0) <= not (s_gate_event_start);
	
   gate_process_end: process(sys_clk_40m)
	begin
	  if rising_edge(sys_clk_40m) then
	    if local_rst = '1' then
  	      s_gate_event_end <= '0';
			gate_counter_end <= std_logic_vector(to_unsigned(39999, 22));
		 elsif  gate_event_end = '1' then
		   gate_counter_end <= std_logic_vector(to_unsigned(39999, 22));
			s_gate_event_end <= '1';
		 elsif unsigned(gate_counter_end) = 0 then
  	      s_gate_event_end <= '0';
		 else
		   gate_counter_end <= std_logic_vector(unsigned(gate_counter_end) - 1);
			s_gate_event_end <= '1';
		 end if;		 
	  end if;
	end process;
	
	LED2_PIN <= s_gate_event_end;
	NIM_OUT(1) <= not s_gate_event_end;
	
	LED3_PIN <= s_gate_event_start or s_gate_event_end;
	NIM_OUT(2) <= not (s_gate_event_start or s_gate_event_end);
	
	
--     fp_gpio3_i       => LVTTLC0_PIN,
--     fp_gpio4_i       => LVTTLC1_PIN,
--     fp_gpio1_o       => LED4_PIN,
--     fp_gpio2_o       => LED3_PIN,
--     fp_gpio1_a2b_o   => LED2_PIN,
--     fp_gpio2_a2b_o   => LED1_PIN,
--     fp_gpio3_4_dir_o => LVTTLA0_PIN,
--     fp_term_en_1_o   => LVTTLA1_PIN,
--     fp_term_en_2_o   => LVTTLA2_PIN,
--     fp_term_en_3_o   => LVTTLA3_PIN,
--     fp_term_en_4_o   => LVTTLA4_PIN,
---------------------------------------------------------------------------------------------------
--                                   MIL decoder                                                 --
---------------------------------------------------------------------------------------------------

	Mil_dec_edge_timed: Mil_dec_edge_timed_vhd
	generic map
    (CLK_in_Hz  => CLK_in_HZ,
     Uni_Pol_Neg_In => 0)
	PORT MAP(
		Manchester_In => s_mil_serialized,
		RD_MIL => mil_rdy_to_rd,
		Res => local_rst,
		High_Speed => '0',
		Clk => sys_clk_40m,
		Rcv_Cmd => open,
		Rcv_Error => open,
		Rcv_Rdy => mil_rdy_to_rd,
		Rcv_Rdy_pulse => mil_rdy,
		Mil_Rcv_Data => mil_data,
		Mil_Decoder_Diag => open
	);
	
--  fp_memory_logger: counter_delay_test 
--  generic map (
--	 CLK_in_Hz  => CLK_in_HZ
--  )
--  PORT MAP (
--    clk => sys_clk_40m,
--    reset => local_rst,
--    gsi_strobe => mil_rdy,
--    gsi_data => mil_data,
--    time_i => s_time,
--    gsi_time_o => s_gsi_time,
--    gsi_sync_o => s_gsi_sync
--  );

   dtim_outputs: clock_generator 
	GENERIC MAP (
	  g_CLK_FREQ => CLK_in_HZ,
	  g_REFRESH_RATE=>  1000, -- 1000Hz
	  g_PULSE_WIDTH=> 100   -- 100uS
	)
	PORT MAP (
          clk_i => sys_clk_40m,
          rst_i => local_rst,
          start_i => gate_event_start,
          stop_i => gate_event_end,
			 refresh_cnt_init_i => s_refresh_cnt_init,
          pulse0_o => s_pulse0,
          pulse1_o => s_pulse1
        );
  NIM_OUT(3) <= not s_pulse0;
  NIM_OUT(4) <= not s_pulse1;

  inst_rtc: test_rtc 
  GENERIC MAP (
			CLK_in_Hz => CLK_in_HZ
  )
  PORT MAP(
    clk_i => sys_clk_40m,
    rst_i => local_rst,
    sync_i => s_gsi_sync,
    time_i => s_gsi_time,
    time_o => s_time
  );		
	
  s_real_mil_serialized <= MILBUS_PIN;
  
  s_mil_serialized <= s_real_mil_serialized when s_simulation = '0' else
                      s_sim_mil_serialized;
	
end BEHAVIORAL;



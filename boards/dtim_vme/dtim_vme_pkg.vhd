-- Standard libraries
library IEEE;
use IEEE.STD_LOGIC_1164.all; -- std_logic definitions
use IEEE.NUMERIC_STD.all;    -- conversion functions
-- Specific libraries
use work.vme64x_pack.all;
use work.VME_CR_pack.all;
use work.VME_CSR_pack.all;

use work.dtim_vme_const_pkg.all;

--=================================================================================================
--                              Package declaration for svec_pts_pkg
--=================================================================================================
package dtim_vme_pkg is

---------------------------------------------------------------------------------------------------
--                                      Components Declarations:                                 --
---------------------------------------------------------------------------------------------------


component VME64xCore_Top is
  generic(
    -- clock period (ns)
    g_clock          : integer := c_clk_period;  -- 100 MHz 
    --WB data width:
    g_wb_data_width  : integer := c_width;       -- must be 32 or 64
    --WB address width:
    g_wb_addr_width  : integer := c_addr_width;  -- 64 or less
    -- CRAM 
    g_cram_size      : integer := c_CRAM_SIZE;
    -- Board ID; each board shall have an unique ID. eg: SVEC_ID = 408.
    -- loc: 0x33, 0x37, 0x3B, 0x3F   CR space
    g_BoardID        : integer := c_SVEC_ID;     -- 4 bytes: 0x00000198
    -- Manufacturer ID: eg the CERN ID is 0x080030
    -- loc: 0x27, 0x2B, 0x2F   CR space
    g_ManufacturerID : integer := c_CERN_ID;     -- 3 bytes: 0x080030
    -- Revision ID
    -- loc: 0x43, 0x47, 0x4B, 0x4F   CR space
    g_RevisionID     : integer := c_RevisionID;  -- 4 bytes: 0x00000001
    -- Program ID: this is the firmware ID
    -- loc: 0x7f    CR space
    g_ProgramID      : integer := 90             -- 1 byte : 0x5a 
    -- The default values can be found in the vme64x_pack
    );
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic;

    -- VME                            
    VME_AS_n_i      : in  std_logic;
    VME_RST_n_i     : in  std_logic;    -- asserted when '0'
    VME_WRITE_n_i   : in  std_logic;
    VME_AM_i        : in  std_logic_vector(5 downto 0);
    VME_DS_n_i      : in  std_logic_vector(1 downto 0);
    VME_GA_i        : in  std_logic_vector(5 downto 0);
    VME_BERR_o      : out std_logic;  -- [In the VME standard this line is asserted when low.
    -- Here is asserted when high indeed the logic will be 
    -- inverted again in the VME transceivers on the board]*.
    VME_DTACK_n_o   : out std_logic;
    VME_RETRY_n_o   : out std_logic;
    VME_LWORD_n_i   : in  std_logic;
    VME_LWORD_n_o   : out std_logic;
    VME_ADDR_i      : in  std_logic_vector(31 downto 1);
    VME_ADDR_o      : out std_logic_vector(31 downto 1);
    VME_DATA_i      : in  std_logic_vector(31 downto 0);
    VME_DATA_o      : out std_logic_vector(31 downto 0);
    VME_IRQ_o       : out std_logic_vector(6 downto 0);  -- the same as []*
    VME_IACKIN_n_i  : in  std_logic;
    VME_IACK_n_i    : in  std_logic;
    VME_IACKOUT_n_o : out std_logic;

    -- VME buffers
    VME_DTACK_OE_o  : out std_logic;
    VME_DATA_DIR_o  : out std_logic;
    VME_DATA_OE_N_o : out std_logic;
    VME_ADDR_DIR_o  : out std_logic;
    VME_ADDR_OE_N_o : out std_logic;
    VME_RETRY_OE_o  : out std_logic;

    -- WishBone
    DAT_i   : in  std_logic_vector(g_wb_data_width - 1 downto 0);
    DAT_o   : out std_logic_vector(g_wb_data_width - 1 downto 0);
    ADR_o   : out std_logic_vector(g_wb_addr_width - 1 downto 0);
    CYC_o   : out std_logic;
    ERR_i   : in  std_logic;
    RTY_i   : in  std_logic;
    SEL_o   : out std_logic_vector(f_div8(g_wb_data_width) - 1 downto 0);
    STB_o   : out std_logic;
    ACK_i   : in  std_logic;
    WE_o    : out std_logic;
    STALL_i : in  std_logic;

    -- IRQ Generator
    INT_ack_o : out std_logic;  -- when the IRQ controller acknowledges the Interrupt
    -- cycle it sends a pulse to the IRQ Generator
    IRQ_i     : in  std_logic;  -- Interrupt request; the IRQ Generator/your Wb application
    -- sends a pulse to the IRQ Controller which asserts one of 
    -- the IRQ lines.
    -- Added by Davide for debug:
    debug     : out std_logic_vector(7 downto 0)
    );
	 end component;
	 
---------------------------------------------------------------------------------------------------
  component wb_addr_decoder
    generic
      (g_WINDOW_SIZE  : integer := 18;   -- Number of bits to address periph on the board (32-bit word address)
       g_WB_SLAVES_NB : integer := 2);
    port
    (clk_i       : in std_logic;
     rst_n_i     : in std_logic;
     wbm_adr_i   : in  std_logic_vector(31 downto 0);
     wbm_dat_i   : in  std_logic_vector(31 downto 0);
     wbm_sel_i   : in  std_logic_vector(3 downto 0);
     wbm_stb_i   : in  std_logic;
     wbm_we_i    : in  std_logic;
     wbm_cyc_i   : in  std_logic;
     wb_dat_i    : in  std_logic_vector((32*g_WB_SLAVES_NB)-1 downto 0);
     wb_ack_i    : in  std_logic_vector(g_WB_SLAVES_NB-1 downto 0);
     wb_stall_i  : in  std_logic_vector(g_WB_SLAVES_NB-1 downto 0);
      -------------------------------------------------------------
     wbm_dat_o   : out std_logic_vector(31 downto 0);
     wbm_ack_o   : out std_logic; 
     wbm_stall_o : out std_logic;
     wb_adr_o    : out std_logic_vector(31 downto 0);
     wb_dat_o    : out std_logic_vector(31 downto 0);
     wb_sel_o    : out std_logic_vector(3 downto 0);
     wb_stb_o    : out std_logic;
     wb_we_o     : out std_logic;
     wb_cyc_o    : out std_logic_vector(g_WB_SLAVES_NB-1 downto 0));
      -------------------------------------------------------------
  end component;


---------------------------------------------------------------------------------------------------
  component wbs_event_logger
    generic(
    g_CLK_FREQ     : natural := 125000000;  -- in Hz
    g_REFRESH_RATE : natural := 250         -- in Hz
    );
    port
      (wb_clk_i           : in  std_logic;
       rst_i              : in  std_logic;
       wb_cyc_i           : in  std_logic;
       wb_stb_i           : in  std_logic;
       wb_addr_i          : in  std_logic_vector(31 downto 0);
       wb_data_i          : in  std_logic_vector(31 downto 0);
       wb_we_i            : in  std_logic;
       wb_data_o          : out std_logic_vector(31 downto 0);
       wb_ack_o           : out std_logic;
	    mil_rdy_i	        : in  std_logic;
	    mil_data_i         : in  std_logic_vector (15 downto 0);
		 
		 simulation_o			: out std_logic;
		 
		 time_i             : in std_logic_vector(39 downto 0);
       gsi_time_o         : out std_logic_vector(39 downto 0);
       gsi_sync_o         : out std_logic;
	  	  refresh_cnt_init_o : out std_logic_vector(31 downto 0);

	    gate_start_o       : out std_logic;
       gate_end_o         : out std_logic
    );
  end component;
---------------------------------------------------------------------------------------------------
component wbs_gsi_simulator is
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
     
	  simulation_i			: in std_logic;
	  
     mil_rdy_o				: out  std_logic;
	  mil_data_o		   : out  std_logic_vector (15 downto 0);
	  mil_serialized_o	: out  std_logic;
	  mil_gate_o	      : out  std_logic
	  
	  );
end component;

---------------------------------------------------------------------------------------------------
  component incr_counter
    generic
      (width             : integer := 32);
    port
      (clk_i             : in std_logic;
       counter_top_i     : in std_logic_vector(width-1 downto 0);
       counter_incr_en_i : in std_logic;
       rst_i             : in std_logic;
      -------------------------------------------------------------
       counter_is_full_o : out std_logic;
       counter_o         : out std_logic_vector(width-1 downto 0));
      ------------------------------------------------------------- 
 end component;


---------------------------------------------------------------------------------------------------
  component intern_rst_generator
    port
      (clk_i       : in  std_logic;
       por_n_i     : in  std_logic;
       vme_rst_n_i : in  std_logic;
      -------------------------------------------------------------
       rst_n_o     : out std_logic);
      -------------------------------------------------------------
 end component;



	  
end dtim_vme_pkg;

---------------------------------------------------------------------------------------------------
--                                      E N D   O F   F I L E
---------------------------------------------------------------------------------------------------

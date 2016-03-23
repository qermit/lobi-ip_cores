library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;

package wishbone_gsi_lobi_pkg is


------------------------------------------------------------------------------
-- Components declaration
-------------------------------------------------------------------------------

  constant c_xwb_gpio_raw_sdb : t_sdb_device := (
    abi_class     => x"0000",              -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7",                 -- 8/16/32-bit port granularity
    sdb_component => (
      addr_first  => x"0000000000000000",
      addr_last   => x"00000000000000ff",
      product     => (
        vendor_id => x"000000000000A8DF",  -- GSI - IANA
        device_id => x"441c5145",
        version   => x"00000001",
        date      => x"20160201",
        name      => "WB-GPIO-RAW        ")));


  component wb_gpio_raw
    generic (
      g_interface_mode         : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity    : t_wishbone_address_granularity := WORD;
      g_num_pins               : natural range 1 to 256;
      g_with_builtin_tristates : boolean                        := false;
      g_debug : boolean                        := false
      );
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      wb_sel_i   : in    std_logic_vector(c_wishbone_data_width/8-1 downto 0);
      wb_cyc_i   : in    std_logic;
      wb_stb_i   : in    std_logic;
      wb_we_i    : in    std_logic;
      wb_adr_i   : in    std_logic_vector(7 downto 0);
      wb_dat_i   : in    std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_dat_o   : out   std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_ack_o   : out   std_logic;
      wb_stall_o : out   std_logic;
      gpio_b     : inout std_logic_vector(g_num_pins-1 downto 0);
      gpio_out_o : out   std_logic_vector(g_num_pins-1 downto 0);
      gpio_in_i  : in    std_logic_vector(g_num_pins-1 downto 0);
      gpio_oen_o : out   std_logic_vector(g_num_pins-1 downto 0);
      gpio_term_o : out  std_logic_vector(g_num_pins-1 downto 0);
      raw_o: out STD_LOGIC_VECTOR (g_num_pins-1 downto 0);
      raw_i: in  STD_LOGIC_VECTOR (g_num_pins-1 downto 0));
  end component;

  component xwb_gpio_raw
    generic (
      g_interface_mode         : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity    : t_wishbone_address_granularity := WORD;
      g_num_pins               : natural range 1 to 256;
      g_with_builtin_tristates : boolean;
      g_debug : boolean                        := false);
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      slave_i    : in    t_wishbone_slave_in;
      slave_o    : out   t_wishbone_slave_out;
      desc_o     : out   t_wishbone_device_descriptor;
      gpio_b     : inout std_logic_vector(g_num_pins-1 downto 0);
      gpio_out_o : out   std_logic_vector(g_num_pins-1 downto 0);
      gpio_in_i  : in    std_logic_vector(g_num_pins-1 downto 0);
      gpio_oen_o : out   std_logic_vector(g_num_pins-1 downto 0);
      gpio_term_o : out  std_logic_vector(g_num_pins-1 downto 0);
      raw_o: out STD_LOGIC_VECTOR (g_num_pins-1 downto 0);
      raw_i: in  STD_LOGIC_VECTOR (g_num_pins-1 downto 0));
  end component;

  constant c_xwb_scaler_sdb : t_sdb_device := (
    abi_class     => x"0000",              -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7",                 -- 8/16/32-bit port granularity
    sdb_component => (
      addr_first  => x"0000000000000000",
      addr_last   => x"00000000000000ff",
      product     => (
        vendor_id => x"000000000000A8DF",  -- GSI - IANA
        device_id => x"441c5146",
        version   => x"00000001",
        date      => x"20160201",
        name      => "WB-SCALER          ")));


  component wb_scaler
    generic (
      g_interface_mode         : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity    : t_wishbone_address_granularity := WORD;
      g_debug : boolean                        := false
      );
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      wb_sel_i   : in    std_logic_vector(c_wishbone_data_width/8-1 downto 0);
      wb_cyc_i   : in    std_logic;
      wb_stb_i   : in    std_logic;
      wb_we_i    : in    std_logic;
      wb_adr_i   : in    std_logic_vector(7 downto 0);
      wb_dat_i   : in    std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_dat_o   : out   std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_ack_o   : out   std_logic;
      wb_stall_o : out   std_logic;
      raw_i: in  STD_LOGIC_VECTOR (7 downto 0));
  end component;

  component xwb_scaler
    generic (
      g_interface_mode         : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity    : t_wishbone_address_granularity := WORD;
      g_debug : boolean                        := false);
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      slave_i    : in    t_wishbone_slave_in;
      slave_o    : out   t_wishbone_slave_out;
      desc_o     : out   t_wishbone_device_descriptor;
      raw_i: in  STD_LOGIC_VECTOR (7 downto 0));
  end component;

	component edge_detector
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
end component;


  component xwb_tics_adv
    generic (
      g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity : t_wishbone_address_granularity := WORD;
      g_period              : integer);
    port (
      clk_sys_i : in  std_logic;
      rst_n_i   : in  std_logic;
      
      trig_i                                    : in std_logic;
      trig_o                                    : out std_logic;
      tick_i                                    : in std_logic;
      tick_o                                    : out std_logic;
      
      slave_i   : in  t_wishbone_slave_in;
      slave_o   : out t_wishbone_slave_out;
      desc_o    : out t_wishbone_device_descriptor);
  end component;
 
end wishbone_gsi_lobi_pkg;

package body wishbone_gsi_lobi_pkg is
  
end wishbone_gsi_lobi_pkg;

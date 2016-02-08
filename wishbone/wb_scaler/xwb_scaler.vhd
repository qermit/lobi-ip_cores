library ieee;
use ieee.std_logic_1164.all;

use work.wishbone_pkg.all;
use work.wishbone_gsi_lobi_pkg.all;

entity xwb_scaler is
  generic(
    g_interface_mode         : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity    : t_wishbone_address_granularity := WORD;
	 g_debug						  : boolean:= false
    );

  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- Wishbone
    slave_i : in  t_wishbone_slave_in;
    slave_o : out t_wishbone_slave_out;
    desc_o  : out t_wishbone_device_descriptor;
	 
    raw_i: in  STD_LOGIC_VECTOR (7 downto 0)

    );

end xwb_scaler;

architecture rtl of xwb_scaler is

begin  -- rtl
  

  Wrapped_SCALER : wb_scaler
    generic map (
      g_interface_mode         => g_interface_mode,
      g_address_granularity    => g_address_granularity,
		g_debug						 => g_debug)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      wb_sel_i   => slave_i.sel,
      wb_cyc_i   => slave_i.cyc,
      wb_stb_i   => slave_i.stb,
      wb_we_i    => slave_i.we,
      wb_adr_i   => slave_i.adr(7 downto 0),
      wb_dat_i   => slave_i.dat(31 downto 0),
      wb_dat_o   => slave_o.dat(31 downto 0),
      wb_ack_o   => slave_o.ack,
      wb_stall_o => slave_o.stall,
      raw_i => raw_i
		
);

  slave_o.err   <= '0';
  slave_o.int   <= '0';
  slave_o.rty   <= '0';
  
end rtl;

----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/09/2015 02:24:27 PM
-- Design Name: 
-- Module Name: fmc_5chttl - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
library UNISIM;
use UNISIM.VComponents.all;

use work.wishbone_pkg.all;

use work.fmc_general_pkg.all;
use work.wishbone_gsi_lobi_pkg.all;

entity fmc_dio5chttl is
generic (
  g_num_io                : natural                        := 5;
  g_negate_in    : std_logic_vector(255 downto 0) := (others => '0');
  g_negate_out   : std_logic_vector(255 downto 0) := (others => '0')
);
    Port ( clk_i : in STD_LOGIC;
           rst_i : in STD_LOGIC;
           
           fmc_in: in t_fmc_signals_in;
           fmc_out: out t_fmc_signals_out;

           slave_i       : in  t_wishbone_slave_in;
           slave_o       : out t_wishbone_slave_out;
           
           raw_o: out STD_LOGIC_VECTOR (g_num_io-1 downto 0);
           raw_i: in  STD_LOGIC_VECTOR (g_num_io-1 downto 0)
           );
end fmc_dio5chttl;

architecture Behavioral of fmc_dio5chttl is

signal s_io_out_tmp: std_logic_vector(g_num_io-1 downto 0);
signal s_io_out: std_logic_vector(g_num_io-1 downto 0);

signal s_io_in: std_logic_vector(g_num_io-1 downto 0);

signal s_io_in_tmp: std_logic_vector(g_num_io-1 downto 0);

signal rst_n_i: std_logic;

signal s_dir: std_logic_vector(g_num_io-1 downto 0);
signal s_term: std_logic_vector(g_num_io-1 downto 0);
signal s_dir_o: std_logic_vector(g_num_io-1 downto 0);
signal s_dir_tmp: std_logic_vector(g_num_io-1 downto 0);
--signal s_term_o: std_logic_vector(g_num_io-1 downto 0);

signal s_output: t_diff_port_array(g_num_io-1 downto 0);
signal s_input: t_diff_port_array(g_num_io-1 downto 0);

signal s_input_raw: std_logic_vector(g_num_io-1 downto 0);
signal r_input: std_logic_vector(g_num_io-1 downto 0);

signal s_output_raw: std_logic_vector(g_num_io-1 downto 0);
signal r_output: std_logic_vector(g_num_io-1 downto 0);

begin
  rst_n_i <= not rst_i;

  
  fmc_out.LA_n(30) <= s_term(0);
  fmc_out.LA_n(6)  <= s_term(1);
  fmc_out.LA_n(5)  <= s_term(2);
  fmc_out.LA_p(9)  <= s_term(3);
  fmc_out.LA_n(9)  <= s_term(4);
  
  fmc_out.LA_p(30) <= s_dir_o(0);
  fmc_out.LA_n(24) <= s_dir_o(1);
  fmc_out.LA_n(15) <= s_dir_o(2);
  fmc_out.LA_p(11) <= s_dir_o(3);
  fmc_out.LA_p(5)  <= s_dir_o(4);
  
  fmc_out.LA_p(29) <= s_output(0).p;
  fmc_out.LA_n(29) <= s_output(0).n;
  fmc_out.LA_p(28) <= s_output(1).p;
  fmc_out.LA_n(28) <= s_output(1).n;
  fmc_out.LA_p(8)  <= s_output(2).p;
  fmc_out.LA_n(8)  <= s_output(2).n;
  fmc_out.LA_p(7)  <= s_output(3).p;
  fmc_out.LA_n(7)  <= s_output(3).n;
  fmc_out.LA_p(4)  <= s_output(4).p;
  fmc_out.LA_n(4)  <= s_output(4).n;
    
  s_input(0).p <= fmc_in.LA_p(33);
  s_input(0).n <= fmc_in.LA_n(33);
  s_input(1).p <= fmc_in.LA_p(20);
  s_input(1).n <= fmc_in.LA_n(20);
  s_input(2).p <= fmc_in.LA_p(16);
  s_input(2).n <= fmc_in.LA_n(16);
  s_input(3).p <= fmc_in.LA_p(3);
  s_input(3).n <= fmc_in.LA_n(3);
  s_input(4).p <= fmc_in.LA_p(0);
  s_input(4).n <= fmc_in.LA_n(0);
  
  cmp_IO : xwb_gpio_raw
  generic map(
    g_interface_mode                        => PIPELINED,
    g_address_granularity                   => BYTE,
    g_num_pins                              => g_num_io,
    g_with_builtin_tristates                => false,
    g_debug                                 => false
  )
  port map(
    clk_sys_i                               => clk_i,
    rst_n_i                                 => rst_n_i,

    -- Wishbone
    slave_i                                 => slave_i,
    slave_o                                 => slave_o,
    desc_o                                  => open,    -- Not implemented

    --gpio_b : inout std_logic_vector(g_num_pins-1 downto 0);

    gpio_out_o                              => r_output,
    gpio_in_i                               => r_input,
    gpio_oen_o                              => s_dir,
    gpio_term_o                             => s_term,
    
    raw_o => raw_o,
    raw_i => raw_i
    
    
    
  );


 GEN_REG:  for I in 0 to (g_num_io-1) generate
   cmp_obuf_dir : OBUF
   generic map (
     IOSTANDARD => "DEFAULT"
   )
   port map (
     O => s_dir_o(I),
     I => s_dir_tmp(I)
   );
  s_dir_tmp(I) <= not s_dir(I);
 
  cmp_ibuf : IBUFDS
  generic map(
    IOSTANDARD => "DEFAULT"
  )
  port map(
    I  => s_input(I).p,
    IB => s_input(I).n,
    O  => s_input_raw(I)
    
  );
  -- mozna zrobic xor
--   GEN_REG_NORM: if g_negate_in(I) = '0' generate
--   s_io_in(I) <= s_io_in_tmp(I);
--   end generate GEN_REG_NORM;

--   GEN_REG_NEG: if g_negate_in(I) = '0' generate
--   s_io_in(I) <= not s_io_in_tmp(I);
--   end generate GEN_REG_NEG;   
   
   
  cmp_obuf : OBUFDS
    generic map(
      IOSTANDARD => "DEFAULT"
    )
    port map(
      O  => s_output(I).p,
      OB => s_output(I).n,
      I  => s_output_raw(I)
    );
 
   end generate;
   
   s_output_raw <= r_output xor g_negate_out(g_num_io -1 downto 0);
   r_input <= s_input_raw xor g_negate_in(g_num_io -1 downto 0);
   
   
   
  -- LVDS input to internal single
  
end Behavioral;

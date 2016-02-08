library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.genram_pkg.all;

package fmc_general_pkg is

  type t_diff_port is record
    p : std_logic;
    n : std_logic;
  end record t_diff_port;
  
  type t_diff_port_array is array(natural range <>) of t_diff_port; 


  type t_jtag_port is record
    TCK    : std_logic;
    TDI    : std_logic;
    TDO    : std_logic;
    TMS    : std_logic;
    TRST_L : std_logic;
  end record t_jtag_port;

  type t_fmc_signals is record
    -- misc    
    ga0         : std_logic;
    ga1         : std_logic;
    scl         : std_logic;
    sda         : std_logic;      
    pg_c2m      : std_logic;
    pg_m2c      : std_logic;
    prsnt_m2c_l : std_logic;
  
  
    -- LPC ports
    LA : t_diff_port_array(33 downto 0);
    
    -- HPC ports
    HA : t_diff_port_array(33 downto 0);
    HB : t_diff_port_array(21 downto 0);
  
    -- FMC CLocks
    CLK_DIR : std_logic;
    CLK_M2C  : t_diff_port_array(1 downto 0);
    CLK_BIDIR: t_diff_port_array(1 downto 0);
    

    -- MGT
    CLK_MGT: t_diff_port_array(1 downto 0);
    DP : t_diff_port_array(9 downto 0);

    -- todo: jtaq    
    jtag : t_jtag_port;
  end record t_fmc_signals;

  type t_fmc_signals_in is record
    -- misc    
    ga0         : std_logic;
    ga1         : std_logic;
    scl         : std_logic;
    sda         : std_logic;      
    pg_c2m      : std_logic;
    pg_m2c      : std_logic;
    prsnt_m2c_l : std_logic;
  
  
    -- LPC ports
    LA_p : std_logic_vector(33 downto 0);
    LA_n : std_logic_vector(33 downto 0);
    
    -- HPC ports
    HA_p : std_logic_vector(33 downto 0);
    HA_n : std_logic_vector(33 downto 0);
    HB_p : std_logic_vector(21 downto 0);
    HB_n : std_logic_vector(21 downto 0);
  
    -- FMC CLocks
    CLK_DIR : std_logic;
    CLK_M2C_p  : std_logic_vector(1 downto 0);
    CLK_M2C_n  : std_logic_vector(1 downto 0);
    CLK_BIDIR_p: std_logic_vector(1 downto 0);
    CLK_BIDIR_n: std_logic_vector(1 downto 0);
        

    -- MGT
    CLK_MGT_p: std_logic_vector(1 downto 0);
    CLK_MGT_n: std_logic_vector(1 downto 0);
    DP_p : std_logic_vector(9 downto 0);
    DP_n : std_logic_vector(9 downto 0);

    -- todo: jtaq    
    jtag : t_jtag_port;
  end record t_fmc_signals_in;
  
  subtype t_fmc_signals_out is t_fmc_signals_in;
  
  
 
  
end package fmc_general_pkg;

--
--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 
--
--   To use any of the example code shown below, uncomment the lines and modify as necessary
--

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.wishbone_pkg.all;
use work.fmc_general_pkg.all;


package fmc_boards_pkg is

-- type <new_type> is
--  record
--    <type_name>        : std_logic_vector( 7 downto 0);
--    <type_name>        : std_logic;
-- end record;
--
-- Declare constants
--
-- constant <constant_name>		: time := <time_unit> ns;
-- constant <constant_name>		: integer := <value;
--
-- Declare functions and procedure
--
-- function <function_name>  (signal <signal_name> : in <type_declaration>) return <type_declaration>;
-- procedure <procedure_name> (<type_declaration> <constant_name>	: in <type_declaration>);
--
  component fmc_dio5chttl 
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
                      end component; 


end fmc_boards_pkg;

package body fmc_boards_pkg is

---- Example 1
--  function <function_name>  (signal <signal_name> : in <type_declaration>  ) return <type_declaration> is
--    variable <variable_name>     : <type_declaration>;
--  begin
--    <variable_name> := <signal_name> xor <signal_name>;
--    return <variable_name>; 
--  end <function_name>;

---- Example 2
--  function <function_name>  (signal <signal_name> : in <type_declaration>;
--                         signal <signal_name>   : in <type_declaration>  ) return <type_declaration> is
--  begin
--    if (<signal_name> = '1') then
--      return <signal_name>;
--    else
--      return 'Z';
--    end if;
--  end <function_name>;

---- Procedure Example
--  procedure <procedure_name>  (<type_declaration> <constant_name>  : in <type_declaration>) is
--    
--  begin
--    
--  end <procedure_name>;
 
end fmc_boards_pkg;

--=================================================
-- LOBI General package
-- contains common functions used in many design
-- i.e. log2_ceil - function for computing
-- f_int2bool -



-- Standard libraries
library IEEE;
use IEEE.STD_LOGIC_1164.all; -- std_logic definitions
use IEEE.NUMERIC_STD.all;    -- conversion functions

--=================================================================================================
--                              Package declaration for lobi_general_pkg
--=================================================================================================
package lobi_general_pkg is
---------------------------------------------------------------------------------------------------
--                                    Functionc Declarations                                     --
---------------------------------------------------------------------------------------------------

  function log2_ceil  (N : natural) return positive;
  function f_int2bool (x : integer) return boolean;

end lobi_general_pkg;


package body lobi_general_pkg is

  -----------------------------------------------------------------------------
  -- Returns true if
  function f_int2bool (x : integer) return boolean is
  begin
    if (x = 0) then
      return false;
    else
      return true;
    end if;
  end f_int2bool;

  -----------------------------------------------------------------------------
  -- Returns log of 2 of a natural number
  function log2_ceil(N : natural) return positive is
  begin
    if N <= 2 then
      return 1;
    elsif N mod 2 = 0 then
      return 1 + log2_ceil(N/2);
    else
      return 1 + log2_ceil((N+1)/2);
    end if;
  end;

end lobi_general_pkg;
--=================================================================================================
--                                         package end
--=================================================================================================
---------------------------------------------------------------------------------------------------
--                                      E N D   O F   F I L E
---------------------------------------------------------------------------------------------------

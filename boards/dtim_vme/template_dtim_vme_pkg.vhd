-- Standard libraries
library IEEE;
use IEEE.STD_LOGIC_1164.all; -- std_logic definitions
use IEEE.NUMERIC_STD.all;    -- conversion functions
-- Specific libraries
use work.vme64x_pack.all;
use work.VME_CR_pack.all;
use work.VME_CSR_pack.all;


--=================================================================================================
--                              Package declaration for svec_pts_pkg
--=================================================================================================
package vme_timer_const_pkg is

---------------------------------------------------------------------------------------------------
--                                Constants regarding addressing                                 --
---------------------------------------------------------------------------------------------------
  constant c_BAR0_APERTURE         : integer := 18;
  constant c_CSR_WB_SLAVES_NB      : integer := 4;
  --------------------------------------------------
  constant c_LEMO_TRANCEIV_WB_ADDR : integer := 0;
  constant c_GSI_EVENT_LOGGER_WB_ADDR : integer := 0;
  constant c_GSI_EVENT_SIM_WB_ADDR : integer := 1;

  constant c_RST_ACTIVE            : std_logic := '0';  -- Active low reset

end vme_timer_const_pkg;

---------------------------------------------------------------------------------------------------
--                                      E N D   O F   F I L E
---------------------------------------------------------------------------------------------------

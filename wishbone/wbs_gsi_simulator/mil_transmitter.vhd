----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    11:33:40 03/15/2011 
-- Design Name: 
-- Module Name:    MIL_TRANSMITTER - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity MIL_TRANSMITTER is
	GENERIC(
			CLK_in_Hz		: INTEGER := 40000000
			);
    Port ( clk : in  STD_LOGIC;
	        pulse_clk: in std_logic;
           data : in  STD_LOGIC_VECTOR (15 downto 0);
           stb : in  STD_LOGIC;
           serdata : out  STD_LOGIC;
           gate : out  STD_LOGIC;
			  milidle : out  std_logic;
			  reset: in STD_LOGIC);
end MIL_TRANSMITTER;

architecture Behavioral of MIL_TRANSMITTER is
TYPE t_state is (idle,h1,h2,wordp,wordn,parp,parm,fin);


-- @todo fix, to po to by przesunac o jeden takt zegara
signal pulse_clk2: std_logic;

begin


process (clk)
begin
 if rising_edge(clk) then
   if reset = '1' then
	  pulse_clk2 <= '0';
	else
     pulse_clk2 <= pulse_clk;
	end if;
 end if;
end process;

process (clk,reset)
variable state : t_state := idle;
variable cnt   : integer;
variable reg   : std_logic_vector(15 downto 0);
variable par   : std_logic;
begin
	if reset = '0' then
--		if state=idle then
--			if stb='1' then
--				state:=h1;
--				cnt  :=0;
--		--		reg  :=data;
--				par  :='0';
--			end if;	
--		else
		if rising_edge(clk) then
		  if pulse_clk2 = '1' then
			case state is
			when idle =>
				if stb = '1' then
					state := h1;
					cnt := 0;
					--reg := data;
					par := '0';
				end if;
			when h1 => -- gate<='1';
				  serdata<='0';
				  cnt:=cnt+1;
				  reg := data;
				  if cnt=3 then cnt:=0; state:=h2; end if;
			when h2 => 
				  serdata<='1'; gate<='1';
				  cnt:=cnt+1;

				  if cnt=3 then cnt:=0; state:=wordp; end if;	
			when wordp =>
				  serdata<= not reg(15);
				  state:=wordn;
				  if reg(15)='1' then par:= not par; end if;
			when wordn =>
				  serdata<= reg(15);
				  reg(15 downto 1):=reg(14 downto 0);
				  cnt:=cnt+1;
				  if cnt=16 then state:=parp; else state:=wordp; end if;
			when parp  =>			  
				   serdata<= par;
					state:=parm;
			when parm  =>			  
				   serdata<= not par;
					state:=fin;
			when fin   =>
					gate<='0';
					state:=idle;
					serdata <= '0';
			when others =>
					state:=idle;				
					--milidle <= '1';
					gate<='0';
					serdata <= '0';
			end case;	
        end if;			
		end if;   
	
		if state=idle then
			milidle<='1';
		else
			milidle<='0'; 
		end if;
		
	else
		state := idle;
		serdata <= '0';
		cnt := 0;
		par :='0';
		gate <='0';
		milidle <= '0';
	end if;

end process;




end Behavioral;


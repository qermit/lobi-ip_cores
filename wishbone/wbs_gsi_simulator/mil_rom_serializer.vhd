---- Old, not used
library ieee;				-- component #1
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
library unisim;
use unisim.vcomponents.all;
--library XILINXCORELIB;
--use XILINXCORELIB.all;

------------------
entity mil_rom_serializer is
  	GENERIC(
			CLK_in_Hz		: INTEGER := 40000000	
			);
    Port ( data_out: out STD_LOGIC;
			  debug_state: out std_logic_vector(2 downto 0);
	        clk_in : in  STD_LOGIC;
			  reset: in STD_LOGIC;
			  
			  debug_data: out std_logic_vector(15 downto 0);
			  debug_addr: out std_logic_vector(4 downto 0);
			  debug_gate: out std_logic;
			  debug_milidle: out std_logic;
			  debug_data_load: out std_logic
			  
			  );
end mil_rom_serializer;

architecture mrs_Behavioral of mil_rom_serializer is
TYPE t_state is (unknown, idle,sending,delay);



COMPONENT blkrom32x16 IS
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(47 DOWNTO 0)
  );
END component;

    COMPONENT MIL_TRANSMITTER
	 	GENERIC(
			CLK_in_Hz		: INTEGER := 40000000		
			);
    PORT(
         clk : IN  std_logic;
         data : IN  std_logic_vector(15 downto 0);
         stb : IN  std_logic;
         serdata : OUT  std_logic;
         gate : OUT  std_logic;
         milidle : OUT  std_logic;
			reset : IN STD_LOGIC
        );
    END COMPONENT;


signal addr: std_logic_vector(4 downto 0);
--signal x: std_logic_vector(15 downto 0);
signal data: std_logic_vector(15 downto 0);
signal event_duration: std_logic_vector(31 downto 0);

signal data_load: std_logic := '0';


signal debug_1: std_logic_vector(2 downto 0);
signal debug_counter: std_logic_vector(23 downto 0);

signal gate: std_logic;
signal milidle: std_logic;


--signal clkp : STD_LOGIC;
begin
	
	
	--clkp <= clk_in;
U0: blkrom32x16 port map (
	addra => addr, 
	clka => clk_in, 
	douta(47 downto 32) => data(15 downto 0),
	douta(31 downto 0) => event_duration(31 downto 0)
);

U2: MIL_TRANSMITTER 
generic map (
CLK_in_Hz => CLK_in_Hz
)
port map (
		clk => clk_in,
		data => data,
		stb => data_load,
		serdata => data_out,
		gate => gate,
		milidle => milidle,
		reset => reset
);
--data_out <= x(15);

process (clk_in, reset)
 variable state : t_state := unknown;
 --variable counter: integer;
 variable counter: std_logic_vector(23 downto 0);
begin
  if (reset = '0') then
    if(clk_in'event and clk_in = '1') then
	   case state is
	   when idle => 
	     data_load <= '1';
		  if gate = '1' then
		    --counter := 
			 counter := event_duration(22 downto 0) & '0';
		    state := sending;
		  end if;
	   when sending =>
		  data_load <= '0';
		  
		  
	     if gate = '0' then
		  	 if addr = x"0F" then
			   addr <= "00000";
			 else
			   addr <= addr + 1;
			 end if;
		    state := delay;			
		  end if;
	   when delay =>
		  if event_duration = x"00000000" then
		    addr <= "00000";
		  end if;
		  
		  if (counter = x"000000") then
			state := idle;
		  else
		  
		    counter := counter - 1;
		  end if;
	   when others =>
			--addr <= "00000";
			state:=idle;		
         addr <= "00000";			
		--	data_out <= '0';
	   end case;		
    end if;
  else
     addr <= "00000";
     state:= idle;
	 --data_out <= '0';
  end if;
  --debug_1 <= counter; 
  if (state = sending) then
	debug_1 <= "001";
	elsif (state = delay) then
	debug_1 <= "010";
	elsif state = idle then
	debug_1 <= "100";
	else
	debug_1 <= "000";
	end if;
	debug_counter <= counter;
	
end process;

debug_state <= debug_1;
debug_data <= data;
debug_addr <= addr;
debug_gate <= gate;
debug_milidle <= milidle;
debug_data_load <= data_load;


end mrs_Behavioral;

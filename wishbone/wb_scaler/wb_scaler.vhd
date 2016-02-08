------------------------------------------------------------------------------
-- Title      : Wishbone GPIO port
-- Project    : General Core Collection (gencores) Library
------------------------------------------------------------------------------
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-05-18
-- Last update: 2011-10-05
-- Platform   : FPGA-generic
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: Bidirectional GPIO port of configurable width (1 to 256 bits).
-------------------------------------------------------------------------------
-- Copyright (c) 2010, 2011 CERN
--
-- 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-05-18  1.0      twlostow        Created
-- 2010-10-04  1.1      twlostow        Added WB slave adapter
-------------------------------------------------------------------------------

library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

use work.genram_pkg.all;

use work.wishbone_pkg.all;
use work.gencores_pkg.all;
use work.wishbone_gsi_lobi_pkg.all;

entity wb_scaler is
  generic(
    g_interface_mode         : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity    : t_wishbone_address_granularity := WORD;
    g_debug                  : boolean                        := false
    );
  port(
-- System reset, active low
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    wb_sel_i   : in  std_logic_vector(c_wishbone_data_width/8-1 downto 0);
    wb_cyc_i   : in  std_logic;
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_adr_i   : in  std_logic_vector(7 downto 0);
    wb_dat_i   : in  std_logic_vector(c_wishbone_data_width-1 downto 0);
    wb_dat_o   : out std_logic_vector(c_wishbone_data_width-1 downto 0);
    wb_ack_o   : out std_logic;
    wb_stall_o : out std_logic;
    raw_i : in std_logic_vector(7 downto 0)



    );
end wb_scaler;


architecture behavioral of wb_scaler is

attribute keep : string;



  constant c_SCALER_REG_SC        : std_logic_vector(2 downto 0) := "000";  -- *reg* status/control register
  constant c_SCALER_REG_SETUP     : std_logic_vector(2 downto 0) := "001";  -- *reg* scaler input matrix
  constant c_SCALER_REG_LATCH     : std_logic_vector(2 downto 0) := "010";  -- *reg* data direction register
  constant c_SCALER_REG_STARTSTOP : std_logic_vector(2 downto 0) := "011";  -- *reg* pin state register

  constant c_SCALER_CURRENT_COUNTER : std_logic_vector(2 downto 0) := "100";  -- *reg* pin state register
  constant c_SCALER_LATCH_COUNTER   : std_logic_vector(2 downto 0) := "101";  -- *reg* pin state register
  constant c_SCALER_FIFO_COUNTER    : std_logic_vector(2 downto 0) := "110";  -- *reg* pin state register
  constant c_SCALER_FRAME_COUNTER   : std_logic_vector(2 downto 0) := "111";  -- *reg* pin state register
  
   
  constant c_num_scaler: natural := 4;
  constant c_num_inputs: natural := 8;
  constant c_counter_width: natural := 32;
 
  type map_array_type is array (0 to c_num_scaler-1) of std_logic_vector(7 downto 0); 
  type evt_array_type is array (0 to c_num_scaler-1) of std_logic_vector(7 downto 0); 
 
  signal s_reset_counters: std_logic;
  signal s_reset_counters_n : std_logic;
  signal s_external_reset: std_logic;
 
  component scaler_counter
  port(
-- System reset, active low
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;
	 
	 scaler_reset_int: in std_logic;
	 latch_int: in std_logic;
	 counter_inc : in std_logic;
 	 scaler_enable_i: in std_logic;

	 counter_val_out: out std_logic_vector(31 downto 0);
	 counter_ovf: out std_logic
	 );
end component;


  component generic_async_fifo
    generic (
      g_data_width             : natural;
      g_size                   : natural;
      g_show_ahead             : boolean;
      g_with_rd_empty          : boolean;
      g_with_rd_full           : boolean;
      g_with_rd_almost_empty   : boolean;
      g_with_rd_almost_full    : boolean;
      g_with_rd_count          : boolean;
      g_with_wr_empty          : boolean;
      g_with_wr_full           : boolean;
      g_with_wr_almost_empty   : boolean;
      g_with_wr_almost_full    : boolean;
      g_with_wr_count          : boolean;
      g_almost_empty_threshold : integer;
      g_almost_full_threshold  : integer);
    port (
      rst_n_i           : in  std_logic := '1';
      clk_wr_i          : in  std_logic;
      d_i               : in  std_logic_vector(g_data_width-1 downto 0);
      we_i              : in  std_logic;
      wr_empty_o        : out std_logic;
      wr_full_o         : out std_logic;
      wr_almost_empty_o : out std_logic;
      wr_almost_full_o  : out std_logic;
      wr_count_o        : out std_logic_vector(f_log2_size(g_size)-1 downto 0);
      clk_rd_i          : in  std_logic;
      q_o               : out std_logic_vector(g_data_width-1 downto 0);
      rd_i              : in  std_logic;
      rd_empty_o        : out std_logic;
      rd_full_o         : out std_logic;
      rd_almost_empty_o : out std_logic;
      rd_almost_full_o  : out std_logic;
      rd_count_o        : out std_logic_vector(f_log2_size(g_size)-1 downto 0));
  end component;
  
  signal counter_val_in        : std_logic_vector(c_counter_width*c_num_scaler - 1 downto 0);
  signal counter_val_current   : std_logic_vector(c_counter_width*c_num_scaler - 1 downto 0);
  signal counter_latch_current : std_logic_vector(c_counter_width - 1 downto 0);
  signal counter_val_saved     : std_logic_vector(c_counter_width*c_num_scaler - 1 downto 0);
  
  signal s_counter_val_fifo      : std_logic_vector(c_counter_width*c_num_scaler - 1 downto 0);
  signal counter_val_fifo      : std_logic_vector(c_counter_width*c_num_scaler - 1 downto 0);
  signal counter_frameid_fifo    : std_logic_vector(c_counter_width - 1 downto 0);
  signal s_counter_frameid_fifo    : std_logic_vector(c_counter_width - 1 downto 0);
    
  signal counter_map  : map_array_type;
  signal counter_evt : evt_array_type;
  
  signal counter_inc : std_logic_vector(c_num_scaler-1 downto 0);
  signal counter_ovf : std_logic_vector(c_num_scaler-1 downto 0);


--attribute keep of counter0_map : signal is "true";
--attribute keep of counter1_map : signal is "true";
--attribute keep of counter2_map : signal is "true";
--attribute keep of counter3_map : signal is "true";
--
--attribute keep of counter0_evt : signal is "true";
--attribute keep of counter1_evt : signal is "true";
--attribute keep of counter2_evt : signal is "true";
--attribute keep of counter3_evt : signal is "true";  
--
--attribute keep of counter_inc : signal is "true";
--attribute keep of counter_ovf : signal is "true";

  
  signal latch_map      : std_logic_vector(7 downto 0);
  signal latch_val      : std_logic_vector(7 downto 0);
  signal latch_int      : std_logic;
  signal s_latch : std_logic;

--attribute keep of latch_map : signal is "true";
--attribute keep of latch_val : signal is "true";
--attribute keep of latch_int : signal is "true";
  
  
  signal start_stop_map : std_logic_vector(7 downto 0);
  signal start_stop_val : std_logic_vector(7 downto 0);
  
  signal wb_in  : t_wishbone_slave_in;
  signal wb_out : t_wishbone_slave_out;

  signal sel          : std_logic;
  signal resized_addr : std_logic_vector(c_wishbone_address_width-1 downto 0);
  
  signal scaler_fifo_empty     : std_logic;
  signal scaler_fifo_full      : std_logic;
  signal scaler_fifo_threshold : std_logic;
  signal scaler_running : std_logic;
  
  signal scaler_reset_int : std_logic;
  signal scaler_start_int : std_logic;
  signal scaler_stop_int : std_logic;
  
  
  signal scaler_load_current_int: std_logic;
  signal scaler_shift_current_int: std_logic;
  signal scaler_load_fifo_int: std_logic;
  signal scaler_shift_fifo_int: std_logic;
  
  signal s_scaler_load_fifo : std_logic;
  
  signal reg_scaler_start_stop: std_logic;
  
signal ack_sreg                                 : std_logic_vector(9 downto 0);
signal ack_in_progress : std_logic;
 

 
 
 signal sync_i:std_logic_vector(c_num_inputs - 1 downto 0);
 signal sync_i_rise:std_logic_vector(c_num_inputs - 1 downto 0);
 signal sync_i_fall:std_logic_vector(c_num_inputs - 1 downto 0);
 signal sync_i_edge:std_logic_vector(c_num_inputs - 1 downto 0);
  
  
  component chipscope_icon
  PORT (
    CONTROL0 : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0));
  end component;

component chipscope_ila
  PORT (
    CONTROL : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0);
    CLK : IN STD_LOGIC;
    TRIG0 : IN STD_LOGIC_VECTOR(63 DOWNTO 0));

end component;



  signal icon_control0: std_logic_vector(35 downto 0);
  signal TRIG0 : STD_LOGIC_VECTOR(63 DOWNTO 0);
  
begin

  resized_addr(7 downto 0) <= wb_adr_i;
  resized_addr(c_wishbone_address_width-1 downto 8) <= (others => '0');

  U_Adapter : wb_slave_adapter
    generic map (
      g_master_use_struct  => true,
      g_master_mode        => PIPELINED,
      g_master_granularity => WORD,
      g_slave_use_struct   => false,
      g_slave_mode         => g_interface_mode,
      g_slave_granularity  => g_address_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      master_i   => wb_out,
      master_o   => wb_in,
      sl_adr_i   => resized_addr,
      sl_dat_i   => wb_dat_i,
      sl_sel_i   => wb_sel_i,
      sl_cyc_i   => wb_cyc_i,
      sl_stb_i   => wb_stb_i,
      sl_we_i    => wb_we_i,
      sl_dat_o   => wb_dat_o,
      sl_ack_o   => wb_ack_o,
      sl_stall_o => wb_stall_o);

  --sel <= '1' when (unsigned(not wb_in.sel) = 0) else '0';

  wb_out.ack   <= ack_sreg(0);
  wb_out.stall <= '0';
  wb_out.err <= '0';
  wb_out.int <= '0';
  wb_out.rty <='0';
  
 -- wb_out.dat <= rddata_reg;
 -- wrdata_reg <= wb_in.dat;
  
  -- Main register bank access process.
  process (clk_sys_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
	 
      ack_sreg <= "0000000000";
      ack_in_progress <= '0';
		
      wb_out.dat <= "00000000000000000000000000000000";

	   scaler_reset_int <= '0';
		scaler_start_int <= '0';
		scaler_stop_int <= '0';
		scaler_load_current_int <= '0';
		scaler_shift_current_int <= '0';
		scaler_load_fifo_int <= '0';
      scaler_shift_fifo_int <= '0';


      counter_map(0)   <= "00000001";
      counter_map(1)   <= "00000010";
      counter_map(2)   <= "00000100";
      counter_map(3)   <= "00001000";
      latch_map      <= "00010000";
		start_stop_map <= "10000000";
		
    elsif rising_edge(clk_sys_i) then
-- advance the ACK generator shift register
      ack_sreg(8 downto 0) <= ack_sreg(9 downto 1);
      ack_sreg(9) <= '0';
      if (ack_in_progress = '1') then
        if (ack_sreg(0) = '1') then
		    ack_in_progress <= '0';
        end if;
        scaler_reset_int <= '0';
        scaler_start_int <= '0';
        scaler_stop_int <= '0';
		  scaler_load_current_int <= '0';
		  scaler_shift_current_int <= '0';
		  scaler_load_fifo_int <= '0';
        scaler_shift_fifo_int <= '0';
      else
        if ((wb_in.cyc = '1') and (wb_in.stb = '1')) then
            case wb_in.adr(2 downto 0) is
            when c_SCALER_REG_SC => 
              if (wb_in.we = '1') then
                scaler_reset_int <= wb_in.dat(0);
					 scaler_start_int <= wb_in.dat(3);
					 scaler_stop_int <= wb_in.dat(4);
					 scaler_load_current_int <= wb_in.dat(5);
					 scaler_load_fifo_int <= wb_in.dat(6);
              end if;
				  
              wb_out.dat(0) <= scaler_fifo_empty;
              wb_out.dat(1) <= scaler_fifo_full;
              wb_out.dat(2) <= scaler_fifo_threshold;
				  wb_out.dat(3) <= scaler_running;
				  
				  wb_out.dat(7 downto 4) <= "XXXX";
				  
				  wb_out.dat(11 downto 8)  <= counter_ovf;
				  
              wb_out.dat(31 downto 12) <= (others => 'X');
				  
              ack_sreg(0) <= '1';
              ack_in_progress <= '1';
            when c_SCALER_REG_SETUP => 
              if (wb_in.we = '1') then
				    counter_map(0) <= wb_in.dat(7  downto  0);
				    counter_map(1) <= wb_in.dat(15 downto  8);
				    counter_map(2) <= wb_in.dat(23 downto 16);
				    counter_map(3) <= wb_in.dat(31 downto 24);
              end if;
				  
              wb_out.dat(7  downto  0) <= counter_map(0);
				  wb_out.dat(15 downto  8) <= counter_map(1);
				  wb_out.dat(23 downto 16) <= counter_map(2);
				  wb_out.dat(31 downto 24) <= counter_map(3);
				  
              ack_sreg(0) <= '1';
              ack_in_progress <= '1';
            when c_SCALER_REG_LATCH => 
              if (wb_in.we = '1') then
                latch_map <= wb_in.dat(7  downto  0);
              end if;
              wb_out.dat(7 downto 0) <= latch_map;
				  wb_out.dat(31 downto 8) <= ( others => 'X');
            
              ack_sreg(0) <= '1';
              ack_in_progress <= '1';
            when c_SCALER_REG_STARTSTOP => 
              if (wb_in.we = '1') then
                start_stop_map <= wb_in.dat(7  downto  0);
              end if;
              wb_out.dat(7 downto 0) <= start_stop_map;
				  wb_out.dat(31 downto 8) <= ( others => 'X');
            
              ack_sreg(0) <= '1';
              ack_in_progress <= '1';
				  
            when c_SCALER_CURRENT_COUNTER => 
              if (wb_in.we = '1') then
              end if;
              wb_out.dat(31 downto 0) <= counter_val_current(c_counter_width -1 downto 0);
				  scaler_shift_current_int <= '1';
              ack_sreg(0) <= '1';
              ack_in_progress <= '1';
				when c_SCALER_LATCH_COUNTER =>
				  wb_out.dat(31 downto 0) <= counter_latch_current;
				  ack_sreg(0) <= '1';
              ack_in_progress <= '1';
            when c_SCALER_FIFO_COUNTER => 
              if (wb_in.we = '1') then
              end if;
              wb_out.dat(31 downto 0) <= counter_val_fifo(c_counter_width -1 downto 0);
				  scaler_shift_fifo_int <= '1';
              ack_sreg(0) <= '1';
              ack_in_progress <= '1';
				when c_SCALER_FRAME_COUNTER =>
				  wb_out.dat(31 downto 0) <= counter_frameid_fifo;
				  ack_sreg(0) <= '1';
              ack_in_progress <= '1';
            when others =>
-- prevent the slave from hanging the bus on invalid address
              ack_in_progress <= '1';
              ack_sreg(0) <= '1';
            end case;
        end if;
      end if;
    end if;
  end process;
  
  u_edge_detector: edge_detector
	generic map ( g_num_pins => c_num_inputs)
	port map (
	   clk_sys_i => clk_sys_i,
		rst_n_i => rst_n_i,
		async_i => raw_i,
	   sync_o => sync_i,
      sync_fall_o => sync_i_fall,
      sync_rise_o => sync_i_rise,
	   sync_edge_o => sync_i_edge
	);
  
GEN_SCALER: 
  for I in 0 to  c_num_scaler-1 generate
  
  u_scaler_counter_X: scaler_counter
  port map(
-- System reset, active low
    clk_sys_i => clk_sys_i,
    rst_n_i   => rst_n_i,
	 
	 scaler_reset_int => scaler_reset_int,
	 latch_int => latch_int,
	 counter_inc => counter_inc(I),
	 scaler_enable_i => scaler_running,
	 
	 counter_val_out => counter_val_in(((I+1)*c_counter_width)-1 downto I*c_counter_width),
	 counter_ovf => counter_ovf(I)
	 );
	 
	 counter_evt(I) <= (sync_i_rise(7 downto 0)) AND counter_map(I);
    counter_inc(I) <= '0' when counter_evt(I) = "00000000" else
             '1';
	 
  end generate GEN_SCALER;



  u_latch_counter: scaler_counter
  port map(
-- System reset, active low
    clk_sys_i => clk_sys_i,
    rst_n_i   => rst_n_i,
	 
	 scaler_reset_int => scaler_reset_int,
	 latch_int => '0',
	 counter_inc => latch_int,
	 scaler_enable_i => scaler_running,
	 
	 counter_val_out => counter_latch_current,
	 counter_ovf => open
	 );

latch_val <= (sync_i_rise(7 downto 0)) AND latch_map;
latch_int <= '0' when latch_val = "00000000" else
             '1';
				 


start_stop_val <= (reg_scaler_start_stop & sync_i(6 downto 0)) AND start_stop_map;
scaler_running <= '0' when start_stop_val = "00000000" else
                  '1';

s_latch_proc: process(latch_int, scaler_running)
begin
	s_latch <= latch_int and scaler_running;						
end process;
						
s_scaler_load_fifo <= scaler_load_fifo_int and not scaler_fifo_empty;
						
  wrapped_fifo : generic_async_fifo
    generic map (
      g_data_width             => c_counter_width*c_num_scaler,
      g_size                   => 256,
      g_show_ahead             => true,
		
      g_with_rd_empty          => true,
      g_with_rd_full           => true,
      g_with_rd_almost_empty   => false,
      g_with_rd_almost_full    => true,
      g_with_rd_count          => false,
		
      g_with_wr_empty          => true,
      g_with_wr_full           => true,
      g_with_wr_almost_empty   => false,
      g_with_wr_almost_full    => false,
      g_with_wr_count          => true,
		
      g_almost_empty_threshold => 0,
      g_almost_full_threshold  => 128)
    port map (
      rst_n_i           => s_reset_counters_n,
		
      clk_wr_i          => clk_sys_i,
      d_i               => counter_val_in,
      we_i              => s_latch,
		
      wr_empty_o        => open,
      wr_full_o         => open,
      wr_almost_empty_o => open,
      wr_almost_full_o  => open,
      wr_count_o        => open,
		
      clk_rd_i          => clk_sys_i,
      q_o               => s_counter_val_fifo,
      rd_i              => s_scaler_load_fifo,
		
      rd_empty_o        => scaler_fifo_empty,
      rd_full_o         => scaler_fifo_full,
      rd_almost_empty_o => open,
      rd_almost_full_o  => scaler_fifo_threshold,
      rd_count_o        => s_counter_frameid_fifo(7 downto 0));

external_reset_proc: process(clk_sys_i)
begin
  if rising_edge(clk_sys_i) then
    if rst_n_i = '0' then
	   s_external_reset <= '1';
	 else
	   s_external_reset <= '0';
	 end if;
  end if;
end process;

s_reset_counters <= s_external_reset or scaler_reset_int;
s_reset_counters_n <= not s_reset_counters;

start_stop_proc: process(clk_sys_i)
begin
	if rising_edge(clk_sys_i) then
	  if rst_n_i = '0' then
	    reg_scaler_start_stop <= '0';
     else
	      if scaler_reset_int = '1' or
			   scaler_stop_int = '1' then
			  
			  reg_scaler_start_stop <= '0';
			elsif scaler_start_int = '1' then
			  reg_scaler_start_stop <= '1';
			end if;
		end if;
	end if;
end process;

val_current_proc: process(clk_sys_i)
begin
  if rising_edge(clk_sys_i) then
    if s_reset_counters = '1' then
	    counter_val_current <= (others => '0');
	 else
	   if  scaler_shift_current_int = '1' then
		  counter_val_current <= counter_val_current(c_counter_width -1 downto 0) & counter_val_current(c_num_scaler * c_counter_width -1 downto c_counter_width);
		elsif scaler_load_current_int = '1' then
		  counter_val_current <= counter_val_in;
		end if;
	 end if;
	end if;
end process;

counter_latch_proc: process(clk_sys_i)
begin
  if rising_edge(clk_sys_i) then
    if s_reset_counters = '1' then
		 counter_val_saved <= (others => '0');
	 else
	   if latch_int = '1' and scaler_running = '1' then
		  counter_val_saved <= counter_val_in;
		end if;
	 end if;
  end if;
end process;

val_fifo_proc: process(clk_sys_i)
begin
  if rising_edge(clk_sys_i) then
    if s_reset_counters = '1' then
	    counter_val_fifo <= (others => '0');
		 counter_frameid_fifo <= (others => '1');		 
	 else
	   if  scaler_shift_fifo_int = '1' then
		  counter_val_fifo <= counter_val_fifo(c_counter_width -1 downto 0) & counter_val_fifo(c_num_scaler * c_counter_width -1 downto c_counter_width);
		elsif s_scaler_load_fifo = '1' then
		  counter_val_fifo <= s_counter_val_fifo;
		  counter_frameid_fifo <= std_logic_vector(unsigned(counter_frameid_fifo) + 1);  
		  --counter_frameid_fifo <= x"000000" & s_counter_frameid_fifo(7 downto 0);
		end if;
	 end if;
	end if;
end process;



GEN_DEBUG: if g_debug = true generate

u_chipscope_icon : chipscope_icon
  port map (
    CONTROL0 => icon_control0);


u_chipscope_ila : chipscope_ila
  port map (
    CONTROL => icon_control0,
    CLK => clk_sys_i,
    TRIG0 => TRIG0);

TRIG0(7 downto 0) <= wb_in.adr(7 downto 0);
TRIG0(15 downto 8) <= wb_in.dat(7 downto 0);
TRIG0(23 downto 16) <= wb_out.dat(7 downto 0);

TRIG0(24) <= wb_in.cyc;
TRIG0(25) <= wb_in.stb;
TRIG0(26) <= wb_in.we;
TRIG0(27) <= wb_out.ack;

TRIG0(28) <= ack_in_progress;
TRIG0(29) <= scaler_reset_int;
TRIG0(30) <= scaler_start_int;
TRIG0(31) <= scaler_stop_int;
TRIG0(32) <= rst_n_i;

TRIG0(40 downto 33) <= counter_val_in(7 downto 0);
TRIG0(41) <= counter_ovf(0);

TRIG0(42) <= scaler_running;
TRIG0(43) <= latch_int;
TRIG0(47 downto 44) <= counter_inc;
TRIG0(55 downto 48) <= sync_i_rise;



TRIG0(59 downto 56) <= (others => '0');

TRIG0(60) <= s_external_reset;
TRIG0(61) <= scaler_reset_int;
TRIG0(62) <= s_reset_counters;
TRIG0(63) <= s_reset_counters_n;

end generate GEN_DEBUG;

	 
end behavioral;




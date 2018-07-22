LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_std.ALL;
ENTITY l2 IS
	PORT (
		clk : IN Std_logic;
		otp : OUT std_logic_Vector(15 DOWNTO 0);
		rx_line : IN std_logic;

		tx_line : OUT std_logic
	);
END l2;

ARCHITECTURE Behavioral OF l2 IS
	--component tram_wrapper is
	-- port (
	-- BRAM_PORTA_0_addr : in STD_LOGIC_VECTOR (9 downto 0 );
	-- BRAM_PORTA_0_clk : in STD_LOGIC;
	-- BRAM_PORTA_0_din : in STD_LOGIC_VECTOR (15 downto 0 );
	-- BRAM_PORTA_0_dout : out STD_LOGIC_VECTOR (15 downto 0 );
	-- BRAM_PORTA_0_we : in STD_LOGIC_VECTOR (0 to 0 )
	-- );
	--end component;
	COMPONENT UART_RX IS
		PORT (
			i_Clk : IN std_logic;
			i_RX_Serial : IN std_logic;
			o_RX_DV : OUT std_logic;
			o_RX_Byte : OUT std_logic_vector(7 DOWNTO 0)
		);
	END COMPONENT UART_RX;
component tram_wrapper is
      port (
        BRAM_PORTA_0_addr : in STD_LOGIC_VECTOR ( 9 downto 0 );
        BRAM_PORTA_0_clk : in STD_LOGIC;
        BRAM_PORTA_0_din : in STD_LOGIC_VECTOR ( 15 downto 0 );
        BRAM_PORTA_0_dout : out STD_LOGIC_VECTOR ( 15 downto 0 );
        BRAM_PORTA_0_we : in STD_LOGIC_VECTOR ( 0 to 0 )
      );
    end component;
	COMPONENT UART_TX IS
		PORT (
			i_Clk : IN std_logic;
			i_TX_DV : IN std_logic;
			i_TX_Byte : IN std_logic_vector(7 DOWNTO 0);
			o_TX_Active : OUT std_logic;
			o_TX_Serial : OUT std_logic;
			o_TX_Done : OUT std_logic
		);
	END COMPONENT UART_TX;
	COMPONENT bram_wrapper IS
		PORT (
			BRAM_PORTA_0_addr : IN STD_LOGIC_VECTOR (6 DOWNTO 0);
			BRAM_PORTA_0_clk : IN STD_LOGIC;
			BRAM_PORTA_0_din : IN STD_LOGIC_VECTOR (47 DOWNTO 0);
			BRAM_PORTA_0_dout : OUT STD_LOGIC_VECTOR (47 DOWNTO 0);
			BRAM_PORTA_0_we : IN STD_LOGIC_VECTOR (0 TO 0)
		);
	END COMPONENT;
	COMPONENT lram_wrapper IS
		PORT (
			BRAM_PORTA_0_addr : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
			BRAM_PORTA_0_clk : IN STD_LOGIC;
			BRAM_PORTA_0_din : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
			BRAM_PORTA_0_dout : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
			BRAM_PORTA_0_we : IN STD_LOGIC_VECTOR (0 TO 0)
		);
	END COMPONENT;
	TYPE reg IS ARRAY(0 TO 127, 7 DOWNTO 0) OF INTEGER RANGE 0 TO 8;
	SIGNAL r : reg := (OTHERS => (OTHERS => 8));
	TYPE lreg IS ARRAY(0 TO 31, 3 DOWNTO 0) OF INTEGER RANGE 0 TO 4;
	SIGNAL lr : lreg := (OTHERS => (OTHERS => 4));
	TYPE bloc IS ARRAY(0 TO 1023) OF std_logic_vector(15 DOWNTO 0);
	--TYPE l2state IS (ini,se,wai);
	TYPE l1state IS (lw,lread,send,se,wai,linit, lset, lcheck, lvalid, init, set, check, valid, l1wait, l2wait, ini, second, init_wait, mode_wait, save, display, wait_display, output, halt);
	--SIGNAL state : l2state := ini;
	TYPE dis IS (hit_msb, hit_lsb, miss_msb, miss_lsb);
	SIGNAL dis_state : dis := hit_msb;
	SIGNAL lstate : l1state := ini;
	--SIGNAL trace : bloc;
	SIGNAL counter, count : INTEGER := 0;
 	TYPE lcache IS ARRAY(0 TO 128) OF std_logic_vector(47 DOWNTO 0);
        --TYPE l2state IS (ini,se,wai);
            Signal co:lcache:=(others=>(others=>'0'));
	SIGNAL address : std_logic_vector(15 DOWNTO 0);
	SIGNAL l2index : std_logic_vector(6 DOWNTO 0) := "0000000";
	SIGNAL l2tag : std_logic_vector(4 DOWNTO 0);
	SIGNAL we, twe : std_logic_vector(0 DOWNTO 0) := "0";
	SIGNAL din : std_logic_vector(47 DOWNTO 0) := (OTHERS => '0');
	SIGNAL dout : std_logic_vector(47 DOWNTO 0);
	SIGNAL h, m, c, h2, m2, th, tm : INTEGER := 0;
	--SIGNAL h2, m2 : std_logic_vector(15 DOWNTO 0) := "0000";
	SIGNAL lwe : std_logic_vector(0 DOWNTO 0) := "0";
	SIGNAL ldin : std_logic_vector(31 DOWNTO 0) := (OTHERS => '0');
	SIGNAL ldout : std_logic_vector(31 DOWNTO 0);
	SIGNAL l1index : std_logic_vector(4 DOWNTO 0) := (OTHERS => '0');
	SIGNAL l1tag : std_logic_vector(6 DOWNTO 0) := (OTHERS => '0');
	SIGNAL add : std_logic_vector(9 DOWNTO 0) := (OTHERS => '0');
	SIGNAL tracein : std_logic_vector(15 DOWNTO 0) := (OTHERS => '0');
	SIGNAL traceout : std_logic_vector(15 DOWNTO 0);
	SIGNAL trace : std_logic_vector(15 DOWNTO 0);
	SIGNAL rx_busy : std_logic := '0';
	SIGNAL instr, msb, lsb : std_logic_vector(7 DOWNTO 0) := "11111111";
	SIGNAL tx_busy : std_logic;
	SIGNAL tx_active : std_logic := '0';
	SIGNAL tx_done : std_logic;
	SIGNAL t_data : std_logic_vector(7 DOWNTO 0);
	SIGNAL divider : std_logic_vector(5 DOWNTO 0) := (OTHERS => '0');
	SIGNAL TotalHit, TotalMiss, var : std_logic_vector(15 DOWNTO 0) := (OTHERS => '0');
 
BEGIN
	-- trace(0) <= "0000000000000010";
	-- trace(1) <= "0000100000001010";
	-- trace(2) <= "0001000000001110";
	-- trace(3) <= "0001100000000011";
	-- trace(4) <= "0010000000000011";
	-- trace(5) <= "0010100000000010";
	-- trace(6) <= "0011000000000010";
	-- trace(7) <= "0011100000001110";
	-- trace(8) <= "0000000000000011";
	-- trace(9) <= "0000100000000110";
	-- trace(10) <= "1100100000000010";
	-- trace(11) <= "1101000000001111";
	-- trace(12) <= "0001000000001111";
	-- trace(13) <= "0001100000000011";
	-- trace(14) <= "0000010000000010";
	-- trace(15) <= "110010001000010";
	T1: tram_wrapper PORT MAP(add,clk,tracein,traceout,twe);
  
	RX1 : UART_RX PORT MAP(clk, rx_line, rx_busy, instr);
	TX1 : UART_TX PORT MAP(clk, tx_busy, t_data, tx_active, tx_line, tx_done);
	L1 : lram_wrapper PORT MAP(l1index, clk, ldin, ldout, lwe);
   
PROCESS (clk)
BEGIN
	IF (rising_edge(clk)) THEN
		divider <= std_logic_vector(unsigned(divider) + 1);
	END IF;
END PROCESS;
--PROCESS (divider(3))
--begin
-- if (rising_edge(divider(3)))then
--case state is
--when ini=>twe<="0";
-- IF (count < 16) THEN 
-- add<=std_logic_vector(to_unsigned(count, 10));
 
-- state <= se;
-- count <= count + 1;
-- ELSE
-- bool<='1';
-- twe<="0";
-- END IF;
--when se=>tracein <= trace(count-1);
-- twe<="1"; 
-- state<=wai;
--when wai=>state<=ini;
--end case;
--end if;
--end process;
PROCESS (divider(5))
VARIABLE p, last1, n1 : INTEGER;
VARIABLE p1 : std_logic_vector(31 DOWNTO 0);
VARIABLE t, last, n : INTEGER;
VARIABLE t1 : std_logic_vector(47 DOWNTO 0);
BEGIN
	IF (rising_edge(divider(5))) THEN
	-- this state to read the frst 8 bits lsb from fpga
		CASE lstate IS
			WHEN ini => 
				IF rx_busy = '1' THEN
					lsb <= instr;
					lstate <= init_wait;
 
				END IF;
 -- this to read msb
			WHEN init_wait => 
				IF rx_busy = '1' THEN
					lstate <= init_wait;
				ELSE
					lstate <= second;
				END IF;
 
 
			WHEN second => 
				IF rx_busy = '1' THEN
					msb <= instr;
					lstate <= mode_wait;
				END IF;
 
			WHEN mode_wait => 
				-- rx busy means we need to wait
				IF rx_busy = '1' THEN
					lstate <= mode_wait;
				ELSE
					lstate <= save;
				END IF;
 -- here we save it in a trace memory
			WHEN save => 
				--IF c < 1024 THEN
					trace <= msb & lsb;
					--c <= c + 1;
					lstate <= send;
		-- now store in tram		
			when send=>
			 IF (c < 1024) THEN                                         
                                         add<=std_logic_vector(to_unsigned(c, 10));
                                          
                                         lstate <= se;
                                         count <= count + 1;
                                       ELSE
                                       twe<="0";
                                          lstate<=linit;
                                       END IF;
                   when se=>tracein <= trace;
                                   twe<="1";   
                               lstate<=wai;
                   when wai=>
                   if(c=1024)then
                   lstate<=linit;
                   else
                   lstate<=ini;
                   end if;
				-- twe<="0";
				-- IF (count < 16) THEN 
				-- add<=std_logic_vector(to_unsigned(count, 10));
 
				-- lstate <= se;
				-- count <= count + 1;
				-- ELSE
				-- twe<="0";
				-- lstate<=linit;
				-- END IF;
				-- when se=>tracein <= trace(count-1);
				-- twe<="1"; 
				-- lstate<=wai;
				-- when wai=>lstate<=ini;
				-- first read from trace
			WHEN linit => 
				lwe <= "0";
				we <= "0";
				 twe<="0";
				 
				IF (counter < 16) THEN
 
					 add<=std_logic_vector(to_unsigned(counter,10));
					--address <= trace(counter);
					lstate <= lread;
					counter <= counter + 1;
				ELSE
					lstate <= display;
				END IF;
				 when lread=> address <= traceout;
				 lstate<=lset; 
				 -- check in lcache
			WHEN lset => 
				l1index <= address(8 DOWNTO 4);
				l1tag <= address(15 DOWNTO 9);
				lstate <= lcheck;
				-- hits and misses
			WHEN lcheck => 
				p := to_integer(unsigned(l1index));
				IF (ldout(7) = '0') THEN
					--co(0)<='1'; 
					m <= m + 1;
					ldin <= ldout(31 DOWNTO 8) & '1' & l1tag;
					lwe <= "1";
					lr(p, 3) <= 0;
					lstate <= l1wait;
 
				ELSIF (ldout(7) = '1' AND ldout(6 DOWNTO 0) = l1tag) THEN
					-- co(1)<='1';
					FOR i IN 0 TO 3 LOOP
						IF lr(p, i) = 0 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 3 LOOP
 
						IF (i > n) THEN
							lr(p, i - 1) <= lr(p, i);
						END IF;
					END LOOP;
					lr(p, 3) <= 0;
					h <= h + 1;
					lstate <= linit;
				ELSIF (ldout(15) = '0') THEN
					--co(2)<='1';
					m <= m + 1;
					ldin <= ldout(31 DOWNTO 16) & '1' & l1tag & ldout(7 DOWNTO 0);
					lwe <= "1";
 
					lr(p, 3) <= 1;
					lr(p, 2) <= 0;
					lstate <= l1wait;
				ELSIF (ldout(15) = '1' AND ldout(14 DOWNTO 8) = l1tag) THEN
					--co(3)<='1';
					FOR i IN 0 TO 3 LOOP
						IF lr(p, i) = 1 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 3 LOOP
 
						IF (i > n) THEN
							lr(p, i - 1) <= lr(p, i);
						END IF;
 
					END LOOP;
					lr(p, 3) <= 1;
					h <= h + 1;
 
					lstate <= linit;
				ELSIF (ldout(23) = '0') THEN
					--co(6)<='1';
					m <= m + 1;
					ldin <= ldout(31 DOWNTO 24) & '1' & l1tag & ldout(15 DOWNTO 0);
					lwe <= "1";
					lstate <= l1wait;
					FOR i IN 2 TO 3 LOOP
						lr(p, i - 1) <= lr(p, i);
					END LOOP;
					lr(p, 3) <= 2;
				ELSIF (ldout(23) = '1' AND ldout(22 DOWNTO 16) = l1tag) THEN
					FOR i IN 0 TO 3 LOOP
						IF lr(p, i) = 2 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 3 LOOP
						IF (i > n) THEN
							lr(p, i - 1) <= lr(p, i);
						END IF;
					END LOOP;
					lr(p, 3) <= 2;
					--co(4)<='1';
					h <= h + 1;
					lstate <= linit;
				ELSIF (ldout(31) = '0') THEN
 
					m <= m + 1;
					ldin <= '1' & l1tag & ldout(23 DOWNTO 0);
					lwe <= "1";
					FOR i IN 1 TO 3 LOOP
						lr(p, i - 1) <= lr(p, i);
					END LOOP;
					lr(p, 3) <= 3;
					--co(5)<='1';
					lstate <= l1wait;
				ELSIF (ldout(31) = '1' AND ldout(30 DOWNTO 24) = l1tag) THEN
					FOR i IN 0 TO 3 LOOP
						IF lr(p, i) = 3 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 3 LOOP
						IF (i > n) THEN
							lr(p, i - 1) <= lr(p, i);
						END IF;
					END LOOP;
					lr(p, 3) <= 3;
					h <= h + 1;
					lstate <= linit;
					--co(6)<='1'; 
					-- if all filled and misses
				ELSE
					--co(7)<='1';
					lstate <= lvalid;
					p1 := ldout;
					last1 := lr(p, 0);
				END IF;
			WHEN lvalid => 
				m <= m + 1;
 
				p1(8 * last1 + 6 DOWNTO 8 * last1) := l1tag;
				FOR i IN 1 TO 3 LOOP
					lr(p, i - 1) <= lr(p, i);
				END LOOP;
				lr(p, 3) <= last1;
				ldin <= p1;
				lwe <= "1";
				lstate <= l1wait;
				-- s <= std_logic_vector(to_unsigned(last1, 3));
			WHEN l1wait => 
				p1 := (OTHERS => '0');
				lstate <= init;
 
 
			WHEN init => 
				-- s2 <= std_logic_vector(to_unsigned(last, 3));
				we <= "0";
				lwe <= "0";
				lstate <= set;
			WHEN set => 
				l2index <= address(10 DOWNTO 4);
				l2tag <= address(15 DOWNTO 11);
			                                  lstate <= lw;
                
                when lw =>
                t := to_integer(unsigned(l2index));
                lstate<=lcheck;
                
                dout<=co(t);
			WHEN check => 
				--t := to_integer(unsigned(l2index));
				IF (dout(5) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= dout(47 DOWNTO 6) & '1' & l2tag;
					--we <= "1";
					r(t, 7) <= 0;
 

				ELSIF (dout(5) = '1' AND dout(4 DOWNTO 0) = l2tag) THEN
					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 0 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP
						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;
					END LOOP;
					r(t, 7) <= 0;
					h2 <= h2 + 1;
					lstate <= linit;
				ELSIF (dout(11) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= dout(47 DOWNTO 12) & '1' & l2tag & dout(5 DOWNTO 0);
					--we <= "1";

					r(t, 7) <= 1;
					r(t, 6) <= 0;
				ELSIF (dout(11) = '1' AND dout(10 DOWNTO 6) = l2tag) THEN

					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 1 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP

						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;

					END LOOP;
					r(t, 7) <= 1;
					h2 <= h2 + 1;
					lstate <= linit;
				ELSIF (dout(17) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= dout(47 DOWNTO 18) & '1' & l2tag & dout(11 DOWNTO 0);
					we <= "1";
					FOR i IN 6 TO 7 LOOP
						r(t, i - 1) <= r(t, i);
					END LOOP;
					r(t, 7) <= 2;
				ELSIF (dout(17) = '1' AND dout(16 DOWNTO 12) = l2tag) THEN
					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 2 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP
						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;
					END LOOP;
					r(t, 7) <= 2;
					h2 <= h2 + 1;
					lstate <= linit;
				ELSIF (dout(23) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= dout(47 DOWNTO 24) & '1' & l2tag & dout(17 DOWNTO 0);
					we <= "1";
					FOR i IN 5 TO 7 LOOP
						r(t, i - 1) <= r(t, i);
					END LOOP;
					r(t, 7) <= 3;
				ELSIF (dout(23) = '1' AND dout(22 DOWNTO 18) = l2tag) THEN
					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 3 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP
						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;
					END LOOP;
					r(t, 7) <= 3;
					h2 <= h2 + 1;
					lstate <= linit;
				ELSIF (dout(29) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= dout(47 DOWNTO 30) & '1' & l2tag & dout(23 DOWNTO 0);
					we <= "1";
					FOR i IN 4 TO 7 LOOP
						r(t, i - 1) <= r(t, i);
					END LOOP;
					r(t, 7) <= 4;
				ELSIF (dout(29) = '1' AND dout(28 DOWNTO 24) = l2tag) THEN
					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 4 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP
						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;
					END LOOP;
					r(t, 7) <= 4;
					h2 <= h2 + 1;
					lstate <= linit;
				ELSIF (dout(35) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= dout(47 DOWNTO 36) & '1' & l2tag & dout(29 DOWNTO 0);
					we <= "1";
					FOR i IN 3 TO 7 LOOP
						r(t, i - 1) <= r(t, i);
					END LOOP;
					r(t, 7) <= 5;
				ELSIF (dout(35) = '1' AND dout(34 DOWNTO 30) = l2tag) THEN
					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 5 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP
						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;
					END LOOP;
					r(t, 7) <= 5;
					h2 <= h2 + 1;
					lstate <= linit;
				ELSIF (dout(41) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= dout(47 DOWNTO 42) & '1' & l2tag & dout(35 DOWNTO 0);
					we <= "1";
					FOR i IN 2 TO 7 LOOP
						r(t, i - 1) <= r(t, i);
					END LOOP;
					r(t, 7) <= 6;
				ELSIF (dout(41) = '1' AND dout(40 DOWNTO 36) = l2tag) THEN
					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 6 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP
						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;
					END LOOP;
					r(t, 7) <= 6;
					h2 <= h2 + 1;
					lstate <= linit;
				ELSIF (dout(47) = '0') THEN
					lstate <= l2wait;
					m2 <= m2 + 1;
					din <= '1' & l2tag & dout(41 DOWNTO 0);
					we <= "1";
					FOR i IN 1 TO 7 LOOP
						r(t, i - 1) <= r(t, i);
					END LOOP;
					r(t, 7) <= 7;
				ELSIF (dout(47) = '1' AND dout(46 DOWNTO 42) = l2tag) THEN
					FOR i IN 0 TO 7 LOOP
						IF r(t, i) = 7 THEN
							n := i;
							EXIT;
						END IF;
					END LOOP;
					FOR i IN 1 TO 7 LOOP
						IF (i > n) THEN
							r(t, i - 1) <= r(t, i);
						END IF;
					END LOOP;
					r(t, 7) <= 7;
					h2 <= h2 + 1;
					lstate <= linit;

				ELSE
					lstate <= valid;
					t1 := dout;
					last := r(t, 0);
				END IF;
			WHEN valid => 
				m2 <= m2 + 1;
				lstate <= l2wait;
				t1(6 * last + 4 DOWNTO 6 * last) := l2tag;
				FOR i IN 1 TO 7 LOOP
					r(t, i - 1) <= r(t, i);
				END LOOP;
				r(t, 7) <= last;
				din <= t1;
				we <= "1";
				--s <= std_logic_vector(to_unsigned(last, 3));
			WHEN l2wait => 
			co(t)<=din;
				t1 := (OTHERS => '0');
				dout<=(OTHERS => '0');
				lstate <= linit;

			WHEN display => 
				IF tx_active = '1' THEN
					tx_busy <= '0';
					lstate <= output;
					TotalHit <= std_logic_vector(to_unsigned(th, 16));
					TotalMiss <= std_logic_vector(to_unsigned(tm, 16));
 
				END IF;
			WHEN output => 
				IF tx_active = '0' THEN
					tx_busy <= '1';
					CASE dis_state IS
						WHEN hit_msb => 
							var <= TotalHit;
							t_data <= TotalHit(15 DOWNTO 8);
							dis_state <= hit_lsb;
							lstate <= wait_display;
						WHEN hit_lsb => 
							t_data <= TotalHit(7 DOWNTO 0);
							dis_state <= miss_msb;
							lstate <= wait_display;
						WHEN miss_msb => 
							var <= TotalMiss;
							t_data <= TotalMiss(15 DOWNTO 8);
							dis_state <= miss_lsb;
							lstate <= wait_display;
						WHEN miss_lsb => 
							t_data <= TotalMiss(7 DOWNTO 0);
							lstate <= halt;
					END CASE;
 
				END IF;
			WHEN wait_display => 
				IF tx_active = '1' THEN
					tx_busy <= '0';
					lstate <= output;
				END IF;
			WHEN halt => 
				IF tx_active = '1' THEN
					tx_busy <= '0';
					lstate <= halt;
				END IF;
		END CASE;
	END IF;
END PROCESS;
th <= h + h2;
tm <= m + m2;
otp <= std_logic_vector(to_unsigned(tm, 16));
END Behavioral;
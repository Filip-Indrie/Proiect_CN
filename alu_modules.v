module b9wordgate(input[8:0] x, input s, output[8:0] y);
	/*
		XOR gate - applies XOR with s to every bit of x.
		s = 1 --> y = x 1's complement
		s = 0 --> y = x unchanged
	*/
	assign y[0] = x[0] ^ s;
	assign y[1] = x[1] ^ s;
	assign y[2] = x[2] ^ s;
	assign y[3] = x[3] ^ s;
	assign y[4] = x[4] ^ s;
	assign y[5] = x[5] ^ s;
	assign y[6] = x[6] ^ s;
	assign y[7] = x[7] ^ s;
	assign y[8] = x[8] ^ s;

endmodule

module control_unit( 
	input clk, rst, _begin, b7, b6, a8, q2, cnt0, cnt4, cnt8,
	input amsb3ne, // checks if first three bits of the left most reg, divisions first reg, are equal or not and if we should only lshift
	input qlsb3ne, // checks if last three bits of the middle reg, multiplys second reg, are equal or not and if we should only double rshift
	input[1:0]op, // 2 bits that represent the operation
	output l1,l2,
	output cl3, // the third load signal that is specific to division
	output add, sub, ls, rs, drs, w1, w2, cnt_incr,
	output load_qs, // used in division, signals the load of the lsb bit of Q and Q`
	output reg[1:0] div_op, // used in division, retains the operation to be performed on the shifted register
	output div_correction,
	output div_hold_init, // signal that halts counter function, lshifts and remains set as long as the right most registers msb is 0 at the start, when doing division
	output div_hold_fin, // signal that halts counter function, rshifts and remains set as long as the cnt0 counter is not 0 at the end, when doing division
	output ds_conv_sub, // signal that is used to do the last subtraction for the quotient Q-Q`, when doing division
	output cuend, // signal that specifies that the operation is done for all alu  
	output[3:0] phase, // signal that outputs the current phase of the control unit. FOR VISUAL REPRESENTATION PURPOSE ONLY!
	output[2:0] cycle // signal that outputs the current cycle of the control unit . FOR VISUAL REPRESENTATION PURPOSE ONLY!
);
	/*
		Based on 'op', performs a set of instructions to calculate the result.
		The many different input signals help the control unit decide which instruction to execute and when.
		The output signals ar sent to the registers, counters, parallel adder and some other modules to help
		achive the desired result.
	*/
    // reg and wires declaration
    wire op_add = ~op[0] & ~op[1] ;
    wire op_sub =  op[0] & ~op[1] ;
    wire op_mult= ~op[0] &  op[1] ;
    wire op_div =  op[0] &  op[1] ;

    wire adder_op_inter; // signal that specifies wether the alu is doing one of the two simple operations

	reg internal_end    = 0 ;
    reg fetch_cycle     = 0 ;
    reg write_cycle     = 0 ;
    reg execution_cycle = 0 ;
	assign cycle={write_cycle,execution_cycle,fetch_cycle};

    wire fetch_cycle_end;
    wire execution_cycle_end;
	wire write_cycle_end;

    wire ph4,ph3,ph2,ph1;
	assign phase={ph4,ph3,ph2,ph1};

    wire ph_cnt_composed_clk;
	wire ph_cnt_composed_set;
    
	wire div_corr; // used in division, signals if a correction must be performed on register A

    // alu cycle operations
    assign div_hold_init = ~( ~b7 & op_div & ph4 & fetch_cycle ) ; // signal halts phase counter when its value is 0; signal is tethered to fetch_cycle
    assign div_hold_fin  = ~( ~cnt0 & write_cycle & op_div & ph2 ) ; // signal halts phase counter when its value is 0; signal is tethered to write_cycle
    assign cnt_incr      = ~( (op_div & cnt8) | (op_mult & cnt4) ) & execution_cycle & ph3 ; //signal reset phase counter when the specific op counter has not reached its desired value; signal is tethered to execution_cycle

    assign fetch_cycle_end     = ( ( (b6 & ph4) | (~op_div & ph2) ) & fetch_cycle ) ; // signal that indicates end of fetch cycle and start of execution cycle
    assign execution_cycle_end = ( execution_cycle & ( ~op[1] | ( (op_div & cnt8) | (~op_div & cnt4) ) ) ) ; // signal that inidicates end of execution cycle and start of write cycle
    assign write_cycle_end     = w2 | ( ~op[1] & w1 ) ; // signal that indicates end of write cycle and overall alu operation


    // modulo phase counter signal assignment
	assign ph_cnt_composed_clk = clk & ( div_hold_init & div_hold_fin ) ;
	assign ph_cnt_composed_set = cnt_incr | execution_cycle_end | fetch_cycle_end ;
   
	ph4_cnt ph_cnt(.clk(ph_cnt_composed_clk),.rst(rst),.set0(ph_cnt_composed_set),
	._begin(_begin),._end(cuend),.ph3(ph4),.ph2(ph3),.ph1(ph2),.ph0(ph1)) ;


	// fetch cycle ff operations
	always@( posedge clk ) begin
        if( fetch_cycle_end )
            fetch_cycle <= 0 ;
        else if( _begin )
            fetch_cycle <= 1 ;
    end
	
	assign l1  = fetch_cycle & ph1 ;
	assign l2  = fetch_cycle & ph2 ;
	assign cl3 = fetch_cycle & ph3 & op_div ;


	// execution cycle ff operations
	always@( posedge clk ) begin
        if( execution_cycle_end )
            execution_cycle <= 0 ;
        else if( fetch_cycle_end )
            execution_cycle <= 1 ;
    end
    
    always@(posedge ph1) begin
        div_op[0] = a8 & op_div & amsb3ne & execution_cycle; // add
        div_op[1] = ~a8 & op_div & amsb3ne & execution_cycle;// sub
    end
	
    assign add     = (op_add & ph1 & execution_cycle) |  ( div_op[0] & execution_cycle & ph2 ) | ( ~q2 & qlsb3ne & op_mult & ph1 & execution_cycle ) | div_corr; // add signal equation that happens in the execution cycle
    assign sub     = (op_sub & ph1 & execution_cycle) | ( div_op[1] & execution_cycle & ph2 ) | (  q2 & qlsb3ne & op_mult & ph1 & execution_cycle ) ;
    assign ls      = ~div_hold_init | ( execution_cycle & op_div & ph1 ) ;
    assign rs      = ~div_hold_fin ;
    assign drs     = execution_cycle & ph2 & op_mult ; 
    assign load_qs = execution_cycle & ph3;

	// write cycle ff operations
	always@( posedge clk ) begin
        if( write_cycle_end ) begin
            write_cycle <= 0;
			internal_end <= 1;
		end
        else if( execution_cycle_end )
            write_cycle <= 1;
    end

    assign w1 = ( ph3 & write_cycle & op_div ) | ( ph1 & write_cycle & ~op_div ) ;
    assign w2 = ( (op_mult & ph2 & write_cycle) | (write_cycle & ph4 & op_div) ) ;
	assign ds_conv_sub = write_cycle & ph3 & op_div ;
	assign div_corr = a8 & write_cycle & ph1 & op_div;
	assign div_correction = div_corr;
	assign cuend = internal_end;

endmodule

module counter_4b (
	input clk, rst,
	output reg [3:0] val
);
	/*
		4-bit counter; counts indefinitly; max value = 15
	*/
	always @ (posedge clk or posedge rst) begin
		if(rst) begin
			val <= 4'b0;
		end
		else if(clk) begin
			val = val + 1;
		end
	end
endmodule

module cnt_47s (
	input clk, rst,
	output is4, is8
);
	/*
		This module sends signals when the value of its counter reaches 4 or 8
	*/
	wire [3:0] cnt_out;

	counter_4b cnt2(
		.clk(clk), .rst(rst), .val(cnt_out)
	);
  
	assign is4 = ~cnt_out[3] & cnt_out[2] & ~cnt_out[1] & ~cnt_out[0];
	assign is8 = cnt_out[3] & ~cnt_out[2] & ~cnt_out[1] & ~cnt_out[0];
  
endmodule

module cnt_k (
	input clk, rst, cnt_up, cnt_down,
	output is0
);
	/*
		This module has two counters and outputs a signal when they have the same value.
		One counter counts up when cnt_up is active and the other one counts up when cnt_down is active.
	*/
	wire [3:0] cnt_out1, cnt_out2;
	
	counter_4b cnt1 (
		.clk( clk & cnt_up ), .rst(rst), .val(cnt_out1)
	);
	
	counter_4b cnt2 (
		.clk( clk & cnt_down ), .rst(rst), .val(cnt_out2)
	);
	// to be honest idk why these next 2 lines make the alu work properly but they do. DO NOT MODIFY!
	// this counter should output if the clocks are at the same value, but that makes the control unit skip an instruction
	wire [3:0] diff = cnt_out1 - cnt_out2;
	assign is0 = (diff == 4'b1);
endmodule

module dec2_4( 
	input[1:0] c,
	output d3, d2, d1, d0
);
	/*
		2bit-4bit decoder; decodes a 2-bit signal in a 4-bit one (OneHot)
	*/
    assign d0 = ~c[0] & ~c[1];
    assign d1 = c[0] & ~c[1];
    assign d2 = ~c[0] & c[1];
    assign d3 = c[0] & c[1];

endmodule

module mod4c( 
	input clk, rst, enable, set0,
	output[1:0] c
);
	/*
		A 2-bit counter that has the option to set its value to 0 when set0 is active
		and count only when enable is active.
	*/
	reg[1:0] count;       

    always@(posedge clk or posedge rst) begin
        if(rst) begin
            count <= 2'b11; // set to 3 so when enable (begin) is active it becomes 0 and remains like that for an entire clock cycle
		end
		else if (set0) // NOTE: sets counter to 0
			count <= 2'b00;
		else if(enable)
            count = count + 1;
    end
	
  	assign c = count;	
  
endmodule

module b9s_21mux(
	input[8:0] x, y,
	input s,
	output[8:0] z
);
	/*
		A MUX that outputs y when s is active and x otherwise.
	*/
	assign z = s ? y : x;

endmodule

module ph4_cnt( 
	input clk, rst, _begin, _end, set0,
	output ph3, ph2, ph1, ph0
);
	/*
		Sends the _begin signal to a mod4c module and a dec2_4 module and outputs the coresponding signal of each phase.
	*/
    wire[1:0] c;
    reg ff_begin = 0;

	always@(posedge clk or posedge rst or posedge _end) begin
		if(_end) begin
            ff_begin <= 0;
        end
		else if(_begin)
            ff_begin = _begin;
    end

    mod4c mc(.clk(clk), .rst( rst | _end ), .enable(ff_begin), .set0(set0), .c(c));
    dec2_4 ph_dec(.c(c), .d3(ph3), .d2(ph2), .d1(ph1), .d0(ph0));

endmodule

module fac (
	input x, y, c_in,
	output z, c_out
);
	/*
		A FAC that takes in x, y and c_in and outputs their sum and the carry out.
	*/
	assign z = x ^ y ^ c_in;
	assign c_out = (x & y) | (x & c_in) | (y & c_in);
endmodule

module rca #(
	parameter n = 9 // number of bits of the rca
)(
	input[n-1:0] x, y,
	input c_in,
	output[n-1:0] z,
	output c_out
);
	/*
		A n-bit RCA; takes in two n-bit numbers and outputs their sum and the carry out.
	*/

	wire [n-1:1] carries;
	
	// generate n FACs
	generate
	genvar i;
	for(i = 0; i < n; i = i + 1) begin
		if(i == 0) begin
			fac slink0(
				.x(x[i]), .y(y[i]), .c_in(c_in),
				.z(z[i]), .c_out(carries[i + 1])
			);
		end
		else if(i == n - 1) begin
			fac slinki(
				.x(x[i]), .y(y[i]), .c_in(carries[i]),
				.z(z[i]), .c_out(c_out)
			);
		end
		else begin
			fac slinkn(
				.x(x[i]), .y(y[i]), .c_in(carries[i]),
				.z(z[i]), .c_out(carries[i + 1])
				);
		end
	end
	endgenerate
  
endmodule

module bit9_reg(
	input clk, rst, ls, rs, drs, load,
	input oregbl1, oregbl0, // bits with index 1 and 0 (2 lsb) of the register "to the left"
	input oregbr1, oregbr0, // bits with index 8 and 7 (2 msb) of the register "to the right"
	input setb0, b0, // sets bit with index 0 to b0 when setb0 is active(1)
	input[8:0] load_bits, // bits received along with the load signal
	output[8:0] write_bits // bits sent along when received write signal
);
	/*
		A 9-bit register that allows left/right shifts, double right shifts and the option to set the msb.
	*/
	reg [8:0] value;
	
	always@(posedge clk or posedge rst) begin
		if(rst) value <= 0;
		else if(load) value <= load_bits;
		else if(ls) value <= {value[7:0],oregbr1};
		else if(rs) begin
			value = value[8] ? {1'b1,value[8:1]} : {1'b0,value[8:1]};
		end
		else if(drs) begin
			value = value[8] ? {1'b1,value[8:1]} : {1'b0,value[8:1]};
			value[8] = oregbl0;
			value = value[8] ? {1'b1,value[8:1]} : {1'b0,value[8:1]};
			value[8] = oregbl1;
		end
		else if(setb0) begin
		    value[0]=b0;
		end
	end
	
	always@(negedge rs) begin
		value[8] <= oregbl0;
	end

	assign write_bits = value;
	
endmodule

module sub_1(
	input[8:0] x,
	input enable,
	output[8:0] y
);
	/*
		Subtracts 1 from x when enable is active(1).
	*/
	assign y = x - enable;

endmodule

module alu(
	input[7:0] in,
	input[1:0] op,
	input _begin,clk,rst,
	output [7:0]out,
	output _end
);

	// assigning output
	assign out = ( w1 ) ? a_out[7:0] : ( ( w2 & op_mult ) ? q_out[8:1] : ( ( w2 & op_div ) ? q_out[7:0] : 8'bZ ) );

	// factoring common signals
	wire load_enable = add | sub;
	wire composed_rst = rst | l1;
	
	// defining operation wires
	wire op_add = ~op[0] & ~op[1] ;
    wire op_sub =  op[0] & ~op[1] ;
    wire op_mult= ~op[0] &  op[1] ;
    wire op_div =  op[0] &  op[1] ;
	
	// muxes for choosing if we load from inbus or rca output
	wire[8:0] mux1_out, mux2_out;
	b9s_21mux mux1(.x({1'b0,in}), .y(operation_out), .s(load_enable), .z(mux1_out)); // A
	b9s_21mux mux2(.x((op_mult) ? {in,1'b0} : {1'b0,in}), .y(operation_out), .s(ds_conv_sub), .z(mux2_out)); // Q
	
	// muxes for chosing which register is sent to the rca
	wire[8:0] mux3_out, mux4_out;
	wire[8:0] b_out_rca = (op_mult & ~(q_out[1] ^ q_out[0])) ? {b_out[7:0],1'b0} : {b_out[7] & ~op_div,b_out[7:0]}; // decides which form of B is sent to the Adder
	b9s_21mux mux3(.x(a_out), .y(q_out), .s(ds_conv_sub), .z(mux3_out)); // A/Q
	b9s_21mux mux4(.x(b_out_rca), .y(qprim_out), .s(ds_conv_sub), .z(mux4_out)); // B/Q`
	
	// A reg
	wire[8:0] a_out;
	wire amsb3ne;
	bit9_reg a(.clk(clk), .rst(composed_rst), .ls(ls), .rs(rs), .drs(drs),
	.load(load_enable | (l2 & ~op_mult)), .oregbl1(a_out[8]), .oregbl0(drs & a_out[8]),
	.oregbr1(q_out[7]), .oregbr0(1'b0), .setb0(1'b0), .b0(1'b0), .load_bits(mux1_out), .write_bits(a_out));
	assign amsb3ne = (a_out[8] ^ a_out[7]) | (a_out[7] ^ a_out[6]);
	
	// Q reg
	wire[8:0] q_out;
	wire qlsb3ne; 
	bit9_reg q(.clk(clk), .rst(composed_rst), .ls(ls), .rs(rs & div_hold_fin), .drs(drs),
	.load( (op_div & cl3) | (op_mult & l2) | ds_conv_sub ), .oregbl1(a_out[1]), .oregbl0(a_out[0]), 
	.oregbr1(  ~div_hold_init & b_out[7] ),
	.oregbr0(1'b0), .setb0(load_qs), .b0(div_op[1]), .load_bits(mux2_out), .write_bits(q_out));
	assign qlsb3ne = (q_out[2] ^ q_out[1]) | (q_out[1] ^ q_out[0]);
	
	// B reg
	wire [8:0]b_out;
	bit9_reg b(.clk(clk), .rst(rst), .ls(ls & ~div_hold_init), .rs(1'b0), .drs(1'b0), .load(l1),
	.oregbl1(1'b0), .oregbl0(1'b0), .oregbr1(1'b0), .oregbr0(1'b0), .setb0(1'b0), .b0(1'b0), .load_bits({1'b0,in}), .write_bits(b_out));
	
	// Q` reg
	wire [8:0]qprim_out;
	bit9_reg qprim(.clk(clk), .rst(composed_rst), .ls(ls & div_hold_init), .rs(1'b0), .drs(1'b0),
	.load(1'b0), .oregbl1(1'b0), .oregbl0(1'b0), .oregbr1(1'b0),
	.oregbr0(1'b0), .setb0(load_qs), .b0(div_op[0]), .load_bits(9'b0), .write_bits(qprim_out));

	// xor gate
	wire[8:0] xor_gate_out;
	b9wordgate xor_gate(.x(mux4_out), .s(sub | ds_conv_sub), .y(xor_gate_out));

	// rca
	wire[8:0] rca_out;
	rca rca1(.x(mux3_out), .y(xor_gate_out), .c_in(sub | ds_conv_sub), .z(rca_out));
	
	// moudle that performs the "- 1" part of "Q - Q` - 1" 
	wire[8:0] operation_out;
	sub_1 s(.x(rca_out), .enable(div_correction), .y(operation_out));
	
	// counters
	wire cnt0,cnt4,cnt8;
	cnt_47s cnt_1(.clk(cnt_incr), .rst(composed_rst), .is4(cnt4), .is8(cnt8));
	cnt_k cnt_2(.clk(clk), .rst(composed_rst), .cnt_up(div_hold_init), .cnt_down(div_hold_fin), .is0(cnt0));

	// control unit
	
	wire div_corr; // takes the signal out of the control unit
	reg div_correction; // sets to 1 when div_corr first sets to 1
	
	always@(posedge clk or posedge rst) begin
	
		if(rst) div_correction <= 0;
		else if(div_corr) div_correction = 1;
	
	end
	
	wire[1:0] div_op;
	wire l1,l2,cl3,add,sub,ls,rs,drs,w1,w2,load_qs,cnt_incr,div_hold_init,div_hold_fin,ds_conv_sub;
	control_unit cu(.clk(clk), .rst(rst), ._begin(_begin), .b7(b_out[7]), .b6(b_out[6]), .a8(a_out[8]),
	.q2(q_out[2]), .cnt0(cnt0), .cnt4(cnt4), .cnt8(cnt8), .amsb3ne(amsb3ne), .qlsb3ne(qlsb3ne),
	.op(op), .l1(l1), .l2(l2), .cl3(cl3), .add(add), .sub(sub), .ls(ls), .rs(rs), .drs(drs),
	.w1(w1), .w2(w2), .load_qs(load_qs), .div_op(div_op), .div_correction(div_corr), .cnt_incr(cnt_incr), .div_hold_init(div_hold_init), .div_hold_fin(div_hold_fin),
	.ds_conv_sub(ds_conv_sub), .cuend(_end), .phase(/*empty*/), .cycle(/*empty*/));

endmodule
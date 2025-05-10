module testbench_alu();

	// inputs and outputs
	reg [7:0]in;
	reg [1:0]op;
	reg _begin,clk,rst;
	wire [7:0]out;
	wire _end;
	
	//TEST ASSIGNMENTS
	assign a_bits={a_regbr1,a_regbr0};
	assign load1=l1;
	assign load2=l2;

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
	b9s_21mux mux1(.x({1'b0,in}), .y(operation_out), .s(load_enable), .z(mux1_out)); // a
	b9s_21mux mux2(.x((op_mult) ? {in,1'b0} : {1'b0,in}), .y(operation_out), .s(ds_conv_sub), .z(mux2_out)); // q
	
	// muxes for chosing which register is sent to the rca
	wire[8:0] mux3_out, mux4_out;
	wire[8:0] b_out_rca;
	assign b_out_rca = (op_mult & ~(q_regbr1 ^ q_regbr0)) ? {b_out[7:0],1'b0} : {b_regbl1 & ~op_div,b_out[7:0]};
	b9s_21mux mux3(.x(a_out), .y(q_out), .s(ds_conv_sub), .z(mux3_out)); // A/Q
	b9s_21mux mux4(.x(b_out_rca), .y(qprim_out), .s(ds_conv_sub), .z(mux4_out)); // B/Q`
	
	// A reg
	wire[8:0] a_out;
	wire a_regbl2, a_regbl1, a_regbl0, a_regbr1, a_regbr0, amsb3ne;
	bit9_reg a(.clk(clk), .rst(composed_rst), .ls(ls), .rs(rs), .drs(drs),
	.load(load_enable | (l2 & ~op_mult)), .oregbl1(/*aux[1]*/a_regbl2), .oregbl0(/*aux[0]*/drs & a_regbl2),
	.oregbr1(q_regbl1), .oregbr0(1'b0), .setb0(1'b0), .b0(1'b0), .load_bits(mux1_out), .regbl2(a_regbl2), .regbl1(a_regbl1),
	.regbl0(a_regbl0), .regbr2(/*empty*/), .regbr1(a_regbr1), .regbr0(a_regbr0), .write_bits(a_out));
	assign amsb3ne = (a_regbl2 ^ a_regbl1) | (a_regbl1 ^ a_regbl0);
	
	// Q reg
	wire[8:0] q_out;
	wire q_regbl1, q_regbr2, q_regbr1, q_regbr0, qlsb3ne; 
	bit9_reg q(.clk(clk), .rst(composed_rst), .ls(ls), .rs(rs & div_hold_fin), .drs(drs),
	.load( (op_div & cl3) | (op_mult & l2) | ds_conv_sub ), .oregbl1(a_regbr1), .oregbl0(a_regbr0), 
	.oregbr1(  ~div_hold_init & b_regbl1 )/*~( op_div & add ) | ( op_div & sub )*/,
	.oregbr0(1'b0), .setb0(load_qs), .b0(div_op[1]), .load_bits(mux2_out), .regbl2(/*empty*/), .regbl1(q_regbl1),
	.regbl0(/*empty*/), .regbr2(q_regbr2), .regbr1(q_regbr1), .regbr0(q_regbr0), .write_bits(q_out));
	assign qlsb3ne = (q_regbr2 ^ q_regbr1) | (q_regbr1 ^ q_regbr0);
	
	// B reg
	wire [8:0]b_out;
	wire b_regbl1, b_regbl0;
	bit9_reg b(.clk(clk), .rst(rst), .ls(ls & ~div_hold_init), .rs(1'b0), .drs(1'b0), .load(l1),
	.oregbl1(1'b0), .oregbl0(1'b0), .oregbr1(1'b0), .oregbr0(1'b0), .setb0(1'b0), .b0(1'b0), .load_bits({1'b0,in}), .regbl2(/*empty*/),
	.regbl1(b_regbl1), .regbl0(b_regbl0), .regbr2(/*empty*/), .regbr1(/*empty*/), //regbr1 si regbr0 trebuie sa fie goale (nu sunt pt a face teste)
	.regbr0(/*empty*/), .write_bits(b_out));
	
	// Q` reg
	wire [8:0]qprim_out;
	reg [1:0]op_reg;
	bit9_reg qprim(.clk(clk), .rst(composed_rst), .ls(ls & div_hold_init), .rs(1'b0), .drs(1'b0),
	.load(1'b0), .oregbl1(1'b0), .oregbl0(1'b0), .oregbr1(1'b0 /*( op_div & sub ) | ( op_div & add )*/),
	.oregbr0(1'b0), .setb0(load_qs), .b0(div_op[0]), .load_bits(9'b0), .regbl2(/*empty*/), .regbl1(/*empty*/), .regbl0(/*empty*/),
	.regbr2(/*empty*/), .regbr1(/*empty*/), .regbr0(/*empty*/), .write_bits(qprim_out));

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
	
	wire div_corr;
	reg div_correction;
	
	always@(posedge clk or posedge rst) begin
	
		if(rst) div_correction <= 0;
		else if(div_corr) div_correction = 1;
	
	end
	
	wire[3:0] phase;
	wire[2:0] cycle;
	wire[1:0] div_op;
	wire l1,l2,cl3,add,sub,ls,rs,drs,w1,w2,load_qs,cnt_incr,div_hold_init,div_hold_fin,ds_conv_sub;
	control_unit cu(.clk(clk), .rst(rst), ._begin(_begin), .b7(b_regbl1), .b6(b_regbl0), .a8(a_regbl2),
	.q2(q_regbr2), .cnt0(cnt0), .cnt4(cnt4), .cnt8(cnt8), .amsb3ne(amsb3ne), .qlsb3ne(qlsb3ne),
	.op(op), .l1(l1), .l2(l2), .cl3(cl3), .add(add), .sub(sub), .ls(ls), .rs(rs), .drs(drs),
	.w1(w1), .w2(w2), .load_qs(load_qs), .div_op(div_op), .div_correction(div_corr), .cnt_incr(cnt_incr), .div_hold_init(div_hold_init), .div_hold_fin(div_hold_fin),
	.ds_conv_sub(ds_conv_sub), .cuend(_end), .phase(phase), .cycle(cycle));



	//testbench
	
	localparam CLOCK_CYCLES=50;
	localparam CCT=10; // clock cycle time

    initial begin
        clk=1'b0;
        repeat (2*CLOCK_CYCLES) #(CCT/2) clk=~clk;
    end

    initial begin
		
		//adunare
		/*
		rst=1;
		op=2'b00;
		_begin=0;
		#CCT rst=0;
		#CCT _begin=1;
		#CCT _begin=0; in=8'b00000011;
		#CCT in=8'b00000011;
		*/
		
		//scadere
		/*
		rst=1;
		op=2'b01;
		_begin=0;
		#CCT rst=0;
		#CCT _begin=1;
		#CCT _begin=0; in=8'b00000010;
		#CCT in=8'b00000111;
		*/
		
		
		//inmultire
		/*
		rst=1;
		op=2'b10;
		_begin=0;
		#CCT rst=0;
		#CCT _begin=1;
		#CCT _begin=0; in=8'b11000101;
		#CCT in=8'b00000100;
		*/
		
		//impartire cu corectie
		/*
		rst=1;
		op=2'b11;
		_begin=0;
		#CCT rst=0;
		#CCT _begin=1;
		#CCT _begin=0; in=8'b00000011;
		#CCT in=8'b00000000;
		#CCT in=8'b00010000;
		*/
		
		//impartire fara corectie;
		/*
		rst=1;
		op=2'b11;
		_begin=0;
		#CCT rst=0;
		#CCT _begin=1;
		#CCT _begin=0; in=8'b00110001;
		#CCT in=8'b00010010;
		#CCT in=8'b01111011;
		*/
    end
    initial begin
        $display("Time:\tClk:\tRst:\tBegin:\tEnd:\tPhase:\tCycle:\tIn:\t\tl1:\tl2:\tA:\t\tQ:\t\tB:\t\tQ':\t\tAdd:\tSub:\tls:\trs:\tcnt8:\tcnt0:\tdiv_correction:\tOut:");
        $monitor("%0t\t%b\t%b\t%b\t%b\t%4b\t%3b\t%8b\t%b\t%b\t%9b\t%9b\t%9b\t%9b\t%b\t%b\t%b\t%b\t%b\t%b\t%b\t%8b",$time,clk,rst,_begin,_end,phase,cycle,in,l1,l2,a_out,q_out,b_out,qprim_out,add,sub,ls,rs,cnt8,cnt0,div_corr,out);
    end

endmodule
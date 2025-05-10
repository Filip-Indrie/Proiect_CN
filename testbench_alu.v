module testbench();
	
	reg [7:0]in;
	reg [1:0]op;
	reg _begin,clk,rst;
	wire [7:0]out;
	wire _end;
	
	alu alu1(.in(in), .op(op), ._begin(_begin), .clk(clk), .rst(rst), .out(out), ._end(_end));
	
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
		#CCT _begin=0; in=8'b00000010;
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
        $display("Time:\tClk:\tRst:\tBegin:\tEnd\tOperation:\tIn:\tOut:");
        $monitor("%0t\t%b\t%b\t%b\t%b\t%2b\t%8b\t%8b",$time,clk,rst,_begin,_end,op,in,out);
    end

endmodule
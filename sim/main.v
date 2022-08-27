
module main();
   initial $display("*nic8 simulation*");
   reg clk;
   initial clk = 1;
   always #5 clk = ~clk;
   always @(clk) if ($time >= 100) $finish();
   reg [7:0] pc;
   initial pc = 0;
   always @(posedge clk) pc = pc + 1;
   always @(posedge clk) #1 $display("%03d: pc=%03d",$time,pc);

endmodule

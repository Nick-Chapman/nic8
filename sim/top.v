
`define Control [1:13]

module top;

   reg clk, reset;
   initial reset = 1;
   initial #2 clk = 0;
   initial #3 reset = 0;
   always #5 clk <= ~clk;

   whole_cpu cpu (clk,reset);

   monitor m (clk,
              cpu.pc,
              cpu.ir,
              cpu.registers.A.contents,
              cpu.registers.B.contents,
              cpu.registers.X.contents,
              cpu.registers.Q.contents
              );

endmodule

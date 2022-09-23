
module top;

   reg clk, resetBar;
   initial resetBar = 0;
   initial #2 clk = 0;
   initial #3 resetBar = 1;
   always #5 clk <= ~clk;

   whole_cpu cpu (clk,resetBar);

   monitor m (clk,
              resetBar,
              cpu.pc,
              cpu.ir,
              cpu.registers.A.contents,
              cpu.registers.B.contents,
              cpu.registers.X.contents,
              cpu.registers.Q.contents
              );

endmodule

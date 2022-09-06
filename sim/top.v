
`define Control [1:14]

module top;

   reg clk, reset;
   initial reset = 1;
   initial #2 clk = 0;
   initial #3 reset = 0;
   always #5 clk <= ~clk;

   wire [7:0] ir,pc,areg,breg,xreg,qreg,dbus;
   wire `Control controlBits;

   whole_cpu c (clk,reset,ir,pc,areg,breg,xreg,qreg,dbus,controlBits);
   monitor m (clk,ir,pc,areg,breg,xreg,qreg,controlBits,dbus);

endmodule

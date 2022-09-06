
module registers
  (input clkBar, resetBar, input `Control controlBits, inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg);

   wire _;
   wire loadA,loadB,loadX,loadQ;
   wire assertBarA,assertBarX;

   assign {_,_,loadA,loadB,loadX,loadQ,_,
           _,_,assertBarA,assertBarX,
           _,_,_} = controlBits;

   GPR A(~clkBar,~resetBar,loadA,dbus,areg);
   GPR B(~clkBar,~resetBar,loadB,dbus,breg);
   GPR X(~clkBar,~resetBar,loadX,dbus,xreg);
   GPR Q(~clkBar,~resetBar,loadQ,dbus,qreg);

   assign dbus = ~assertBarA ? areg : 'z;
   assign dbus = ~assertBarX ? xreg : 'z;

endmodule

module GPR(input clk,reset,load,
           input [7:0] data,
           output reg [7:0] contents);

   always #1 if (reset) contents = 0;
   always @(posedge clk) if (load) contents <= data;

endmodule


module registers
  (input clkBar, resetBar,
   input triggerA,triggerB,triggerX,triggerQ,assertBarA,assertBarX,
   inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg
   );

   wire clk = ~clkBar;

   GPR A(~resetBar,triggerA,dbus,areg);
   GPR B(~resetBar,triggerB,dbus,breg);
   GPR X(~resetBar,triggerX,dbus,xreg);
   GPR Q(~resetBar,triggerQ,dbus,qreg);

   assign dbus = ~assertBarA ? areg : 'z;
   assign dbus = ~assertBarX ? xreg : 'z;

endmodule

module GPR(input reset,trigger, input [7:0] data,
           output reg [7:0] contents);

   always #1 if (reset) contents = 0;
   always @(posedge trigger) contents <= data;

endmodule

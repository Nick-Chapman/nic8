
module registers
  (input reset, clk, input `Control controlBits, inout [7:0] dbus,
   output reg [7:0] areg, breg, xreg, qreg);

   wire _;
   wire loadA,loadB,loadX,loadQ;
   wire assertA,assertX;

   assign {_,_,loadA,loadB,loadX,loadQ,_,
           _,_,assertA,assertX,
           _,_,_} = controlBits;

   always #1 if (reset) begin
      areg = 0;
      breg = 0;
      xreg = 0;
      qreg = 0;
   end

   always @(posedge clk) if (loadA) areg <= dbus;
   always @(posedge clk) if (loadB) breg <= dbus;
   always @(posedge clk) if (loadX) xreg <= dbus;
   always @(posedge clk) if (loadQ) qreg <= dbus;

   assign dbus = assertA ? areg : 'z;
   assign dbus = assertX ? xreg : 'z;

endmodule

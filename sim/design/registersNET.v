
module registersNET
  (input reset, clk, input `Control controlBits, inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg);

   wire _;
   wire loadA,loadB,loadX,loadQ;
   wire assertBarA,assertBarX;

   assign {_,_,loadA,loadB,loadX,loadQ,_,
           _,_,assertBarA,assertBarX,
           _,_,_} = controlBits;

   wire clkBar = ~clk;

   wire triggerA, triggerB, triggerX, triggerQ;

   LS00 u1
     (.A1(clkBar),
      .A2(clkBar),
      .A3(clkBar),
      .A4(clkBar),
      .B1(loadA),
      .B2(loadB),
      .B3(loadX),
      .B4(loadQ),
      .Y1(triggerA),
      .Y2(triggerB),
      .Y3(triggerX),
      .Y4(triggerQ));

   wire assertBarB = 1'b1;
   wire assertBarQ = 1'b1;

   wire resetBar = ~reset;

   GPR_NET A(resetBar, assertBarA, triggerA, dbus, areg);
   GPR_NET B(resetBar, assertBarB, triggerB, dbus, breg);
   GPR_NET X(resetBar, assertBarX, triggerX, dbus, xreg);
   GPR_NET Q(resetBar, assertBarQ, triggerQ, dbus, qreg);

endmodule

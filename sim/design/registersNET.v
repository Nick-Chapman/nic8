
module registersNET
  (input reset, clk, input `Control controlBits, inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg);

   wire _;
   wire loadA,loadB,loadX,loadQ;
   wire assertBarA,assertBarX;

   assign {_,_,loadA,loadB,loadX,loadQ,_,
           _,_,assertBarA,assertBarX,
           _,_,_} = controlBits;

   wire triggerA = clk || ~loadA;
   wire triggerB = clk || ~loadB;
   wire triggerX = clk || ~loadX;
   wire triggerQ = clk || ~loadQ;

   wire assertBarB = 1'b1;
   wire assertBarQ = 1'b1;

   wire resetBar = ~reset;

   GPR_NET A(resetBar, assertBarA, triggerA, dbus, areg);
   GPR_NET B(resetBar, assertBarB, triggerB, dbus, breg);
   GPR_NET X(resetBar, assertBarX, triggerX, dbus, xreg);
   GPR_NET Q(resetBar, assertBarQ, triggerQ, dbus, qreg);

endmodule


module registersNET
  (input reset, clk, input `Control controlBits, inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg);

   wire _;
   wire loadA,loadB,loadX,loadQ;
   wire assertA,assertX;

   assign {_,_,loadA,loadB,loadX,loadQ,_,
           _,_,assertA,assertX,
           _,_,_} = controlBits;

   wire triggerA = clk || ~loadA;
   wire triggerB = clk || ~loadB;
   wire triggerX = clk || ~loadX;
   wire triggerQ = clk || ~loadQ;

   wire assertB = 1'b0;
   wire assertQ = 1'b0;

   GPR_NET A(reset, ~assertA, triggerA, dbus, areg);
   GPR_NET B(reset, ~assertB, triggerB, dbus, breg);
   GPR_NET X(reset, ~assertX, triggerX, dbus, xreg);
   GPR_NET Q(reset, ~assertQ, triggerQ, dbus, qreg);

endmodule

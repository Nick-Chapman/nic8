
module registers_NET
  (input clkBar, resetBar, input `Control controlBits, inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg);

   wire _;
   wire triggerA,triggerB,triggerX,triggerQ;
   wire assertBarA,assertBarX;

   assign {_,_,_,
           triggerA,triggerB,triggerX,triggerQ,
           _,_,
           _,assertBarA,assertBarX,
           _,_} = controlBits;

   wire assertBarB = 1'b1;
   wire assertBarQ = 1'b1;

   GPR_NET A(resetBar, assertBarA, triggerA, dbus, areg);
   GPR_NET B(resetBar, assertBarB, triggerB, dbus, breg);
   GPR_NET X(resetBar, assertBarX, triggerX, dbus, xreg);
   GPR_NET Q(resetBar, assertBarQ, triggerQ, dbus, qreg);

endmodule

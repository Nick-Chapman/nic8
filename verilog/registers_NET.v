
module registers_NET
  (input clkBar, resetBar,
   input triggerA,triggerB,triggerX,triggerQ,assertBarA,assertBarX,
   inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg
   );

   wire assertBarB = 1'b1;
   wire assertBarQ = 1'b1;

   GPR_NET A(resetBar, assertBarA, triggerA, dbus, areg);
   GPR_NET B(resetBar, assertBarB, triggerB, dbus, breg);
   GPR_NET X(resetBar, assertBarX, triggerX, dbus, xreg);
   GPR_NET Q(resetBar, assertBarQ, triggerQ, dbus, qreg);

endmodule

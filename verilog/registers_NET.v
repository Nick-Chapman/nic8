
module registers_NET
  (input resetBar,
   input triggerA,triggerB,triggerX,triggerQ,assertBarA,assertBarB,assertBarX,
   inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg
   );

   wire assertBarQ = 1'b1;

   GPR_NET A(resetBar, assertBarA, triggerA, dbus, areg);
   GPR_NET B(resetBar, assertBarB, triggerB, dbus, breg);
   GPR_NET X(resetBar, assertBarX, triggerX, dbus, xreg);
   GPR_NET Q(resetBar, assertBarQ, triggerQ, dbus, qreg);

endmodule

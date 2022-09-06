
module registersNET
  (input reset, clk, input `Control controlBits, inout [7:0] dbus,
   output [7:0] areg, breg, xreg, qreg);

   wire _;
   wire loadA,loadB,loadX,loadQ;
   wire assertA,assertX;

   assign {_,_,loadA,loadB,loadX,loadQ,_,
           _,_,assertA,assertX,
           _,_,_} = controlBits;

   LS273 u1 (.MRB(!reset), .CP(clk || ~loadA), .D(dbus), .Q(areg));
   LS273 u2 (.MRB(!reset), .CP(clk || ~loadB), .D(dbus), .Q(breg));
   LS273 u3 (.MRB(!reset), .CP(clk || ~loadX), .D(dbus), .Q(xreg));
   LS273 u4 (.MRB(!reset), .CP(clk || ~loadQ), .D(dbus), .Q(qreg));

   LS245 u5 (.ENB(~assertA), .DIR(1'b1), .A(areg), .B(dbus));
   LS245 u6 (.ENB(~assertX), .DIR(1'b1), .A(xreg), .B(dbus));

endmodule

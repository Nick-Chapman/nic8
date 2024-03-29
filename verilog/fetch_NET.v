
module fetch_NET(input clk,resetBar,denyFetch, inout [7:0] dbus,
                 output [7:0] ir);

   wire [7:0] dbusOrZero;

   LS273 u0 (.MRB(resetBar), .CP(clk), .D(dbusOrZero), .Q(ir));
   LS245 u1 (.ENB(denyFetch), .DIR(1'b1), .A(dbus), .B(dbusOrZero));

   assign dbusOrZero = denyFetch ? 8'b0 : 8'bz; // register pulldown

endmodule

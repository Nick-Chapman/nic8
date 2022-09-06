
module fetch_unit_NET(input clk,reset,loadIR, inout [7:0] dbus,
                      output [7:0] ir);

   wire [7:0] dbusOrZero;

   LS273 u0 (.MRB(!reset), .CP(clk), .D(dbusOrZero), .Q(ir));
   LS245 u1 (.ENB(~loadIR), .DIR(1'b1), .A(dbus), .B(dbusOrZero));

   assign dbusOrZero = loadIR ? 8'bz : 8'b0; // register pulldown

endmodule

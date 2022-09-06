
module fetch_unit_NET(input clk,reset,loadIR, input [7:0] dbus,
                      output [7:0] ir);

   LS273 u0
     (.MRB(!reset),
      .CP(clk),
      .D(loadIR ? dbus : 8'b0),
      .Q(ir));

endmodule

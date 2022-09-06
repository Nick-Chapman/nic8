
module fetch_unit(input clk,reset,loadIR, input [7:0] dbus,
                  output reg [7:0] ir);

   always #1 if (reset) begin
      ir = 0;
   end
   always @(posedge clk) ir <= loadIR ? dbus : 0;
   //LS273 u0 (.MRB(!reset), .CP(clk), .D(loadIR ? dbus : 8'b0), .Q(ir));

endmodule

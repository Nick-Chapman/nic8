
module fetch(input clk,resetBar,denyFetch, input [7:0] dbus,
             output reg [7:0] ir);

   always #1 if (~resetBar) begin
      ir = 0;
   end
   always @(posedge clk) ir <= denyFetch ? 0 : dbus;

endmodule


module fetch(input clk,resetBar,loadBarIR, input [7:0] dbus,
             output reg [7:0] ir);

   always #1 if (~resetBar) begin
      ir = 0;
   end
   always @(posedge clk) ir <= ~loadBarIR ? dbus : 0;

endmodule

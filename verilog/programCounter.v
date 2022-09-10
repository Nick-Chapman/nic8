
module programCounter
  (input resetBar, clk, doJumpBar, assertRom, input [7:0] dbus,
   output reg [7:0] pc);

   always #1 if (~resetBar) begin
      pc = 0;
   end

   always @(posedge(clk)) begin
      if (~doJumpBar) pc <= dbus;
      else if (assertRom) pc <= pc + 1;
   end

endmodule


module programCounter
  (input reset, clk, doJump, immediate, input [7:0] dbus,
   output reg [7:0] pc
   );

   always #1 if (reset) begin
      pc = 0;
   end

   always @(posedge(clk)) begin
      if (doJump) pc <= dbus;
      else if (immediate) pc <= pc + 1;
   end

/*
   always @(posedge(clk || ~doJump),
            posedge(clk || ~immediate))
     pc <=  doJump ? dbus : pc + 1;
*/

endmodule

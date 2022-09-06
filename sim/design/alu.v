
module alu(input clk, reset, doSubtract, assertE,
           input [7:0] areg, breg,
           output [7:0] dbus,
           output aIsZero,
           output reg [0:0] flagCarry);

   wire [7:0] aluOut = doSubtract ? areg - breg : areg + breg;
   assign dbus = assertE ? aluOut : 'z;

   assign aIsZero = (areg == 0);

   wire carry = doSubtract ? !(breg > areg) : (areg + breg >= 256);

   always #1 if (reset) begin
      flagCarry = 0;
   end

   always @(posedge clk) if (assertE) flagCarry = carry;
   //always @(posedge(clk || ~assertE)) flagCarry = carry;

endmodule

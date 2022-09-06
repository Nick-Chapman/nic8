
module alu(input clk, reset, doSubtract, assertBarE,
           input [7:0] areg, breg,
           output [7:0] dbus,
           output aIsZero,
           output reg [0:0] flagCarry);

   wire [7:0] aluOut = doSubtract ? areg - breg : areg + breg;
   assign dbus = ~assertBarE ? aluOut : 'z;

   assign aIsZero = (areg == 0);

   wire carry = doSubtract ? !(breg > areg) : (areg + breg >= 256);

   always #1 if (reset) begin
      flagCarry = 0;
   end

   always @(posedge clk) if (~assertBarE) flagCarry = carry;

endmodule

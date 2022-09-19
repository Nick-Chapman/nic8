
module alu(input clk, resetBar, doSubtract, doCarryIn, doShiftIn, assertBarE, assertBarS, triggerC, triggerS,
           input [7:0] areg, breg,
           output [7:0] dbus,
           output aIsZero,
           output reg [0:0] flagCarry, flagShift);

   wire cin = flagCarry & doCarryIn;
   wire [7:0] aluOut = doSubtract ? areg - breg : areg + breg + cin;
   assign dbus = ~assertBarE ? aluOut : 'z;

   wire [7:0] shifted = {flagShift & doShiftIn, areg[7:1]};
   assign dbus = ~assertBarS ? shifted : 'z;

   assign aIsZero = (areg == 0);

   wire carry = doSubtract ? ~(breg > areg) : (areg + breg >= 256);

   always #1 if (!resetBar) begin
      flagCarry = 0;
      flagShift = 0;
   end

   always @(posedge triggerC) flagCarry <= carry;
   always @(posedge triggerS) flagShift <= areg[0];

endmodule

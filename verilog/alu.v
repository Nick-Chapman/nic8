
module alu(input clk, reset, doSubtract, assertBarE, assertBarS,
           input [7:0]      areg, breg,
           output [7:0]     dbus,
           output           aIsZero,
           output reg [0:0] flagCarry, flagShift);


   wire [7:0] aluOut = doSubtract ? areg - breg : areg + breg;
   assign dbus = ~assertBarE ? aluOut : 'z;

   wire [7:0] shifted = {flagShift, areg[7:1]};
   assign dbus = ~assertBarS ? shifted : 'z;

   assign aIsZero = (areg == 0);

   wire carry = doSubtract ? ~(breg > areg) : (areg + breg >= 256);

   always #1 if (reset) begin
      flagCarry = 0;
      flagShift = 0;
   end

   always @(posedge clk) if (~assertBarE) flagCarry <= carry;
   always @(posedge clk) if (~assertBarS) flagShift <= areg[0];

endmodule

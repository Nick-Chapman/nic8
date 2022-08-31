
module alu(input [7:0] areg, breg, input doSubtract,
           output [7:0] aluOut, output carry,aIsZero);

   assign aluOut = doSubtract ? areg - breg : areg + breg;
   assign carry = doSubtract ? !(breg > areg) : (areg + breg >= 256);
   assign aIsZero = (areg == 0);

endmodule

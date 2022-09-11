
module alu_NET(input clk, reset, doSubtract, assertBarE, assertBarS, triggerC,triggerS,
               input [7:0] areg, breg,
               output [7:0] dbus,
               output aIsZero,
               output flagCarry,flagShift);

   wire coutLO,coutHI;
   wire [7:0] other,aluOut,shifted;

   assign aIsZero = (areg == 0); // TODO: 8-wide nor

   LS74 flags
     (
      .CLRB1(!reset),
      .D1(coutHI),
      .CLK1(triggerC),
      .PRB1(1'b1),
      .Q1(flagCarry),
      .QB1(),

      .CLRB2(!reset),
      .D2(areg[0]),
      .CLK2(triggerS),
      .PRB2(1'b1),
      .Q2(flagShift),
      .QB2());

   assign shifted = {flagShift, areg[7:1]};

   LS245 u0 (.ENB(assertBarE), .DIR(1'b1), .A(aluOut), .B(dbus));
   LS245 u1 (.ENB(assertBarS), .DIR(1'b1), .A(shifted), .B(dbus));

   LS86 u2
     (.A1(doSubtract),
      .A2(doSubtract),
      .A3(doSubtract),
      .A4(doSubtract),
      .B1(breg[0]),
      .B2(breg[1]),
      .B3(breg[2]),
      .B4(breg[3]),
      .Y1(other[0]),
      .Y2(other[1]),
      .Y3(other[2]),
      .Y4(other[3]));

   LS86 u3
     (.A1(doSubtract),
      .A2(doSubtract),
      .A3(doSubtract),
      .A4(doSubtract),
      .B1(breg[4]),
      .B2(breg[5]),
      .B3(breg[6]),
      .B4(breg[7]),
      .Y1(other[4]),
      .Y2(other[5]),
      .Y3(other[6]),
      .Y4(other[7]));

   LS283 lo
     (.A1(areg[0]),
      .A2(areg[1]),
      .A3(areg[2]),
      .A4(areg[3]),
      .B1(other[0]),
      .B2(other[1]),
      .B3(other[2]),
      .B4(other[3]),
      .E1(aluOut[0]),
      .E2(aluOut[1]),
      .E3(aluOut[2]),
      .E4(aluOut[3]),
      .CIN(doSubtract),
      .COUT(coutLO));

   LS283 hi
     (.A1(areg[4]),
      .A2(areg[5]),
      .A3(areg[6]),
      .A4(areg[7]),
      .B1(other[4]),
      .B2(other[5]),
      .B3(other[6]),
      .B4(other[7]),
      .E1(aluOut[4]),
      .E2(aluOut[5]),
      .E3(aluOut[6]),
      .E4(aluOut[7]),
      .CIN(coutLO),
      .COUT(coutHI));

endmodule


module alu_NET(input clk, resetBar, doSubtract, doCarryIn, doShiftIn, assertBarE, assertBarS, triggerC,triggerS,
               input [7:0] areg, breg,
               output [7:0] dbus,
               output aIsZero,
               output flagCarry,flagShift);

   wire coutLO,coutHI;
   wire [7:0] other,aluOut,shifted;
   wire cin,carryOrBorrowIn,shiftIn;

   LS86 u1 // HI nibble invert B for subtract
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

   LS283 u2 // HI nibble add
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

   LS86 u3 // LO nibble invert B for subtract
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

   LS283 u4 // LO nibble add
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
      .CIN(carryOrBorrowIn),
      .COUT(coutLO));

   LS245 u5 // ALU output line driver
     (.ENB(assertBarE), .DIR(1'b0), .A(dbus), .B(aluOut));

   // Shifter...

   wire aLoZero,aHiZero;

   SN7425 u6 // A-reg (Hi/Lo nibbles) are zero?
     (.A1(areg[0]),
      .B1(areg[1]),
      .C1(areg[2]),
      .D1(areg[3]),
      .G1(1'b1),
      .Y1(aLoZero),

      .A2(areg[4]),
      .B2(areg[5]),
      .C2(areg[6]),
      .D2(areg[7]),
      .G2(1'b1),
      .Y2(aHiZero));

   LS08 u7
     (
      // A-reg is zero
      .A1(aLoZero),
      .B1(aHiZero),
      .Y1(aIsZero),

      .A2(flagShift),
      .B2(doShiftIn),
      .Y2(shiftIn),

      .A3(flagCarry),
      .B3(doCarryIn),
      .Y3(cin),

      .A4(1'bz),
      .B4(1'bz),
      .Y4());

   LS86 u8
     (.A1(doSubtract),
      .B1(cin),
      .Y1(carryOrBorrowIn),

      .A2(1'bz),
      .B2(1'bz),
      .Y2(),
      .A3(1'bz),
      .B3(1'bz),
      .Y3(),
      .A4(1'bz),
      .B4(1'bz),
      .Y4());

   LS74 u9 // flags
     (
      .CLRB1(resetBar),
      .D1(coutHI),
      .CLK1(triggerC),
      .PRB1(1'b1),
      .Q1(flagCarry),
      .QB1(),

      .CLRB2(resetBar),
      .D2(areg[0]),
      .CLK2(triggerS),
      .PRB2(1'b1),
      .Q2(flagShift),
      .QB2());

   assign shifted = {shiftIn, areg[7:1]};

   LS245 u10
     (.ENB(assertBarS), .DIR(1'b0), .A(dbus), .B(shifted));

endmodule

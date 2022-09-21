
module control_NET
  (input [7:0] ir, input clk, aIsZero, flagCarry, flagShift,
   output loadBarIR,storeMemBar,
   output triggerA,triggerB,triggerX,triggerQ,triggerC,triggerS,
   output assertRom,assertRam,assertRomBar,
   output assertBarE,assertBarS,assertBarA,assertBarB,assertBarX,
   output doSubtract,doCarryIn,doShiftIn,doJumpBar
   );

   wire bit7, bit3;
   wire [2:0] source;
   wire [2:0] dest;
   assign {bit7,dest,bit3,source} = ir;

   wire loadBarPC, loadBarA, loadBarB, loadBarX, loadBarQ;

   LS138 demuxDest
     (.A(dest[0]),
      .B(dest[1]),
      .C(dest[2]),
      .G2A(1'b0),
      .G2B(1'b0),
      .G1(1'b1),
      .Y0(loadBarIR),
      .Y1(loadBarPC),
      .Y2(loadBarA),
      .Y3(loadBarB),
      .Y4(loadBarX),
      .Y5(storeMemBar),
      .Y6(loadBarQ),
      .Y7());

   wire assertBarRam;

   LS138 demuxSource
     (.A(source[0]),
      .B(source[1]),
      .C(source[2]),
      .G2A(1'b0),
      .G2B(1'b0),
      .G1(1'b1),
      .Y0(assertRomBar),
      .Y1(), // TODO: assert zero
      .Y2(assertBarA),
      .Y3(assertBarB),
      .Y4(assertBarX),
      .Y5(assertBarRam),
      .Y6(assertBarE),
      .Y7(assertBarS));

   LS32 clockGateTriggers1
     (.A1(clk),
      .A2(clk),
      .A3(clk),
      .A4(clk),
      .B1(loadBarA),
      .B2(loadBarB),
      .B3(loadBarX),
      .B4(loadBarQ),
      .Y1(triggerA),
      .Y2(triggerB),
      .Y3(triggerX),
      .Y4(triggerQ));

   LS32 clockGateTriggers2
     (.A1(clk),
      .A2(clk),
      .A3(1'bz),
      .A4(1'bz),
      .B1(assertBarE),
      .B2(assertBarS),
      .B3(1'bz),
      .B4(1'bz),
      .Y1(triggerC),
      .Y2(triggerS),
      .Y3(),
      .Y4());

   assign doSubtract = bit3;
   assign doCarryIn = bit7;
   assign doShiftIn = bit3;

   wire jumpControl;

   LS153 u2
     (.A(bit3),
      .B(bit7),

      .iG(1'b0),
      .iC3(flagShift),
      .iC2(flagCarry),
      .iC1(aIsZero),
      .iC0(1'b1),
      .iY(jumpControl),

      .jG(1'bz),
      .jC3(1'bz),
      .jC2(1'bz),
      .jC1(1'bz),
      .jC0(1'bz),
      .jY());

   // 4 nands... ?
   //assign assertRom = ~assertRomBar;
   //assign assertRam = ~assertBarRam;
   //assign doJumpBar = ~(~loadBarPC & jumpControl);

   wire loadPC;

   LS00 u3
     (
      .A1(assertRomBar),
      .B1(assertRomBar),
      .Y1(assertRom),

      .A2(assertBarRam),
      .B2(assertBarRam),
      .Y2(assertRam),

      .A3(loadBarPC),
      .B3(loadBarPC),
      .Y3(loadPC),

      .A4(loadPC),
      .B4(jumpControl),
      .Y4(doJumpBar));

endmodule

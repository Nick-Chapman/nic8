
module control_NET
  (input [7:0] ir, input clk, aIsZero, flagCarry,
   output loadBarIR,storeMemBar,
   output triggerA,triggerB,triggerX,triggerQ,
   output assertRom,assertRam,
   output assertBarE,assertBarS,assertBarA,assertBarX,
   output doSubtract,doJumpBar
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

   wire assertBarRom;
   wire assertBarRam;

   LS138 demuxSource
     (.A(source[0]),
      .B(source[1]),
      .C(source[2]),
      .G2A(1'b0),
      .G2B(1'b0),
      .G1(1'b1),
      .Y0(assertBarRom),
      .Y1(), // TODO: assert zero
      .Y2(assertBarA),
      .Y3(), //assertBarB), //TODO
      .Y4(assertBarX),
      .Y5(assertBarRam),
      .Y6(assertBarE),
      .Y7(assertBarS));

   LS32 clockGateTriggers
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

   assign doSubtract = bit3;

   // These 6 inverters can go on a single chip
   assign assertRom = ~assertBarRom;
   assign assertRam = ~assertBarRam;
   wire dontRequireZ = ~bit3;
   wire dontRequireC = ~bit7;
   wire suppressJumpForZ = ~takeJumpForZ;
   wire suppressJumpForC = ~takeJumpForC;

   wire takeJumpForZ,takeJumpForC,suppressJump;

   LS32 u2
     (.A1(dontRequireZ),
      .B1(aIsZero),
      .Y1(takeJumpForZ),

      .A2(dontRequireC),
      .B2(flagCarry),
      .Y2(takeJumpForC),

      .A3(suppressJumpForZ),
      .B3(suppressJumpForC),
      .Y3(suppressJump),

      .A4(loadBarPC),
      .B4(suppressJump),
      .Y4(doJumpBar)
      );


endmodule

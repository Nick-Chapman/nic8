
module control_NET (input [7:0] ir, input clk, aIsZero, flagCarry, output `Control controlBits);
   wire `Control controlBits =
        {loadBarIR,storeMemBar,
         triggerA,triggerB,triggerX,triggerQ,
         assertBarRom,assertBarRam,
         assertBarE,assertBarA,assertBarX,
         doSubtract,doJump};
   wire bit7, bit6;
   wire [1:0] source;
   wire [2:0] dest;
   wire indexed;
   assign {bit7,bit6,source,indexed,dest} = ir;

   wire loadBarIR, loadBarPC, loadBarA, loadBarB, loadBarX, storeMemBar, loadBarQ;

   LS138 d
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

   wire assertBarE = ~(source==1);
   wire assertBarA = ~(source==2);
   wire assertBarX = ~(source==3);
   wire immediate = ~indexed;
   wire assertBarRom = ~(source==0 &  immediate);
   wire assertBarRam = ~(source==0 & ~immediate);
   wire jumpIfZero = bit6;
   wire jumpIfCarry = bit7;
   wire unconditionalJump = bit6 && bit7;
   wire jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump;
   wire doSubtract = bit6;
   wire doJump = ~loadBarPC && jumpControl;

   wire triggerA, triggerB, triggerX, triggerQ;

   LS32 u1
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

endmodule

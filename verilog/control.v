
module control
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

   assign loadBarIR = ~(dest==0);
   wire loadPC = (dest==7);
   wire loadA = (dest==2);
   wire loadB = (dest==3);
   wire loadX = (dest==4);
   assign storeMemBar = ~(dest==5);
   wire loadQ = (dest==6);

   assign triggerA = clk | ~loadA;
   assign triggerB = clk | ~loadB;
   assign triggerX = clk | ~loadX;
   assign triggerQ = clk | ~loadQ;
   assign triggerC = clk | assertBarE;
   assign triggerS = clk | assertBarS;

   assign assertRom = (source==0);
   assign assertRomBar = ~assertRom;
   // TODO: (source==1) -- drive zero on bus
   assign assertBarA = ~(source==2);
   assign assertBarB = ~(source==3);
   assign assertBarX = ~(source==4);
   assign assertRam = (source==5);
   assign assertBarE = ~(source==6);
   assign assertBarS = ~(source==7);

   wire jumpUncond  = ~bit3 & ~bit7;
   wire jumpIfZero  =  bit3 & ~bit7;
   wire jumpIfCarry = ~bit3 &  bit7;
   wire jumpIfShift =  bit3 &  bit7;
   wire jumpControl
        = (jumpIfZero & aIsZero)
        | (jumpIfCarry & flagCarry)
        | (jumpIfShift & flagShift)
        | jumpUncond;

   assign doSubtract = bit3;
   assign doCarryIn = bit7;
   assign doShiftIn = bit3;
   assign doJumpBar = ~(loadPC && jumpControl);

endmodule

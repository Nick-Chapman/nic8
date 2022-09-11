
module control
  (input [7:0] ir, input clk, aIsZero, flagCarry,
   output loadBarIR,storeMemBar,
   output triggerA,triggerB,triggerX,triggerQ,triggerC,triggerS,
   output assertRom,assertRam,assertRomBar,
   output assertBarE,assertBarS,assertBarA,assertBarX,
   output doSubtract,doJumpBar
   );

   wire bit7, bit3;
   wire [2:0] source;
   wire [2:0] dest;
   assign {bit7,dest,bit3,source} = ir;

   assign loadBarIR = ~(dest==0);
   wire loadPC = (dest==1);
   wire loadA = (dest==2);
   wire loadB = (dest==3);
   wire loadX = (dest==4);
   assign storeMemBar = ~(dest==5); //TODO: rename
   wire loadQ = (dest==6);
   //wire loadQhi = (dest==7); //TODO

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
   //assign assertBarB = ~(source==3); //TODO: connect to register
   assign assertBarX = ~(source==4);
   assign assertRam = (source==5);
   assign assertBarE = ~(source==6);
   assign assertBarS = ~(source==7);

   wire jumpIfZero = bit3;
   wire jumpIfCarry = bit7;
   wire unconditionalJump = ~bit3 && ~bit7;
   wire jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump;

   assign doSubtract = bit3;
   assign doJumpBar = ~(loadPC && jumpControl);

endmodule

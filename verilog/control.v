
module control
  (input [7:0] ir, input clk, aIsZero, flagCarry,
   output loadBarIR,storeMemBar,
   output triggerA,triggerB,triggerX,triggerQ,
   output assertBarRom,assertBarRam,
   output assertBarE,assertBarS,assertBarA,assertBarX,
   output doSubtract,doJump
   );

   assign loadBarIR = ~loadIR;
   assign storeMemBar = ~storeMem;

   wire bit7, bit3;
   wire [2:0] source;
   wire [2:0] dest;
   assign {bit7,dest,bit3,source} = ir;
   assign assertBarRom = ~(source==0);
   // TODO: (source==1) -- drive zero on bus
   assign assertBarA = ~(source==2);
   //assign assertBarB = ~(source==3); //TODO: connect to register
   assign assertBarX = ~(source==4);
   assign assertBarRam = ~(source==5);
   assign assertBarE = ~(source==6);
   assign assertBarS = ~(source==7);
   wire loadIR = (dest==0);
   wire loadPC = (dest==1);
   wire loadA = (dest==2);
   wire loadB = (dest==3);
   wire loadX = (dest==4);
   wire storeMem = (dest==5); //TODO: rename
   wire loadQ = (dest==6);
   //wire loadQhi = (dest==7); //TODO
   wire jumpIfZero = bit3;
   wire jumpIfCarry = bit7;
   wire unconditionalJump = ~bit3 && ~bit7;
   wire jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump;
   assign doSubtract = bit3;
   assign doJump = loadPC && jumpControl;

   assign triggerA = ~(~clk & loadA);
   assign triggerB = ~(~clk & loadB);
   assign triggerX = ~(~clk & loadX);
   assign triggerQ = ~(~clk & loadQ);

endmodule

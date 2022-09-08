
`define Control [1:13]

module control (input [7:0] ir, input clk, aIsZero, flagCarry, output `Control controlBits);
   wire `Control controlBits =
        {~loadIR,~storeMem,
         triggerA,triggerB,triggerX,triggerQ,
         assertBarRom,assertBarRam,
         assertBarE,assertBarA,assertBarX,
         doSubtract,doJump};
   wire bit7, bit6;
   wire [2:0] source;
   wire [2:0] dest;
   assign {bit7,bit6,source,dest} = ir;
   wire assertBarRom = ~(source==0);
   wire assertBarRam = ~(source==1);
   wire assertBarA = ~(source==2);
   wire assertBarX = ~(source==3);
   wire assertBarE = ~(source==4);
   wire loadIR = (dest==0);
   wire loadPC = (dest==1);
   wire loadA = (dest==2);
   wire loadB = (dest==3);
   wire loadX = (dest==4);
   wire storeMem = (dest==5);
   wire loadQ = (dest==6);
   wire jumpIfZero = bit6;
   wire jumpIfCarry = bit7;
   wire unconditionalJump = bit6 && bit7;
   wire jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump;
   wire doSubtract = bit6;
   wire doJump = loadPC && jumpControl;

   wire triggerA = ~(~clk & loadA);
   wire triggerB = ~(~clk & loadB);
   wire triggerX = ~(~clk & loadX);
   wire triggerQ = ~(~clk & loadQ);

endmodule

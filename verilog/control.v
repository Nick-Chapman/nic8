
`define Control [1:13]

module control (input [7:0] ir, input clk, aIsZero, flagCarry, output `Control controlBits);
   wire `Control controlBits =
        {~loadIR,~storeMem,
         triggerA,triggerB,triggerX,triggerQ,
         assertBarM,assertBarE,assertBarA,assertBarX,
         immediate,doSubtract,doJump};
   wire bit7, bit6;
   wire [1:0] source;
   wire [2:0] dest;
   wire indexed;
   assign {bit7,bit6,source,dest,indexed} = ir;
   wire assertBarM = ~(source==0);
   wire assertBarE = ~(source==1);
   wire assertBarA = ~(source==2);
   wire assertBarX = ~(source==3);
   wire loadIR = (dest==0);
   wire loadPC = (dest==1);
   wire loadA = (dest==2);
   wire loadX = (dest==3);
   wire loadB = (dest==4);
   wire storeMem = (dest==5);
   wire loadQ = (dest==6);
   wire immediate = ~indexed;
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

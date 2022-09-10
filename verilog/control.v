
`define Control [1:14]

module control (input [7:0] ir, input clk, aIsZero, flagCarry, output `Control controlBits);
   wire `Control controlBits =
        {~loadIR,~storeMem,
         triggerA,triggerB,triggerX,triggerQ,
         assertBarRom,assertBarRam,
         assertBarE,assertBarS,assertBarA,assertBarX,
         doSubtract,doJump};
   wire bit7, bit3;
   wire [2:0] source;
   wire [2:0] dest;
   assign {bit7,dest,bit3,source} = ir;
   wire assertBarRom = ~(source==0);
   wire assertBarRam = ~(source==1);
   wire assertBarA = ~(source==2);
   wire assertBarB = ~(source==3); //TODO
   wire assertBarX = ~(source==4);
   wire assertBarE = ~(source==5);
   wire assertBarS = ~(source==6);
   wire loadIR = (dest==0);
   wire storeMem = (dest==1);
   wire loadA = (dest==2);
   wire loadB = (dest==3);
   wire loadX = (dest==4);
   wire loadPC = (dest==5);
   wire loadQ = (dest==6);
   wire jumpIfZero = bit3;
   wire jumpIfCarry = bit7;
   wire unconditionalJump = ~bit3 && ~bit7;
   wire jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump;
   wire doSubtract = bit3;
   wire doJump = loadPC && jumpControl;

   wire triggerA = ~(~clk & loadA);
   wire triggerB = ~(~clk & loadB);
   wire triggerX = ~(~clk & loadX);
   wire triggerQ = ~(~clk & loadQ);

endmodule

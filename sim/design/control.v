
module control (input [7:0] ir, input aIsZero, flagCarry, output `Control controlBits);
   wire `Control controlBits =
        {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
         assertM,assertE,assertA,assertX,
         immediate,doSubtract,doJump};
   wire bit7, bit6;
   wire [1:0] source;
   wire [2:0] dest;
   wire indexed;
   assign {bit7,bit6,source,dest,indexed} = ir;
   wire assertM = (source==0);
   wire assertE = (source==1);
   wire assertA = (source==2);
   wire assertX = (source==3);
   wire loadIR = (dest==0);
   wire loadPC = (dest==1);
   wire loadA = (dest==2);
   wire loadX = (dest==3);
   wire loadB = (dest==4);
   wire storeMem = (dest==5);
   wire doOut = (dest==6);
   wire immediate = ~indexed;
   wire jumpIfZero = bit6;
   wire jumpIfCarry = bit7;
   wire unconditionalJump = bit6 && bit7;
   wire jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump;
   wire doSubtract = bit6;
   wire doJump = loadPC && jumpControl;
endmodule

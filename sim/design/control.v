
module control (input [7:0] ir, input aIsZero, flagCarry, output `Control controlBits);
   wire `Control controlBits =
        {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
         provideMem,provideA,provideX,provideAlu,
         immediate,jumpControl,doSubtract};
   wire bit7, bit6;
   wire [1:0] source;
   wire [2:0] dest;
   wire indexed;
   assign {bit7,bit6,source,dest,indexed} = ir;
   wire provideMem = (source==0);
   wire provideAlu = (source==1);
   wire provideA = (source==2);
   wire provideX = (source==3);
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
endmodule


module LS273(input CP, MRB, input [7:0] D, output reg [7:0] Q);
   always @(negedge MRB, posedge CP) Q <= MRB ? D : 0;
endmodule


module LS161(input CLRB,CLK,A,B,C,D,ENP,LOADB,ENT,
             output reg [0:0] QD,QC,QB,QA, output CO);

   always @(posedge CLK, negedge CLRB)
      if (!CLRB)
        {QD,QC,QB,QA} <= 5'b0;
      else if (!LOADB)
        {QD,QC,QB,QA} <= {D,C,B,A};
      else if (ENP & ENT)
        {QD,QC,QB,QA} <= {QD,QC,QB,QA} + 1'b1;

   assign CO = &{QD,QC,QB,QA,ENT};

endmodule


module LS245(input ENB,DIR, inout [7:0] A,B);

   assign A = ~ENB & ~DIR ? B : 8'bz;
   assign B = ~ENB &  DIR ? A : 8'bz;

endmodule


module LS283 (E2,B2,A2,E1,A1,B1,CIN,COUT,E4,B4,A4,E3,A3,B3);

   input A1,A2,A3,A4, B1,B2,B3,B4,CIN;
   output E1,E2,E3,E4,COUT;

   assign {COUT,E4,E3,E2,E1} = {A4,A3,A2,A1} + {B4,B3,B2,B1} + CIN;

endmodule


module LS00 (A1,B1,Y1,A2,B2,Y2,Y3,A3,B3,Y4,A4,B4);

   input A1,A2,A3,A4, B1,B2,B3,B4;
   output Y1,Y2,Y3,Y4;

   assign Y1 = ~(A1 & B1);
   assign Y2 = ~(A2 & B2);
   assign Y3 = ~(A3 & B3);
   assign Y4 = ~(A4 & B4);

endmodule


module LS32 (A1,B1,Y1,A2,B2,Y2,Y3,A3,B3,Y4,A4,B4);

   input A1,A2,A3,A4, B1,B2,B3,B4;
   output Y1,Y2,Y3,Y4;

   assign Y1 = A1 | B1;
   assign Y2 = A2 | B2;
   assign Y3 = A3 | B3;
   assign Y4 = A4 | B4;

endmodule


module LS86 (A1,B1,Y1,A2,B2,Y2,Y3,A3,B3,Y4,A4,B4);

   input A1,A2,A3,A4, B1,B2,B3,B4;
   output Y1,Y2,Y3,Y4;

   assign Y1 = A1 ^ B1;
   assign Y2 = A2 ^ B2;
   assign Y3 = A3 ^ B3;
   assign Y4 = A4 ^ B4;

endmodule


module LS138 (input A,B,C,G2A,G2B,G1,
              output Y7,Y6,Y5,Y4,Y3,Y2,Y1,Y0);

   wire [1:3] select = {C,B,A};
   wire enable = G1 & ~G2A & ~G2B;

   assign Y0 = ~(enable & (select == 0));
   assign Y1 = ~(enable & (select == 1));
   assign Y2 = ~(enable & (select == 2));
   assign Y3 = ~(enable & (select == 3));
   assign Y4 = ~(enable & (select == 4));
   assign Y5 = ~(enable & (select == 5));
   assign Y6 = ~(enable & (select == 6));
   assign Y7 = ~(enable & (select == 7));

endmodule

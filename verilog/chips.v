
// 4x NAND
module LS00 (A1,B1,Y1,A2,B2,Y2,Y3,A3,B3,Y4,A4,B4);

   input A1,A2,A3,A4, B1,B2,B3,B4;
   output Y1,Y2,Y3,Y4;

   assign Y1 = ~(A1 & B1);
   assign Y2 = ~(A2 & B2);
   assign Y3 = ~(A3 & B3);
   assign Y4 = ~(A4 & B4);

endmodule

// 6x NOT
module LS04 (A1,Y1,A2,Y2,A3,Y3,Y4,A4,Y5,A5,Y6,A6);

   input A1,A2,A3,A4,A5,A6;
   output Y1,Y2,Y3,Y4,Y5,Y6;

   assign Y1 = ~A1;
   assign Y2 = ~A2;
   assign Y3 = ~A3;
   assign Y4 = ~A4;
   assign Y5 = ~A5;
   assign Y6 = ~A6;

endmodule

// 4x AND
module LS08 (A1,B1,Y1,A2,B2,Y2,Y3,A3,B3,Y4,A4,B4);

   input A1,A2,A3,A4, B1,B2,B3,B4;
   output Y1,Y2,Y3,Y4;

   assign Y1 = A1 & B1;
   assign Y2 = A2 & B2;
   assign Y3 = A3 & B3;
   assign Y4 = A4 & B4;

endmodule

// 4x OR
module LS32 (A1,B1,Y1,A2,B2,Y2,Y3,A3,B3,Y4,A4,B4);

   input A1,A2,A3,A4, B1,B2,B3,B4;
   output Y1,Y2,Y3,Y4;

   assign Y1 = A1 | B1;
   assign Y2 = A2 | B2;
   assign Y3 = A3 | B3;
   assign Y4 = A4 | B4;

endmodule

// dual FF with preset/clear
module LS74 (input CLRB1, D1, CLK1, PRB1, output reg [0:0] Q1, QB1,
             input CLRB2, D2, CLK2, PRB2, output reg [0:0] Q2, QB2
             );

   always @(posedge CLK1, negedge CLRB1, negedge PRB1)
     if (!PRB1 & CLRB1) {Q1,QB1} <= 2'b10;
     else if (PRB1 & !CLRB1) {Q1,QB1} <= 2'b01;
     else if (!PRB1 & !CLRB1) {Q1,QB1} <= 2'b11;
     else {Q1,QB1} <= {D1,~D1};

   always @(posedge CLK2, negedge CLRB2, negedge PRB2)
     if (!PRB2 & CLRB2) {Q2,QB2} <= 2'b10;
     else if (PRB2 & !CLRB2) {Q2,QB2} <= 2'b01;
     else if (!PRB2 & !CLRB2) {Q2,QB2} <= 2'b11;
     else {Q2,QB2} <= {D2,~D2};

endmodule

// 4x XOR
module LS86(A1,B1,Y1,A2,B2,Y2,Y3,A3,B3,Y4,A4,B4);

   input A1,A2,A3,A4, B1,B2,B3,B4;
   output Y1,Y2,Y3,Y4;

   assign Y1 = A1 ^ B1;
   assign Y2 = A2 ^ B2;
   assign Y3 = A3 ^ B3;
   assign Y4 = A4 ^ B4;

endmodule

// 3->8 demux
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


// Dual 1-of-4 line data selectors
module LS153
  (input A,B, // shared select
   input iG, iC3,iC2,iC1,iC0, output iY, //1(i)
   input jG, jC3,jC2,jC1,jC0, output jY  //2(j)
   );
   assign iY = ~iG & (B?(A?iC3:iC2):(A?iC1:iC0));
   assign jY = ~jG & (B?(B?jC3:jC2):(B?jC1:jC0));
endmodule


// 4 bit counter
module LS161(input CLRB,CLK,A,B,C,D,ENP,LOADB,ENT,
             output reg [0:0] QD,QC,QB,QA, output CO);

   always @(posedge CLK, negedge CLRB)
      if (~CLRB)
        {QD,QC,QB,QA} <= 5'b0;
      else if (~LOADB)
        {QD,QC,QB,QA} <= {D,C,B,A};
      else if (ENP & ENT)
        {QD,QC,QB,QA} <= {QD,QC,QB,QA} + 1'b1;

   assign CO = &{QD,QC,QB,QA,ENT};

endmodule

// 8 bit line-driver
module LS245(input ENB,DIR, inout [7:0] A,B);

   assign A = ~ENB & ~DIR ? B : 8'bz;
   assign B = ~ENB &  DIR ? A : 8'bz;

endmodule

// 8 bit FF
module LS273(input CP, MRB, input [7:0] D, output reg [7:0] Q);
   always @(negedge MRB, posedge CP) Q <= MRB ? D : 0;
endmodule

// 4 bit adder
module LS283 (E2,B2,A2,E1,A1,B1,CIN,COUT,E4,B4,A4,E3,A3,B3);

   input A1,A2,A3,A4, B1,B2,B3,B4,CIN;
   output E1,E2,E3,E4,COUT;

   assign {COUT,E4,E3,E2,E1} = {A4,A3,A2,A1} + {B4,B3,B2,B1} + CIN;

endmodule

// dual 4-input nor with strobe
module SN7425 (input A1,B1,G1,C1,D1, output Y1,
               input A2,B2,G2,C2,D2, output Y2);
   assign Y1 = (~A1 & ~B1 & ~C1 & ~D1) | ~G1;
   assign Y2 = (~A2 & ~B2 & ~C2 & ~D2) | ~G2;
endmodule

// 2048 byte ROM chip (16k bit)
module CAT28c16 (input WEB, OEB, CEB,
                 input [10:0] A,
                 inout [7:0] IO);

   reg [7:0] mem [0:255]; // only use 256 bytes

   string prog;
   initial begin
      if (! $value$plusargs("prog=%s", prog)) begin
         $display("ERROR: please specify +prog=<value>.");
         $finish;
      end
      $readmemh(prog, mem);
   end

   assign IO = ~OEB & ~CEB ? mem[A[7:0]] : 'z;

endmodule

// 2048 byte RAM chip (16k bit)
module MB8416A (input WB, GB, EB,
                input [10:0] A,
                inout [7:0] DQ);

   reg [7:0] mem [0:2047];
   assign DQ = ~EB & ~GB & WB ? mem[A] : 'z;
   always @(WB,EB) if (~EB & ~WB) mem[A] <= DQ;

endmodule

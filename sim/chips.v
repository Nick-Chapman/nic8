
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

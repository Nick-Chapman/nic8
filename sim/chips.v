
module LS273(input CP, MRB, input [7:0] D, output reg [7:0] Q);
   always @(negedge MRB, posedge CP) Q <= MRB ? D : 0;
endmodule

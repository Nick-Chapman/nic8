
module ram (input clk, outputEnable, writeEnableBar,
            input [7:0] addr,
            inout [7:0] data);

   reg [7:0] mem [0:255];

   assign data = outputEnable ? mem[addr] : 'z;

   always @(posedge clk) if (~writeEnableBar) mem[addr] = data;

endmodule

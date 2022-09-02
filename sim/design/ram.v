
module ram (input clk, outputEnable, writeEnable,
            input [7:0] addr,
            inout [7:0] data);

   reg [7:0] mem [0:255];

   string prog;
   initial begin
      for (int i = 0; i <= 255; i++) mem[i] = 'h00;
      if (! $value$plusargs("prog=%s", prog)) begin
         $display("ERROR: please specify +prog=<value>.");
         $finish;
      end
      $readmemh(prog, mem);
   end

   assign data = outputEnable ? mem[addr] : 'z;

   always @(posedge clk) if (writeEnable) mem[addr] = data;

endmodule

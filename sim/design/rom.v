
module rom (input outputEnable,
            input [7:0] addr,
            output [7:0] data);

   reg [7:0] mem [0:255];

   string prog;
   initial begin
      if (! $value$plusargs("prog=%s", prog)) begin
         $display("ERROR: please specify +prog=<value>.");
         $finish;
      end
      $readmemh(prog, mem);
   end

   assign data = outputEnable ? mem[addr] : 'z;

endmodule

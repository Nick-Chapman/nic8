
module memory (input clk, assertM, storeMem,
               input [7:0] abus,
               inout [7:0] dbus);

   reg [7:0] ram [0:255];

   string prog;
   initial begin
      for (int i = 0; i <= 255; i++) ram[i] = 'h00;
      if (! $value$plusargs("prog=%s", prog)) begin
         $display("ERROR: please specify +prog=<value>.");
         $finish;
      end
      $readmemh(prog, ram);
   end

   assign dbus = assertM ? ram[abus] : 'z;

   always @(posedge clk) if (storeMem) ram[abus] = dbus;

endmodule


`define Control [1:14]

module main;

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

   always @(posedge clk) if (storeMem) ram[abus] = dbus;

   reg clk = 0;
   always #5 clk = ~clk;

   wire [7:0] ir,pc,areg,breg,xreg,qreg;
   wire flagCarry;

   wire [7:0] aluOut = doSubtract ? areg - breg : areg + breg;
   wire       carry = doSubtract ? !(breg > areg) : (areg + breg >= 256);
   wire       aIsZero = (areg == 0);

   wire [7:0] abus = immediate?pc:xreg;
   wire [7:0] mbus = ram[abus];

   wire [7:0] dbus;
   assign dbus = provideMem ? mbus : 'z;
   assign dbus = provideA ? areg : 'z;
   assign dbus = provideX ? xreg : 'z;
   assign dbus = provideAlu ? aluOut : 'z;

   wire `Control controlBits;

   wire storeMem;
   wire provideMem,provideA,provideX,provideAlu;
   wire immediate,jumpControl,doSubtract;

   assign {storeMem,
           provideMem,provideA,provideX,provideAlu,
           immediate,jumpControl,doSubtract} = controlBits[7:14];

   monitor m (clk,ir,pc,areg,breg,xreg,qreg,controlBits,abus,dbus);

   registers r (clk,controlBits,carry,dbus,abus,mbus,
                ir,pc,areg,breg,xreg,qreg,flagCarry);

   control c (ir,aIsZero,flagCarry,controlBits);

endmodule


`define Control [1:14]

module main;

   initial $display("*nic8 simulation*");

   reg [7:0] ram [0:255];

   int steps;
   initial begin
     if (! $value$plusargs("steps=%d", steps)) begin
        $display("ERROR: please specify +steps=<value>.");
        $finish;
     end
   end

   string prog;
   initial begin
      for (int i = 0; i <= 255; i++) ram[i] = 'h00;
      if (! $value$plusargs("prog=%s", prog)) begin
         $display("ERROR: please specify +prog=<value>.");
         $finish;
      end
      $readmemh(prog, ram);
   end

   reg clk = 0;
   always #5 clk = ~clk;

   int ticks = 0;
   always @(posedge clk) ticks++;

   always @(clk) if (ticks >= steps) $finish();

   initial printBar;
   always @(posedge clk) #1 printStatus;

   task printBar;
      $display("------------------------------------------------------------");
      $display("ticks(^)   PC AR BR XR IR  MEAX IPAXBMQ  i j  OUT abus/dbus");
      $display("------------------------------------------------------------");
   endtask

   task printStatus;
      $display("%4d(%s)  %2h %2h %2h %2h %2h |%b%b%b%b|%b%b%b%b%b%b%b| %b %b {%03d}  %2h/%2h"
               ,ticks,(clk?"pos":"neg")
               ,pc,areg,breg,xreg,ir
               ,provideMem,provideAlu,provideA,provideX
               ,loadIR,loadPC,loadA,loadX,loadB,storeMem,doOut
               ,immediate,jumpControl
               ,qreg
               ,abus,dbus
               );
   endtask

   wire [7:0] ir, pc, areg, breg, xreg, qreg;
   wire flagCarry;

   always @(posedge clk) if (storeMem) ram[abus] = dbus;

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

   wire loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem;
   wire provideMem,provideA,provideX,provideAlu;
   wire immediate,jumpControl,doSubtract;

   assign {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
           provideMem,provideA,provideX,provideAlu,
           immediate,jumpControl,doSubtract
           } = controlBits;

   registers r (clk,controlBits,carry,dbus,abus,mbus,
                ir,pc,areg,breg,xreg,qreg,flagCarry);

   control c (ir,aIsZero,flagCarry,controlBits);

endmodule

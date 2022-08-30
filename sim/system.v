
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

   reg clk = 1;
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
               ,immediate,unconditionalJump
               ,qreg
               ,abus,dbus
               );
   endtask

   reg [7:0] pc = 0;
   reg [7:0] areg = 0;
   reg [7:0] breg = 0;
   reg [7:0] xreg = 0;
   reg [7:0] ir = 0;
   reg [7:0] qreg;
   reg       flagCarry = 0;

   always @(posedge clk) if (loadPC && jumpControl) pc <= abus;
   always @(posedge clk) if (immediate) pc <= pc + 1;
   always @(posedge clk) if (loadA) areg <= dbus;
   always @(posedge clk) if (loadB) breg <= dbus;
   always @(posedge clk) if (loadX) xreg <= dbus;
   always @(posedge clk) if (doOut) qreg <= dbus;
   always @(posedge clk) ir <= loadIR ? ram[abus] : 0;
   always @(posedge clk) if (storeMem) ram[abus] = dbus;
   always @(posedge clk) if (provideAlu) flagCarry = carry;

   wire      bit7, bit6;
   wire [1:0] source;
   wire [2:0] dest;
   wire       indexed;
   assign {bit7,bit6,source,dest,indexed} = ir;

   wire       provideMem = (source==0);
   wire       provideAlu = (source==1);
   wire       provideA = (source==2);
   wire       provideX = (source==3);

   wire       loadIR = (dest==0);
   wire       loadPC = (dest==1);
   wire       loadA = (dest==2);
   wire       loadX = (dest==3);
   wire       loadB = (dest==4);
   wire       storeMem = (dest==5);
   wire       doOut = (dest==6);

   wire       immediate = ~indexed;

   wire       aIsZero = (areg == 0);

   wire       jumpIfZero = bit6;
   wire       jumpIfCarry = bit7;
   wire       unconditionalJump = bit6 && bit7;

   wire       jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump;

   wire       doSubtract = bit6;
   wire [7:0] aluOut = doSubtract ? areg - breg : areg + breg;
   wire       carry = doSubtract ? !(breg > areg) : (areg + breg >= 256);

   wire [7:0] abus = immediate?pc:xreg;

   wire [7:0] dbus;
   assign dbus = provideMem ? ram[abus] : 'z;
   assign dbus = provideA ? areg : 'z;
   assign dbus = provideX ? xreg : 'z;
   assign dbus = provideAlu ? aluOut : 'z;

endmodule

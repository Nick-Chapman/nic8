
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

   reg       clk;
   initial clk = 1;
   always #5 clk = ~clk;

   int ticks = 0;
   always @(posedge clk) ticks++;

   always @(clk) if (ticks >= steps) $finish();

   int lines = 0;
   //always @(clk) #1 begin
   always @(posedge clk) #1 begin
   //always @(qreg) #1 begin
      //if (lines % 10 == 0) begin
      if (lines == 0) begin
         $display("------------------------------------------------------------");
         $display("ticks(edge)  PC AR BR XR IR  MEAX IPAXBMQ  i j  OUT abus/dbus");
         $display("------------------------------------------------------------");
      end
      lines++;
      $display("%6d(%s)  %2h %2h %2h %2h %2h |%b%b%b%b|%b%b%b%b%b%b%b| %b %b {%03d}  %2h/%2h"
               ,ticks,(clk?"pos":"neg")
               ,pc,areg,breg,xreg,ir
               ,provideMem,provideAlu,provideA,provideX
               ,loadIR,loadPC,loadA,loadX,loadB,storeMem,doOut
               ,immediate,unconditionalJump
               ,qreg
               ,abus,dbus
               );
   end

   reg [7:0] pc;
   reg [7:0] areg;
   reg [7:0] breg;
   reg [7:0] xreg;
   reg [7:0] ir;
   reg [7:0] qreg;

   initial pc = 0;
   initial areg = 0;
   initial breg = 0;
//   initial xreg = 0;
   initial ir = 0;
//   initial qreg = 0;

   always @(posedge clk) if (loadPC && jumpControl) pc <= abus;
   always @(posedge clk) if (immediate) pc <= pc + 1;

   always @(posedge clk) if (loadA) areg <= dbus;
   always @(posedge clk) if (loadB) breg <= dbus;
   always @(posedge clk) if (loadX) xreg <= dbus;
   always @(posedge clk) if (doOut) qreg <= dbus;

   always @(posedge clk) ir <= loadIR ? ram[abus] : 0;

   always @(posedge clk) if (storeMem) ram[abus] = dbus;

   wire [7:0] dbus;
   assign dbus = provideMem ? ram[abus] : 'z;
   assign dbus = provideA ? areg : 'z;
   assign dbus = provideX ? xreg : 'z;
   assign dbus = provideAlu ? aluOut : 'z; //TODO

   wire [7:0] aluOut = areg + breg;

   wire [7:0] abus = immediate?pc:xreg;

   wire      bit7, bit6;
   wire [1:0] source;
   wire [2:0] dest;
   wire       indexed;
   assign {bit7,bit6,source,dest,indexed} = ir;

   wire       immediate = ~indexed;

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

   wire       unconditionalJump = bit6 && bit7;

   wire       jumpControl = unconditionalJump; //TEMP
   //wire       jumpControl = (jumpIfZero && aIsZero) || (jumpIfCarry && flagCarry) || unconditionalJump; //TODO

endmodule

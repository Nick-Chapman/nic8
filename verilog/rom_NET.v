
module rom_NET (input outputEnableBar,
                input [7:0] addr,
                output [7:0] dbus, ibus);

   CAT28c16 rom
     (.WEB(1'b1),
      .OEB(1'b0),
      .CEB(1'b0),
      .A({3'b0,addr}),
      .IO(ibus)
      );

   LS245 lineDriver (.ENB(outputEnableBar), .DIR(1'b1), .A(ibus), .B(dbus));

endmodule

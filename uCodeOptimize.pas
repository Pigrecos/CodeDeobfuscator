unit uCodeOptimize;

interface
    uses
     System.SysUtils,Winapi.Windows,System.Classes,Vcl.ComCtrls,
     System.Generics.Collections,
     UnivDisasm.Cnsts.Instructions,
     UnivDisasm.Cnsts.Mnemonics,
     UnivDisasm.Cnsts,
     UnivDisasm.Cnsts.Regs,
     UnivDisasm.Disasm,
     uDisAsm,
     UnivDisasm.SyntaxManager;

 Function _ProcessaHandler(sFile: string;VAAddr: UInt64): string;
 procedure ParsingV2(var aListAsm: TList___DisAsm);
 procedure ParsingCodeXJcc(var aListAsm: TList___DisAsm);
 function EmuCPU(const OpCode: Word; const Size: Byte; var val1: UInt64; val2: TImmediate): Boolean;
 function IsEgualBaseReg(R1, R2 : TReg ): Boolean;


 var
  LstD_ASM : TList___DisAsm;

  fIs64 : Boolean;

implementation

function isSpReg(nReg: TReg):Boolean;
begin
     Result := (nReg = REG_RSP) or (nReg = REG_ESP) or (nReg = REG_SP)
End;

function IsEgualBaseReg(R1, R2 : TReg ): Boolean;
var
  R1bit8HFix,
  R2bit8HFix : Byte;
begin
     R1bit8HFix := 0;
     R2bit8HFix := 0;
     if (R1 = REG_AH) or (R1 = REG_CH) or (R1 = REG_DH) or (R1 = REG_BH) then  R1bit8HFix := 4;
     if (R2 = REG_AH) or (R2 = REG_CH) or (R2 = REG_DH) or (R2 = REG_BH) then  R2bit8HFix := 4;

     Result := (R1 and REGS_TYPE_MASK) = (R2 and REGS_TYPE_MASK);
     Result := Result and ((R1 and $0F) - R1bit8HFix = (R2 and $0F) - R2bit8HFix);

end;

function Compare2MemRef(m1,m2: PMemory):Boolean ;
var
 scale1,scale2 : Byte;
begin
     // scale 0 disassembla scale 1
     scale1 := m1.Scale;
     scale2 := m2.Scale;
     if scale1 = 1 then scale1 := 0;
     if scale2 = 1 then scale2 := 0;

     Result := m1.Flags = m2.Flags;
     Result := Result and (m1.BaseReg = m2.BaseReg);
     Result := Result and (m1.IndexReg = m2.IndexReg);
     Result := Result and (scale1 = scale2);
end;

function Compare2Imm(im1,im2: PImmediate):Boolean ;
begin
     Result := Cardinal(im1.Value) = Cardinal(im2.Value);
     Result := Result and (im1.Size = im2.Size);
end;

function Compare2ImmEx(im1x,im2x: PImmediate):Boolean ;
begin
     Result := CompareMem(im1x,im2x,SizeOf(TImmediate))
end;

function Compare2Reg(r1,r2: TReg):Boolean ;
begin
     Result := CompareMem(@r1,@r2,SizeOf(TReg))
end;

function Compare2Arg(arg1,arg2: TArgument):Boolean ;
begin
     // diverificare ne  caso del parsin 64 bit
     // si trovano reg. a 32bit e reg. 64bit equivalenti
     Result := True;
     if  ((arg1.Flags and AF_TYPE_MASK) = AF_REG) or  ((arg2.Flags and AF_TYPE_MASK) = AF_REG) then
     begin
         if ((arg1.Size = SIZE_QWORD) and (arg2.Size = SIZE_DWORD))or((arg1.Size = SIZE_DWORD) and (arg2.Size = SIZE_QWORD)) then
             Result := IsEgualBaseReg(arg1.Reg,arg2.Reg)
         else
             Result := Compare2Reg(arg1.Reg,arg2.Reg);
     end;
     Result := Result and  Compare2MemRef(@arg1.Mem,@arg2.Mem);

      if ((arg1.Size = SIZE_QWORD) and (arg2.Size = SIZE_DWORD))or((arg1.Size = SIZE_DWORD) and (arg2.Size = SIZE_QWORD)) then
          // Modified by Max 24/01/2016 18:02:09 per operazioni su codice a 64bit tanto sopno sempre dword in questo caso!!!!!!
          Result := Result and  (Cardinal(arg1.Imm.Value) = Cardinal(arg2.Imm.Value))
      else
          Result := Result and  Compare2Imm(@arg1.Imm,@arg2.Imm);

     Result := Result and  Compare2ImmEx(@arg1.ImmEx,@arg2.ImmEx);
     Result := Result and (arg1.Flags = arg2.Flags);
end;

function FindFoldable(Cmd : Word; isMov, isOneOp, isTwoOp : byte): Boolean;
begin

  if ( isMov <> 1) or (Cmd <> INST_ID_MOV ) then
  begin
       if (isOneOp <> 1) or ( (Cmd <> INST_ID_INC ) and (Cmd <> INST_ID_DEC ) and (Cmd <> INST_ID_NEG ) and (Cmd <> INST_ID_NOT ) ) then
         Result := (isTwoOp = 1) and ( (Cmd = INST_ID_ADD) or (Cmd = INST_ID_SUB) or (Cmd = INST_ID_XOR) or (Cmd = INST_ID_OR) or
                                  (Cmd = INST_ID_AND) or (Cmd = INST_ID_SHL) or (Cmd = INST_ID_SHR))
       else
         Result := True;

  end else
      Result  := True;
end;

function FIndIncDec(Operando : TInstruction): Boolean;
begin
     Result := ((Operando.InstID = INST_ID_ADD) or (Operando.InstID = INST_ID_SUB)) and
                (Operando.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                ( (UInt64(Operando.Arg2.Imm.Value) = UInt64(-1) ) or (Operando.Arg2.Imm.Value = 1) )

end;

function IsFoldableReg(Reg:TReg):Boolean;
var
  bReg : Word;
begin
     Result := False;
     bReg := Reg and $FF;

     if (bReg = RID_EAX) or (bReg = RID_ECX) or (bReg = RID_EDX) or (bReg = RID_EBX) or (bReg = RID_EBP)then
       Result := True;

     if Result = False then
       Result := (Reg = REG_AL) or (Reg = REG_CL) or (Reg = REG_DL) or (Reg = REG_BL) or (Reg = REG_AH) or
                 (Reg = REG_CH) or (Reg = REG_DH) or (Reg = REG_BH)
end;


function EmuCPU(const OpCode: Word; const Size: Byte; var val1: UInt64; val2: TImmediate): Boolean;   overload;
const
    UNITY       =   $20002;                             (* operand equals 1 *)
    SBYTEWORD   =   $40002;                             (* operand is in the range -128..127 mod 2^16 *)
    SBYTEDWORD  =   $80002;                             (* operand is in the range -128..127 mod 2^32 *)
    SDWORD      =   $100002;                            (* operand is in the range -0x80000000..0x7FFFFFFF *)
    UDWORD      =   $200002;                            (* operand is in the range 0..0xFFFFFFFF *)
    UBYTEWORD   =   $400002;                            (* operand is in the range 0..0xFF mod 2^16 *)
    UBYTEDWORD  =   $800002;                            (* operand is in the range 0..0xFF mod 2^32 *)
var
  nVal32 : Int32;
  nValU32: UInt32;
  nValI64: Int64;
  nValU64: UInt64;
  nB     : Byte  absolute val1;
  nW     : Word  absolute val1;
  nD     : DWord absolute val1;
  nInt   : Int64;
  ImmSize: DWORD;

     procedure CalcRealSize(n: UInt64);
     begin
          ImmSize  := 0;
          nInt     := n and $7FFFFFFFFFFFFFFF;

          if n <= High(Byte)      then  nInt :=  n and $7F;
          if n <= High(Word)      then  nInt :=  n and $7FFF;
          if n <= High(Cardinal)  then  nInt :=  n and $7FFFFFFF;

          if (Cardinal(nInt + 128) <= 255)  then  ImmSize := ImmSize or SBYTEDWORD;
          if (Cardinal(nInt) <= 255)        then  ImmSize := ImmSize or UBYTEDWORD;

          if (UInt16(nInt + 128) <= 255)    then  ImmSize := ImmSize or SBYTEWORD;
          if (UInt16(nInt) <= 255)          then  ImmSize := ImmSize or UBYTEWORD;

          if (nInt <= $FFFFFFFF)            then  ImmSize := ImmSize or UDWORD;
          if (nInt + $80000000 <= $FFFFFFFF)then  ImmSize := ImmSize or SDWORD;

     end;

     function GetI32(n : TImmediate): Int32;
     begin
          Result := 0;
          case n.Size of
              SIZE_BYTE : Result := Int8(n.Value);
              SIZE_WORD : Result := Int16(n.Value);
              SIZE_DWORD: Result := Int32(n.Value);
          end;
          if n.Size = SIZE_DWORD then
          begin
                if n.Value = 255 then
                  Result := -1;
          end;

     end;

     function GetI64(n : TImmediate): Int64;
     begin
          Result   := 0;

          case n.Size of
              SIZE_BYTE : Result := Int8(n.Value);
              SIZE_WORD : Result := Int16(n.Value);
              SIZE_DWORD: Result := Int32(n.Value);
              SIZE_QWORD: Result := Int64(n.Value);
          end;

          CalcRealSize(n.Value);

          // fix signed /unsigned constant in x64 context
          if int32(n.Value) <> int64(n.Value) then
             Result := Uint64(n.Value) ;
     end;

 begin
      Result := True;
      if fIs64 = False then
      begin
           nValU32 := UInt32(val1) ;
           nVal32  := GetI32(val2);
           // Modified by Max 24/01/2016 22:36:46  fb00000007
           Val1    := 0;
           if Size = SIZE_DWORD then
           begin
               case OpCode of
                  INST_ID_SHL:  nD  := nValU32 shl nVal32;
                  INST_ID_SHR:  nD  := nValU32 shr nVal32;
                  INST_ID_XOR:  nD  := nValU32 xor nVal32;
                  INST_ID_OR:   nD  := nValU32 or  nVal32;
                  INST_ID_SUB:  nD  := nValU32 -   nVal32;
                  INST_ID_NOT:  nD  := not nValU32;
                  INST_ID_INC:  nD  := nValU32 + 1 ;
                  INST_ID_DEC:  nD  := nValU32 - 1;
                  INST_ID_NEG:  nD  := -nValU32;
                  INST_ID_AND:  nD  := nValU32 and nVal32;
                  INST_ID_ADD:  nD  := nValU32  +  nVal32;
               else
                  Result := False
               end;
           end
           else if Size = SIZE_WORD then
           begin
               case OpCode of
                  INST_ID_SHL:  nW  := nValU32 shl nVal32;
                  INST_ID_SHR:  nW  := nValU32 shr nVal32;
                  INST_ID_XOR:  nW  := nValU32 xor nVal32;
                  INST_ID_OR:   nW  := nValU32 or  nVal32;
                  INST_ID_SUB:  nW  := nValU32 -   nVal32;
                  INST_ID_NOT:  nW  := not nValU32;
                  INST_ID_INC:  nW  := nValU32 + 1 ;
                  INST_ID_DEC:  nW  := nValU32 - 1;
                  INST_ID_NEG:  nW  := -nValU32;
                  INST_ID_AND:  nW  := nValU32 and nVal32;
                  INST_ID_ADD:  nW  := nValU32  +  nVal32;
               else
                  Result := False
               end;
           end
           else if Size = SIZE_BYTE then
           begin
               case OpCode of
                  INST_ID_SHL:  nB  := nValU32 shl nVal32;
                  INST_ID_SHR:  nB  := nValU32 shr nVal32;
                  INST_ID_XOR:  nB  := nValU32 xor nVal32;
                  INST_ID_OR:   nB  := nValU32 or  nVal32;
                  INST_ID_SUB:  nB  := nValU32 -   nVal32;
                  INST_ID_NOT:  nB  := not nValU32;
                  INST_ID_INC:  nB  := nValU32 + 1 ;
                  INST_ID_DEC:  nB  := nValU32 - 1;
                  INST_ID_NEG:  nB  := -nValU32;
                  INST_ID_AND:  nB  := nValU32 and nVal32;
                  INST_ID_ADD:  nB  := nValU32  +  nVal32;
               else
                  Result := False
               end;
           end;
      end else  {per operazioni 64bit }
      begin
           nValU64 := val1 ;
           nValI64 := GetI64(val2);
           // Modified by Max 24/01/2016 22:36:46  fb00000007
           Val1    := 0;
           if Size = SIZE_QWORD then
           begin
               case OpCode of
                  INST_ID_SHL:  val1  := nValU64 shl nValI64;
                  INST_ID_SHR:  val1  := nValU64 shr nValI64;
                  INST_ID_XOR:  val1  := nValU64 xor nValI64;
                  INST_ID_OR:   val1  := nValU64 or nValI64;
                  INST_ID_SUB:  val1  := nValU64 - nValI64;
                  INST_ID_NOT:  val1  := not nValU64;
                  INST_ID_INC:  val1  := nValU64 + 1 ;
                  INST_ID_DEC:  val1  := nValU64 - 1;
                  INST_ID_NEG:  val1  := -nValU64;
                  INST_ID_AND:  val1  := nValU64  and nValI64;
                  INST_ID_ADD:  val1  := nValU64  + nValI64;
               else
                  Result := False
               end
           end
           else if Size = SIZE_DWORD then
           begin
               case OpCode of
                  INST_ID_SHL:  nD  := nValU64 shl nValI64;
                  INST_ID_SHR:  nD  := nValU64 shr nValI64;
                  INST_ID_XOR:  nD  := nValU64 xor nValI64;
                  INST_ID_OR:   nD  := nValU64 or  nValI64;
                  INST_ID_SUB:  nD  := nValU64 -   nValI64;
                  INST_ID_NOT:  nD  := not nValU64;
                  INST_ID_INC:  nD  := nValU64 + 1 ;
                  INST_ID_DEC:  nD  := nValU64 - 1;
                  INST_ID_NEG:  nD  := -nValU64;
                  INST_ID_AND:  nD  := nValU64 and nValI64;
                  INST_ID_ADD:  nD  := nValU64  +  nValI64;
               else
                  Result := False
               end;
           end
           else if Size = SIZE_WORD then
           begin
               case OpCode of
                  INST_ID_SHL:  nW  := nValU64 shl nValI64;
                  INST_ID_SHR:  nW  := nValU64 shr nValI64;
                  INST_ID_XOR:  nW  := nValU64 xor nValI64;
                  INST_ID_OR:   nW  := nValU64 or  nValI64;
                  INST_ID_SUB:  nW  := nValU64 -   nValI64;
                  INST_ID_NOT:  nW  := not nValU64;
                  INST_ID_INC:  nW  := nValU64 + 1 ;
                  INST_ID_DEC:  nW  := nValU64 - 1;
                  INST_ID_NEG:  nW  := -nValU64;
                  INST_ID_AND:  nW  := nValU64 and nValI64;
                  INST_ID_ADD:  nW  := nValU64  +  nValI64;
               else
                  Result := False
               end;
           end
           else if Size = SIZE_BYTE then
           begin
               case OpCode of
                  INST_ID_SHL:  nB  := nValU64 shl nValI64;
                  INST_ID_SHR:  nB  := nValU64 shr nValI64;
                  INST_ID_XOR:  nB  := nValU64 xor nValI64;
                  INST_ID_OR:   nB  := nValU64 or  nValI64;
                  INST_ID_SUB:  nB  := nValU64 -   nValI64;
                  INST_ID_NOT:  nB  := not nValU64;
                  INST_ID_INC:  nB  := nValU64 + 1 ;
                  INST_ID_DEC:  nB  := nValU64 - 1;
                  INST_ID_NEG:  nB  := -nValU64;
                  INST_ID_AND:  nB  := nValU64 and nValI64;
                  INST_ID_ADD:  nB  := nValU64  +  nValI64;
               else
                  Result := False
               end;
           end;

           CalcRealSize(val1);
		   // Fix cont emu in x64 ctx
           if ((ImmSize and  UBYTEDWORD ) = UBYTEDWORD ) then
           begin
               if ((ImmSize and  UDWORD ) <> UDWORD ) then
                  val1 := val1 and $FF;
           end
           else if ((ImmSize and  UBYTEWORD ) = UBYTEWORD ) then
           begin
               if ((ImmSize and  UDWORD ) <> UDWORD ) then
                   val1 := val1 and $FF
           end;
      end;

      if Result = False then
        MessageBoxA(0, '[EmuCPU] Id Comando Sconosciuto', 'Errore', MB_OK);
 end;

function EmuCPU_EFLAGS(iCmd : Word; vSize: Byte; Operand1, Operand2: UInt64; var eFlags: NativeUInt): Byte;
var
  dwEFlags: DWORD;
begin
     Result    := 1;

     asm
        pushad
                cmp vSize,1
                je @isByte
                cmp vSize,2
                je @isWord
                jmp @isNativeuInt
        @isByte:
                mov al, Byte(Operand1)
                mov cl, Byte(Operand2)
                jmp @TipoOperaz
        @isWord:
                mov ax, Word(Operand1)
                mov cx, Word(Operand2)
                jmp @TipoOperaz
        @isNativeuInt:
                mov eax, dword(Operand1)
                mov ecx, dword(Operand2)
                jmp @TipoOperaz
         @TipoOperaz:
                cmp iCmd, INST_ID_SHL
                je @_shl
                cmp iCmd, INST_ID_SHR
                je @_shr
                cmp iCmd, INST_ID_XOR
                je @_xor
                cmp iCmd, INST_ID_OR
                je @_or
                cmp iCmd, INST_ID_SUB
                je @_sub
                cmp iCmd, INST_ID_NOT
                je @_not
                cmp iCmd, INST_ID_INC
                je @_inc
                cmp iCmd, INST_ID_DEC
                je @_dec
                cmp iCmd, INST_ID_NEG
                je @_neg
                cmp iCmd, INST_ID_AND
                je @_and
                cmp iCmd, INST_ID_ADD
                je @_add
                jmp @NotFound
         @_shl:
                cmp vSize,1
                jne @nextSize
                shl al,cl
                jmp @GetEFlags
             @nextSize:
                cmp vSize,2
                jne @nextSize1
                shl ax,cl
                jmp @GetEFlags
             @nextSize1:
                shl eax,cl
                jmp @GetEFlags
         @_shr:
                cmp vSize,1
                jne @nextSize2
                shr al,cl
                jmp @GetEFlags
             @nextSize2:
                cmp vSize,2
                jne @nextSize3
                shr ax,cl
                jmp @GetEFlags
             @nextSize3:
                shr eax,cl
                jmp @GetEFlags
         @_xor:
                cmp vSize,1
                jne @nextSize4
                xor al,cl
                jmp @GetEFlags
             @nextSize4:
                cmp vSize,2
                jne @nextSize5
                xor ax,cx
                jmp @GetEFlags
             @nextSize5:
                xor eax,ecx
                jmp @GetEFlags
         @_or:
                cmp vSize,1
                jne @nextSize6
                or al,cl
                jmp @GetEFlags
             @nextSize6:
                cmp vSize,2
                jne @nextSize7
                or ax,cx
                jmp @GetEFlags
             @nextSize7:
                or eax,ecx
                jmp @GetEFlags
         @_sub:
                cmp vSize,1
                jne @nextSize8
                sub al,cl
                jmp @GetEFlags
             @nextSize8:
                cmp vSize,2
                jne @nextSize9
                sub ax,cx
                jmp @GetEFlags
             @nextSize9:
                sub eax,ecx
                jmp @GetEFlags
         @_not:
                cmp vSize,1
                jne @nextSizeA
                not al
                jmp @GetEFlags
             @nextSizeA:
                cmp vSize,2
                jne @nextSizeB
                not ax
                jmp @GetEFlags
             @nextSizeB:
                not eax
                jmp @GetEFlags
         @_inc:
                cmp vSize,1
                jne @nextSizeC
                inc al
                jmp @GetEFlags
             @nextSizeC:
                cmp vSize,2
                jne @nextSizeD
                inc ax
                jmp @GetEFlags
             @nextSizeD:
                inc eax
                jmp @GetEFlags
         @_dec:
                cmp vSize,1
                jne @nextSizeE
                dec al
                jmp @GetEFlags
             @nextSizeE:
                cmp vSize,2
                jne @nextSizeF
                dec ax
                jmp @GetEFlags
             @nextSizeF:
                dec eax
                jmp @GetEFlags
         @_neg:
                cmp vSize,1
                jne @nextSize10
                neg al
                jmp @GetEFlags
             @nextSize10:
                cmp vSize,2
                jne @nextSize11
                neg ax
                jmp @GetEFlags
             @nextSize11:
                neg eax
                jmp @GetEFlags
         @_and:
               cmp vSize,1
                jne @nextSize12
                and al,cl
                jmp @GetEFlags
             @nextSize12:
                cmp vSize,2
                jne @nextSize13
                and ax,cx
                jmp @GetEFlags
             @nextSize13:
                and eax,ecx
                jmp @GetEFlags
         @_add:
               cmp vSize,1
                jne @nextSize14
                add al,cl
                jmp @GetEFlags
             @nextSize14:
                cmp vSize,2
                jne @nextSize15
                add ax,cx
                jmp @GetEFlags
             @nextSize15:
                add eax,ecx
                jmp @GetEFlags
         @NotFound:
                mov Result,0
                jmp @Fine
         @GetEFlags:
                Pushfd
                Pop dwEFlags
         @Fine:
                popad
     end;

     eFlags := dwEFlags;
     if Result = 0 then
        MessageBoxA(0, '[EmuCPU_EFLAGS] Id Comando Sconosciuto', 'Errore', MB_OK);
end;


function TestEFlagsJcc(jcc_Cmd, eFlags: Word): Boolean;
begin

    case jcc_Cmd of
       INST_ID_JNBE: Result := ((eFlags and 1)= 0) and (((eFlags shr 6) and 1)=0);                                         // Ja
       INST_ID_JNB:  Result := (eFlags and 1) = 0;                                                                         // Jnb
       INST_ID_JB:   Result := (eFlags and 1) = 1;                                                                         // Jb
       INST_ID_JBE:  Result := ( (eFlags and 1) = 1 ) or ( ((eFlags shr 6) and 1) = 1 );                                   // Jbe
       INST_ID_JZ:   Result := ((eFlags shr 6) and 1) = 1;                                                                 // Je
       INST_ID_JNLE: Result := ((eFlags shr 7) and 1) =  ((HI(eFlags) shr 3) and 1) and ( not ((eFlags shr 6) and 1) ) ;   // Jg
       INST_ID_JNL:  Result := ((eFlags shr 7) and 1) = ((HI(eFlags) shr 3) and 1);                                        // Jge
       INST_ID_JL:   Result := ((eFlags shr 7) and 1) <> ((HI(eFlags) shr 3) and 1);                                       // Jl
       INST_ID_JLE:  Result := ( ((eFlags shr 7) and 1) <> ((HI(eFlags) shr 3) and 1) ) or ( ((eFlags shr 6) and 1) = 1 ); // Jle
       INST_ID_JNZ:  Result := ((eFlags shr 6) and 1) = 0;                                                                 // Jnz
       INST_ID_JNO:  Result := ((HI(eFlags) shr 3) and 1) = 0;                                                             // Jno
       INST_ID_JNP:  Result := ((eFlags shr 2) and 1) = 0;                                                                 // Jnp
       INST_ID_JNS:  Result := ((eFlags shr 7) and 1) = 0;                                                                 // Jns
       INST_ID_JO:   Result := ((HI(eFlags) shr 3) and 1) = 1;                                                             // Jo
       INST_ID_JP:   Result := ((eFlags shr 2) and 1) = 1;                                                                 // Jp
       INST_ID_JS:   Result := ((eFlags shr 7) and 1) = 1;                                                                 // Js

    else begin
      MessageBoxA(0, '[TestEFlagsJcc] Id Jcc Sconosciuto', 'Errore', MB_OK);
      Result := False;
    end;

    end;
end;

Function j_GetDati_Operaz(ListaIstr: TList___DisAsm; var Istruz_Index_Start: NativeUInt; var nCostante: UInt64): Boolean;
var
  vIndexIstr, LastIdx : NativeUInt;
begin
    vIndexIstr := ListaIstr.Count ;
    LastIdx    := vIndexIstr - 1;
    while true do
    Begin
        if vIndexIstr = 0 then break;

        Dec(vIndexIstr);

        if   not FindFoldable(ListaIstr[vIndexIstr].ins.InstID, 1, 1, 1) then
        begin
            Result := False;
            Exit;
        end;

        if FindFoldable(ListaIstr[vIndexIstr].ins.InstID, 1, 0, 1) then
        begin
            if (ListaIstr[vIndexIstr].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) and (ListaIstr[vIndexIstr].ins.Arg1.Flags and AF_TYPE_MASK <> AF_MEM ) then
            begin
                Result := False;
                Exit;
            end;
            if ListaIstr[vIndexIstr].ins.Arg2.Flags and AF_TYPE_MASK <> AF_IMM  then
            begin
                Result := False;
                Exit;
            end;
            if not Compare2Arg(ListaIstr[vIndexIstr].ins.Arg1,ListaIstr[LastIdx].ins.Arg1) then
            Begin
                Result := False;
                Exit;
            end;
            if  FindFoldable(ListaIstr[vIndexIstr].ins.InstID, 1, 0, 0)  then
            begin
                nCostante           := ListaIstr[vIndexIstr].ins.Arg2.Imm.Value;
                Istruz_Index_Start := vIndexIstr;
                Result := True;
                Exit;
            end;
        end;

        if FindFoldable(ListaIstr[vIndexIstr].ins.InstID, 0, 1, 0) then
        begin
            if (ListaIstr[vIndexIstr].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) and (ListaIstr[vIndexIstr].ins.Arg1.Flags and AF_TYPE_MASK <> AF_MEM ) then
            begin
                Result := False;
                Exit;
            end;
            if not Compare2Arg(ListaIstr[vIndexIstr].ins.Arg1,ListaIstr[LastIdx].ins.Arg1) then
            Begin
                Result := False;
                Exit;
            end;
        end;
    end; // While
    Result := True;
end;


function MultiBranchSystem(var ListaIstr: TList___DisAsm; jcc_Cmd: Word; var lEsegueSalto: Boolean):Boolean;
var
  nLastItem,i     : Integer;
  RetArg1         : UInt64;
  Eflags,idxStart : NativeUInt;

  function ParsingAndGetDati: Boolean;
  begin
      ParsingCodeXJcc(ListaIstr);
      Result := j_GetDati_Operaz(ListaIstr,idxStart,UInt64(RetArg1));
      nLastItem := ListaIstr.Count - 1;

  end;

begin

     nLastItem := ListaIstr.Count - 1;

     if nLastItem < 0  then
     begin
         Result := False;
         Exit;
     end;

  // Mov
  if ( ListaIstr[nLastItem ].ins.InstID = INST_ID_MOV ) then
    Result := False
  // cmp
  else if ( ListaIstr[nLastItem ].ins.InstID = INST_ID_CMP) or ( ListaIstr[nLastItem ].ins.InstID = INST_ID_CMPXCHG) then
    Result := True
  // test
  else if ( ListaIstr[nLastItem ].ins.InstID = INST_ID_TEST )  then
    Result := True
  // or reg_1,Reg_1
  else if ( ListaIstr[nLastItem ].ins.InstID = INST_ID_OR) and
          ( ListaIstr[nLastItem ].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
          ( ListaIstr[nLastItem ].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
          ( ListaIstr[nLastItem ].ins.Arg1.Reg = ListaIstr[nLastItem].ins.Arg2.Reg) then
    Result := True
  else begin
        if j_GetDati_Operaz(ListaIstr,idxStart,UInt64(RetArg1)) or  ParsingAndGetDati then
        begin
            for i := idxStart+1 to nLastItem  do
            begin
                if ListaIstr[nLastItem - 1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG  then
                begin
                  EmuCPU_EFLAGS(ListaIstr[i].ins.InstID,
                               (ListaIstr[i].ins.Arg1.Reg shr 8) and $F,
                               RetArg1,
                               ListaIstr[i].ins.Arg2.Imm.Value,
                               Eflags);

                  EmuCPU     (ListaIstr[i].ins.InstID,
                               (ListaIstr[i].ins.Arg1.Reg shr 8) and $F,
                               RetArg1,
                               ListaIstr[i].ins.Arg2.Imm);
                end
                else begin
                  EmuCPU_EFLAGS(ListaIstr[i].ins.InstID,
                               ListaIstr[i].ins.Arg1.Size ,
                               RetArg1,
                               ListaIstr[i].ins.Arg2.Imm.Value,
                               Eflags);

                  EmuCPU     (ListaIstr[i].ins.InstID,
                               ListaIstr[i].ins.Arg1.Size,
                               RetArg1,
                               ListaIstr[i].ins.Arg2.Imm);
                end;
            end;
            lEsegueSalto := TestEFlagsJcc(jcc_Cmd, Eflags and $FFFF);
            Result := True;
        end
        else Result := False;
  end;

end;

Function _ProcessaHandler(sFile: string;VAAddr: UInt64): string;
var
  DisAsm      : TDisAsm;
  k           : Integer;
  len         : Integer;
  Indirizzo   : UInt64;
  bx86        : Boolean;
  s1          : AnsiString;
  isFollowJcc : Boolean;
  FAsmList    : TStringStream;
  emulEFlags  : DWORD;

  function IsCodExist(Addr: UInt64): Boolean;
  var
    i : Integer;
  begin
       Result := False;

       for i := 0 to LstD_ASM.Count - 1 do
       begin
            if UInt64(LstD_ASM.Items[i].VAddr) = Addr then
            begin
                Result := True;
                Break;
            end;

       end;
  end;

  function Validat_Jcc: Boolean;
  begin
       Result := False ;

       if LstD_ASM.Count < 1 then
          Exit;

       with LstD_ASM.Items[LstD_ASM.Count - 1] do
       begin
           if (ins.InstID = INST_ID_CMP) or (ins.InstID = INST_ID_CMPXCHG) then
             Result := True
           else if  ins.InstID = INST_ID_TEST then
             Result := True
           else if  (ins.InstID = INST_ID_OR) and  (ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and (ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and (ins.Arg1.Reg = ins.Arg2.Reg) then
             Result := True
       end;

  end;


begin
     Result    := 'Proc_'+ IntToHex(VAAddr,8)+'.asm';
     Indirizzo := VAAddr;

     asm
        pushfd
        pop emulEFlags
     end;

     DisAsm   := TDisAsm.Create(AnsiString(sFile));
     LstD_ASM := TList<TInsData>.Create;
     bx86     := (DisAsm.BinType = CPUX32);
     FAsmList := TStringStream.Create;
     try
       try
         while True do
         begin
               if LstD_ASM.Count > 20000 then Break;

               DisAsm.DisAssembleVA(VAAddr);

               len := DisAsm.InsData.nsize;
               if len <= 0 then len := 1;

               VAAddr  := VAAddr + len;

               if DisAsm.InsData.ins.InstID = INST_ID_RDTSC then
               begin
                    Continue;
               end
               else if (DisAsm.InsData.ins.InstID = INST_ID_CALL) and (DisAsm.InsData.ins.DstAddr.Addr <> nil) then
               begin
                    DisAsm.DisAssembleVA(UInt64(DisAsm.InsData.ins.DstAddr.Addr));
                    if (DisAsm.InsData.ins.InstID = INST_ID_JMP) and (DisAsm.InsData.ins.DstAddr.Addr <> nil) then
                    begin
                         VAAddr    := UInt64(DisAsm.InsData.ins.DstAddr.Addr);
                         Continue;
                    end else
                    begin
                         VAAddr := VAAddr - len ;
                         DisAsm.DisAssembleVA(VAAddr);
                         len := DisAsm.InsData.nsize;
                         if len <= 0 then len := 1;
                         VAAddr  := VAAddr + len;
                    end;
               end
               else if (DisAsm.InsData.ins.InstID = INST_ID_JMP) and (DisAsm.InsData.ins.DstAddr.Addr <> nil) then
               begin
                    if not IsCodExist(UInt64(DisAsm.InsData.ins.DstAddr.Addr)) then
                    begin
                         VAAddr    := UInt64(DisAsm.InsData.ins.DstAddr.Addr);
                         Continue;
                    end
                    else begin
                         //SetLength(LstD_ASM,Length(LstD_ASM)-1);
                         //Continue;
                    end;
               end
               else if (DisAsm.InsData.ins.InstID = INST_ID_JMP) and  ((DisAsm.InsData.ins.Arg1.Flags and AF_TYPE_MASK) = AF_REG) then
               begin
                    LstD_ASM.Add(DisAsm.InsData);
                    Break;
               end
               else if (DisAsm.InsData.ins.InstID = INST_ID_JMP) and  ((DisAsm.InsData.ins.Arg1.Flags and AF_TYPE_MASK) = AF_MEM) then
               begin
                    LstD_ASM.Add(DisAsm.InsData);
                    Break;
               end
               else if (DisAsm.InsData.ins.InstID = INST_ID_RET) then
               begin
                    LstD_ASM.Add(DisAsm.InsData);
                    Break;
               end
               else if (DisAsm.InsData.ins.InstID >= INST_ID_JB) and  (DisAsm.InsData.ins.InstID  <= INST_ID_JZ) then
               begin
                    isFollowJcc := False;
                    LstD_ASM.TrimExcess;
                    if not MultiBranchSystem(LstD_ASM,DisAsm.InsData.ins.InstID,isFollowJcc) then
                    begin
                         if Uint64(DisAsm.InsData.ins.DstAddr.Addr) = VAAddr then
                              isFollowJcc := True
                         else begin
                              isFollowJcc := TestEFlagsJcc(DisAsm.InsData.ins.InstID, emulEFlags and $FFFF);
                              //s1 := Format('Address: %08x Id: %d Follow Jump?',[VAAddr-len, FDisAsm.LasDisData.CmdIdx]);
                              //if MessageBox(0,PChar(s1),'MultiBranch Info',MB_YESNO) =  IDYES then isFollowJcc := True;
                         end;
                    end;

                    if isFollowJcc then
                        VAAddr    := UInt64(DisAsm.InsData.ins.DstAddr.Addr);

                    if not Validat_Jcc then
                    begin
                      // SetLength(LstD_ASM,Length(LstD_ASM)-1);
                       Continue;
                    end;

                    if IsCodExist(VAAddr) then
                       break;
               end;

               LstD_ASM.Add(DisAsm.InsData);
         end;

         FAsmList.Seek(0,soFromEnd);
         FAsmList.WriteString('; '+sFile +#13#10#13#10);
         FAsmList.WriteString(';<'+IntToHex(Indirizzo,8)+'>' +#13#10#13#10);
         if bx86 then
               FAsmList.WriteString('Bits 32'+#13#10#13#10)
         else
               FAsmList.WriteString('Bits 64'+#13#10#13#10);

         for k := 0 to LstD_ASM.Count - 1 do
         begin
             FAsmList.Seek(0,soFromEnd);
             s1 := LstD_ASM.Items[k].ins.InstStr;

             if bx86 then FAsmList.WriteString(IntToHex(UInt64(LstD_ASM.Items[k].VAddr),4)+': '+String(s1)+#13#10)
             else         FAsmList.WriteString(IntToHex(UInt64(LstD_ASM.Items[k].VAddr),8)+': '+String(s1)+#13#10) ;
         end;
         FAsmList.SaveToFile(Result);
       except
          MessageBox(0,'linea errore','errore',MB_OK);

       end;
     finally
       FAsmList.Free;
       DisAsm.Free;
     end;

end;

procedure AggiornaIstruzione(var aList : TList___DisAsm; idx: Integer);
var
  Istr  : TInsData;
  p    : PByte;
begin
     Istr    := aList[idx];
     P       := PByte(Istr.ins.InstStr);

     SyntaxManager.SyntaxDecoderArray[Istr.ins.InternalData.SyntaxID](@Istr.ins);

     Istr.ins.InstStr := PAnsiChar(P);
     aList[idx]       := Istr;
end;

(*********************)
function PeepHolePush(var aList : TList___DisAsm): Boolean;
var
  i,k  : Integer;
  Istr : TInsData;
begin
    i := 0;
    Result := True;
    try
      repeat

           if (i+1) <= aList.Count - 1 then
           begin
               // Junk_Push_01
               //-------------
               // sub  esp, 2/4
               // mov  [esp 16/32], x  ----->  push x
               // if item3 Add [esp],2/4 -----> push x
               if (aList[i].ins.InstID   = INST_ID_SUB) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                  (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                  (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and
                  (aList[i+1].ins.Arg1.Size <> SIZE_BYTE) and ( isSpReg(aList[i].ins.Arg1.Reg) ) and
                  ((aList[i].ins.Arg2.Imm.Value = 2) or (aList[i].ins.Arg2.Imm.Value = 4)or (aList[i].ins.Arg2.Imm.Value = 8)) and
                   ( isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) and
                   (aList[i+1].ins.Arg1.Mem.IndexReg = REG_NIL) and
                   (aList[i+1].ins.Arg1.Mem.Scale    = 0) and
                   (aList[i+1].ins.disp.Value        = 0) then
               begin
                     if isSpReg(aList[i+1].ins.Arg2.Reg) then
                     begin
                           if (aList[i+2].ins.InstID   = INST_ID_ADD) and
                              (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and
                              (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                              (isSpReg(aList[i+2].ins.Arg1.Mem.BaseReg)) and
                              (aList[i+2].ins.Arg1.Mem.IndexReg = REG_NIL) and
                              (aList[i+2].ins.Arg1.Mem.Scale    = 0) and
                              (aList[i+2].ins.Disp.Value        = 0) and
                             ((aList[i+2].ins.Arg2.Imm.Value = 2) or (aList[i+2].ins.Arg2.Imm.Value = 4)or  (aList[i+2].ins.Arg2.Imm.Value = 8)) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                istr.ins.Mnem   := MNEM_PUSH;
                                istr.ins.InstID := INST_ID_PUSH;
                                Istr.ins.nArg   := 1;
                                Istr.ins.Arg1   := aList[i+1].ins.Arg2;
                                if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp;

                                ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end
                     end
                     else begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                istr.ins.Mnem   := MNEM_PUSH;
                                istr.ins.InstID := INST_ID_PUSH;
                                Istr.ins.nArg   := 1;
                                Istr.ins.Arg1   := aList[i+1].ins.Arg2;
                                if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp;

                                ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                     end;
               end;
           end;
           // Junk_Push_03
           //-------------
           // push reg1/Costante
           // mov  [Esp/Sp],Reg2  ----> push reg2
           // if reg2 = esp
           // if item3 Add [esp],2/4 -----> push reg2
           if (i+1) <= aList.Count - 1 then
           begin
               if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                  ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM)) and
                  (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and
                  (aList[i+1].ins.Arg1.Size  <>  SIZE_BYTE) and
                  (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                  (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) and
                  (aList[i+1].ins.Arg1.Mem.IndexReg= REG_NIL) and
                  (aList[i+1].ins.Arg1.mem.Scale    = 0) and
                  (aList[i+1].ins.Disp.Value        = 0) then
               begin
                    if isSpReg(aList[i+1].ins.Arg2.Reg)then
                    begin
                         if (aList[i+2].ins.InstID   = INST_ID_ADD) and
                            (aList[i+2].ins.Arg1.Flags  and AF_TYPE_MASK = AF_MEM) and
                            (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK  = AF_IMM) and
                            (isSpReg(aList[i+2].ins.Arg1.Mem.BaseReg)) and
                            (aList[i+2].ins.Arg1.Mem.IndexReg= REG_NIL) and
                            (aList[i+2].ins.Arg1.mem.Scale   = 0) and
                            (aList[i+2].ins.Disp.Value       = 0) and
                            ((aList[i+2].ins.Arg2.Imm.Value = 2) or (aList[i+2].ins.Arg2.Imm.Value = 4) or (aList[i+2].ins.Arg2.Imm.Value = 8)) then
                         begin
                             // Aggiorna Istruzione
                             Istr := aList[i];
                             istr.ins.Mnem   := MNEM_PUSH;
                             istr.ins.InstID := INST_ID_PUSH;
                             Istr.ins.nArg   := 1;
                             Istr.ins.Arg1   := aList[i+1].ins.Arg2;
                             if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp;

                             ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                             aList[i] := Istr;
                             AggiornaIstruzione(aList,i);
                             aList.Delete(i+1);
                             aList.Delete(i+1);
                             aList.TrimExcess;
                         end
                    end
                    else begin
                             // Aggiorna Istruzione
                             Istr := aList[i];
                             istr.ins.Mnem   := MNEM_PUSH;
                             istr.ins.InstID := INST_ID_PUSH;
                             Istr.ins.nArg   := 1;
                             Istr.ins.Arg1   := aList[i+1].ins.Arg2;
                             if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp;

                             ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                             aList[i] := Istr;
                             AggiornaIstruzione(aList,i);
                             aList.Delete(i+1);
                             aList.TrimExcess;
                    end;
               end;
           end;
           // Modified by Max 18/01/2016 18:42:17 fb00000006
           // push r15
           // mov r15,r15
           // mov qword [rsp],0x5eecea50    push costante
           if (i+3) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV)and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1)) and (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg2)) and

                    (aList[i+2].ins.InstID   = INST_ID_MOV) and (aList[i+2].ins.Arg1.Flags  and AF_TYPE_MASK = AF_MEM) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (isSpReg(aList[i+2].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+2].ins.Arg1.Mem.IndexReg = REG_NIL) and
                    (aList[i+2].ins.Arg1.Mem.Scale    = 0) and
                    (aList[i+2].ins.Disp.Value        = 0) then
                 begin
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Arg1    := aList[i+2].ins.Arg2;
                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHolePush] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHolePop(var aList : TList___DisAsm): Boolean;
var
  i    : Integer;
  Istr : TInsData;
begin
    i := 0;
    Result := True;
    // Junk_Pop_01
    //-------------
    try
      repeat
           // mov reg,[Esp]
           // add Esp, 4/2     ---------> Pop reg
           if (i+1) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_MOV) and (aList[i+1].ins.InstID = INST_ID_ADD) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM)   and
                    (aList[i].ins.Arg2.Size <> SIZE_BYTE) and
                    (isSpReg(aList[i].ins.Arg2.Mem.BaseReg)) and
                    (aList[i].ins.Arg2.Mem.IndexReg   = REG_NIL) and
                    (aList[i].ins.Arg2.Mem.Scale      = 0) and
                    (aList[i].ins.Disp.Value          = 0) and
                    (isSpReg(aList[i+1].ins.Arg1.Reg)) and
                    (isSpReg(aList[i].ins.Arg1.Reg) = False) and
                    ((aList[i+1].ins.Arg2.Imm.Value = 2) or (aList[i+1].ins.Arg2.Imm.Value = 4) or (aList[i+1].ins.Arg2.Imm.Value = 8)) then
                 begin
                     // Aggiorna Istruzione
                     Istr := aList[i];
                     istr.ins.Mnem   := MNEM_POP;
                     istr.ins.InstID := INST_ID_POP;
                     Istr.ins.nArg   := 1;
                     ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                     aList[i] := Istr;
                     AggiornaIstruzione(aList,i);
                     aList.Delete(i+1);
                     aList.TrimExcess;
                 end;
           end;

           // mov esp/sp,[esp] -----> pop esp
           if aList.Count > 0 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_MOV)  and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM)   and
                    (aList[i].ins.Arg2.Size <> SIZE_BYTE) and (isSpReg(aList[i].ins.Arg1.Reg)) and
                    (isSpReg(aList[i].ins.Arg2.Mem.BaseReg)) and
                    (aList[i].ins.Arg2.Mem.IndexReg= REG_NIL) and
                    (aList[i].ins.Arg2.Mem.Scale   = 0) and
                    (aList[i].ins.Disp.Value       = 0) then
                 begin
                     // Aggiorna Istruzione
                     Istr := aList[i];
                     istr.ins.Mnem   := MNEM_POP;
                     istr.ins.InstID := INST_ID_POP;
                     Istr.ins.nArg   := 1;
                     ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                     aList[i] := Istr;
                     AggiornaIstruzione(aList,i);
                 end;
           end;
           Inc(i);
      Until i > aList.Count -1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHolePop] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHoleAddSub_ESp4(var aList : TList___DisAsm): Boolean;
var
  i    : Integer;
  Istr : TInsData;
begin
    i := 0;
    Result := True;
    try
      repeat
           // push reg1      che non sia esp
           // mov reg1,esp
           // add reg1,4
           // add/sub reg1, 4
           // xchg [reg],reg1 or reg,[reg1]
           // pop esp                       ------> add/sub esp, costante item3
           if (i+5) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_ADD) and
                    ((aList[i+3].ins.InstID = INST_ID_ADD) or (aList[i+3].ins.InstID = INST_ID_SUB)) and
                    (aList[i+4].ins.InstID = INST_ID_XCHG) and (aList[i+5].ins.InstID = INST_ID_POP) and
                    (aList[i].ins.Arg1.Flags   and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and (
                     aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (
                     ((aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and (aList[i+4].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG)) or
                    ((aList[i+4].ins.Arg2.Flags  and AF_TYPE_MASK = AF_MEM) and (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG))
                    ) then
                 begin
                      if (aList[i+5].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                         (isSpReg(aList[i].ins.Arg1.Reg) = False) and
                         (aList[i+1].ins.Arg1.Reg = aList[i].ins.Arg1.Reg) and
                         (isSpReg(aList[i+1].ins.Arg2.Reg)) and
                         (aList[i+2].ins.Arg1.Reg = aList[i].ins.Arg1.Reg) and
                         ((aList[i+2].ins.Arg2.Imm.Value = 4) or (aList[i+2].ins.Arg2.Imm.Value = 8))and
                         (aList[i+3].ins.Arg1.Reg = aList[i].ins.Arg1.Reg) and
                         (
                         ((isSpReg(aList[i+4].ins.Arg1.Mem.BaseReg)) and
                          (aList[i+4].ins.Arg1.Mem.IndexReg= REG_NIL) and
                          (aList[i+4].ins.Arg1.Mem.Scale    = 0) and
                          (aList[i+4].ins.Disp.Value        = 0) and
                          (aList[i+4].ins.Arg2.Reg = aList[i].ins.Arg1.Reg))
                          or
                         ((isSpReg(aList[i+4].ins.Arg2.Mem.BaseReg)) and
                          (aList[i+4].ins.Arg2.Mem.IndexReg = REG_NIL) and
                          (aList[i+4].ins.Arg2.Mem.Scale    = 0) and
                          (aList[i+4].ins.Disp.Value        = 0) and
                          (aList[i+4].ins.Arg1.Reg = aList[i].ins.Arg1.Reg))
                         )
                         then
                      begin
                           if isSpReg(aList[i+5].ins.Arg1.Reg) then
                           begin
                                // Aggiorna Istruzione
                               Istr := aList[i];
                               istr.ins.Mnem    := aList[i+3].ins.Mnem;
                               istr.ins.InstID  := aList[i+3].ins.InstID;
                               Istr.ins.nArg    := aList[i+3].ins.nArg;;
                               Istr.ins.Arg1.Reg:= aList[i+5].ins.Arg1.Reg ;
                               Istr.ins.Arg2    := aList[i+3].ins.Arg2;
                               if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+3].ins.Disp;

                               aList[i] := Istr;
                               AggiornaIstruzione(aList,i);
                               aList.Delete(i+1);
                               aList.Delete(i+1);
                               aList.Delete(i+1);
                               aList.Delete(i+1);
                               aList.Delete(i+1);
                               aList.TrimExcess;
                           end;
                      end;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleStak] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHoleXchg(var aList : TList___DisAsm): Boolean;
var
  i    : Integer;
  Istr : TInsData;
begin
    i := 0;
    Result := True;
    try
      repeat
           //Junk_Xchg_01
           //------------
           // xor A, B
           // xor B, A
           // xor A, B 		-> xchg A,B
           if (i+2) <= aList.Count - 1 then
           begin
                if (aList[i].ins.InstID   = INST_ID_XOR) and (aList[i+1].ins.InstID = INST_ID_XOR) and
                    (aList[i+2].ins.InstID = INST_ID_XOR) and
                    Compare2Arg(aList[i].ins.Arg1,  aList[i+1].ins.Arg2)  and
                    Compare2Arg(aList[i+1].ins.Arg1,aList[i+2].ins.Arg2)  and
                    Compare2Arg(aList[i].ins.Arg2,  aList[i+1].ins.Arg1)  and
                    Compare2Arg(aList[i+1].ins.Arg2,aList[i+2].ins.Arg1)   then
                 begin
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := MNEM_XCHG;
                      Istr.ins.InstID  := INST_ID_XCHG;
                      Istr.ins.Arg1    := aList[i+1].ins.Arg1;
                      Istr.ins.Arg2    := aList[i+2].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp
                      else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp ;
                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
                 // push reg1_32         non esp
                 // mov reg1_32,[esp+4]
                 // pop [esp]            -----> xchg   [esp],reg1_32
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_POP) and
                    (aList[i].ins.Arg1.Reg  = aList[i+1].ins.Arg1.Reg) and
                    (isSpReg(aList[i].ins.Arg1.Reg) = False)    and
                    (aList[i].ins.Arg1.Flags   and AF_TYPE_MASK = AF_REG)  and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG)and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM)and
                    (isSpReg(aList[i+1].ins.Arg2.Mem.BaseReg))   and
                    (aList[i+1].ins.Arg2.Mem.IndexReg= REG_NIL) and
                    (aList[i+1].ins.Arg2.Mem.Scale   = 0) and
                    (aList[i+1].ins.Disp.Value       = 4) and
                    (aList[i+2].ins.Arg1.Flags  and AF_TYPE_MASK = AF_MEM)and
                    (isSpReg(aList[i+2].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+2].ins.Arg1.Mem.IndexReg= REG_NIL) and
                    (aList[i+2].ins.Arg1.Mem.Scale    = 0) and
                    (aList[i+2].ins.Disp.Value        = 0) then
                 begin
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := MNEM_XCHG;
                      Istr.ins.InstID  := INST_ID_XCHG;
                      Istr.ins.nArg    := 2;
                      Istr.ins.Arg2    := aList[i+2].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp;
                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;
           // Junk_Xchg_05
           //-------------
           // push regx
           // mov  reg,reg2
           // mov  reg2,reg3
           // mov  reg3 ,reg
           // pop  regx         ---> xchg reg3,reg2
           if (i+4) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                     (aList[i+2].ins.InstID = INST_ID_MOV) and (aList[i+3].ins.InstID = INST_ID_MOV) and
                     (aList[i+4].ins.InstID = INST_ID_POP) and
                     (aList[i].ins.Arg1.Flags   and AF_TYPE_MASK = AF_REG) and
                     (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                     (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                     (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                     (IsEgualBaseReg(aList[i].ins.Arg1.Reg,aList[i+1].ins.Arg1.Reg)) and
                     (aList[i].ins.Arg1.Flags   = aList[i+3].ins.Arg2.Flags) and
                     (aList[i+1].ins.Arg1.Reg   = aList[i+3].ins.Arg2.Reg) and
                     (Compare2Arg(aList[i+1].ins.Arg2,aList[i+2].ins.Arg1)) and
                     (Compare2Arg(aList[i+2].ins.Arg2,aList[i+3].ins.Arg1)) and
                     (aList[i+4].ins.Arg1.Reg = aList[i].ins.Arg1.Reg) then
                 begin
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := MNEM_XCHG;
                      Istr.ins.InstID  := INST_ID_XCHG;
                      Istr.ins.nArg    := 2;
                      Istr.ins.Arg1    := aList[i+3].ins.Arg1;
                      Istr.ins.Arg2    := aList[i+2].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+3].ins.Disp
                      else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp ;

                      if (Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and (isSpReg(Istr.ins.Arg1.Mem.BaseReg)) then
                      begin
                           if      aList[i+4].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := Istr.ins.Disp.Value - 4
                           else if aList[i+4].ins.Arg1.Size = SIZE_WORD then Istr.ins.Disp.Value := Istr.ins.Disp.Value - 2
                           else if aList[i+4].ins.Arg1.Size = SIZE_QWORD then Istr.ins.Disp.Value := Istr.ins.Disp.Value - 8;
                           // fix in caso non era presente
                           if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                      end;
                      if (Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM) and (isSpReg(Istr.ins.Arg2.Mem.BaseReg)) then
                      begin
                           if      aList[i+4].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := Istr.ins.Disp.Value - 4
                           else if aList[i+4].ins.Arg1.Size = SIZE_WORD then Istr.ins.Disp.Value := Istr.ins.Disp.Value - 2
                           else if aList[i+4].ins.Arg1.Size = SIZE_QWORD then Istr.ins.Disp.Value := Istr.ins.Disp.Value - 8;
                           // fix in caso non era presente
                           if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                      end;
                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleXchg] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;


function PeepHoleMov(var aList : TList___DisAsm): Boolean;
var
  i   : Integer;
  v2  : UInt64;
  Istr: TInsData;
begin
    i  := 0;
    Result := True;

    try
      repeat
           // push x               reg         /mem16/mem32/imm
           // pop  y    ------>    mem16/mem32 /reg        /reg            mov y,x
           if (i+1) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_POP)and
                    (
                     ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and
                      (aList[i].ins.Arg1.Size <> SIZE_BYTE)) or
                     ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                      (aList[i+1].ins.Arg1.Size <> SIZE_BYTE)) or
                     ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG)) or
                     ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG)){ or
                     // Modified by Max 08/01/2016 18:44:26 aggiunto per push costate pop [mem]
                     ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and
                      (aList[i].ins.Arg1.Size <> SIZE_BYTE)) }
                    ) then
                 begin
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := MNEM_MOV;
                      Istr.ins.InstID  := INST_ID_MOV;
                      Istr.ins.nArg    := 2;
                      Istr.ins.Arg2    := aList[i].ins.Arg1;
                      Istr.ins.Arg1    := aList[i+1].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp
                      else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i].ins.Disp ;

                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;

           // ver 2 verificare
           // mov dword ds:[esp],costante
           // pop ebx                   mov ebx,costante
           if (i+1) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_MOV) and (aList[i+1].ins.InstID = INST_ID_POP) and
                    (aList[i].ins.Arg1.Flags  and AF_TYPE_MASK = AF_MEM) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (isSpReg(aList[i].ins.Arg1.Mem.BaseReg)) and
                    (aList[i].ins.Arg1.Mem.IndexReg = REG_NIL) and
                    (aList[i].ins.Arg1.Mem.Scale    = 0) and
                    (aList[i].ins.Disp.Value        = 0) then
                 begin
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := MNEM_MOV;
                      Istr.ins.InstID  := INST_ID_MOV;
                      Istr.ins.nArg    := 2;
                      Istr.ins.Arg2    := aList[i].ins.Arg2;
                      Istr.ins.Arg1    := aList[i+1].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp
                      else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i].ins.Disp ;

                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      if (aList[i-1].ins.InstID = INST_ID_PUSH) then
                           aList.Delete(i-1);
                      aList.TrimExcess;
                 end;
           end;

           // push reg/[reg]
           // sub/add/xor [esp], Costante
           // pop Reg1/[reg1]
           // add/sub/xor reg1/[reg1], costante    mov Reg1,Reg
           if (i+3) <= aList.Count - 1  then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+2].ins.InstID = INST_ID_POP) and
                    ( ((aList[i+1].ins.InstID   = INST_ID_SUB) and (aList[i+3].ins.InstID = INST_ID_ADD)) or
                      ((aList[i+1].ins.InstID   = INST_ID_ADD) and (aList[i+3].ins.InstID = INST_ID_SUB)) or
                      ((aList[i+1].ins.InstID   = INST_ID_XOR) and (aList[i+3].ins.InstID = INST_ID_XOR))  ) and

                    ( (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) ) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    ((aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    ((aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+3].ins.Arg1.Flags = aList[i+2].ins.Arg1.Flags) and
                    (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+1].ins.Arg1.Mem.IndexReg= REG_NIL) and
                    (aList[i+1].ins.Arg1.Mem.Scale   = 0 )  and
                    (aList[i+1].ins.Disp.Value       = 0 ) and
                    (Compare2Arg(aList[i+1].ins.Arg2,aList[i+3].ins.Arg2)) and
                    (Compare2Arg(aList[i+3].ins.Arg1,aList[i+2].ins.Arg1)) then
                 begin
                       // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := MNEM_MOV;
                      Istr.ins.InstID  := INST_ID_MOV;
                      Istr.ins.nArg    := 2;
                      Istr.ins.Arg2    := aList[i].ins.Arg1;
                      Istr.ins.Arg1    := aList[i+2].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp
                      else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i].ins.Disp ;

                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;
           // push reg1
           // mov  reg, costante
           // mov  regx/[regmemx]   , costante
           // xOp  regx[regmem]   , reg             mov/twoArg
           // pop reg1            ------> mov regx[item3],Costante(item2]
           if (i+4) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_MOV) and (aList[i+4].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+3].ins.InstID,1,0,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (IsEgualBaseReg(aList[i].ins.Arg1.Reg, aList[i+1].ins.Arg1.Reg)) and
                    (Compare2Arg(aList[i+2].ins.Arg1,aList[i+3].ins.Arg1)) and
                    (Compare2Arg(aList[i+3].ins.Arg2,aList[i+1].ins.Arg1)) and
                    (aList[i+4].ins.Arg1.Reg  = aList[i].ins.Arg1.Reg) then
                 begin
                      v2 := aList[i+2].ins.Arg2.Imm.Value;
                      if (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) then
                      begin
                           EmuCPU(aList[i+3].ins.InstID,
                                   aList[i+3].ins.Arg1.Size,
                                   v2,
                                   aList[i+1].ins.Arg2.Imm)
                      end
                      else begin
                           if aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                           begin
                                if isSpReg(aList[i+3].ins.Arg1.Mem.BaseReg) then
                                begin
                                     Istr := aList[i+3];
                                     if aList[i].ins.Arg1.Size      = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 4
                                     else if aList[i].ins.Arg1.Size = SIZE_WORD then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 2
                                     else if aList[i].ins.Arg1.Size = SIZE_QWORD then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 8 ;
                                     // fix in caso non era presente
                                     if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                     aList[i+3] := Istr ;
                                     AggiornaIstruzione(aList,i+3);
                                end;
                                EmuCPU(aList[i+3].ins.InstID,
                                        aList[i+3].ins.Arg1.Size,
                                        v2,
                                        aList[i+1].ins.Arg2.Imm);
                           end;
                      end;
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := MNEM_MOV;
                      Istr.ins.InstID  := INST_ID_MOV;
                      Istr.ins.nArg    := 2;
                      Istr.ins.Arg1    := aList[i+3].ins.Arg1;
                      Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                      Istr.ins.Disp    := aList[i+3].ins.Disp;
                      Istr.ins.Arg2.Imm.Value:= v2;  // aList[i].ins.Arg2.Imm      := FixSize(v2,aList[i].ins.Arg2.Flags and  $0F);
                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;
           // push reg2
           // mov  reg, costante
           // mov reg1, costante
           // xOp reg1,reg
           // xOp reg1,costante
           // pop reg2           ------> mov Reg1(item3),v1  ************
           if (i+5) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_MOV) and
                    (FindFoldable(aList[i+3].ins.InstID,1,0,1)) and
                    (FindFoldable(aList[i+4].ins.InstID,1,0,1)) and
                    (aList[i+5].ins.InstID = INST_ID_POP) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+4].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+5].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (IsEgualBaseReg(aList[i].ins.Arg1.Reg, aList[i+1].ins.Arg1.Reg)) and

                    (aList[i+2].ins.Arg1.Reg  = aList[i+3].ins.Arg1.Reg) and
                    (aList[i+3].ins.Arg1.Reg  <> aList[i+1].ins.Arg1.Reg)  and
                    (aList[i+3].ins.Arg1.Reg  = aList[i+4].ins.Arg1.Reg) and
                    (aList[i+3].ins.Arg2.Reg  = aList[i+1].ins.Arg1.Reg)  then
                 begin
                      if (aList[i+5].ins.Arg1.Reg  = aList[i].ins.Arg1.Reg) then
                      begin
                           v2 := aList[i+2].ins.Arg2.Imm.Value;
                           EmuCPU(aList[i+3].ins.InstID,
                                   aList[i+3].ins.Arg1.Size,
                                   v2,
                                   aList[i+1].ins.Arg2.Imm);
                           EmuCPU(aList[i+4].ins.InstID,
                                   aList[i+4].ins.Arg1.Size,
                                   v2,
                                   aList[i+4].ins.Arg2.Imm);

                          // Aggiorna Istruzione
                          Istr := aList[i];
                          Istr.ins.Mnem    := MNEM_MOV;
                          Istr.ins.InstID  := INST_ID_MOV;
                          Istr.ins.nArg    := 2;
                          Istr.ins.Arg1    := aList[i+3].ins.Arg1;
                          Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                          Istr.ins.Arg2.Imm.Value := v2; // aList[i].ins.Arg2.Imm      := FixSize(v2,aList[i].ins.Arg2.Flags and  $0F);;
                          aList[i]         := Istr;
                          AggiornaIstruzione(aList,i);
                          aList.Delete(i+1);
                          aList.Delete(i+1);
                          aList.Delete(i+1);
                          aList.Delete(i+1);
                          aList.Delete(i+1);
                          aList.TrimExcess;
                      end;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleMov] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHoleOneTwoArg(var aList : TList___DisAsm): Boolean;
var
  i   : Integer;
  Istr: TInsData;
begin
    i  := 0;
    Result := True;

    try
      repeat
           // push reg1
           // mov  reg,
           // xOp  reg /esp/[esp]   ,reg
           // pop  reg1                    ------> xOp    reg,
           if (i+3) <= aList.Count -1  then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+3].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+2].ins.InstID,1,0,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (IsEgualBaseReg(aList[i].ins.Arg1.Reg, aList[i+1].ins.Arg1.Reg) )and
                    (aList[i+2].ins.Arg2.Reg  = aList[i+1].ins.Arg1.Reg) and
                    (aList[i+3].ins.Arg1.Reg  = aList[i].ins.Arg1.Reg) then
                 begin
                      if (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and  (isSpReg(aList[i+2].ins.Arg1.Mem.BaseReg))then
                      begin
                           Istr := aList[i+2];
                           if      aList[i].ins.Arg1.Size = SIZE_DWORD then  Istr.ins.Disp.Value := aList[i+2].ins.Disp.Value - 4
                           else if aList[i].ins.Arg1.Size = SIZE_WORD then  Istr.ins.Disp.Value := aList[i+2].ins.Disp.Value - 2
                           else if aList[i].ins.Arg1.Size = SIZE_QWORD then  Istr.ins.Disp.Value := aList[i+2].ins.Disp.Value - 8;
                           // fix in caso non era presente
                           if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                           aList[i+2] := Istr;
                           AggiornaIstruzione(aList,i+2);
                      end;
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem    := aList[i+2].ins.Mnem;
                      Istr.ins.InstID  := aList[i+2].ins.InstID;
                      Istr.ins.nArg    := aList[i+2].ins.nArg;
                      Istr.ins.Arg1    := aList[i+2].ins.Arg1;
                      Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                      Istr.ins.Disp    := aList[i+2].ins.Disp;
                      aList[i]         := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;
           // push reg1 /[reg]
           // xOp  reg /esp/[esp]   ,reg   oneARg/TwoArg
           // pop  reg1 /[reg1]                   ------> xOp    reg,
           if (i+2) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+2].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+1].ins.InstID,0,1,1)) and
                    ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    ((aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) )and
                    ((aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+1].ins.Arg1.Mem.IndexReg = 0) and
                    (aList[i+1].ins.Arg1.Mem.Scale    = 0) and
                    (Compare2Arg(aList[i+2].ins.Arg1,aList[i].ins.Arg1))  then
                 begin
                      if (aList[i+1].ins.Disp.Value <> 1) or (aList[i+1].ins.Arg1.Size <> SIZE_BYTE) or
                         ( aList[i].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG ) {or ((aList[i].ins.Arg1.ArgRM.Registro and $0F) >= 4)}then   ///////////////////
                      begin
                           if (aList[i+1].ins.Disp.Value = 0) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                if aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG then
                                begin
                                    Istr.ins.Arg1.Reg   := REGS_GP or (aList[i+1].ins.Arg1.Size shl 8) or (aList[i+2].ins.Arg1.Reg and $FF);
                                    // Modified by Max 16/01/2016 12:24:58  fb00000003
                                    Istr.ins.Arg1.Size  := aList[i+1].ins.Arg1.Size;
                                end
                                else begin
                                     Istr.ins.Arg1.Flags := aList[i+2].ins.Arg1.Flags ;
                                     Istr.ins.Arg1.Size  := aList[i+1].ins.Arg1.Size
                                end;
                                Istr.ins.Mnem    := aList[i+1].ins.Mnem;
                                Istr.ins.InstID  := aList[i+1].ins.InstID;
                                Istr.ins.nArg    := aList[i+1].ins.nArg;
                                Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                                if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                                aList[i]         := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end
                           else if (aList[i+1].ins.Disp.Value = 4) or (aList[i+1].ins.Disp.Value = 8) or (aList[i+1].ins.Disp.Value = 2)then
                           begin
                               // Modified by Max 12/01/2016 22:36:30 (fb00000001) aggiunto per
                               // push reg
                               // one/twoopcode [esp+4]
                               // pop reg
                               if (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and  (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg))then
                               begin
                                     Istr := aList[i+1];
                                     if      aList[i+1].ins.Arg1.Size = SIZE_DWORD then  Istr.ins.Disp.Value := aList[i+1].ins.Disp.Value - 4
                                     else if aList[i+1].ins.Arg1.Size = SIZE_WORD then  Istr.ins.Disp.Value := aList[i+1].ins.Disp.Value - 2
                                     else if aList[i+1].ins.Arg1.Size = SIZE_QWORD then  Istr.ins.Disp.Value := aList[i+1].ins.Disp.Value - 8;
                                     // fix in caso non era presente
                                     if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                     aList[i+1] := Istr;
                                     AggiornaIstruzione(aList,i+1);
                                     aList.Delete(i);
                                     aList.Delete(i+1);
                                     aList.TrimExcess;
                                     Exit;
                               end;
                           end;

                      end else
                      begin
                            // Aggiorna Istruzione
                            Istr := aList[i];
                            Istr.ins.Arg1.Reg:= REGS_GP or (SIZE_BYTE shl 8) or ((aList[i+2].ins.Arg1.Reg and $FF)+ RID_AH);
                            // Modified by Max 16/01/2016 11:41:03 fb00000002
                            Istr.ins.Arg1.Size := aList[i+1].ins.Arg1.Size ;
                            Istr.ins.Mnem    := aList[i+1].ins.Mnem;
                            Istr.ins.InstID  := aList[i+1].ins.InstID;
                            Istr.ins.nArg    := aList[i+1].ins.nArg;
                            Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                            if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;
                            aList[i]         := Istr;
                            AggiornaIstruzione(aList,i);
                            aList.Delete(i+1);
                            aList.Delete(i+1);
                            aList.TrimExcess;
                      end;
                 end;
           end;
           // ver 2  --- test_fish_black.exe
           // push reg1
           // xOpOneArg  reg8 <> reg1 <> esp
           // pop  reg1                    ------> xOpOneArg  reg8
           if (i+2) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+2].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+1].ins.InstID,0,1,0)) and (Compare2Arg(aList[i+2].ins.Arg1,aList[i].ins.Arg1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG)  and (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    ( isSpReg(aList[i+1].ins.Arg1.Reg)= False) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) {and (aList[i+1].ins.Arg1.Size = SIZE_BYTE)} and
                    (IsEgualBaseReg(aList[i+1].ins.Arg1.Reg,aList[i].ins.Arg1.Reg)= False) then
                 begin
                      aList.Delete(i);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                      Continue
                 end;
           end;

           // push reg1
           // xOp  reg /esp/[esp]   ,reg
           // xOp  reg /esp/[esp]   ,reg
           // pop  reg1                    ------> xOp    reg,  ***********
           if (i+3) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+3].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+1].ins.InstID,0,1,1)) and
                    (FindFoldable(aList[i+2].ins.InstID,0,1,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    ((aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)or (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) )and
                    ((aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+1].ins.Arg1.Mem.IndexReg= REG_NIL) and
                    (aList[i+1].ins.Arg1.Mem.Scale   = 0) and
                    (isSpReg(aList[i+2].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+2].ins.Arg1.Mem.IndexReg= REG_NIL) and
                    (aList[i+2].ins.Arg1.mem.Scale   = 0) and
                    (Compare2Arg(aList[i+1].ins.Arg1,aList[i+2].ins.Arg1)) and
                    (Compare2Arg(aList[i+3].ins.Arg1,aList[i].ins.Arg1)) then
                 begin
                      if (aList[i+1].ins.Disp.Value <> 1) or (aList[i+1].ins.Arg1.Size  <> SIZE_BYTE) or
                         ( aList[i].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG )  {or((aList[i].ins.Arg1.ArgRM.Registro and $0F) >= 4)}then
                      begin
                           if (aList[i+1].ins.Disp.Value = 0) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem      := aList[i+1].ins.Mnem;
                                Istr.ins.InstID    := aList[i+1].ins.InstID;
                                Istr.ins.nArg      := aList[i+1].ins.nArg;
                                Istr.ins.Arg2      := aList[i+1].ins.Arg2;
                                if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                                Istr.ins.Arg1.Flags:= aList[i+3].ins.Arg1.Flags;

                                if aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG then
                                begin
                                     Istr.ins.Disp.Value := 0;
                                     Istr.ins.Disp.Size  := 0;
                                     Istr.ins.Arg1.Reg := REGS_GP or (aList[i+1].ins.Arg1.Size shl 8) or (aList[i+3].ins.Arg1.Reg and $FF)
                                end
                                else begin
                                     Istr.ins.Arg1.Flags := aList[i+3].ins.Arg1.Flags ;
                                     Istr.ins.Arg1.Size  := aList[i+1].ins.Arg1.Size;
                                end;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);

                                Istr := aList[i+1];
                                Istr.ins.Mnem        := aList[i+2].ins.Mnem;
                                Istr.ins.InstID      := aList[i+2].ins.InstID;
                                Istr.ins.nArg        := aList[i+2].ins.nArg;
                                Istr.ins.Arg2        := aList[i+2].ins.Arg2;
                                if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp ;

                                Istr.ins.Arg1.Flags  := aList[i+3].ins.Arg1.Flags;
                                if aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG then
                                begin
                                     Istr.ins.Disp.Value := 0;
                                     Istr.ins.Disp.Size  := 0;
                                     Istr.ins.Arg1.Reg := REGS_GP or (aList[i+2].ins.Arg1.Size shl 8) or (aList[i+3].ins.Arg1.Reg and $FF)
                                end
                                else begin
                                     Istr.ins.Arg1.Flags := aList[i+3].ins.Arg1.Flags ;
                                     Istr.ins.Arg1.Size  := aList[i+2].ins.Arg1.Size;
                                end;
                                aList[i+1] := Istr;
                                AggiornaIstruzione(aList,i+1);
                                aList.Delete(i+2);
                                aList.Delete(i+2);
                                aList.TrimExcess;
                           end;
                      end else
                      begin
                            // Aggiorna Istruzione
                            Istr := aList[i];
                            Istr.ins.Mnem        := aList[i+1].ins.Mnem;
                            Istr.ins.InstID      := aList[i+1].ins.InstID;
                            Istr.ins.nArg        := aList[i+1].ins.nArg;
                            Istr.ins.Arg1        := aList[i+1].ins.Arg1;
                            Istr.ins.Arg2        := aList[i+1].ins.Arg2;
                            Istr.ins.Disp        := aList[i+1].ins.Disp;
                            Istr.ins.Arg1.Flags  := aList[i+3].ins.Arg1.Flags;
                            Istr.ins.Arg1.Reg    := REGS_GP or (SIZE_BYTE shl 8) or (aList[i+3].ins.Arg1.Reg and $FF);
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);

                            // Aggiorna Istruzione
                            Istr := aList[i+1];
                            Istr.ins.Mnem        := aList[i+2].ins.Mnem;
                            Istr.ins.InstID      := aList[i+2].ins.InstID;
                            Istr.ins.nArg        := aList[i+2].ins.nArg;
                            Istr.ins.Arg1        := aList[i+2].ins.Arg1;
                            Istr.ins.Arg2        := aList[i+2].ins.Arg2;
                            Istr.ins.Disp        := aList[i+2].ins.Disp ;
                            Istr.ins.Arg1.Flags  := aList[i+3].ins.Arg1.Flags;
                            Istr.ins.Arg1.Reg    := REGS_GP or (SIZE_BYTE shl 8) or (aList[i+3].ins.Arg1.Reg and $FF);
                            aList[i+1] := Istr;
                            AggiornaIstruzione(aList,i+1);
                            aList.Delete(i+2);
                            aList.Delete(i+2);
                            aList.TrimExcess;
                      end;
                 end;
           end;
           ////
           // push regx
           // mov  reg1,reg2
           // xOp  reg1,
           // mov  reg2,reg1
           // pop  regx
           if (i+4) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    ((FindFoldable(aList[i+2].ins.InstID,0,1,1)) or (FIndIncDec(aList[i+2].ins))) then
                 begin
                      if (aList[i+3].ins.InstID   = INST_ID_MOV) and (aList[i+4].ins.InstID = INST_ID_POP)  and
                         (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                         (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                         (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                         (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG ) and
                         ( IsEgualBaseReg(aList[i].ins.Arg1.Reg, aList[i+1].ins.Arg1.Reg)) and
                         (aList[i+1].ins.Arg1.Reg  = aList[i+3].ins.Arg2.Reg) and
                         (aList[i+1].ins.Arg2.Reg  = aList[i+3].ins.Arg1.Reg) and
                         (aList[i+2].ins.Arg1.Reg  = aList[i+3].ins.Arg2.Reg) then
                      begin
                           if (aList[i+4].ins.Arg1.Reg  = aList[i].ins.Arg1.Reg) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem    := aList[i+2].ins.Mnem;
                                Istr.ins.InstID  := aList[i+2].ins.InstID;
                                Istr.ins.nArg    := aList[i+2].ins.nArg;
                                Istr.ins.Arg1    := aList[i+3].ins.Arg1;
                                Istr.ins.Arg2    := aList[i+2].ins.Arg2;
                                if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+3].ins.Disp
                                else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp ;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.Delete(i+1);
                                aList.Delete(i+1);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end;
                      end;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleOneTwoArg] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;


function PeepHoleRemoveAddSub(var aList : TList___DisAsm): Boolean;
var
  i   : Integer;
  Istr: TInsData;
begin
    i := 0;
    Result := True;
    try
      repeat
           // sub/add   reg  ,cost
           // add / sub reg  ,not const
           // add/sub    ,conts
           if (i+2) <= aList.Count - 1 then
           begin
                 if ((aList[i].ins.InstID   = INST_ID_ADD) and (aList[i+2].ins.InstID = INST_ID_SUB)) or
                    ((aList[i].ins.InstID   = INST_ID_SUB) and (aList[i+2].ins.InstID = INST_ID_ADD)) then
                 begin
                      if (aList[i+1].ins.InstID = INST_ID_ADD) or (aList[i+1].ins.InstID = INST_ID_SUB) then
                      begin
                           if (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK <> AF_IMM) and
                              (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                              (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1)) and
                              (Compare2Arg(aList[i+1].ins.Arg1,aList[i+2].ins.Arg1)) then
                           begin
                                if (aList[i].ins.Arg2.Imm.Value = aList[i+2].ins.Arg2.Imm.Value) then
                                begin
                                      // Aggiorna Istruzione
                                     Istr := aList[i];
                                     Istr.ins.Mnem  := aList[i+1].ins.Mnem;
                                     Istr.ins.InstID:= aList[i+1].ins.InstID;
                                     Istr.ins.Arg2  := aList[i+1].ins.Arg2;
                                     if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                          Istr.ins.Disp := aList[i+1].ins.Disp;

                                     aList[i] := Istr;
                                     AggiornaIstruzione(aList,i);
                                     aList.Delete(i+1);
                                     aList.Delete(i+1);
                                     aList.TrimExcess;
                                end;
                           end;
                      end;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleRemoveAddSub] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PepHoleMemReg(var aList : TList___DisAsm): Boolean;
var
  i   : Integer;
  v2  : UInt64;
  Istr: TInsData;
begin
    i  := 0;
    Result := True;

    try
      repeat
           // push   ECX
           // mov    ECX, Costante
           // add    ECX,EAX
           // xOp    EDX,dword ptr [ECX]      mov/one/TwoOpcode
           // pop    ECX                   ----> mov edx,[ecx]
           if (i+4) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_ADD) and (aList[i+4].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+3].ins.InstID,1,1,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    ((aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i].ins.Arg1.Reg  = aList[i+1].ins.Arg1.Reg) and
                    (aList[i+1].ins.Arg1.Reg= aList[i+2].ins.Arg1.Reg) and
                    (aList[i+2].ins.Arg1.Reg  <> aList[i+2].ins.Arg2.Reg) and
                    (((aList[i+3].ins.Arg2.Mem.BaseReg   = aList[i+2].ins.Arg1.Reg) and
                      (aList[i+3].ins.Arg2.Mem.IndexReg = REG_NIL) and (aList[i+3].ins.Arg2.Mem.Scale = 0) ) or
                     ((aList[i+3].ins.Arg1.Mem.BaseReg   = aList[i+2].ins.Arg1.Reg) and
                      (aList[i+3].ins.Arg1.Mem.IndexReg = REG_NIL) and (aList[i+3].ins.Arg1.Mem.Scale = 0)) or
                       // Modified by Max 03/01/2016 17:58:53 per supporto nuovo mov
                       ( (aList[i+3].ins.Arg2.Flags = 0) and (aList[i+3].ins.Arg1.Mem.BaseReg   = aList[i+2].ins.Arg1.Reg) and
                      (aList[i+3].ins.Arg1.Mem.IndexReg = REG_NIL) and (aList[i+3].ins.Arg1.Mem.Scale = 0))

                    ) and
                    (aList[i+4].ins.Arg1.Reg  = aList[i].ins.Arg1.Reg) then
                 begin
                      // Aggiorna Istruzione
                      if aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                      begin
                            if isSpReg(aList[i+3].ins.Arg2.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+3] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+3] := Istr;
                                 AggiornaIstruzione(aList,i+3);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem             := aList[i+3].ins.Mnem;
                            Istr.ins.InstID           := aList[i+3].ins.InstID;
                            Istr.ins.nArg             := aList[i+3].ins.nArg;
                            Istr.ins.Arg1             := aList[i+3].ins.Arg1;
                            Istr.ins.Arg2.Flags       := aList[i+3].ins.Arg2.Flags;
                            Istr.ins.Arg2.Reg         := REG_NIL;
                            Istr.ins.Arg2.Mem.BaseReg := aList[i+2].ins.Arg2.Reg;
                            Istr.ins.Arg2.Mem.IndexReg:= REG_NIL;
                            Istr.ins.Arg2.Mem.Scale   := 0;
                            Istr.ins.Disp.Value       := aList[i+1].ins.Arg2.Imm.Value;
                            Istr.ins.Disp.Size        := aList[i+1].ins.Arg2.Imm.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                      end
                      else begin
                            // Aggiorna Istruzione
                            if isSpReg(aList[i+3].ins.Arg1.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+3] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+3].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+3] := Istr;
                                 AggiornaIstruzione(aList,i+3);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem             := aList[i+3].ins.Mnem;
                            Istr.ins.InstID           := aList[i+3].ins.InstID;
                            Istr.ins.nArg             := aList[i+3].ins.nArg;
                            Istr.ins.Arg2             := aList[i+3].ins.Arg2;
                            // Modified by Max 16/01/2016 12:57:27 fb00000004
                            Istr.ins.Arg1.Size        := aList[i+3].ins.Arg1.Size;
                            Istr.ins.Arg1.Flags       := aList[i+3].ins.Arg1.Flags;
                            Istr.ins.Arg1.Reg         := REG_NIL;
                            Istr.ins.Arg1.Mem.BaseReg := aList[i+2].ins.Arg2.Reg;
                            Istr.ins.Arg1.Mem.IndexReg:= 0;
                            Istr.ins.Arg1.Mem.Scale   := 0;
                            Istr.ins.Disp.Value       := aList[i+1].ins.Arg2.Imm.Value;
                            Istr.ins.Disp.Size        := aList[i+1].ins.Arg2.Imm.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                      end;
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                 end;
           end;
           // ver 2
           // push   EDX
           // mov    EDX,EAX
           // add    EDX,0
           // add    EDX,EBX
           // xOp    ECX,dword ptr [EDX]    mov/one/TwoOpcode
           // pop    EDX                    -----> mov ecx,dword[ebx+eax]
           if (i+5) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_ADD)  and (aList[i+3].ins.InstID = INST_ID_ADD) and
                    (aList[i+5].ins.InstID = INST_ID_POP) and  (FindFoldable(aList[i+4].ins.InstID,1,1,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    ((aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+4].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (aList[i+5].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i].ins.Arg1.Reg  = aList[i+1].ins.Arg1.Reg) and
                    (aList[i+1].ins.Arg1.Reg= aList[i+2].ins.Arg1.Reg) and
                    (aList[i+2].ins.Arg1.Reg= aList[i+3].ins.Arg1.Reg) and
                    (aList[i+3].ins.Arg1.Reg  <> aList[i+3].ins.Arg2.Reg) and

                    (((aList[i+4].ins.Arg2.Mem.BaseReg   = aList[i+3].ins.Arg1.Reg) and
                      (aList[i+4].ins.Arg2.Mem.IndexReg = 0) and (aList[i+4].ins.Arg2.Mem.Scale = 0) ) or
                     ((aList[i+4].ins.Arg1.Mem.BaseReg   = aList[i+3].ins.Arg1.Reg) and
                      (aList[i+4].ins.Arg1.Mem.IndexReg = 0) and (aList[i+4].ins.Arg1.Mem.Scale = 0))
                    ) and
                    (aList[i+5].ins.Arg1.Reg  = aList[i].ins.Arg1.Reg) then
                 begin

                       if aList[i+4].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                       begin
                            // Aggiorna Istruzione
                            if isSpReg(aList[i+4].ins.Arg2.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+4] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+4] := Istr;
                                 AggiornaIstruzione(aList,i+4);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem             := aList[i+4].ins.Mnem;
                            Istr.ins.InstID           := aList[i+4].ins.InstID;
                            Istr.ins.nArg             := aList[i+4].ins.nArg;
                            Istr.ins.Arg1             := aList[i+4].ins.Arg1;
                            Istr.ins.Arg2.Reg         := REG_NIL;
                            Istr.ins.Arg2.Flags       := aList[i+4].ins.Arg2.Flags;
                            Istr.ins.Arg2.Mem.BaseReg := aList[i+3].ins.Arg2.Reg;
                            Istr.ins.Arg2.Mem.IndexReg:= aList[i+1].ins.Arg2.Reg;
                            Istr.ins.Arg2.Mem.Scale   := 0;
                            Istr.ins.Disp.Value       := aList[i+2].ins.Arg2.Imm.Value;
                            Istr.ins.Disp.Size        := aList[i+2].ins.Arg2.Imm.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                       end
                       else begin
                            // Aggiorna Istruzione
                            if isSpReg(aList[i+4].ins.Arg1.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+4] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+4] := Istr;
                                 AggiornaIstruzione(aList,i+4);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem             := aList[i+4].ins.Mnem;
                            Istr.ins.InstID           := aList[i+4].ins.InstID;
                            Istr.ins.nArg             := aList[i+4].ins.nArg;
                            Istr.ins.Arg2             := aList[i+4].ins.Arg2;
                            Istr.ins.Arg1.Reg         := REG_NIL;
                            Istr.ins.Arg1.Flags       := aList[i+4].ins.Arg1.Flags;
                            Istr.ins.Arg1.Mem.BaseReg := aList[i+3].ins.Arg2.Reg;
                            Istr.ins.Arg1.Mem.IndexReg:= aList[i+1].ins.Arg2.Reg;
                            Istr.ins.Arg1.Mem.Scale   := 0;
                            Istr.ins.Disp.Value       := aList[i+2].ins.Arg2.Imm.Value;
                            Istr.ins.Disp.Size        := aList[i+2].ins.Arg2.Imm.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                       end;
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.TrimExcess;
                 end;
           end;

           // push reg
           // mov  reg,const
           // add  reg,reg1
           // xOp     ,const
           // xOp     ,reg
           // pop  reg
           if (i+5) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_ADD) and (aList[i+5].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+3].ins.InstID,0,0,1)) and
                    (FindFoldable(aList[i+4].ins.InstID,1,0,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    ((aList[i+4].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (aList[i+5].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG)and
                    (aList[i].ins.Arg1.Reg    = aList[i+1].ins.Arg1.Reg) and
                    (aList[i+1].ins.Arg1.Reg  = aList[i+2].ins.Arg1.Reg) and
                    (aList[i+2].ins.Arg1.Reg  <> aList[i+2].ins.Arg2.Reg)and
                    (aList[i+3].ins.Arg1.Reg  = aList[i+2].ins.Arg1.Reg) and
                    (
                     ((aList[i+4].ins.Arg2.Mem.BaseReg  = aList[i+2].ins.Arg1.Reg)and
                      (aList[i+4].ins.Arg2.Mem.IndexReg = REG_NIL) and (aList[i+4].ins.Arg2.Mem.Scale = 0)) or
                     ((aList[i+4].ins.Arg1.Mem.BaseReg  = aList[i+2].ins.Arg1.Reg)and
                      (aList[i+4].ins.Arg1.Mem.IndexReg = REG_NIL) and (aList[i+4].ins.Arg1.Mem.Scale = 0))
                    ) and
                    (aList[i+5].ins.Arg1.Reg = aList[i].ins.Arg1.Reg) then
                 begin
                       v2 := aList[i+1].ins.Arg2.Imm.Value;
                       EmuCPU(aList[i+3].ins.InstID,
                               aList[i+3].ins.Arg1.Size,
                               v2,
                               aList[i+3].ins.Arg2.Imm);

                       if aList[i+4].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                       begin
                            // Aggiorna Istruzione
                            if isSpReg(aList[i+4].ins.Arg2.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+4] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+4] := Istr;
                                 AggiornaIstruzione(aList,i+4);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem              := aList[i+4].ins.Mnem;
                            Istr.ins.InstID            := aList[i+4].ins.InstID;
                            Istr.ins.nArg              := aList[i+4].ins.nArg;
                            Istr.ins.Arg1              := aList[i+4].ins.Arg1;
                            Istr.ins.Arg2.Reg          := REG_NIL;
                            Istr.ins.Arg2.Flags        := aList[i+4].ins.Arg2.Flags;
                            Istr.ins.Arg2.Mem.BaseReg  := aList[i+2].ins.Arg2.Reg;
                            Istr.ins.Arg2.Mem.IndexReg := REG_NIL;
                            Istr.ins.Arg2.Mem.Scale    := 0;
                            Istr.ins.Disp.Value        := v2;
                            Istr.ins.Disp.Size         := aList[i+3].ins.Arg1.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                       end
                       else begin
                            // Aggiorna Istruzione
                            if isSpReg(aList[i+4].ins.Arg1.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+4] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+4].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+4] := Istr;
                                 AggiornaIstruzione(aList,i+4);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem              := aList[i+4].ins.Mnem;
                            Istr.ins.InstID            := aList[i+4].ins.InstID;
                            Istr.ins.nArg              := aList[i+4].ins.nArg;
                            Istr.ins.Arg2              := aList[i+4].ins.Arg2;
                            Istr.ins.Arg1.Reg          := REG_NIL;
                            Istr.ins.Arg1.Flags        := aList[i+4].ins.Arg1.Flags;
                            Istr.ins.Arg1.Mem.BaseReg  := aList[i+2].ins.Arg2.Reg;
                            Istr.ins.Arg1.Mem.IndexReg := REG_NIL;
                            Istr.ins.Arg1.Mem.Scale    := 0;
                            Istr.ins.Disp.Value        := v2;
                            Istr.ins.Disp.Size         := aList[i+3].ins.Arg1.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                       end;
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.TrimExcess;
                 end;
           end;
           //---------------
           // push reg
           // mov  reg,reg1
           // shl  reg,const
           // add  reg,const
           // add  reg,reg1
           // xOp      [Esp]   mov/TwoOpcode
           // pop  reg
           if (i+6) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID = INST_ID_SHL) and (aList[i+3].ins.InstID = INST_ID_ADD) and
                    (aList[i+4].ins.InstID = INST_ID_ADD) and (aList[i+6].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+5].ins.InstID,1,0,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+4].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                    ((aList[i+5].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+5].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (aList[i+6].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i].ins.Arg1.Reg    = aList[i+1].ins.Arg1.Reg) and
                    (aList[i+1].ins.Arg1.Reg  = aList[i+2].ins.Arg1.Reg) and
                    (aList[i+2].ins.Arg1.Reg  = aList[i+3].ins.Arg1.Reg) and
                    (aList[i+3].ins.Arg1.Reg  = aList[i+4].ins.Arg1.Reg) and
                    (aList[i+1].ins.Arg1.Reg  <> aList[i+1].ins.Arg2.Reg)and
                    (
                     ((aList[i+5].ins.Arg2.Mem.BaseReg  = aList[i+2].ins.Arg1.Reg)and
                      (aList[i+5].ins.Arg2.Mem.IndexReg = REG_NIL) and (aList[i+5].ins.Arg2.Mem.Scale = 0)) or
                     ((aList[i+5].ins.Arg1.Mem.BaseReg  = aList[i+2].ins.Arg1.Reg)and
                      (aList[i+5].ins.Arg1.Mem.IndexReg = REG_NIL) and (aList[i+5].ins.Arg1.Mem.Scale = 0))
                    ) and
                    (aList[i+6].ins.Arg1.Reg  = aList[i].ins.Arg1.Reg) then
                 begin

                       if aList[i+5].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                       begin
                            // Aggiorna Istruzioni
                            if isSpReg(aList[i+5].ins.Arg2.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+5] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+5].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+5].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+5].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+5] := Istr;
                                 AggiornaIstruzione(aList,i+5);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem             := aList[i+5].ins.Mnem;
                            Istr.ins.InstID           := aList[i+5].ins.InstID;
                            Istr.ins.nArg             := aList[i+5].ins.nArg;
                            Istr.ins.Arg1             := aList[i+5].ins.Arg1;
                            Istr.ins.Arg2.Reg         := REG_NIL;
                            Istr.ins.Arg2.Flags       := aList[i+5].ins.Arg2.Flags;
                            Istr.ins.Arg2.Mem.BaseReg := aList[i+4].ins.Arg2.Reg;
                            Istr.ins.Arg2.Mem.IndexReg:= aList[i+1].ins.Arg2.Reg;
                            Istr.ins.Arg2.Mem.Scale   := aList[i+2].ins.Arg2.Imm.Value shl 1;
                            Istr.ins.Disp.Value       := aList[i+3].ins.Arg2.Imm.Value;
                            Istr.ins.Disp.Size        := aList[i+3].ins.Arg2.Imm.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                       end
                       else begin
                            // Aggiorna Istruzione
                            if isSpReg(aList[i+5].ins.Arg1.Mem.BaseReg) then
                            begin
                                 Istr := aList[i+5] ;
                                 if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+5].ins.Disp.Value - 4
                                 else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+5].ins.Disp.Value - 2
                                 else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+5].ins.Disp.Value - 8;
                                 // fix in caso non era presente
                                 if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                                 aList[i+5] := Istr;
                                 AggiornaIstruzione(aList,i+5);
                            end;
                            Istr := aList[i];
                            Istr.ins.Mnem             := aList[i+5].ins.Mnem;
                            Istr.ins.InstID           := aList[i+5].ins.InstID;
                            Istr.ins.nArg             := aList[i+5].ins.nArg;
                            Istr.ins.Arg2             := aList[i+5].ins.Arg2;
                            Istr.ins.Arg1.Reg         := REG_NIL;
                            Istr.ins.Arg1.Flags       := aList[i+5].ins.Arg1.Flags;
                            Istr.ins.Arg1.Mem.BaseReg := aList[i+4].ins.Arg2.Reg;
                            Istr.ins.Arg1.Mem.IndexReg:= aList[i+1].ins.Arg2.Reg;;
                            Istr.ins.Arg1.Mem.Scale   := aList[i+2].ins.Arg2.Imm.Value shl 1;
                            Istr.ins.Disp.Value       := aList[i+3].ins.Arg2.Imm.Value;
                            Istr.ins.Disp.Size        := aList[i+3].ins.Arg2.Imm.Size;
                            aList[i] := Istr;
                            AggiornaIstruzione(aList,i);
                       end;
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.TrimExcess;
                 end;
           end;
           Inc(i);
      Until i > aList.Count -1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PepHoleMemReg] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHoleXchg_01(var aList : TList___DisAsm): Boolean;
var
  i    : Integer;
  v4   : UInt64;
  Istr : TInsData;
begin
    i      := 0;
    v4     := 0;
    Result := True;
    try
      repeat
           // xchg a,b
           //  xop
           // xchg a,b(b,a)
           if (i+2) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_XCHG) and  (FindFoldable(aList[i+1].ins.InstID,0,1,1)) and
                    (aList[i+2].ins.InstID = INST_ID_XCHG) and
                    (
                     (Compare2Arg(aList[i].ins.Arg1,aList[i+2].ins.Arg1) and  Compare2Arg(aList[i].ins.Arg2,aList[i+2].ins.Arg2)) or
                     (Compare2Arg(aList[i].ins.Arg1,aList[i+2].ins.Arg2) and  Compare2Arg(aList[i].ins.Arg2,aList[i+2].ins.Arg1))
                    ) then
                 begin
                      if  (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1)= False) or
                         ( (aList[i+1].ins.Arg2.Flags <> 0) and (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK  <> AF_IMM) ) then
                      begin
                           if (Compare2Arg(aList[i].ins.Arg2,aList[i+1].ins.Arg1)) and
                              ( (aList[i+1].ins.Arg2.Flags = 0) or (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) ) then
                           begin
                               // Aggiorna Istruzione
                               Istr := aList[i];
                               Istr.ins.Mnem   := aList[i+1].ins.Mnem;
                               Istr.ins.InstID := aList[i+1].ins.InstID;
                               Istr.ins.nArg   := aList[i+1].ins.nArg;
                               Istr.ins.Arg2   := aList[i+1].ins.Arg2;
                               if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                               aList[i] := Istr;
                               AggiornaIstruzione(aList,i);
                               aList.Delete(i+1);
                               aList.Delete(i+1);
                               aList.TrimExcess;
                           end;
                      end
                      else begin
                           // Aggiorna Istruzione
                           Istr := aList[i];
                           Istr.ins.Mnem   := aList[i+1].ins.Mnem;
                           Istr.ins.InstID := aList[i+1].ins.InstID;
                           Istr.ins.nArg   := aList[i+1].ins.nArg;
                           Istr.ins.Arg1   := aList[i].ins.Arg2;
                           Istr.ins.Arg2   := aList[i+1].ins.Arg2;
                           if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i].ins.Disp
                           else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                           aList[i] := Istr;
                           AggiornaIstruzione(aList,i);
                           aList.Delete(i+1);
                           aList.Delete(i+1);
                           aList.TrimExcess;
                      end;
                 end;
                 //  push
                 //  mov
                 //  pop
                 {01A7F913 : push       SI
                  01A7F915 : mov        SI,word ptr [ESP+2]
                  01A821F4 : pop        word ptr [ESP]}
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and  (aList[i+1].ins.InstID   = INST_ID_MOV) and
                    (aList[i+2].ins.InstID   = INST_ID_POP)  then
                 begin
                      if (aList[i].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) or (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK <> AF_REG) then
                      begin
                           if (aList[i].ins.Arg1.Flags and AF_TYPE_MASK <> AF_MEM) or (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK <> AF_REG) then
                           begin
                                if (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and  (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM) and
                                   (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1)) and
                                   (aList[i+1].ins.Arg2.Flags = aList[i+2].ins.Arg1.Flags) then
                                begin
                                     if      aList[i+1].ins.Arg2.Size = SIZE_DWORD then  v4 := 4
                                     else if aList[i+1].ins.Arg2.Size = SIZE_WORD  then  v4 := 2
                                     else if aList[i+1].ins.Arg2.Size = SIZE_QWORD  then v4 := 8;
                                     if (isSpReg(aList[i+1].ins.Arg2.Mem.BaseReg) = False) or
                                        (aList[i+1].ins.Arg2.Mem.IndexReg <> aList[i+2].ins.Arg1.Mem.IndexReg ) or
                                        (aList[i+1].ins.Arg2.Mem.Scale <>    aList[i+2].ins.Arg1.Mem.Scale ) or
                                        (aList[i+1].ins.Disp.Value <>  aList[i+2].ins.Disp.Value + v4 ) then
                                     begin
                                          if (Compare2Arg(aList[i+1].ins.Arg2,aList[i+2].ins.Arg1)) and (aList[i+1].ins.Disp.Value = aList[i+2].ins.Disp.Value) then
                                          begin
                                               // Aggiorna Istruzione
                                               Istr := aList[i];
                                               Istr.ins.Mnem   := MNEM_XCHG;
                                               Istr.ins.InstID := INST_ID_XCHG;
                                               Istr.ins.nArg   := 2;
                                               Istr.ins.Arg2   := aList[i+1].ins.Arg2;
                                               if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                                   Istr.ins.Disp := aList[i+1].ins.Disp;

                                               aList[i] := Istr;
                                               AggiornaIstruzione(aList,i);
                                               aList.Delete(i+1);
                                               aList.Delete(i+1);
                                               aList.TrimExcess;
                                          end;
                                     end
                                     else begin
                                          // Aggiorna Istruzione
                                          Istr := aList[i];
                                          Istr.ins.Mnem   := MNEM_XCHG;
                                          Istr.ins.InstID := INST_ID_XCHG;
                                          Istr.ins.nArg   := 2;
                                          Istr.ins.Arg2   := aList[i+2].ins.Arg1;
                                          if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                              Istr.ins.Disp := aList[i+2].ins.Disp ;

                                          aList[i] := Istr;
                                          AggiornaIstruzione(aList,i);
                                          aList.Delete(i+1);
                                          aList.Delete(i+1);
                                          aList.TrimExcess;
                                     end;
                                end;
                           end
                           else begin
                                if (aList[i].ins.Arg1.Flags = aList[i+1].ins.Arg1.Flags) and
                                   (Compare2Arg(aList[i+1].ins.Arg2,aList[i+2].ins.Arg1)) then
                                begin
                                     if      aList[i].ins.Arg1.Size = SIZE_DWORD then  v4 := 4
                                     else if aList[i].ins.Arg1.Size = SIZE_WORD  then  v4 := 2
                                     else if aList[i].ins.Arg1.Size = SIZE_QWORD  then v4 := 8;
                                     if (isSpReg(aList[i].ins.Arg1.Mem.BaseReg) = False) or
                                        (aList[i].ins.Arg1.Mem.IndexReg <> aList[i+1].ins.Arg1.Mem.IndexReg ) or
                                        (aList[i].ins.Arg1.Mem.Scale <>    aList[i+1].ins.Arg1.Mem.Scale ) or
                                        (aList[i].ins.Disp.Value <> aList[i+1].ins.Disp.Value + v4 ) then
                                     begin
                                          if (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1)) and (aList[i].ins.Disp.Value = aList[i+1].ins.Disp.Value) then
                                          begin
                                               // Aggiorna Istruzione
                                               Istr := aList[i];
                                               Istr.ins.Mnem    := MNEM_XCHG;
                                               Istr.ins.InstID  := INST_ID_XCHG;
                                               Istr.ins.nArg    := 2;
                                               Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                                               if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                                    Istr.ins.Disp := aList[i+1].ins.Disp;

                                               aList[i] := Istr;
                                               AggiornaIstruzione(aList,i);
                                               aList.Delete(i+1);
                                               aList.Delete(i+1);
                                               aList.TrimExcess;
                                          end;
                                     end
                                     else begin
                                          // Aggiorna Istruzione
                                          Istr := aList[i];
                                          Istr.ins.Mnem    := MNEM_XCHG;
                                          Istr.ins.InstID  := INST_ID_XCHG;
                                          Istr.ins.nArg    := 2;
                                          Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                                          if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                                  Istr.ins.Disp := aList[i+1].ins.Disp;

                                          aList[i] := Istr;
                                          AggiornaIstruzione(aList,i);
                                          aList.Delete(i+1);
                                          aList.Delete(i+1);
                                          aList.TrimExcess;
                                     end;
                                end;
                           end;
                      end
                      else begin
                           if (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1)) and
                              (aList[i].ins.Disp.Value = aList[i+1].ins.Disp.Value) and
                              (Compare2Arg(aList[i+1].ins.Arg2,aList[i+2].ins.Arg1)) then
                           begin
                                if (aList[i+1].ins.Disp.Value = aList[i+2].ins.Disp.Value) then
                                begin
                                     // Aggiorna Istruzione
                                     Istr := aList[i];
                                     Istr.ins.Mnem    := MNEM_XCHG;
                                     Istr.ins.InstID  := INST_ID_XCHG;
                                     Istr.ins.nArg    := 2;
                                     Istr.ins.Arg2    := aList[i+1].ins.Arg2;
                                     if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                          Istr.ins.Disp := aList[i+1].ins.Disp;

                                     aList[i] := Istr;
                                     AggiornaIstruzione(aList,i);
                                     aList.Delete(i+1);
                                     aList.Delete(i+1);
                                     aList.TrimExcess;
                                end;
                           end;
                      end;
                 end;
           end;
           // xchg a,b
           //  xop
           // xop
           // xchg a,b(b,a)
           if (i+3) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_XCHG) and  (FindFoldable(aList[i+1].ins.InstID,0,1,1)) and
                    (FindFoldable(aList[i+2].ins.InstID,0,1,1)) and (aList[i+3].ins.InstID = INST_ID_XCHG) and
                    (
                     (Compare2Arg(aList[i].ins.Arg1,aList[i+3].ins.Arg1) and  Compare2Arg(aList[i].ins.Arg2,aList[i+3].ins.Arg2)) or
                     (Compare2Arg(aList[i].ins.Arg1,aList[i+3].ins.Arg2) and  Compare2Arg(aList[i].ins.Arg2,aList[i+3].ins.Arg1))
                    ) then
                 begin
                      if (Compare2Arg(aList[i].ins.Arg1, aList[i+1].ins.Arg1) = False) or
                         (aList[i].ins.Disp.Value <> aList[i+1].ins.Disp.Value) or
                         ( (aList[i+1].ins.Arg2.Flags <> 0) and (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK <> AF_IMM) ) or

                         (Compare2Arg(aList[i+2].ins.Arg1, aList[i+1].ins.Arg1) = False) or
                         (aList[i+2].ins.Disp.Value <> aList[i+1].ins.Disp.Value) or
                         ( (aList[i+2].ins.Arg2.Flags <> 0) and (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK <> AF_IMM) ) then
                      begin
                           if (Compare2Arg(aList[i].ins.Arg2,aList[i+1].ins.Arg1)) and
                              ( (aList[i+1].ins.Arg2.Flags = 0) or (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) ) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem  := aList[i+1].ins.Mnem;
                                Istr.ins.InstID:= aList[i+1].ins.InstID;
                                Istr.ins.Arg2  := aList[i+1].ins.Arg2;
                                if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp;

                                Istr.ins.nArg  := aList[i+1].ins.nArg;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);

                                Istr := aList[i+1];
                                Istr.ins.Mnem  := aList[i+2].ins.Mnem;
                                Istr.ins.InstID:= aList[i+2].ins.InstID;
                                Istr.ins.nArg  := aList[i+2].ins.nArg;
                                Istr.ins.Arg1  := aList[i].ins.Arg1;
                                Istr.ins.Arg2  := aList[i+2].ins.Arg2;
                                if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i].ins.Disp
                                else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp ;

                                aList[i+1] := Istr;
                                AggiornaIstruzione(aList,i+1);
                                aList.Delete(i+2);
                                aList.Delete(i+2);
                                aList.TrimExcess;
                           end;
                      end
                      else begin
                           // Aggiorna Istruzione
                           Istr := aList[i];
                           Istr.ins.Mnem  := aList[i+1].ins.Mnem;
                           Istr.ins.InstID:= aList[i+1].ins.InstID;
                           Istr.ins.nArg  := aList[i+1].ins.nArg;
                           Istr.ins.Arg1  := aList[i].ins.Arg2;
                           Istr.ins.Arg2  := aList[i+1].ins.Arg2;
                           if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i].ins.Disp
                           else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;
                           aList[i] := Istr;
                           AggiornaIstruzione(aList,i);

                           Istr := aList[i+1];
                           Istr.ins.Mnem     := aList[i+2].ins.Mnem;
                           Istr.ins.InstID   := aList[i+2].ins.InstID;
                           Istr.ins.nArg     := aList[i+2].ins.nArg;
                           Istr.ins.Arg1     := aList[i].ins.Arg1;
                           Istr.ins.Arg2     := aList[i+2].ins.Arg2;
                           if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i].ins.Disp
                           else if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp ;
                           aList[i+1] := Istr;
                           AggiornaIstruzione(aList,i+1);
                           aList.Delete(i+2);
                           aList.Delete(i+2);
                           aList.TrimExcess;
                      end;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleXchg_01] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHoleNeg(var aList : TList___DisAsm): Boolean;
var
  i     : Integer;
  Istr  : TInsData;
begin
    i := 0;
    Result := True;
    try
      repeat
           // not
           // inc/add/sub
           if (i+1) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_NOT) then
                 begin
                      if (aList[i+1].ins.InstID   = INST_ID_INC) then
                      begin
                           if ( (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                              ( Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1) ) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem     := MNEM_NEG;
                                Istr.ins.InstID   := INST_ID_NEG;
                                Istr.ins.nArg     := 1;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end;
                      end
                      else if (aList[i+1].ins.InstID   = INST_ID_ADD) then
                      begin
                           if ( (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                              ( Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1) ) and
                              (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                              (aList[i+1].ins.Arg2.imm.Value = 1)then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem     := MNEM_NEG;
                                Istr.ins.InstID   := INST_ID_NEG;
                                Istr.ins.nArg     := 1;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end;
                      end
                      else if (aList[i+1].ins.InstID   = INST_ID_SUB) then
                      begin
                            if ( (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                              ( Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1) ) and
                              (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                              ((aList[i+1].ins.Arg2.imm.Value = $FF)or (aList[i+1].ins.Arg2.imm.Value = $FFFFFFFF))then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem    := MNEM_NEG;
                                Istr.ins.InstID  := INST_ID_NEG;
                                Istr.ins.nArg    := 1;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end;
                      end;
                 end;
           end;
           // dec/add/sub
           // not
           if (i+1) <= aList.Count - 1 then
           begin
                 if (aList[i+1].ins.InstID   = INST_ID_NOT) then
                 begin
                      if (aList[i].ins.InstID   = INST_ID_DEC) then
                      begin
                           if ( (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                              ( Compare2Arg(aList[i+1].ins.Arg1,aList[i].ins.Arg1) ) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem     := MNEM_NEG;
                                Istr.ins.InstID   := INST_ID_NEG;
                                ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                                Istr.ins.nArg     := 1;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end;
                      end
                      else if (aList[i].ins.InstID   = INST_ID_ADD) then
                      begin
                           if ( (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                              ( Compare2Arg(aList[i+1].ins.Arg1,aList[i].ins.Arg1) ) and
                              (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                              ((aList[i].ins.Arg2.imm.Value = $FF)or (aList[i].ins.Arg2.imm.Value = $FFFFFFFF)) then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem   := MNEM_NEG;
                                Istr.ins.InstID := INST_ID_NEG;
                                ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                                Istr.ins.nArg   := 1;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end;
                      end
                      else if (aList[i].ins.InstID   = INST_ID_SUB) then
                      begin
                            if ( (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                              ( Compare2Arg(aList[i+1].ins.Arg1,aList[i].ins.Arg1) ) and
                              (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                              (aList[i].ins.Arg2.imm.Value = 1)then
                           begin
                                // Aggiorna Istruzione
                                Istr := aList[i];
                                Istr.ins.Mnem   := MNEM_NEG;
                                Istr.ins.InstID := INST_ID_NEG;
                                ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                                Istr.ins.nArg   := 1;
                                aList[i] := Istr;
                                AggiornaIstruzione(aList,i);
                                aList.Delete(i+1);
                                aList.TrimExcess;
                           end;
                      end;
                 end;
           end;

           // push  costante
           // sub   [mmem], registro
           // pop   registro
           if (i+2) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_SUB) and
                    (aList[i+2].ins.InstID   = INST_ID_POP) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+1].ins.Arg1.Mem.IndexReg = REG_NIL) and
                    (aList[i+1].ins.Arg1.Mem.Scale    = 0) and
                    (aList[i+1].ins.Disp.Value        = 0) and
                    (isSpReg(aList[i+1].ins.Arg2.Reg) = False) and
                    (aList[i+1].ins.Arg2.Reg = aList[i+2].ins.Arg1.Reg )then
                 begin
                       // Aggiorna Istruzione
                       Istr := aList[i];
                       Istr.ins.Mnem    := MNEM_NEG;
                       Istr.ins.InstID  := INST_ID_NEG;
                       ZeroMemory(@Istr.ins.Arg1,SizeOf(TArgument));
                       ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                       Istr.ins.nArg    := 1;
                       Istr.ins.Arg1    := aList[i+1].ins.Arg2;
                       if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp;

                       aList[i] := Istr;
                       AggiornaIstruzione(aList,i);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.TrimExcess;
                 end;
           end;
           // push 0
           // sub  [esp/sp ], reg8
           // mov   reg8,[esp/sp ]
           // add   esp, 4/2
           if (i+3) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_SUB) and
                    (aList[i+2].ins.InstID   = INST_ID_MOV) and (aList[i+3].ins.InstID   = INST_ID_ADD) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+1].ins.Arg1.Flags and  AF_TYPE_MASK = AF_MEM) and
                    (aList[i+1].ins.Arg2.Size = SIZE_BYTE) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM) and
                    (aList[i+2].ins.Arg1.Size = SIZE_BYTE) and
                    (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) and
                    (aList[i+1].ins.Arg1.Mem.IndexReg = REG_NIL) and
                    (aList[i+1].ins.Arg1.Mem.Scale    = 0) and
                    (aList[i+1].ins.Disp.Value        = 0) and
                    (isSpReg(aList[i+1].ins.Arg2.Reg) = False) and
                    (aList[i+1].ins.Arg2.Reg = aList[i+2].ins.Arg1.Reg ) and
                    (isSpReg(aList[i+2].ins.Arg2.Mem.BaseReg)) and
                    (aList[i+2].ins.Arg2.Mem.IndexReg = REG_NIL) and
                    (aList[i+2].ins.Arg2.Mem.Scale    = 0) and
                    (aList[i+2].ins.Disp.Value        = 0) and
                    (isSpReg(aList[i+3].ins.Arg1.Reg)) and
                    ( ((aList[i+3].ins.Arg2.Imm.Value = 2) and (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i].ins.Arg1.Imm.Value = 0)) or
                      ((aList[i+3].ins.Arg2.Imm.Value = 4) and (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i].ins.Arg1.Imm.Value = 0)) or
                      ((aList[i+3].ins.Arg2.Imm.Value = 8) and (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and (aList[i].ins.Arg1.Imm.Value = 0))
                    )
                    then
                 begin
                       // Aggiorna Istruzione
                       Istr := aList[i];
                       Istr.ins.Mnem   := MNEM_NEG;
                       Istr.ins.InstID := INST_ID_NEG;
                       ZeroMemory(@Istr.ins.Arg1,SizeOf(TArgument));
                       ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                       Istr.ins.Arg1   := aList[i+1].ins.Arg2;
                       if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                       aList[i] := Istr;
                       AggiornaIstruzione(aList,i);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.Delete(i+1);
                       aList.TrimExcess;
                 end;
           end;
           // push  reg1
           // mov   reg,cost
           // sub   reg, regx/mem
           // mov/xchg  reg/mem, reg,mem
           // pop  reg1
           if (i+4) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i+2].ins.InstID   = INST_ID_SUB) and
                    ((aList[i+3].ins.InstID   = INST_ID_MOV) or (aList[i+3].ins.InstID   = INST_ID_XCHG)) and
                    (aList[i+4].ins.InstID   = INST_ID_POP) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    ((aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) or (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM)) and
                    ((aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) or (aList[i+3].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)) and
                    ((aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) or (aList[i+3].ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM)) and
                    (aList[i+4].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (IsEgualBaseReg(aList[i].ins.Arg1.Reg,aList[i+1].ins.Arg1.Reg)) and
                    (Compare2Arg(aList[i+1].ins.Arg1,aList[i+2].ins.Arg1)) and
                    (Compare2Arg(aList[i+2].ins.Arg1,aList[i+2].ins.Arg2)= False) and
                    (aList[i].ins.Arg1.Reg = aList[i+4].ins.Arg1.Reg) then
                 begin
                      if (aList[i+3].ins.InstID  <> INST_ID_MOV) or
                         (aList[i+2].ins.Disp.Value <> aList[i+3].ins.Disp.Value) or
                         ( Compare2Arg(aList[i+2].ins.Arg2,aList[i+3].ins.Arg1) = False )or
                         (aList[i+3].ins.Arg2.Reg <> aList[i+2].ins.Arg1.Reg )then
                      begin
                           if (aList[i+3].ins.InstID   = INST_ID_XCHG) and
                              ( (Compare2Arg(aList[i+2].ins.Arg2,aList[i+3].ins.Arg1)) and
                                (aList[i+3].ins.Arg2.Reg = aList[i+2].ins.Arg1.Reg ) ) or
                              ( (Compare2Arg(aList[i+2].ins.Arg2,aList[i+3].ins.Arg2)) and
                                (aList[i+3].ins.Arg1.Reg = aList[i+2].ins.Arg1.Reg ) )
                           then
                           begin
                                 // Aggiorna Istruzione
                                 Istr := aList[i];
                                 Istr.ins.Mnem    := MNEM_NEG;
                                 Istr.ins.InstID  := INST_ID_NEG;
                                 ZeroMemory(@Istr.ins.Arg1,SizeOf(TArgument));
                                 ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                                 Istr.ins.Arg1    := aList[i+2].ins.Arg2;
                                 if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp ;

                                 aList[i] := Istr;
                                 AggiornaIstruzione(aList,i);
                                 aList.Delete(i+1);
                                 aList.Delete(i+1);
                                 aList.Delete(i+1);
                                 aList.Delete(i+1);
                                 aList.TrimExcess;
                           end;
                      end
                      else begin
                           // Aggiorna Istruzione
                           Istr := aList[i];
                           Istr.ins.Mnem    := MNEM_NEG;
                           Istr.ins.InstID  := INST_ID_NEG;
                           ZeroMemory(@Istr.ins.Arg1,SizeOf(TArgument));
                           ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                           Istr.ins.Arg1    := aList[i+2].ins.Arg2;
                           if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+2].ins.Disp;

                           aList[i] := Istr;
                           AggiornaIstruzione(aList,i);
                           aList.Delete(i+1);
                           aList.Delete(i+1);
                           aList.Delete(i+1);
                           aList.Delete(i+1);
                           aList.TrimExcess;
                      end;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleNeg] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHoleMovOr2Arg(var aList : TList___DisAsm): Boolean;
var
  i    : Integer;
  Istr : TInsData;

begin
    i := 0;
    Result := True;
    try
      repeat
           // push reg1
           // xop       mov/TwoArg
           // pop reg1
           if (i+2) <= aList.Count - 1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+2].ins.InstID = INST_ID_POP) and
                    (FindFoldable(aList[i+1].ins.InstID,1,0,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and
                    (Compare2Arg(aList[i].ins.Arg1,aList[i+2].ins.Arg1)) and
                    ((aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) or
                     ((aList[i+1].ins.Arg1.Reg <> aList[i+2].ins.Arg1.Reg) and
                     (isSpReg(aList[i+1].ins.Arg1.Reg) = False)) ) and
                    ((aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK <> AF_REG) or (isSpReg(aList[i+1].ins.Arg2.Reg) = False))  then
                 begin
                      if (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and  (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg)) then
                      begin
                           Istr := aList[i+1] ;
                           if      aList[i].ins.Arg1.Size = SIZE_DWORD then Istr.ins.Disp.Value := aList[i+1].ins.Disp.Value - 4
                           else if aList[i].ins.Arg1.Size = SIZE_WORD  then Istr.ins.Disp.Value := aList[i+1].ins.Disp.Value - 2
                           else if aList[i].ins.Arg1.Size = SIZE_QWORD  then Istr.ins.Disp.Value := aList[i+1].ins.Disp.Value - 8 ;
                           // fix in caso non era presente
                           if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                           aList[i+1] := Istr;
                           AggiornaIstruzione(aList,i+1);
                      end;
                      // Aggiorna Istruzione
                      Istr := aList[i];
                      Istr.ins.Mnem   := aList[i+1].ins.Mnem ;
                      Istr.ins.InstID := aList[i+1].ins.InstID ;
                      Istr.ins.nArg   := aList[i+1].ins.nArg;
                      Istr.ins.Disp   := aList[i+1].ins.Disp;
                      ZeroMemory(@Istr.ins.Arg1,SizeOf(TArgument));
                      ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                      Istr.ins.Arg1   := aList[i+1].ins.Arg1;
                      Istr.ins.Arg2   := aList[i+1].ins.Arg2;
                      Istr.ins.Disp   := aList[i+1].ins.Disp ;
                      aList[i] := Istr;
                      AggiornaIstruzione(aList,i);
                      aList.Delete(i+1);
                      aList.Delete(i+1);
                      aList.TrimExcess;
                      Exit;
                 end;
           end;
           Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleMovOr2Arg] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;


function PeepHoleXSAX(var aList : TList___DisAsm): Boolean;
var
  i    : Integer;
  Istr : TInsData;
begin
    i := 0;
    Result := True;
    try
      repeat
             if (i+1) <= aList.Count - 1 then
             begin
                   if ( (aList[i].ins.InstID <>  INST_ID_XOR ) or (aList[i+1].ins.InstID <>  INST_ID_XOR) ) and
                      ( (aList[i].ins.InstID <>  INST_ID_SUB ) or (aList[i+1].ins.InstID <>  INST_ID_ADD)  ) and
                      ( (aList[i].ins.InstID <>  INST_ID_ADD ) or (aList[i+1].ins.InstID <>  INST_ID_SUB)  ) or
                      (aList[i].ins.Arg1.Flags and AF_TYPE_MASK    <>  AF_REG ) or
                      (aList[i].ins.Arg2.Flags and AF_TYPE_MASK    <> AF_IMM) or
                      (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK  <> AF_IMM) or
                      (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1) <> True) or
                      (Compare2Arg(aList[i].ins.Arg2,aList[i+1].ins.Arg2) <> True)then
                   begin
                         if (aList[i].ins.InstID =  INST_ID_XCHG) and (aList[i+1].ins.InstID =  INST_ID_XCHG)  and
                            (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                            ((Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg1)) or
                             (Compare2Arg(aList[i].ins.Arg1,aList[i+1].ins.Arg2)) )  and
                             // Modified by Max 16/01/2016 19:14:36 fb00000005
                            ((Compare2Arg(aList[i].ins.Arg2,aList[i+1].ins.Arg2)) or
                             (Compare2Arg(aList[i].ins.Arg2,aList[i+1].ins.Arg2)) ) then
                         begin
                             aList.Delete(i);
                             aList.Delete(i);
                             aList.TrimExcess;
                             exit
                         end;
                   end
                   else begin
                         // Modified by Max 08/01/2016 13:48:10 errori vari testare  rimuove Add ed sub necessari sulle operazione dello stack
                         if (isSpReg(aList[i].ins.Arg1.Reg) = False) and (aList[i+1].ins.Arg2.Imm.Value <> 4) and  (aList[i+1].ins.Arg2.Imm.Value <> 8 )then
                         begin
                               // Aggiorna Istruzione
                               Istr := aList[i];
                               Istr.ins.Mnem     := MNEM_MOV;
                               Istr.ins.InstID   := INST_ID_MOV;
                               Istr.ins.nArg     := 2;
                               Istr.ins.Arg2     := aList[i+1].ins.Arg1;
                               if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                               aList[i] :=  Istr;
                               AggiornaIstruzione(aList,i);
                               aList.Delete(i+1);
                               aList.TrimExcess;
                               Exit;
                         end;
                   end;
             end;
             Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleXSAX] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function PeepHoleVari(var aList : TList___DisAsm): Boolean;
var
  i,j,k: Integer;
  Istr : TInsData;
  found: Boolean;
begin
    i := 0;
    Result := True;
    try
      repeat

           if (i+1) <= aList.Count - 1 then
           begin
               // ver 2
               if (aList[i].ins.InstID   = INST_ID_PUSHAD) and (aList[i+1].ins.InstID = INST_ID_POPAD)then
               begin
                    aList.Delete(i);
                    aList.Delete(i);
                    aList.TrimExcess;
                     Exit;
               end;
               // ver 2  mov Reg1,Reg1
               if (aList[i].ins.InstID   = INST_ID_MOV) and
                   (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and  (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                   (aList[i].ins.Arg1.Reg = aList[i].ins.Arg2.Reg) and (aList[i].ins.Arg1.Size > SIZE_WORD) then
               begin
                    aList.Delete(i);
                    aList.TrimExcess;
                     Exit;
               end;
               // ver 2  xchg Reg1,Reg1
               if (aList[i].ins.InstID   = INST_ID_XCHG) and
                   (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) and  (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                   (aList[i].ins.Arg1.Reg = aList[i].ins.Arg2.Reg) then
               begin
                    aList.Delete(i);
                    aList.TrimExcess;
                    Exit;
               end;

               // ver 2
               // push reg1/costante
               // mov  [Esp/Sp],Costante  ----> push Costante
              if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID   = INST_ID_MOV) and
                  ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG)or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM))   and
                  (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM)  and
                  (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg) )and
                  (aList[i+1].ins.Arg1.Mem.IndexReg= REG_NIL )and
                  (aList[i+1].ins.Arg1.Mem.Scale   = 0 )      and
                  (aList[i+1].ins.Disp.Value       = 0 )      and
                  (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) then
               begin
                    // Aggiorna Istruzione
                    Istr := aList[i];
                    Istr.ins.Mnem  := MNEM_PUSH;
                    Istr.ins.InstID:= INST_ID_PUSH;
                    Istr.ins.nArg  := 1;
                    Istr.ins.Arg1  := aList[i+1].ins.Arg2;
                    aList[i] :=  Istr;
                    AggiornaIstruzione(aList,i);
                    aList.Delete(i+1);
                    aList.TrimExcess;
                    Exit;
               end;

               // ver 2
               // mov dword ds:[esp],0x76c76024
               // mov dword ds:[esp],esi  ----> push esi
              if (aList[i].ins.InstID   = INST_ID_MOV) and (aList[i+1].ins.InstID   = INST_ID_MOV) and
                  (isSpReg(aList[i].ins.Arg1.Mem.BaseReg) ) and
                  (aList[i].ins.Arg1.Mem.IndexReg= REG_NIL )and
                  (aList[i].ins.Arg1.Mem.Scale   = 0 )      and
                  (aList[i].ins.Disp.Value       = 0 )      and
                  (isSpReg(aList[i+1].ins.Arg1.Mem.BaseReg) ) and
                  (aList[i+1].ins.Arg1.Mem.IndexReg= REG_NIL )and
                  (aList[i+1].ins.Arg1.Mem.Scale   = 0 )      and
                  (aList[i+1].ins.Disp.Value       = 0 )      and
                  (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                  (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_REG) and
                  (aList[i].ins.Arg1.Mem.BaseReg <> aList[i+1].ins.Arg2.Reg) then
               begin
                    // Aggiorna Istruzione
                    Istr := aList[i];
                    Istr.ins.Mnem  := MNEM_PUSH;
                    Istr.ins.InstID:= INST_ID_PUSH;
                    Istr.ins.nArg  := 1;
                    ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                    Istr.ins.Arg1  := aList[i+1].ins.Arg2;
                    aList[i] :=  Istr;
                    AggiornaIstruzione(aList,i);
                    aList.Delete(i+1);
                    aList.TrimExcess;
                    Exit;
               end;
               //005B6497: push  Costante
               //005B64EC: one/twoOpcode [esp]
               //005B6515: one/twoOpcode [esp],Costante
               // ......
               //005B651D: mov [esp],reg          ----------->> push reg
               if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM)then
               begin
                    j     := 1;
                    found := False;
                    while (FindFoldable(aList[i+j].ins.InstID,0,1,1 )) and

                          ( (aList[i+j].ins.Arg2.Flags and AF_TYPE_MASK =  AF_IMM) or (aList[i+j].ins.Arg2.Flags = 0) )  and
                            (aList[i+j].ins.Arg1.Flags and AF_TYPE_MASK =  AF_MEM)and
                            (isSpReg(aList[i+j].ins.Arg1.Mem.BaseReg))  and
                            (aList[i+j].ins.Arg1.Mem.IndexReg= REG_NIL )and
                            (aList[i+j].ins.Arg1.Mem.Scale   = 0 )      and
                            (aList[i+j].ins.Disp.Value       = 0 )        do
                    begin
                         inc(j) ;
                    end;
                    if (j > 1)and (aList[i+j].ins.InstID   = INST_ID_MOV)  and  (aList[i+j].ins.Arg2.Flags and AF_TYPE_MASK =  AF_REG) and
                       (aList[i+j].ins.Arg1.Flags and AF_TYPE_MASK =  AF_MEM)and
                       (isSpReg(aList[i+j].ins.Arg1.Mem.BaseReg))  and
                       (aList[i+j].ins.Arg1.Mem.IndexReg= REG_NIL )and
                       (aList[i+j].ins.Arg1.Mem.Scale   = 0 )      and
                       (aList[i+j].ins.Disp.Value       = 0 )   then
                    begin
                         found := True;
                    end;
                    if found then
                    begin
                         Istr := alist[i];
                         istr.ins.Arg1 := aList[i+j].ins.Arg2;
                         aList[i] :=  Istr;
                         AggiornaIstruzione(aList,i);
                         for k := j downto 1 do
                         begin
                              Istr := alist[i+k];
                              Istr.ins.InstID := INST_ID_INVALID  ;
                              alist[i+k] := Istr;
                         end;
                         k := 0;
                         repeat
                                if aList[k].ins.InstID = INST_ID_INVALID then aList.Delete(k)
                                else  inc(k);
                         until k > aList.Count -1;
                         aList.TrimExcess;
                         Exit;
                    end;
               end;

           end;
           Inc(i);
      Until i >aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleVari] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;


function RemoveConstFoldingMov(var aList : TList___DisAsm): Boolean;
var
  i,j,k : Integer;
  v7    : UInt64;
  Istr  : TInsData;

begin
    i := aList.Count -1;
    Result := True;
    try
      // mov  reg1/memreg1, costante
      // xOp  reg1/memreg1,             1 o 2 opernadi   stesso reg  del primo mov
      // xop                            1 o 2 opernadi
      // ...                            1 o 2 opernadi
      while (  i >=  0 ) do
      begin
           if (i+1) <= aList.Count - 1 then
           begin

               if (aList[i].ins.InstID =  INST_ID_MOV) and (aList[i+1].ins.InstID  <>  INST_ID_MOV) and
                  ((aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG ) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM))  and
                  (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                  ( (Compare2Arg(aList[i+1].ins.Arg1,aList[i].ins.Arg1)) or
                    (IsEgualBaseReg(aList[i].ins.Arg1.Reg,aList[i+1].ins.Arg1.Reg) )
                  ) and
                  // Modified by Max 17/01/2016 22:33:06
                  (aList[i+1].ins.Disp.Value = aList[i].ins.Disp.Value) then
               begin
                   j :=  0 ;
                   while True do
                   begin
                        if (  j <  (aList.Count -1) -  i ) then
                        begin
                             if FindFoldable(aList[i+j+1].ins.InstID,0,1,1) then
                             begin
                                   if aList[i+j+1].ins.Arg1.Flags = aList[i].ins.Arg1.Flags then
                                   begin
                                        if (aList[i+j+1].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) or (aList[i+j+1].ins.Arg2.Flags = 0) then
                                        begin
                                             // Modified by Max 17/01/2016 22:33:06
                                             if (Compare2Arg(aList[i+j+1].ins.Arg1, aList[i].ins.Arg1)) or
                                                (IsEgualBaseReg(aList[i+j+1].ins.Arg1.Reg,aList[i].ins.Arg1.Reg)) then
                                             begin
                                                  inc( j ) ;
                                                  continue ;
                                             end;

                                        end;
                                   end;
                             end;
                        end;
                        break ;
                   end;
                   if j <> 0 then
                   begin
                       v7 := Int64(aList[i].ins.Arg2.imm.Value) ;
                       k :=  0 ;
                       while (  k <  j ) do
                       begin
                           // Modified by Max 16/01/2016 10:05:12
                           //if aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG then
                               EmuCPU     (aList[i+k+1].ins.InstID,
                                            aList[i+k+1].ins.Arg1.Size,
                                            //(aList[i].ins.Arg1.Reg shr 8) and $F,
                                            v7,
                                            aList[i+k+1].ins.Arg2.Imm);
                           {else
                               Operate     (aList[i+k+1].ins.InstID,
                                            aList[i].ins.Arg1.Size,
                                            v7,
                                            aList[i+k+1].ins.Arg2.Imm);}

                             Istr            := aList[i+k+1];
                             Istr.ins.InstID := INST_ID_INVALID ;
                             aList[i+k+1]    := Istr;
                             inc(k);
                       end;
                       Istr                    := aList[i];
                       Istr.ins.Arg2.imm.Value := v7;//FixSize(v7,aList[i].ins.Arg2.Flags and  $0F);
                       aList[i]                := Istr;
                       AggiornaIstruzione(aList,i);

                       k := 0;
                       repeat
                            if aList[k].ins.InstID = INST_ID_INVALID then aList.Delete(k)
                            else  inc(k);
                       until k > aList.Count - 1;
                       aList.TrimExcess;

                   end;
               end;
           end;
           dec(i);
      end ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[RemoveConstFoldingMov] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function RemoveConstFoldingPush(var aList : TList___DisAsm): Boolean;
var
  i,j,k    : Integer;
  nFlag    : Byte;
  Istr     : TInsData;
  TmpIstr  : TInsData;
  nCFold   : UInt64;
begin
    i := 0;
    Result := True;
    try
      repeat
           // Push reg                 push eax
           // Mov reg, Costante        mov eax,  5FBE3139h
           // xop
           // xop
           // ...
           if (i+1) <= aList.Count -1 then
           begin
                 if (aList[i].ins.InstID   = INST_ID_PUSH) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG ) and (aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG ) and
                    (aList[i+1].ins.Arg2.Flags  and AF_TYPE_MASK = AF_IMM) and
                    (IsEgualBaseReg(aList[i].ins.Arg1.Reg,aList[i+1].ins.Arg1.Reg))then
                 begin
                      nFlag := 0;
                      for j := 0 to  (aList.Count -1 ) -i  do
                      begin
                           if  ((i+j+2) > aList.Count -1) or (not FindFoldable(aList[i+j+2].ins.InstID,1,1,1)) then
                           begin
                               nFlag := 1;
                               Break;
                           end;

                           if (aList[i+j+2].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) or (aList[i+j+2].ins.InstID = INST_ID_MOV) or
                              (aList[i+j+2].ins.Arg2.Flags and AF_TYPE_MASK <> AF_IMM) and (aList[i+j+2].ins.Arg2.Flags <> 0)then
                           begin
                                if (aList[i+j+2].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) or (aList[i+j+2].ins.Arg2.Flags and AF_TYPE_MASK <> AF_REG) then
                                begin
                                     if (aList[i+j+2].ins.Arg1.Flags and AF_TYPE_MASK <> AF_MEM) or (aList[i+j+2].ins.Arg2.Flags and AF_TYPE_MASK <> AF_REG) then
                                     begin
                                          nFlag := 1;
                                          Break;
                                     end;
                                     if (IsEgualBaseReg(aList[i+j+3].ins.Arg1.Reg,aList[i+j+2].ins.Arg2.Reg)= False) or
                                        (aList[i+j+3].ins.InstID   <> INST_ID_POP) or (aList[i+j+3].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) or
                                        (aList[i+j+3].ins.Arg1.Reg <> aList[i].ins.Arg1.Reg )then
                                     begin
                                          nFlag := 1;
                                     end;
                                end
                                else begin
                                     // Modified by Max 19/01/2016 21:56:08
                                     if (aList[i+j+2].ins.Arg1.Reg = aList[i+1].ins.Arg1.Reg ) or ((i+j+3) > aList.Count -1) or
                                        (IsEgualBaseReg(aList[i+j+3].ins.Arg1.Reg,aList[i+j+2].ins.Arg2.Reg)= False) or
                                        (aList[i+j+3].ins.InstID   <> INST_ID_POP)or (aList[i+j+3].ins.Arg1.Flags and AF_TYPE_MASK <> AF_REG) or
                                        (aList[i+j+3].ins.Arg1.Reg <> aList[i].ins.Arg1.Reg )then
                                     begin
                                          nFlag := 1;
                                     end;
                                end;
                                Break;
                           end;
                           // Modified by Max 12/01/2016 21:05:59 provare per casi tipo
                           // push r12
                           // mov r12,0xaa
                           // add r12b,0xac
                           // mov byte [rcx+rax*4+0x8],r12b
                           // 00400021: pop r12
                           if (aList[i+j+2].ins.Arg1.Reg <>  aList[i+1].ins.Arg1.Reg) and
                              (IsEgualBaseReg(aList[i+j+2].ins.Arg1.Reg,aList[i+1].ins.Arg1.Reg)= False) then
                           begin
                                nFlag := 1;
                                Break;
                           end;

                      end;
                      if nFlag = 0 then
                      begin
                          Istr  := aList[i];

                          Istr.ins.nArg    := aList[i+j+2].ins.nArg;
                          Istr.ins.Mnem    := aList[i+j+2].ins.Mnem;
                          Istr.ins.InstID  := aList[i+j+2].ins.InstID;
                          Istr.ins.Arg1    := aList[i+j+2].ins.Arg1;
                          if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+j+2].ins.Disp ;

                          ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));

                          nCFold := aList[i+1].ins.Arg2.Imm.Value;
                          for k  := 0 to j - 1 do
                          begin
                               // Modified by Max 16/01/2016 10:06:48
                               EmuCPU(aList[i+k+2].ins.InstID,
                                       aList[i+k+2].ins.Arg1.Size,//aList[i+1].ins.Arg1.Size,
                                       nCFold,
                                       aList[i+k+2].ins.Arg2.Imm);

                               TmpIstr            :=  aList[i+k+2];
                               TmpIstr.ins.InstID := INST_ID_INVALID;
                               aList[i+k+2]       := TmpIstr;
                          end;
                          TmpIstr            :=  aList[i+1];
                          TmpIstr.ins.InstID := INST_ID_INVALID;
                          aList[i+1]         := TmpIstr;
                          //
                          TmpIstr            :=  aList[i+j+2];
                          TmpIstr.ins.InstID := INST_ID_INVALID;
                          aList[i+j+2]       := TmpIstr;
                          //
                          TmpIstr            :=  aList[i+j+3];
                          TmpIstr.ins.InstID := INST_ID_INVALID;
                          aList[i+j+3]       := TmpIstr;

                          Istr.ins.Arg2 := aList[i+1].ins.Arg2;
                          if Istr.ins.Arg2.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                          Istr.ins.Arg2.Imm.Value:= nCFold; // FixSize(v2,aList[i].ins.Arg2.Flags and  $0F);
                          if (Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) and (isSpReg(Istr.ins.Arg1.Mem.BaseReg)) then
                          begin
                               if      aList[i+j+3].ins.Arg1.Size = SIZE_DWORD then  Istr.ins.Disp.Value :=  Istr.ins.Disp.Value - 4
                               else if aList[i+j+3].ins.Arg1.Size = SIZE_WORD  then  Istr.ins.Disp.Value :=  Istr.ins.Disp.Value - 2
                               else if aList[i+j+3].ins.Arg1.Size = SIZE_QWORD then  Istr.ins.Disp.Value :=  Istr.ins.Disp.Value - 8 ;
                               // fix in caso non era presente
                               if (Istr.ins.Disp.Value <> 0) and (Istr.ins.Disp.Size = 0) then Istr.ins.Disp.Size := 1;
                          end;
                          // aggiorna istruzione
                          alist[i] := Istr;
                          AggiornaIstruzione(aList,i);
                          // Cancella istruzioni superflue
                          k := 0;
                          repeat
                                if aList[k].ins.InstID = INST_ID_INVALID then aList.Delete(k)
                                else  inc(k);
                          until k > aList.Count -1 ;
                          aList.TrimExcess;
                          Continue;
                      end;
                 end;
           end;
           Inc(i);
      Until i > aList.Count -1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[RemoveConstFoldingPush] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;

function RemoveConstFoldingVarie(var aList : TList___DisAsm): Boolean;
var
  i,j,k : Integer;
  v9,v10,v11,l,m,ii,n : Integer;
  v14,v6 : UInt64;
  Istr: TInsData;
begin
    i := aList.Count -1;
    Result := True;
    try
      while (  i >=  0 ) do
      begin
           // mov Reg, Costante
           // mov Regx/[Reg], CoStante
           // xOp Regx/[Reg],reg
           if (i+2) <= aList.Count -1 then
           begin
                 if  (aList[i].ins.InstID =  INST_ID_MOV) and  (aList [i+1].ins.InstID =  INST_ID_MOV)  and
                     (FindFoldable(aList[i+2].ins.InstID,0,0,1 )) and
                     (aList[i].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG) and
                     (aList[i].ins.Arg2.Flags and  AF_TYPE_MASK  =  AF_IMM) and
                     ((aList[i+1].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG ) or (aList [i+1].ins.Arg1.Flags and  AF_TYPE_MASK  =  AF_MEM )) and
                     (aList[i+1].ins.Arg2.Flags  and  AF_TYPE_MASK  =  AF_IMM ) and
                     ((aList[i+2].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG ) or (aList[i+2].ins.Arg1.Flags  and  AF_TYPE_MASK  =  AF_MEM )) and
                     (aList[i+2].ins.Arg2.Flags and AF_TYPE_MASK =  AF_REG ) and
                     (IsFoldableReg(aList[i].ins.Arg1.Reg )) and
                     (IsFoldableReg(aList [i+2].ins.Arg2.Reg )) and
                     (aList[i].ins.Arg1.Reg =  aList [i+2].ins.Arg2.Reg) and
                     (Compare2Arg(aList[i+1].ins.Arg1,aList[i+2].ins.Arg1)) then
                 begin
                      v14 := aList[i+1].ins.Arg2.Imm.Value ;
                      EmuCPU(aList[i+2].ins.InstID,
                              aList[i].ins.Arg1.Size,
                              v14,
                              aList[i].ins.Arg2.Imm) ;

                      Istr := alist[i];
                      Istr.ins.Arg1  := aList[i+1].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                      Istr.ins.Arg2.imm.Value  := v14;//FixSize(v14,aList[i].ins.Arg2.Flags and  $0F);
                      Istr.ins.Arg2.imm.Size   := aList[i].ins.Arg1.Size;
                      alist[i] := Istr;
                      AggiornaIstruzione(aList,i);

                      Istr := alist[i+1];
                      Istr.ins.InstID :=  INST_ID_INVALID ;
                      alist[i+1] := Istr;
                      Istr := alist[i+2];
                      Istr.ins.InstID :=  INST_ID_INVALID ;
                      alist[i+2] := Istr;
                      j :=  0 ;
                      while (j < 3) and (FindFoldable(aList[i+j+3].ins.InstID,0,0,1 )) and
                                        (aList[i+j+3].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG) and
                                        (IsFoldableReg(aList[i+j+3].ins.Arg1.Reg))  and
                                        (aList[i+j+3].ins.Arg1.Reg = aList[i+2].ins.Arg2.Reg ) do
                      begin
                           Istr := alist[i+j+3];
                           Istr.ins.InstID := INST_ID_INVALID  ;
                           alist[i+j+3] := Istr;
                           inc(j) ;
                      end;
                      k := 0;
                      repeat
                            if aList[k].ins.InstID = INST_ID_INVALID then aList.Delete(k)
                            else  inc(k);
                      until k > aList.Count -1;
                      aList.TrimExcess;
                      Exit;
                 end;
           end;

           // Mov Reg,Costante
           // Xop Reg1,Reg
           if (i+1) <= aList.Count -1 then
           begin
                 if (aList [i].ins.InstID = INST_ID_MOV) and (FindFoldable(aList[i+1].ins.InstID,1,0,1)) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG) and
                    (aList[i].ins.Arg2.Flags  and  AF_TYPE_MASK  =  AF_IMM ) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK =  AF_REG ) and
                    (IsFoldableReg(aList[i].ins.Arg1.Reg )) and
                    (IsFoldableReg(aList[i+1].ins.Arg2.Reg)) and
                    (aList[i].ins.Arg1.Reg =  aList [i+1].ins.Arg2.Reg ) and
                    (aList[i+1].ins.Arg1.Reg <>  aList[i+1].ins.Arg2.Reg ) then
                 begin
                      Istr := alist[i];
                      Istr.ins.Mnem    :=  aList[i+1].ins.Mnem ;
                      Istr.ins.InstID  :=  aList[i+1].ins.InstID ;
                      Istr.ins.nArg    :=  aList[i+1].ins.nArg;
                      Istr.ins.Arg1    :=  aList[i+1].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                      alist[i] := Istr;
                      AggiornaIstruzione(aList,i);

                      Istr := alist[i+1];
                      Istr.ins.InstID :=  INST_ID_INVALID ;
                      alist[i+1] := Istr;
                      k :=  0 ;
                      while (k <  3 ) and (FindFoldable(aList[i+k+2].ins.InstID,0,0,1)) and
                                             (aList[i+k+2].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG ) and
                                             (aList[i+k+2].ins.Arg2.Flags and  AF_TYPE_MASK  =  AF_MEM ) and
                                             (IsFoldableReg(aList[i+k+2].ins.Arg1.Reg)) and
                                             (aList[i+k+2].ins.Arg1.Reg =  aList[i+1].ins.Arg2.Reg) do
                      begin
                           Istr := alist[i+k+2];
                           Istr.ins.InstID := INST_ID_INVALID ;
                           alist[i+k+2] := Istr;
                           inc(k) ;
                      end;
                      k := 0;
                      repeat
                            if aList[k].ins.InstID = INST_ID_INVALID then aList.Delete(k)
                            else  inc(k);
                      until k > aList.Count -1;
                      aList.TrimExcess;
                      Exit;
                 end;

                 // mov  Reg, reg1/Costante
                 // mov  Regx ,Reg
                 if (aList[i].ins.InstID =  INST_ID_MOV) and (aList[i+1].ins.InstID = INST_ID_MOV) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG ) and
                    ((aList[i].ins.Arg2.Flags and AF_TYPE_MASK =  AF_REG ) or (aList[i].ins.Arg2.Flags and  AF_TYPE_MASK  =  AF_IMM)) and
                    (aList[i+1].ins.Arg2.Flags and AF_TYPE_MASK =  AF_REG) and  (IsFoldableReg(aList [i+1].ins.Arg2.Reg) ) and
                    (aList[i].ins.Arg1.Reg =  aList[i+1].ins.Arg2.Reg) then
                 begin
                      v11 :=  0 ;
                      v10 :=  i ;
                      v9 :=  0 ;
                      while (v10 <> - 1 ) and (v9 < 3 ) and
                           (aList[i-v9].ins.InstID =  INST_ID_MOV ) and
                           (aList[i-v9].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG ) and
                           (IsFoldableReg(aList[i-v9].ins.Arg1.Reg)) and
                           (aList[i-v9].ins.Arg1.Reg =  aList[i-v9+1].ins.Arg2.Reg) and
                           ((aList[i-v9].ins.Arg2.Flags and AF_TYPE_MASK =  AF_REG )  or (aList[i-v9].ins.Arg2.Flags and  AF_TYPE_MASK  =  AF_IMM)) do
                      begin
                          inc( v11 ) ;
                          if (aList[i-v9].ins.Arg2.Flags  and  AF_TYPE_MASK  =  AF_IMM ) then
                            break ;
                          inc( v9 ) ;
                          dec( v10 ) ;
                      end;
                      l :=  0 ;
                      while (  l <  v11 ) do
                      begin
                          Istr := alist[i-v11+l+2];
                          Istr.ins.InstID :=  INST_ID_INVALID ;
                          alist[i-v11+l+2] := Istr;
                          inc( l ) ;
                      end;
                      Istr := alist[i-v11+1];
                      Istr.ins.Arg1  := aList[i+1].ins.Arg1;
                      if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                   Istr.ins.Disp := aList[i+1].ins.Disp ;

                      aList[i-v11+1] := Istr;
                      AggiornaIstruzione(aList,i-v11+1);
                      k := 0;
                      repeat
                            if aList[k].ins.InstID = INST_ID_INVALID then aList.Delete(k)
                            else  inc(k);
                      until k > aList.Count -1;
                      aList.TrimExcess;
                      Exit;
                 end;

                 // mov Reg, Costante
                 // xop Reg,
                 if (aList[i].ins.InstID =  INST_ID_MOV) and (aList[i+1].ins.InstID   <>  INST_ID_MOV ) and
                    (aList[i].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG) and
                    (aList[i].ins.Arg2.Flags and  AF_TYPE_MASK  =  AF_IMM) and
                    (Compare2Arg(aList[i+1].ins.Arg1, aList[i].ins.Arg1)) then
                 begin
                      m :=  0 ;
                      while true do
                      begin
                          if   m <  ((aList.Count - 1) -  i ) then
                          begin
                               if FindFoldable(aList[i+m+1].ins.InstID ,0,1,1 ) then
                               begin
                                    if aList[i+m+1].ins.Arg1.Flags = aList[i].ins.Arg1.Flags then
                                    begin
                                         if ( (aList[i+m+1].ins.Arg2.Flags and  AF_TYPE_MASK  =  AF_IMM) or (aList[i+m+1].ins.Arg2.Flags = 0)  ) then
                                         begin
                                              if Compare2Arg(aList[i+m+1].ins.Arg1, aList[i].ins.Arg1) then
                                              begin
                                                   inc( m ) ;
                                                   continue ;
                                              end;
                                         end;
                                    end;
                               end;
                          end;
                          break ;
                      end;
                      // Modified by Max 02/01/2016 17:21:51
                      if  i+m+1 > aList.Count - 1 then
                      begin
                           dec(i);
                           Continue;
                      end;

                      if m <> 0 then
                      begin
                           if (FindFoldable(aList[i+m+1].ins.InstID,1,0,1))  and
                              (IsFoldableReg(aList[i+m+1].ins.Arg2.Reg)) and
                              ((aList[i+m+1].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG) or (aList[i+m+1].ins.Arg1.Flags  and AF_TYPE_MASK = AF_MEM  )) and
                              (aList[i+m+1].ins.Arg2.Flags and AF_TYPE_MASK =  AF_REG) and
                              (Compare2Arg(aList[i+m+1].ins.Arg2, aList[i].ins.Arg1)) then
                           begin
                               if (aList[i+m+1].ins.Arg1.Flags  and AF_TYPE_MASK <>  AF_REG) or
                                      (aList[i+m+1].ins.Arg1.Reg  <>  aList[i+m+1].ins.Arg2.Reg) then
                               begin
                                    v6 := aList[i].ins.Arg2.imm.Value;
                                    n :=  0 ;
                                    while (  n <  m ) do
                                    begin
                                         // Modified by Max 16/01/2016 10:09:00
                                        // if aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG then
                                            EmuCPU(aList[i+n+1].ins.InstID ,
                                                    aList[i+n+1].ins.Arg1.Size,
                                                    v6,
                                                    aList[i+n+1].ins.Arg2.imm) ;
                                         {else
                                            Operate(aList[i+n+1].ins.InstID ,
                                                    aList[i].ins.Arg1.Size,
                                                    v6,
                                                    aList[i+n+1].ins.Arg2.imm) ; }
                                         Istr := aList[i+n+1];
                                         Istr.ins.InstID :=  INST_ID_INVALID ;
                                         aList[i+n+1] := Istr;
                                         inc(n) ;
                                    end;
                                    Istr := aList[i];
                                    Istr.ins.Mnem          := aList[i+m+1].ins.Mnem ;
                                    Istr.ins.InstID        := aList[i+m+1].ins.InstID ;
                                    Istr.ins.nArg          := aList[i+m+1].ins.nArg;
                                    Istr.ins.Arg1          := aList[i+m+1].ins.Arg1;
                                    if Istr.ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM then
                                       Istr.ins.Disp := aList[i+m+1].ins.Disp ;

                                    Istr.ins.Arg2.imm.Value:= v6; //FixSize(v6,aList[i].ins.Arg2.Flags and  $0F);
                                    Istr.ins.Arg2.imm.Size :=  aList[i].ins.Arg1.Size;
                                    aList[i] := Istr;
                                    AggiornaIstruzione(aList,i);

                                    Istr := aList[i+m+1];
                                    Istr.ins.InstID:=  INST_ID_INVALID ;
                                    aList[i+m+1] := Istr;
                                    ii :=  0 ;
                                    while ((m+i+ii+2) <= aList.Count -1) and
                                    (ii <  3) and (FindFoldable(aList[m+i+ii+2].ins.InstID,0,0,1 )) and
                                                          (aList[m+i+ii+2].ins.Arg1.Flags and AF_TYPE_MASK =  AF_REG) and
                                                          (aList[m+i+ii+2].ins.Arg2.Flags  and AF_TYPE_MASK <>  AF_MEM) and
                                                          (IsFoldableReg(aList[m+i+ii+2].ins.Arg1.Reg)) and
                                                          (aList [m+i+ii+2].ins.Arg1.Reg  =  aList[i+m+1].ins.Arg2.Reg) do
                                    begin
                                         Istr := aList[m+i+ii+2];
                                         Istr.ins.InstID:= INST_ID_INVALID  ;
                                         aList[m+i+ii+2] := Istr;
                                         inc( ii ) ;
                                    end;
                                    k := 0;
                                    repeat
                                          if aList[k].ins.InstID = INST_ID_INVALID then aList.Delete(k)
                                          else  inc(k);
                                    until k > aList.Count -1;
                                    aList.TrimExcess;
                                    Exit;
                               end;
                           end;
                      end;
                 end;
           end;
           dec(i);
      end;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[RemoveConstFoldingVarie] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;



function PeepHoleIncDec(var aList : TList___DisAsm):Boolean ;
var
  i    : Integer;
  Istr : TInsData;
begin
    i := 0;
    Result := True;
    try
      repeat
          // inc o dec
          if (aList[i].ins.InstID   = INST_ID_ADD) then
          begin
               if ( (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                  (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                  ((aList[i].ins.Arg2.imm.Value = 1) or (aList[i].ins.Arg2.imm.Value = $FF) or  (aList[i].ins.Arg2.imm.Value = $FFFF) or (aList[i].ins.Arg2.imm.Value = $FFFFFFFF) or (aList[i].ins.Arg2.imm.Value = $FFFFFFFFFFFFFFFF))then
               begin
                    // Aggiorna Istruzione
                    Istr          := aList[i];
                    Istr.ins.nArg := 1;
                    ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                    if (aList[i].ins.Arg2.imm.Value = 1) then
                    begin
                        Istr.ins.Mnem   := MNEM_INC ;
                        Istr.ins.InstID := INST_ID_INC;
                    end
                    else if (Uint8(aList[i].ins.Arg2.imm.Value) = $FF)  then
                    begin
                         Istr.ins.Mnem     := MNEM_DEC;
                         Istr.ins.InstID   := INST_ID_DEC;
                    end;
                    aList[i] := Istr;
                    AggiornaIstruzione(aList,i)
               end;
          end
          else if (aList[i].ins.InstID   = INST_ID_SUB) then
          begin
               if ( (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_MEM) or (aList[i].ins.Arg1.Flags and AF_TYPE_MASK = AF_REG) ) and
                  (aList[i].ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) and
                  ((aList[i].ins.Arg2.imm.Value = 1) or (aList[i].ins.Arg2.imm.Value = $FF)or  (aList[i].ins.Arg2.imm.Value = $FFFF) or (aList[i].ins.Arg2.imm.Value = $FFFFFFFF)or (aList[i].ins.Arg2.imm.Value = $FFFFFFFFFFFFFFFF))then
               begin
                  // Aggiorna Istruzione
                    Istr          := aList[i];
                    Istr.ins.nArg := 1;
                    ZeroMemory(@Istr.ins.Arg2,SizeOf(TArgument));
                    if (Uint8(aList[i].ins.Arg2.imm.Value) = $FF)  then
                    begin
                        Istr.ins.Mnem   := MNEM_INC ;
                        Istr.ins.InstID := INST_ID_INC;
                    end
                    else if (aList[i].ins.Arg2.imm.Value =  1) then
                    begin
                         Istr.ins.Mnem     := MNEM_DEC;
                         Istr.ins.InstID   := INST_ID_DEC;
                    end;
                    aList[i] := Istr;
                    AggiornaIstruzione(aList,i)
               end;
          end;
          Inc(i);
      Until i > aList.Count - 1 ;
    except
       on e: Exception do
       begin
            MessageBoxA(0,'[PeepHoleIncDec] errore Processando Istruzione n° :'  ,'Errore',MB_OK)
       end;
    end;
end;


{ $DEFINE Test}
procedure ParsingV2(var aListAsm: TList___DisAsm);
var
  OldLen,
  NewLen,
  numFile,ntest : Integer;

  procedure stampafile(info: string);
   {$IFDEF Test}
  var
    s1,nomefile: string;
    numLinee   : Integer;
    i          : Integer;
    ffile      : TextFile;
    {$ENDIF}
  begin
          {$IFDEF Test}
          numLinee := 0;
          nomefile := 'Parsing_New_'+IntToStr(numFile)+'.txt';
          AssignFile(ffile,nomefile);
          try
            Rewrite(ffile);
            for i  := 0 to aListAsm.Count - 1 do
            begin
                 s1 := string(aListAsm[i].ins.InstStr);
                 Writeln(ffile,IntToHex(UInt64(aListAsm[i].VAddr),8)+ ':  '+  s1);
                 inc(numLinee);
            end;
          finally
            Writeln(ffile,'['+info+']  :'+'Numero Linee: '  +IntToStr(numLinee));
            inc(numFile);
            CloseFile(ffile);
          end;
          {$ENDIF}
  end;

begin
     NewLen := 0;
     OldLen := 1;

     ntest := $FFFF;

     numFile := 0;
     while (NewLen <> OldLen) do
     begin
          OldLen := aListAsm.Count;

          PeepHolePush(aListAsm);          if numFile < ntest  then stampafile('PeepHolePush: ');
          PeepHolePop(aListAsm);           if numFile < ntest then stampafile('PeepHolePop: ');
          PeepHoleAddSub_ESp4(aListAsm);   if numFile < ntest then stampafile('PeepHoleAddSub_ESp4: ');
          PeepHoleXchg(aListAsm);          if numFile < ntest then stampafile('PeepHoleXchg: ');
          PeepHoleMov(aListAsm) ;          if numFile < ntest then stampafile('PeepHoleMov: ');
          PeepHoleOneTwoArg(aListAsm) ;    if numFile < ntest then stampafile('PeepHoleOneTwoArg: ');
          PeepHoleRemoveAddSub(aListAsm) ; if numFile < ntest then stampafile('PeepHoleRemoveAddSub: ');
          PepHoleMemReg(aListAsm) ;        if numFile < ntest then stampafile('PepHoleMemReg: ');
          RemoveConstFoldingPush(aListAsm) ; if numFile < ntest then stampafile('RemoveConstFoldingPush: ');
          PeepHoleXchg_01(aListAsm);        if numFile < ntest then stampafile('PeepHoleXchg_01: ');
          PeepHoleNeg(aListAsm);           if numFile <  ntest then stampafile('PeepHoleNeg: ');
          PeepHoleIncDec(aListAsm);        if numFile < ntest then  stampafile('PeepHoleIncDec: ');

          stampafile('ciclo completo: ');

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue;
          PeepHoleMovOr2Arg(aListAsm) ;             if numFile < ntest then stampafile('PeepHoleMovOr2Arg: ');

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue;
          RemoveConstFoldingVarie(aListAsm) ;          if numFile < ntest then stampafile('RemoveConstFoldingVarie: ');

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue;
          RemoveConstFoldingMov(aListAsm) ;             if numFile < ntest then stampafile('RemoveConstFoldingMov: ');

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue;
          PeepHoleXSAX(aListAsm) ;                      if numFile < ntest then stampafile('PeepHoleXSAX: ');

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue ;
          PeepHoleVari(aListAsm);                       if numFile < ntest then stampafile('PeepHoleVari: ');

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue
          else  Break

     end;
end;

procedure ParsingCodeXJcc(var aListAsm: TList___DisAsm);
var
  OldLen, NewLen : Integer;

begin
     NewLen := 0;
     OldLen := 1;

     while NewLen <> OldLen do
     begin
          OldLen := aListAsm.Count;

          PeepHolePush(aListAsm);
          PeepHolePop(aListAsm);
          PeepHoleAddSub_ESp4(aListAsm);
          PeepHoleXchg(aListAsm);
          PeepHoleMov(aListAsm) ;
          PeepHoleOneTwoArg(aListAsm) ;
          PeepHoleRemoveAddSub(aListAsm) ;
          PepHoleMemReg(aListAsm) ;
          PeepHoleXchg_01(aListAsm);
          PeepHoleNeg(aListAsm);
          PeepHoleIncDEc(aListAsm);

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue;
          PeepHoleMovOr2Arg(aListAsm) ;

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue;
          RemoveConstFoldingVarie(aListAsm) ;

          NewLen := aListAsm.Count;
          if NewLen <>  OldLen then  Continue
          else  Break

     end;
end;

end.

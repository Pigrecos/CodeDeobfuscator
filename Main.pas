unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,System.Diagnostics,System.TimeSpan,
  System.Win.Crtl,System.StrUtils,
  System.Generics.Collections,
  NasmLib,
  Nasm_Def,
  CodeGen,
  uDisAsm,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Instructions,
  UnivDisasm.Cnsts.Regs,
  UnivDisasm.Disasm,
  Vcl.ComCtrls ;

type
  TfrmMain = class(TForm)
    OpenDialog1: TOpenDialog;
    btnAsm: TButton;
    edtAsmFile: TEdit;
    BitBtn1: TBitBtn;
    Memo1: TMemo;
    lblNumLinee: TLabel;
    chkBin: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    edtAddr: TEdit;
    Label3: TLabel;
    btnDeob: TBitBtn;
    Label4: TLabel;
    lvDeob: TListView;
    lvAsm: TListView;
    chkMultiB: TCheckBox;
    lblnulLineP: TLabel;
    rgTipoFile: TRadioGroup;
    btn2: TSpeedButton;
    procedure btnAsmClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnDeobClick(Sender: TObject);
    procedure lvAsmCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvDeobCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure rgTipoFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private

    procedure ParserLogMsg(Severity: Integer; strMsg: string);
    procedure bArrayToDisAsmNew(buffer: TArray<Byte>; VAAddr: UInt64;isX86:Boolean);
    procedure Load(sFile: string);


    { Private declarations }
  public
    { Public declarations }
  end;

const
    cDir_Binary = '..\Binary';
    cDir_TestASm= '..\TestAsm';
    cDir_TestExe= '..\TestExe';

var
  frmMain     : TfrmMain;
  LstD_ASMNew : TList___DisAsm;


implementation
      uses
        uCodeOptimize;


{$R *.dfm}

//***********************************************************
// Funzione utilizzata per allineare il testo
//
//***********************************************************
function AlignText(Buff: string; Aligment: byte; lLeft: Boolean = False):string;
(*********************************************************************)
var
 Len, r: dword;
begin
     Len := Length(Buff);
     if Len < Aligment then
      begin
         for r := 0 to Aligment - Len do
         begin
             if not lLeft then Buff := ' ' + Buff
             else              Buff := Buff + ' ';
         end;

      end;
      Result := Buff;

end;

procedure TfrmMain.ParserLogMsg(Severity : Integer; strMsg : string);
begin

     Memo1.Lines.Add(Format('Errore: %.8x  - %s',[Severity,strMsg]));
end;

procedure TfrmMain.BitBtn1Click(Sender: TObject);
var
  ffile : TextFile;
  i : Integer;
  s : string;
begin
     AssignFile(ffile,'Codice_deob.txt');
     Rewrite(ffile);
     try
        for i := 0 to lvDeob.Items.Count - 1 do
        begin
             s := lvDeob.Items[i].Caption;
             s := s +': '+ lvDeob.Items[i].SubItems[0];
             writeln(ffile,s);
        end;

     finally
       CloseFile(ffile);
     end;

end;


procedure TfrmMain.bArrayToDisAsmNew(buffer  : TArray<Byte> ;VAAddr: UInt64;isX86:Boolean );
var
  DisAsm  : TDisAsm;
  len     : Integer;
  Count   : Uint64;
  bx86    : Boolean;
  P       : PByte;
begin
     bx86 := isX86;

     if Assigned(LstD_ASMNew) then
       LstD_ASMNew.Clear
     else
       LstD_ASMNew := TList<TInsData>.Create;

    // if DisAsm = nil then
        DisAsm := TDisAsm.Create;
       { DisAsm.BinType := CPUX32;
        if not bx86 then
           DisAsm.BinType := CPUX64 ;}

     P     := @buffer[0] ;

     Count := 0;
     repeat
           DisAsm.DisAssemble(P,not bx86 ,VAAddr);

           LstD_ASMNew.Add(DisAsm.InsData);

           len := DisAsm.InsData.nsize;
               if len <= 0 then len := 1;

           P       := DisAsm.InsData.ins.NextInst;
           Count   := Count  + len;
           VAAddr  := VAAddr + len;
     until Count > High(buffer);
     LstD_ASMNew.TrimExcess;
end;

procedure TfrmMain.Load(sFile:string);
var
  buffer  : TOutCmdB;
  s        : string;
  binArray : TArray<Byte>;

  linea    : AnsiString;
  bits     : Byte;
  i,x,pBits,
  pComm    : Integer;
  VAAddr,
  startAdd : Int64;

  tx       : TStreamReader;
  BinFile  : TFileStream;
  lst      : TListItem;

  pCmdAsm  : array of AnsiString;
  numLinee : Int64;
  CG       : TCodeGen;
begin

     Memo1.Lines.Clear;
     bits  := 32;
     VAAddr:= 0;
     lblnulLineP.Caption := 'Linee: ';

     if      (rgTipoFile.ItemIndex = 0) then
     begin
         sFile := _ProcessaHandler(sFile,StrToInt64('$'+edtAddr.Text))
     end ;


     tx    := TStreamReader.Create(sFile);
     try
       ///file to array
       SetLength(pCmdAsm,0);

       while not tx.EndOfStream do
       begin
           linea := tx.ReadLine;
           linea := StringReplace(linea, #9, ' ', [rfReplaceAll]);


           pBits := Pos('bits',LowerCase(Linea));
           pComm := Pos(';',LowerCase(Linea));

           if ( pBits > 0) and( (pComm = 0) or ( (pComm > 0) and (pComm > pBits) ) ) then
           begin
                if      (Pos('64',LowerCase(Linea)) > 0) then bits := 64
                else if (Pos('32',LowerCase(Linea)) > 0) then bits := 32
                else if (Pos('16',LowerCase(Linea)) > 0) then bits := 16
                else begin
                     ShowMessage('errore nello specificare numero bits programma');
                     Exit;
                end;
                uCodeOptimize.fIs64 := bits = 64;

                SetLength(pCmdAsm,Length(pCmdAsm)+1);
                pCmdAsm[High(pCmdAsm)] := 'Bits '+ Inttostr(bits);
                Continue;
           end;

           if linea <> '' then
             linea := nasm_skip_spaces(@linea[1]);

           if Pos(';<',LowerCase(Linea)) > 0 then
             VAAddr  := StrToInt64('$'+ Copy(linea, 3, length(Linea)- 3));

           if (linea = '') then   Continue;
           if linea[1] = ';' then Continue;

           if Pos(': ',LowerCase(Linea)) > 0 then
           begin
                linea := Copy(linea, Pos(': ',LowerCase(Linea))+2, length(Linea)- Pos(': ',LowerCase(Linea)))
           end;

           SetLength(pCmdAsm,Length(pCmdAsm)+1);
           pCmdAsm[High(pCmdAsm)] := linea;
       end;
     finally
         tx.Free;
     end;

     if  VAAddr = 0 then VAAddr :=  $400000;
     startAdd := VAAddr;

     SetLength(binArray,0);

     CG           := TCodeGen.Create(MASM_SYNTAX);
    // Modified by Max 20/01/2016 21:30:42 test!!!!!!
     cg.Optimizing:= $FFFFFFFF;
     CG.OnMsgLog  := ParserLogMsg;

     lvAsm.Items.Clear;
     lvAsm.Items.BeginUpdate;
     try
       numLinee := 0;

       for x := 0 to  High(pCmdAsm) do
       begin
           linea := pCmdAsm[x];
           pBits := Pos('bits',LowerCase(Linea));

           if Pos('jmp',LowerCase(Linea)) > 0 then
              numLinee := numLinee;

           if  pBits > 0 then
           begin
                if      (Pos('64',LowerCase(Linea)) > 0) then bits := 64
                else if (Pos('32',LowerCase(Linea)) > 0) then bits := 32
                else if (Pos('16',LowerCase(Linea)) > 0) then bits := 16;
                Continue;
           end;

           Buffer := CG.Assemble_Cmd(VAAddr,@linea[1],bits);

           s := '';

           if Length(buffer) > 0 then
           begin
                VAAddr := VAAddr + Length(buffer);
                for i := 0 to High(buffer) do
                begin
                    SetLength(binArray,Length(binArray)+1);
                    binArray[High(binArray)] := buffer[i];

                    s := s +  IntToHex(buffer[i],2);
                end;
                inc(numLinee);

           end
           else s := ' errore';

           linea := TrimLeft(linea);
           if (linea <> '') and (linea[1] <> ';') then
           begin
                lst := lvAsm.Items.Add;
                lst.Caption := IntToHex(VAAddr-Length(buffer),8);
                lst.SubItems.Add(s) ;
                lst.SubItems.Add(linea)
           end;
       end;

     finally
       lvAsm.Items.EndUpdate;
       CG.Free;
     end;

     if chkBin.Checked then
     begin
         sFile := ChangeFileExt(sFile,'.bin') ;
         BinFile := TFileStream.Create(sFile,fmCreate);
         try
            BinFile.Write(binArray[0],Length(binArray));
         finally
            BinFile.Free;
         end;
     end;

     lblNumLinee.Caption := 'Linee: '+IntToStr(numLinee);

     bArrayToDisAsmNew(binArray ,UInt64(startAdd), bits = 32);

end;

procedure TfrmMain.btn2Click(Sender: TObject);
var
  sFile: String;
begin
     sFile := edtAsmFile.Text;
     if sFile = '' then Exit;

     if not FileExists(sFile) then Exit;

     lvDeob.Items.Clear;
     Load(sFile);

end;

procedure TfrmMain.btnAsmClick(Sender: TObject);
var
  sFile: String;
begin
     OpenDialog1.InitialDir := '..\'+ExtractFileDir(Application.ExeName);

     if       rgTipoFile.ItemIndex = 0 then  OpenDialog1.Filter := 'Exe files (*.Exe)|*.EXE|Dll files (*.Dll)|*.DLL'
     else if  rgTipoFile.ItemIndex = 1 then  OpenDialog1.Filter := 'Text files (*.Txt)|*.TXT|Asm files (*.Asm)|*.ASM'
     else if  rgTipoFile.ItemIndex = 2 then  OpenDialog1.Filter := 'Bin files (*.Bin)|*.BIN';

     if OpenDialog1.Execute then
     begin
          sfile := OpenDialog1.FileName;
     end;
     edtAsmFile.Text := sfile;

     if sFile = '' then Exit;

     lvDeob.Items.Clear;
     Load(sFile);
end;


procedure TfrmMain.rgTipoFileClick(Sender: TObject);
begin
     if (rgTipoFile.ItemIndex = 0) then
     begin
          edtAddr.Enabled := True;
          chkBin.Enabled  := False;
     end else
     begin
          edtAddr.Enabled := False;
          chkBin.Enabled  := True;
          if rgTipoFile.ItemIndex = 2 then
                chkBin.Enabled  := False;
     end;

end;

procedure TfrmMain.btnDeobClick(Sender: TObject);
var
 i        : Integer;
 s1       : string;
 lst      : TListItem;
 numLinea : Integer;

begin
     ParsingV2(LstD_ASMNew);


     numLinea := 0;

     lvDeob.Items.Clear;
     lvDeob.Items.BeginUpdate;
     try
        for i  := 0 to LstD_ASMNew.Count - 1 do
        begin
             s1 := LstD_ASMNew[i].ins.InstStr;

             lst := lvDeob.Items.Add;
             lst.Caption := IntToHex(UInt64(LstD_ASMNew[i].VAddr),8);
             lst.SubItems.Add(s1) ;
             inc(numLinea)
        end;
     finally
         lvDeob.Items.EndUpdate ;
         lblnulLineP.Caption := 'Linee: '+IntToStr(numLinea);
     end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
     rgTipoFileClick(Self);
end;

procedure TfrmMain.lvAsmCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
    if Item.Index < Sender.Items.Count -1 then
    begin

      if (Pos('j', Item.SubItems[1]) > 0 ) or (Pos('call', LowerCase(Item.SubItems[1])) > 0) then
      begin
           Sender.Canvas.Font.Color := clRed;
          // Sender.Canvas.Brush.Color := clSkyBlue;
          // Sender.Canvas.FillRect(Item.DisplayRect(drBounds));
      end;
    end;
end;

procedure TfrmMain.lvDeobCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
    if Item.Index < Sender.Items.Count -1 then
    begin
      if (Pos('j', Item.SubItems[0]) > 0 ) or (Pos('call', LowerCase(Item.SubItems[0])) > 0) then
      begin
           Sender.Canvas.Font.Color := clRed;
          // Sender.Canvas.Brush.Color := clSkyBlue;
          // Sender.Canvas.FillRect(Item.DisplayRect(drBounds));
      end;
    end;
end;

end.

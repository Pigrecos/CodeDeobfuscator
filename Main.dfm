object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Deobfuscator '
  ClientHeight = 631
  ClientWidth = 1002
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblNumLinee: TLabel
    Left = 516
    Top = 604
    Width = 44
    Height = 14
    Caption = 'Linee : '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 516
    Top = 8
    Width = 173
    Height = 13
    Caption = 'Seleziona File da Decompilare :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 23
    Height = 13
    Caption = 'Log:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 727
    Top = 76
    Width = 93
    Height = 13
    Caption = 'Start Addr:(VA) :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 108
    Width = 481
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Codice Deob:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblnulLineP: TLabel
    Left = 8
    Top = 604
    Width = 44
    Height = 14
    Caption = 'Linee : '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btn2: TSpeedButton
    Left = 963
    Top = 24
    Width = 34
    Height = 21
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000000000000000
      00000000000000000001000000050000000D0000000F00000005000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000020000000A170F093B72472CC3A1643DFF00000010000000000000
      0000000000000000000000000000000000000000000000000000000000010000
      00060604031955382594AB754CF9D5A670FFA56942FF00000022000000110000
      000D000000080000000300000000000000000000000000000000000000053525
      195F9A6A48E5CB9C6DFFE8C089FFE8C189FFA86F48FF88512EFC724125E4532E
      17B92F1A0D7B090502270000000700000001000000000000000000000008BD8D
      64FFECD4B3FFF1D8ACFFEBCA95FFEAC68FFFE7CBA4FFF2DBB9FFE1C39EFFC59D
      74FFA16F47FF512C16BE1B0E065200000008000000010000000000000004392C
      1F59A8815EE2DBBB97FFF3DFBDFFF0D4A6FFC69C74FFBB8B62FFC49870FFD3AE
      89FFE0C29BFFB78A60FF5C351CCB120A043E0000000400000000000000010000
      000407050415614B378CC39D76F9E7CEADFFC1936AFF060503242B21174E644A
      359AB48F6BECD7B891FFAB7E56FF3C2110970000000A00000000000000020000
      00080000000B0000000F1A140F328A6C4FBBC79A70FF0000000C000000040000
      000C56412F86D2AD87FFD8BD9DFF60351CD50000000F00000000000000062379
      49F9237B4AFF247A4AFF030A06220000000900000009000000060000000A0000
      000B100C0927C5996FFFC4986EFFAE7C57F90000000D0000000000000005246E
      49CE5BC49EFF308E62FF0D311C7E00000008000000030000000A247B4AFF0A37
      1ABF020B053900000015000000110000000D000000030000000000000003194B
      32885BB691FF6DC3A6FF2F7A54EA103B229107190F470104021C267F4DFF34A5
      78FF17633BF9092B169101030218000000060000000100000000000000010919
      112F2C7451C07AC8AAFF8BD7BFFF56A886FF33875DFF207345FF3A976AFF6ADA
      BBFF4BC89FFF30966AFF145530E5061D0F5F0000000600000000000000000000
      00020E251A41266C4BB263BB96FF8AD3BAFFA8E7D6FFBAF3E7FFA4E4D2FF89EA
      D5FF77E1C6FF75DDBDFF80D7BCFF278551FF0000000A00000000000000000000
      000000000002050D091A17412D6A276D4CAD338E64DF3AA06FFB3AA271FF9EF0
      E0FFB4F1E5FF6EC5A3FF348B60E1102E1F5A0000000500000000000000000000
      000000000000000000010000000200000004000000050000000C3EA977FF9AE0
      C9FF4CAC82F81E52398902060415000000040000000100000000000000000000
      000000000000000000000000000000000000000000000000000541AF7DFF2D79
      56B70816102B0000000500000001000000000000000000000000}
    OnClick = btn2Click
  end
  object btnAsm: TButton
    Left = 923
    Top = 24
    Width = 34
    Height = 21
    Caption = '...'
    TabOrder = 0
    OnClick = btnAsmClick
  end
  object edtAsmFile: TEdit
    Left = 516
    Top = 24
    Width = 401
    Height = 21
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 414
    Top = 604
    Width = 75
    Height = 25
    Caption = 'Salva Codice'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object Memo1: TMemo
    Left = 37
    Top = 3
    Width = 452
    Height = 71
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object chkBin: TCheckBox
    Left = 644
    Top = 95
    Width = 97
    Height = 17
    Caption = 'Cre Bin File'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
  end
  object edtAddr: TEdit
    Left = 827
    Top = 68
    Width = 90
    Height = 21
    Alignment = taCenter
    CharCase = ecUpperCase
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    Text = '005B5E96'
  end
  object btnDeob: TBitBtn
    Left = 896
    Top = 604
    Width = 101
    Height = 25
    Caption = 'Parsing Codice'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = btnDeobClick
  end
  object lvDeob: TListView
    Left = 8
    Top = 124
    Width = 481
    Height = 474
    Color = clCream
    Columns = <
      item
        Caption = 'Addr.'
        Width = 100
      end
      item
        Caption = 'Cmd'
        Width = 350
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    RowSelect = True
    ParentFont = False
    TabOrder = 7
    ViewStyle = vsReport
    OnCustomDrawItem = lvDeobCustomDrawItem
  end
  object lvAsm: TListView
    Left = 516
    Top = 124
    Width = 481
    Height = 474
    Color = clCream
    Columns = <
      item
        Caption = 'Addr.'
        Width = 100
      end
      item
        Caption = 'Bytes'
        Width = 170
      end
      item
        Caption = 'Cmd'
        Width = 210
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    RowSelect = True
    ParentFont = False
    TabOrder = 8
    ViewStyle = vsReport
    OnCustomDrawItem = lvAsmCustomDrawItem
  end
  object chkMultiB: TCheckBox
    Left = 516
    Top = 95
    Width = 89
    Height = 17
    Caption = 'MultiBranch'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbChecked
    TabOrder = 9
  end
  object rgTipoFile: TRadioGroup
    Left = 516
    Top = 51
    Width = 205
    Height = 38
    Caption = 'Tipo File'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Exe/Dll'
      'Asm/Txt')
    TabOrder = 10
    OnClick = rgTipoFileClick
  end
  object OpenDialog1: TOpenDialog
    InitialDir = 'D:\Applicazioni\CodeBlocks\test_pro\Nasm_Test\bin\debug'
    Left = 280
  end
end

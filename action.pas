unit action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCAction = class
  private
    FName: string;
    FAct: string;
    FPos: Variant;
    FSize: Variant;
    FMsg: Variant;
  public
    constructor Create(AName, AAct: string; APos: Variant = Null);
    property Name: string read FName write FName;
    property Act: string read FAct write FAct;
    property Pos: Variant read FPos write FPos;
    property Size: Variant read FSize write FSize;
    property Msg: Variant read FMsg write FMsg;
  end;

implementation

constructor TCAction.Create(AName, AAct: string; APos: Variant);
begin
  FName := AName;
  FAct := AAct;
  FPos := APos;
  FSize := Null;
  FMsg := Null;
end;

end.

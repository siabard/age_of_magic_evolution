unit action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, game_types;

type
  TAction = class
  private
    FName: string;
    FAct: string;
    FPos: RPos;
    FSize: RSize;
    FMsg: string;
  public
    constructor Create(AName, AAct: string; APos: RPos);
    property Name: string read FName write FName;
    property Act: string read FAct write FAct;
    property Pos: RPos read FPos write FPos;
    property Size: RSize read FSize write FSize;
    property Msg: string read FMsg write FMsg;
  end;

implementation

constructor TAction.Create(AName, AAct: string; APos: RPos);
begin
  FName := AName;
  FAct := AAct;
  FPos := APos;
  FSize.RW := 0;
  FSize.RH := 0;
  FMsg := '';
end;

end.

unit LogUtil;
{$Define GAMELOG}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

Procedure LogDebug(msg: String);

implementation

Procedure LogDebug(msg: String);
begin
  {$IFDEF GAMELOG}
  WriteLn(msg);
  {$ENDIF}
end;

end.


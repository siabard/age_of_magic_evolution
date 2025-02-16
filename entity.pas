unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LogUtil, component;

type

  TEntity = class
  private
    FPosition: TPositionComponent;
    FIsAlive: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property position: TPositionComponent read FPosition write FPosition;
    property IsLive: boolean read FIsAlive write FIsAlive;
  end;



implementation

constructor TEntity.Create;
begin
  FIsAlive := True;
end;

destructor TEntity.Destroy;
begin
  if Assigned(FPosition) then
  begin
    LogDebug('Remove Position Component');
    FreeAndNil(FPosition);

  end;
end;


end.
